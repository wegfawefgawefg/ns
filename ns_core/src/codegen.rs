// ns_project_root/ns_core/src/codegen.rs
use crate::ast::{
    BeginNode, CallNode, Expression, FnNode, GetNode, IfNode, LambdaNode, LetNode, ProgramNode,
    QuotedData, SetNode, StaticNode, StructDefNode, TopLevelForm, UseNode, WhileNode,
};
use crate::error::NsError;
use crate::opcode::{BytecodeInstruction, OpCode, StringOrPc};
use crate::value::Value;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

// Information about symbols known at compile time
#[derive(Debug, Clone)]
enum CompileTimeSymbolInfo {
    Local { depth: usize, index: usize },
    Global,
    Function { label: String, arity: usize },
    Struct { fields: Vec<Rc<String>> },
}

// Information specific to the current function being compiled
#[derive(Debug, Default)]
struct FunctionCompilationContext {
    pub label_count: usize,
    pub current_function_label_prefix: String,
}

pub struct CodeGenerator {
    bytecode_segments: HashMap<String, Vec<BytecodeInstruction>>,
    current_segment_name: String,
    func_comp_context: FunctionCompilationContext,
    compile_time_scopes: Vec<HashMap<Rc<String>, CompileTimeSymbolInfo>>,
    struct_definitions: HashMap<Rc<String>, Vec<Rc<String>>>,
    function_info: HashMap<Rc<String>, (String, usize)>,
    discovered_dependencies: HashSet<String>,
    module_name: Rc<String>,
}

impl CodeGenerator {
    pub fn new(module_name: String) -> Self {
        let main_segment_name = format!("module_{}_main", module_name.replace('-', "_"));
        let mut segments = HashMap::new();
        segments.insert(main_segment_name.clone(), Vec::new());

        CodeGenerator {
            bytecode_segments: segments,
            current_segment_name: main_segment_name.clone(),
            func_comp_context: FunctionCompilationContext {
                label_count: 0,
                current_function_label_prefix: main_segment_name,
            },
            compile_time_scopes: vec![HashMap::new()],
            struct_definitions: HashMap::new(),
            function_info: HashMap::new(),
            discovered_dependencies: HashSet::new(),
            module_name: Rc::new(module_name),
        }
    }

    fn current_bytecode_mut(&mut self) -> &mut Vec<BytecodeInstruction> {
        self.bytecode_segments
            .get_mut(&self.current_segment_name)
            .expect("Internal error: Current bytecode segment should always exist")
    }

    fn emit(&mut self, instruction: BytecodeInstruction) {
        self.current_bytecode_mut().push(instruction);
    }

    fn emit_op(&mut self, opcode: OpCode) {
        self.current_bytecode_mut()
            .push(BytecodeInstruction::Operation(opcode));
    }

    fn new_label_for_current_context(&mut self, basic_name: &str) -> String {
        self.func_comp_context.label_count += 1;
        format!(
            "{}_L{}_{}",
            self.func_comp_context.current_function_label_prefix,
            self.func_comp_context.label_count,
            basic_name
        )
    }

    fn enter_scope(&mut self) {
        self.compile_time_scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        if self.compile_time_scopes.len() > 1 {
            self.compile_time_scopes.pop();
        } else {
            eprintln!("Warning: CodeGenerator attempted to pop global scope.");
        }
    }

    fn define_local(&mut self, name: Rc<String>) -> Result<(), NsError> {
        let depth = self.compile_time_scopes.len().saturating_sub(1);
        if let Some(current_scope) = self.compile_time_scopes.last_mut() {
            let index = current_scope.len();
            current_scope.insert(name.clone(), CompileTimeSymbolInfo::Local { depth, index });
            Ok(())
        } else {
            Err(NsError::Codegen(
                "Cannot define local: no current scope (this should be impossible)".to_string(),
            ))
        }
    }

    fn define_global(&mut self, name: Rc<String>, info: CompileTimeSymbolInfo) {
        if let Some(global_scope) = self.compile_time_scopes.first_mut() {
            global_scope.insert(name, info);
        } else {
            eprintln!(
                "Critical Error: Global scope missing in CodeGenerator during define_global for {}",
                name
            );
        }
    }

    fn find_symbol(&self, name: &Rc<String>) -> Option<CompileTimeSymbolInfo> {
        for (_depth_offset, scope) in self.compile_time_scopes.iter().rev().enumerate() {
            if let Some(info) = scope.get(name) {
                return Some(info.clone());
            }
        }
        if let Some((label, arity)) = self.function_info.get(name) {
            return Some(CompileTimeSymbolInfo::Function {
                label: label.clone(),
                arity: *arity,
            });
        }
        if let Some(fields) = self.struct_definitions.get(name) {
            return Some(CompileTimeSymbolInfo::Struct {
                fields: fields.clone(),
            });
        }
        None
    }

    pub fn generate_program(
        mut self,
        program_node: ProgramNode,
    ) -> Result<(HashMap<String, Vec<BytecodeInstruction>>, HashSet<String>), NsError> {
        let mut use_forms = Vec::new();
        let mut other_forms = Vec::new();

        for form in program_node.forms {
            match form {
                TopLevelForm::Use(u_node) => use_forms.push(u_node),
                _ => other_forms.push(form),
            }
        }

        for use_node in use_forms {
            self.generate_use_node(use_node)?;
        }

        let num_other_forms = other_forms.len();
        for (i, form) in other_forms.into_iter().enumerate() {
            let is_last_exec_form = i == num_other_forms - 1;
            self.generate_top_level_form(form, is_last_exec_form)?;
        }

        let main_bytecode_segment_name =
            format!("module_{}_main", self.module_name.replace('-', "_"));
        let main_bytecode = self
            .bytecode_segments
            .get_mut(&main_bytecode_segment_name)
            .expect("Main segment must exist");

        if main_bytecode.is_empty()
            || !matches!(
                main_bytecode.last(),
                Some(BytecodeInstruction::Operation(OpCode::Halt))
                    | Some(BytecodeInstruction::Operation(OpCode::Return))
                    | Some(BytecodeInstruction::Jump(_))
            )
        {
            main_bytecode.push(BytecodeInstruction::Operation(OpCode::Halt));
        }

        Ok((self.bytecode_segments, self.discovered_dependencies))
    }

    fn generate_top_level_form(
        &mut self,
        form: TopLevelForm,
        is_last_exec_form: bool,
    ) -> Result<(), NsError> {
        let start_len_before_expr = self.current_bytecode_mut().len();
        match form {
            TopLevelForm::StaticDef(node) => self.generate_static_node(node)?,
            TopLevelForm::FnDef(node) => self.generate_fn_node(node)?,
            TopLevelForm::StructDef(node) => self.generate_struct_def_node(node)?,
            TopLevelForm::Use(_) => { /* Already handled */ }
            TopLevelForm::Expression(expr) => {
                self.generate_expression(expr)?;
                if !is_last_exec_form {
                    let end_len_after_expr = self.current_bytecode_mut().len();
                    if end_len_after_expr > start_len_before_expr {
                        let last_instr_opt = self.current_bytecode_mut().last();
                        if let Some(last_instr) = last_instr_opt {
                            match last_instr {
                                BytecodeInstruction::StoreGlobal(_)
                                | BytecodeInstruction::StoreLocal(_)
                                | BytecodeInstruction::Jump(_)
                                | BytecodeInstruction::Operation(OpCode::Return)
                                | BytecodeInstruction::Operation(OpCode::Halt)
                                | BytecodeInstruction::Operation(OpCode::Print) => {}
                                _ => {
                                    self.emit_op(OpCode::Pop);
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn generate_expression(&mut self, expr: Expression) -> Result<(), NsError> {
        match expr {
            Expression::Number(n) => self.emit(BytecodeInstruction::Push(Value::Number(n))),
            Expression::String(s) => self.emit(BytecodeInstruction::Push(Value::String(s.clone()))),
            Expression::Boolean(b) => self.emit(BytecodeInstruction::Push(Value::Boolean(b))),
            Expression::NoneLiteral => self.emit(BytecodeInstruction::Push(Value::NoneValue)),
            Expression::Symbol(name) => match self.find_symbol(&name) {
                Some(CompileTimeSymbolInfo::Local { index, .. }) => {
                    self.emit(BytecodeInstruction::LoadLocal(index));
                }
                Some(CompileTimeSymbolInfo::Global)
                | Some(CompileTimeSymbolInfo::Function { .. })
                | Some(CompileTimeSymbolInfo::Struct { .. })
                | None => {
                    self.emit(BytecodeInstruction::LoadGlobal(name.to_string()));
                }
            },
            Expression::Quote(data_box) => self.generate_quote_node(*data_box)?,
            Expression::Call(call_node_box) => self.generate_call_node(*call_node_box)?,
            Expression::If(if_node_box) => self.generate_if_node(*if_node_box)?,
            Expression::Let(let_node_box) => self.generate_let_node(*let_node_box)?,
            Expression::Lambda(lambda_node_box) => self.generate_lambda_node(*lambda_node_box)?,
            Expression::Get(get_node_box) => {
                let node = *get_node_box;
                self.generate_expression(node.instance)?;
                self.emit(BytecodeInstruction::GetField(node.field_name.to_string()));
            }
            Expression::Set(set_node_box) => {
                let node = *set_node_box;
                self.generate_expression(node.instance)?;
                self.generate_expression(node.value)?;
                self.emit(BytecodeInstruction::SetField(node.field_name.to_string()));
            }
            Expression::While(while_node_box) => {
                // Logic for While
                let node = *while_node_box;
                let start_label = self.new_label_for_current_context("while_start");
                let end_label = self.new_label_for_current_context("while_end");

                self.emit(BytecodeInstruction::LabelDef(start_label.clone()));
                self.generate_expression(node.condition)?;
                self.emit(BytecodeInstruction::JumpIfFalse(StringOrPc::Label(
                    end_label.clone(),
                )));

                if node.body.is_empty() {
                    // No specific bytecode needed for an empty body before the loop jump.
                } else {
                    for body_expr in node.body {
                        self.generate_expression(body_expr)?;
                        self.emit_op(OpCode::Pop); // Results of body expressions are popped.
                    }
                }
                self.emit(BytecodeInstruction::Jump(StringOrPc::Label(start_label))); // Jump back to condition.
                self.emit(BytecodeInstruction::LabelDef(end_label));
                self.emit(BytecodeInstruction::Push(Value::NoneValue)); // While loop evaluates to 'none'.
            }
            Expression::Begin(begin_node_box) => {
                let node = *begin_node_box;
                if node.expressions.is_empty() {
                    self.emit(BytecodeInstruction::Push(Value::NoneValue));
                } else {
                    let num_exprs = node.expressions.len();
                    for (i, inner_expr) in node.expressions.into_iter().enumerate() {
                        self.generate_expression(inner_expr)?;
                        if i < num_exprs - 1 {
                            self.emit_op(OpCode::Pop);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn generate_static_node(&mut self, node: StaticNode) -> Result<(), NsError> {
        self.generate_expression(node.value)?;
        self.emit(BytecodeInstruction::StoreGlobal(node.name.to_string()));
        self.define_global(node.name.clone(), CompileTimeSymbolInfo::Global);
        Ok(())
    }

    fn generate_struct_def_node(&mut self, node: StructDefNode) -> Result<(), NsError> {
        if self.struct_definitions.contains_key(&node.name) {
            return Err(NsError::Codegen(format!(
                "Struct '{}' already defined.",
                node.name
            )));
        }
        self.struct_definitions
            .insert(node.name.clone(), node.fields.clone());
        self.define_global(
            node.name.clone(),
            CompileTimeSymbolInfo::Struct {
                fields: node.fields,
            },
        );
        Ok(())
    }

    fn generate_use_node(&mut self, node: UseNode) -> Result<(), NsError> {
        self.discovered_dependencies
            .insert(node.module_name.to_string());
        println!(
            "Codegen: (use {} ...) dependency recorded. Symbol importing not yet implemented.",
            node.module_name
        );
        Ok(())
    }

    fn generate_fn_node(&mut self, node: FnNode) -> Result<(), NsError> {
        let fn_name = node.name.clone();
        let arity = node.params.len();
        let sanitized_fn_name_for_label =
            fn_name.replace(|c: char| !c.is_alphanumeric() && c != '_', "_");
        let body_label = format!(
            "fn_body_{}_{}",
            self.module_name.replace('-', "_"),
            sanitized_fn_name_for_label
        );

        self.emit(BytecodeInstruction::MakeClosure {
            label: body_label.clone(),
        });
        self.emit(BytecodeInstruction::StoreGlobal(fn_name.to_string()));

        self.function_info
            .insert(fn_name.clone(), (body_label.clone(), arity));
        self.define_global(
            fn_name.clone(),
            CompileTimeSymbolInfo::Function {
                label: body_label.clone(),
                arity,
            },
        );

        let prev_segment_name = self.current_segment_name.clone();
        let prev_func_comp_context = std::mem::replace(
            &mut self.func_comp_context,
            FunctionCompilationContext {
                label_count: 0,
                current_function_label_prefix: body_label.clone(),
            },
        );
        self.current_segment_name = body_label.clone();
        if !self
            .bytecode_segments
            .contains_key(&self.current_segment_name)
        {
            self.bytecode_segments
                .insert(self.current_segment_name.clone(), Vec::new());
        }

        self.enter_scope();
        for (idx, param_name) in node.params.iter().enumerate() {
            self.emit(BytecodeInstruction::StoreLocal(idx));
            self.define_local(param_name.clone())?;
        }

        let num_body_exprs = node.body.len();
        if num_body_exprs == 0 {
            self.exit_scope();
            self.current_segment_name = prev_segment_name;
            self.func_comp_context = prev_func_comp_context;
            return Err(NsError::Codegen(format!(
                "Function body for '{}' cannot be empty.",
                fn_name
            )));
        }
        for (i, expr) in node.body.into_iter().enumerate() {
            self.generate_expression(expr)?;
            if i < num_body_exprs - 1 {
                self.emit_op(OpCode::Pop);
            }
        }

        self.emit_op(OpCode::Return);
        self.exit_scope();

        self.current_segment_name = prev_segment_name;
        self.func_comp_context = prev_func_comp_context;
        Ok(())
    }

    fn generate_lambda_node(&mut self, node: LambdaNode) -> Result<(), NsError> {
        let lambda_id = self.func_comp_context.label_count;
        self.func_comp_context.label_count += 1;
        let body_label = format!(
            "{}_lambda_body_{}",
            self.func_comp_context.current_function_label_prefix, lambda_id
        );

        self.emit(BytecodeInstruction::MakeClosure {
            label: body_label.clone(),
        });

        let prev_segment_name = self.current_segment_name.clone();
        let prev_func_comp_context = std::mem::replace(
            &mut self.func_comp_context,
            FunctionCompilationContext {
                label_count: 0,
                current_function_label_prefix: body_label.clone(),
            },
        );
        self.current_segment_name = body_label.clone();
        if !self
            .bytecode_segments
            .contains_key(&self.current_segment_name)
        {
            self.bytecode_segments
                .insert(self.current_segment_name.clone(), Vec::new());
        }

        self.enter_scope();
        for (idx, param_name) in node.params.iter().enumerate() {
            self.emit(BytecodeInstruction::StoreLocal(idx));
            self.define_local(param_name.clone())?;
        }
        let num_body_exprs = node.body.len();
        if num_body_exprs == 0 {
            self.exit_scope();
            self.current_segment_name = prev_segment_name;
            self.func_comp_context = prev_func_comp_context;
            return Err(NsError::Codegen("Lambda body cannot be empty.".to_string()));
        }
        for (i, expr) in node.body.into_iter().enumerate() {
            self.generate_expression(expr)?;
            if i < num_body_exprs - 1 {
                self.emit_op(OpCode::Pop);
            }
        }
        self.emit_op(OpCode::Return);
        self.exit_scope();

        self.current_segment_name = prev_segment_name;
        self.func_comp_context = prev_func_comp_context;
        Ok(())
    }

    fn generate_call_node(&mut self, node: CallNode) -> Result<(), NsError> {
        if let Expression::Symbol(name_rc) = &node.callable_expr {
            let name = name_rc.as_str();
            match name {
                "+" | "-" | "*" | "/" | "=" | ">" | "<" => {
                    if node.arguments.len() != 2 {
                        return Err(NsError::Codegen(format!(
                            "Operator '{}' expects 2 arguments, got {}",
                            name,
                            node.arguments.len()
                        )));
                    }
                    self.generate_expression(node.arguments[0].clone())?;
                    self.generate_expression(node.arguments[1].clone())?;
                    match name {
                        "+" => self.emit_op(OpCode::Add),
                        "-" => self.emit_op(OpCode::Sub),
                        "*" => self.emit_op(OpCode::Mul),
                        "/" => self.emit_op(OpCode::Div),
                        "=" => self.emit_op(OpCode::Eq),
                        ">" => self.emit_op(OpCode::Gt),
                        "<" => self.emit_op(OpCode::Lt),
                        _ => unreachable!("Handled by outer match"),
                    }
                    return Ok(());
                }
                "not" => {
                    if node.arguments.len() != 1 {
                        return Err(NsError::Codegen(format!(
                            "Operator 'not' expects 1 argument, got {}",
                            node.arguments.len()
                        )));
                    }
                    self.generate_expression(node.arguments[0].clone())?;
                    self.emit_op(OpCode::Not);
                    return Ok(());
                }
                "print" => {
                    if node.arguments.is_empty() {
                        self.emit(BytecodeInstruction::Push(Value::String(Rc::new(
                            "".to_string(),
                        ))));
                        self.emit_op(OpCode::Print);
                    } else {
                        for arg_expr in node.arguments {
                            self.generate_expression(arg_expr)?;
                            self.emit_op(OpCode::Print);
                        }
                    }
                    self.emit(BytecodeInstruction::Push(Value::NoneValue));
                    return Ok(());
                }
                "list" => {
                    for arg_expr in node.arguments.iter() {
                        self.generate_expression(arg_expr.clone())?;
                    }
                    self.emit(BytecodeInstruction::MakeList {
                        count: node.arguments.len(),
                    });
                    return Ok(());
                }
                "is_none" | "is_boolean" | "is_number" | "is_string" | "is_list" | "is_struct"
                | "is_function" => {
                    if node.arguments.len() != 1 {
                        return Err(NsError::Codegen(format!(
                            "Type predicate '{}' expects 1 argument, got {}",
                            name,
                            node.arguments.len()
                        )));
                    }
                    self.generate_expression(node.arguments[0].clone())?;
                    match name {
                        "is_none" => self.emit_op(OpCode::IsNone),
                        "is_boolean" => self.emit_op(OpCode::IsBoolean),
                        "is_number" => self.emit_op(OpCode::IsNumber),
                        "is_string" => self.emit_op(OpCode::IsString),
                        "is_list" => self.emit_op(OpCode::IsList),
                        "is_struct" => self.emit_op(OpCode::IsStruct),
                        "is_function" => self.emit_op(OpCode::IsFunction),
                        _ => unreachable!("Handled by outer match"),
                    }
                    return Ok(());
                }
                "cons" => {
                    if node.arguments.len() != 2 {
                        return Err(NsError::Codegen(format!(
                            "'cons' expects 2 arguments, got {}",
                            node.arguments.len()
                        )));
                    }
                    self.generate_expression(node.arguments[1].clone())?;
                    self.generate_expression(node.arguments[0].clone())?;
                    self.emit_op(OpCode::Cons);
                    return Ok(());
                }
                "first" | "rest" => {
                    if node.arguments.len() != 1 {
                        return Err(NsError::Codegen(format!(
                            "'{}' expects 1 argument, got {}",
                            name,
                            node.arguments.len()
                        )));
                    }
                    self.generate_expression(node.arguments[0].clone())?;
                    match name {
                        "first" => self.emit_op(OpCode::First),
                        "rest" => self.emit_op(OpCode::Rest),
                        _ => unreachable!("Handled by outer match"),
                    }
                    return Ok(());
                }
                _ if self.struct_definitions.contains_key(name_rc) => {
                    let field_names_clone: Vec<Rc<String>> = self
                        .struct_definitions
                        .get(name_rc)
                        .expect("Struct definition checked with contains_key")
                        .clone();

                    if node.arguments.len() != field_names_clone.len() {
                        return Err(NsError::Codegen(format!(
                            "Struct constructor '{}' expects {} arguments, got {}",
                            name,
                            field_names_clone.len(),
                            node.arguments.len()
                        )));
                    }
                    for arg_expr in node.arguments {
                        self.generate_expression(arg_expr)?;
                    }
                    self.emit(BytecodeInstruction::MakeStruct {
                        type_name: name.to_string(),
                        field_names: field_names_clone
                            .iter()
                            .map(|rc_s| rc_s.to_string())
                            .collect(),
                    });
                    return Ok(());
                }
                _ => {}
            }
        }

        for arg_expr in node.arguments.iter() {
            self.generate_expression(arg_expr.clone())?;
        }
        self.generate_expression(node.callable_expr)?;
        self.emit(BytecodeInstruction::Call {
            arity: node.arguments.len(),
        });
        Ok(())
    }

    fn generate_if_node(&mut self, node: IfNode) -> Result<(), NsError> {
        let else_label = self.new_label_for_current_context("else");
        let end_if_label = self.new_label_for_current_context("endif");

        self.generate_expression(node.condition)?;
        self.emit(BytecodeInstruction::JumpIfFalse(StringOrPc::Label(
            else_label.clone(),
        )));

        self.generate_expression(node.then_branch)?;
        self.emit(BytecodeInstruction::Jump(StringOrPc::Label(
            end_if_label.clone(),
        )));

        self.emit(BytecodeInstruction::LabelDef(else_label));
        self.generate_expression(node.else_branch)?;

        self.emit(BytecodeInstruction::LabelDef(end_if_label));
        Ok(())
    }

    fn generate_quote_node(&mut self, data: QuotedData) -> Result<(), NsError> {
        match data {
            QuotedData::Number(n) => self.emit(BytecodeInstruction::Push(Value::Number(n))),
            QuotedData::String(s) => self.emit(BytecodeInstruction::Push(Value::String(s.clone()))),
            QuotedData::Boolean(b) => self.emit(BytecodeInstruction::Push(Value::Boolean(b))),
            QuotedData::None => self.emit(BytecodeInstruction::Push(Value::NoneValue)),
            QuotedData::Symbol(s_rc) => {
                self.emit(BytecodeInstruction::Push(Value::QuotedSymbol(s_rc.clone())))
            }
            QuotedData::List(items) => {
                let count = items.len();
                for item_data in items {
                    self.generate_quote_node(item_data)?;
                }
                self.emit(BytecodeInstruction::MakeList { count });
            }
            QuotedData::Quote(nested_data_box) => {
                self.emit(BytecodeInstruction::Push(Value::QuotedSymbol(Rc::new(
                    "quote".to_string(),
                ))));
                self.generate_quote_node(*nested_data_box)?;
                self.emit(BytecodeInstruction::MakeList { count: 2 });
            }
        }
        Ok(())
    }

    fn generate_let_node(&mut self, node: LetNode) -> Result<(), NsError> {
        self.enter_scope();
        // let mut local_indices: HashMap<Rc<String>, usize> = HashMap::new();

        for (idx, binding) in node.bindings.iter().enumerate() {
            self.generate_expression(binding.value.clone())?;
            self.emit(BytecodeInstruction::StoreLocal(idx));
            self.define_local(binding.name.clone())?;
            // local_indices.insert(binding.name.clone(), idx);
        }

        if node.body.is_empty() {
            self.emit(BytecodeInstruction::Push(Value::NoneValue));
        } else {
            let num_body_exprs = node.body.len();
            for (i, expr) in node.body.iter().enumerate() {
                self.generate_expression(expr.clone())?;
                if i < num_body_exprs - 1 {
                    self.emit_op(OpCode::Pop);
                }
            }
        }
        self.exit_scope();
        Ok(())
    }
}
