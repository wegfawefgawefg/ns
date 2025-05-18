// ns_project_root/ns_core/src/codegen.rs
use crate::ast::{
    CallNode,
    Expression,
    FnNode,
    IfNode,
    LambdaNode,
    LetNode,
    ProgramNode,
    QuotedData,
    StaticNode,
    StructDefNode,
    TopLevelForm,
    UseNode,
    // Removed GetNode, SetNode, WhileNode, BeginNode as they are part of Expression enum
};
use crate::error::NsError;
use crate::opcode::{BytecodeInstruction, OpCode, StringOrPc};
use crate::value::Value;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

// SymbolBinding is simplified as all named variables are now lexical.
// We primarily distinguish between things known at compile time (functions, structs)
// and general variables that will be resolved lexically at runtime.
#[derive(Debug, Clone)]
enum SymbolResolution {
    Lexical, // Will use LoadGlobal/StoreGlobal, VM handles lexical lookup
             // CompileTimeFunction: Could be used for direct calls or arity checks if needed later
             // CompileTimeStruct: Could be used for direct field count checks if needed later
}

// FunctionContext is simplified. It primarily manages labels for the current function/lambda body.
// Named locals (parameters) are handled by the VM creating a new lexical scope.
#[derive(Debug, Default)]
struct FunctionContext {
    label_prefix: String, // For generating unique labels within this function's segment
    label_counter: usize, // Counter for unique labels
                          // No need for named_locals or local_idx_counter here anymore,
                          // as parameters become named entries in a new lexical scope created by the VM during CALL.
}

pub struct CodeGenerator {
    bytecode_segments: HashMap<String, Vec<BytecodeInstruction>>,
    current_segment_name: String,
    function_contexts: Vec<FunctionContext>, // Stack for nested fn/lambda label generation

    // These store info about globally visible definitions *within the current module being compiled*.
    // This helps codegen know if a symbol is a function (for arity in MakeClosure) or struct (for MakeStruct).
    global_functions: HashMap<Rc<String>, (String, usize, Vec<Rc<String>>)>, // name -> (segment_label, arity, param_names)
    global_structs: HashMap<Rc<String>, Vec<Rc<String>>>,                    // name -> field_names

    discovered_dependencies: HashSet<String>,
    module_name: Rc<String>,
}

impl CodeGenerator {
    pub fn new(module_name_str: String) -> Self {
        let module_name_rc = Rc::new(module_name_str);
        let main_segment_name = format!("module_{}_main", module_name_rc.replace('-', "_"));
        let mut segments = HashMap::new();
        segments.insert(main_segment_name.clone(), Vec::new());

        CodeGenerator {
            bytecode_segments: segments,
            current_segment_name: main_segment_name.clone(),
            function_contexts: vec![FunctionContext {
                // Context for the main module body
                label_prefix: main_segment_name,
                label_counter: 0,
            }],
            global_functions: HashMap::new(),
            global_structs: HashMap::new(),
            discovered_dependencies: HashSet::new(),
            module_name: module_name_rc,
        }
    }

    fn current_fn_context_mut(&mut self) -> &mut FunctionContext {
        self.function_contexts
            .last_mut()
            .expect("Function context stack should not be empty")
    }

    // Enters a new context for generating labels within a function/lambda body.
    fn enter_function_label_scope(&mut self, fn_segment_name_for_labels: String) {
        self.function_contexts.push(FunctionContext {
            label_prefix: fn_segment_name_for_labels,
            label_counter: 0,
        });
    }

    fn exit_function_label_scope(&mut self) {
        if self.function_contexts.len() > 1 {
            self.function_contexts.pop();
        } else {
            eprintln!(
                "Warning: CodeGenerator: Attempted to pop the main module's function context."
            );
        }
    }

    // Determines how a symbol should be accessed.
    // Now, all symbols that aren't built-in primitives or known struct constructors
    // will resolve to SymbolResolution::Lexical.
    fn resolve_symbol_type(&self, name: &Rc<String>) -> SymbolResolution {
        if self.global_functions.contains_key(name) || self.global_structs.contains_key(name) {
            // If it's a function or struct defined in this module, it's still accessed lexically
            // via LoadGlobal. The VM will find the Closure or handle struct construction.
            // This function is less critical now for Load/Store, but useful for Call/MakeStruct.
        }
        SymbolResolution::Lexical
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
        self.emit(BytecodeInstruction::Operation(opcode));
    }
    fn new_label(&mut self, basic_name: &str) -> String {
        let context = self.current_fn_context_mut();
        context.label_counter += 1;
        format!(
            "{}_L{}_{}",
            context.label_prefix, context.label_counter, basic_name
        )
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

        let main_module_segment_name =
            format!("module_{}_main", self.module_name.replace('-', "_"));
        if let Some(main_bytecode) = self.bytecode_segments.get_mut(&main_module_segment_name) {
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
        } else {
            return Err(NsError::Codegen(format!(
                "Main segment {} not found at end of codegen.",
                main_module_segment_name
            )));
        }
        Ok((self.bytecode_segments, self.discovered_dependencies))
    }

    fn generate_top_level_form(
        &mut self,
        form: TopLevelForm,
        is_last_exec_form: bool,
    ) -> Result<(), NsError> {
        let start_len = self.current_bytecode_mut().len();
        match form {
            TopLevelForm::StaticDef(node) => self.generate_static_node(node)?,
            TopLevelForm::FnDef(node) => self.generate_fn_node(node)?,
            TopLevelForm::StructDef(node) => self.generate_struct_def_node(node)?,
            TopLevelForm::Use(_) => { /* Already processed */ }
            TopLevelForm::Expression(expr) => {
                self.generate_expression(expr)?;
                if !is_last_exec_form && self.current_bytecode_mut().len() > start_len {
                    if let Some(last_instr) = self.current_bytecode_mut().last().cloned() {
                        match last_instr {
                            BytecodeInstruction::StoreGlobal(_)
                            | BytecodeInstruction::Jump(_)
                            | BytecodeInstruction::Operation(OpCode::Return)
                            | BytecodeInstruction::Operation(OpCode::Halt)
                            | BytecodeInstruction::Operation(OpCode::Print) => { /* No pop needed */
                            }
                            _ => {
                                self.emit_op(OpCode::Pop);
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
            Expression::Symbol(name_rc) => {
                // All symbols are loaded as globals; VM handles lexical lookup.
                self.emit(BytecodeInstruction::LoadGlobal(name_rc.to_string()));
            }
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
                let node = *while_node_box;
                let start_label = self.new_label("while_start");
                let end_label = self.new_label("while_end");
                self.emit(BytecodeInstruction::LabelDef(start_label.clone()));
                self.generate_expression(node.condition)?;
                self.emit(BytecodeInstruction::JumpIfFalse(StringOrPc::Label(
                    end_label.clone(),
                )));
                for body_expr in node.body {
                    self.generate_expression(body_expr)?;
                    self.emit_op(OpCode::Pop);
                }
                self.emit(BytecodeInstruction::Jump(StringOrPc::Label(start_label)));
                self.emit(BytecodeInstruction::LabelDef(end_label));
                self.emit(BytecodeInstruction::Push(Value::NoneValue));
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
        Ok(())
    }

    fn generate_struct_def_node(&mut self, node: StructDefNode) -> Result<(), NsError> {
        if self.global_structs.contains_key(&node.name) {
            return Err(NsError::Codegen(format!(
                "Struct '{}' already defined.",
                node.name
            )));
        }
        self.global_structs
            .insert(node.name.clone(), node.fields.clone());
        Ok(())
    }

    fn generate_use_node(&mut self, node: UseNode) -> Result<(), NsError> {
        self.discovered_dependencies
            .insert(node.module_name.to_string());
        Ok(())
    }

    // Generates bytecode for a function/lambda body in a new segment.
    fn generate_fn_or_lambda_body(
        &mut self,
        params: Vec<Rc<String>>,
        body: Vec<Expression>,
        fn_body_segment_name: String,
    ) -> Result<(), NsError> {
        let prev_segment_name =
            std::mem::replace(&mut self.current_segment_name, fn_body_segment_name.clone());
        self.bytecode_segments
            .entry(self.current_segment_name.clone())
            .or_insert_with(Vec::new);

        // Enter a new context for label generation within this function's segment
        self.enter_function_label_scope(fn_body_segment_name);

        // Parameters are handled by the VM's CALL instruction, which creates a new lexical scope
        // and populates it with arguments bound to param_names.
        // No StoreLocal opcodes are needed here for parameters.

        if body.is_empty() {
            let current_fn_label_prefix = self
                .function_contexts
                .last()
                .map_or_else(|| "unknown_fn".to_string(), |ctx| ctx.label_prefix.clone());
            self.exit_function_label_scope();
            self.current_segment_name = prev_segment_name;
            return Err(NsError::Codegen(format!(
                "Function/Lambda body ('{}') cannot be empty.",
                current_fn_label_prefix
            )));
        }

        let num_body_exprs = body.len();
        for (i, expr) in body.into_iter().enumerate() {
            self.generate_expression(expr)?;
            if i < num_body_exprs - 1 {
                self.emit_op(OpCode::Pop);
            }
        }
        self.emit_op(OpCode::Return);

        self.exit_function_label_scope();
        self.current_segment_name = prev_segment_name;
        Ok(())
    }

    fn generate_fn_node(&mut self, node: FnNode) -> Result<(), NsError> {
        let fn_name_rc = node.name.clone();
        let arity = node.params.len();
        let param_names_clone = node.params.clone(); // Clone for MakeClosure

        let sanitized_fn_name_for_label =
            fn_name_rc.replace(|c: char| !c.is_alphanumeric() && c != '_', "_");
        let body_segment_label = format!(
            "fn_body_{}_{}",
            self.module_name.replace('-', "_"),
            sanitized_fn_name_for_label
        );

        self.emit(BytecodeInstruction::MakeClosure {
            label: body_segment_label.clone(),
            arity,
            param_names: param_names_clone,
        });
        self.emit(BytecodeInstruction::StoreGlobal(fn_name_rc.to_string()));

        self.global_functions.insert(
            fn_name_rc.clone(),
            (body_segment_label.clone(), arity, node.params.clone()),
        );

        self.generate_fn_or_lambda_body(node.params, node.body, body_segment_label)?;
        Ok(())
    }

    fn generate_lambda_node(&mut self, node: LambdaNode) -> Result<(), NsError> {
        let arity = node.params.len();
        let param_names_clone = node.params.clone(); // Clone for MakeClosure
        let lambda_body_segment_label = self.new_label("lambda_body");

        self.emit(BytecodeInstruction::MakeClosure {
            label: lambda_body_segment_label.clone(),
            arity,
            param_names: param_names_clone,
        });

        self.generate_fn_or_lambda_body(node.params, node.body, lambda_body_segment_label)?;
        Ok(())
    }

    fn generate_call_node(&mut self, node: CallNode) -> Result<(), NsError> {
        let num_args = node.arguments.len();

        if let Expression::Symbol(symbol_rc_callable) = &node.callable_expr {
            let name = symbol_rc_callable.as_str();
            match name {
                "+" | "-" | "*" | "/" | "%" | "=" | ">" | "<" | ">=" | "<=" | "!=" => {
                    if num_args != 2 {
                        return Err(NsError::Codegen(format!(
                            "Operator '{}' expects 2 args, got {}",
                            name, num_args
                        )));
                    }
                    self.generate_expression(node.arguments[0].clone())?;
                    self.generate_expression(node.arguments[1].clone())?;
                    match name {
                        "+" => self.emit_op(OpCode::Add),
                        "-" => self.emit_op(OpCode::Sub),
                        "*" => self.emit_op(OpCode::Mul),
                        "/" => self.emit_op(OpCode::Div),
                        "%" => self.emit_op(OpCode::Modulo),
                        "=" => self.emit_op(OpCode::Eq),
                        ">" => self.emit_op(OpCode::Gt),
                        "<" => self.emit_op(OpCode::Lt),
                        ">=" => self.emit_op(OpCode::GreaterThanOrEqual),
                        "<=" => self.emit_op(OpCode::LessThanOrEqual),
                        "!=" => self.emit_op(OpCode::NotEqual),
                        _ => unreachable!(),
                    }
                    return Ok(());
                }
                "not" => {
                    if num_args != 1 {
                        return Err(NsError::Codegen(format!(
                            "'not' expects 1 arg, got {}",
                            num_args
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
                "error" => {
                    if num_args != 1 {
                        return Err(NsError::Codegen(format!(
                            "'error' expects 1 arg, got {}",
                            num_args
                        )));
                    }
                    self.generate_expression(node.arguments[0].clone())?;
                    self.emit_op(OpCode::ThrowError);
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
                    if num_args != 1 {
                        return Err(NsError::Codegen(format!(
                            "Predicate '{}' expects 1 arg, got {}",
                            name, num_args
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
                        _ => unreachable!(),
                    }
                    return Ok(());
                }
                "cons" => {
                    if num_args != 2 {
                        return Err(NsError::Codegen(format!(
                            "'cons' expects 2 args, got {}",
                            num_args
                        )));
                    }
                    self.generate_expression(node.arguments[1].clone())?;
                    self.generate_expression(node.arguments[0].clone())?;
                    self.emit_op(OpCode::Cons);
                    return Ok(());
                }
                "first" | "rest" => {
                    if num_args != 1 {
                        return Err(NsError::Codegen(format!(
                            "'{}' expects 1 arg, got {}",
                            name, num_args
                        )));
                    }
                    self.generate_expression(node.arguments[0].clone())?;
                    if name == "first" {
                        self.emit_op(OpCode::First);
                    } else {
                        self.emit_op(OpCode::Rest);
                    }
                    return Ok(());
                }
                _ => {
                    if let Some(struct_fields_rc_clone) =
                        self.global_structs.get(symbol_rc_callable).cloned()
                    {
                        if num_args != struct_fields_rc_clone.len() {
                            return Err(NsError::Codegen(format!(
                                "Struct '{}' expects {} args, got {}",
                                name,
                                struct_fields_rc_clone.len(),
                                num_args
                            )));
                        }
                        for arg_expr in node.arguments {
                            self.generate_expression(arg_expr)?;
                        }
                        let field_names_for_instr: Vec<String> = struct_fields_rc_clone
                            .iter()
                            .map(|s| s.to_string())
                            .collect();
                        self.emit(BytecodeInstruction::MakeStruct {
                            type_name: name.to_string(),
                            field_names: field_names_for_instr,
                        });
                        return Ok(());
                    }
                }
            }
        }

        for arg_expr in node.arguments.iter() {
            self.generate_expression(arg_expr.clone())?;
        }
        self.generate_expression(node.callable_expr)?;
        self.emit(BytecodeInstruction::Call { arity: num_args });
        Ok(())
    }

    fn generate_if_node(&mut self, if_node_param: IfNode) -> Result<(), NsError> {
        let else_label = self.new_label("else");
        let end_if_label = self.new_label("endif");
        self.generate_expression(if_node_param.condition)?;
        self.emit(BytecodeInstruction::JumpIfFalse(StringOrPc::Label(
            else_label.clone(),
        )));
        self.generate_expression(if_node_param.then_branch)?;
        self.emit(BytecodeInstruction::Jump(StringOrPc::Label(
            end_if_label.clone(),
        )));
        self.emit(BytecodeInstruction::LabelDef(else_label));
        self.generate_expression(if_node_param.else_branch)?;
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
                for item_data in items.into_iter() {
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
        self.emit_op(OpCode::EnterScope);

        for binding in node.bindings.iter() {
            self.generate_expression(binding.value.clone())?;
            self.emit(BytecodeInstruction::StoreGlobal(binding.name.to_string()));
            // VM's StoreGlobal pops the value. No extra Pop needed here.
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

        self.emit_op(OpCode::ExitScope);
        Ok(())
    }
}
