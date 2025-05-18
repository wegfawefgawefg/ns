// ns_project_root/ns_core/src/vm.rs
use crate::error::NsError;
use crate::opcode::{BytecodeInstruction, OpCode, StringOrPc};
use crate::value::{Closure, EnvironmentChain, Scope, StructData, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct CallFrame {
    return_pc: usize,
    caller_segment_name: Rc<String>,
    previous_env_chain: EnvironmentChain,
}

pub struct VirtualMachine {
    bytecode_segments: HashMap<Rc<String>, Rc<Vec<BytecodeInstruction>>>,
    current_segment_name: Rc<String>,
    current_bytecode: Rc<Vec<BytecodeInstruction>>,
    ip: usize,
    operand_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    current_env_chain: EnvironmentChain,
    label_to_pc: HashMap<Rc<String>, HashMap<String, usize>>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        VirtualMachine {
            bytecode_segments: HashMap::new(),
            current_segment_name: Rc::new("".to_string()),
            current_bytecode: Rc::new(Vec::new()),
            ip: 0,
            operand_stack: Vec::new(),
            call_stack: Vec::new(),
            current_env_chain: Vec::new(),
            label_to_pc: HashMap::new(),
        }
    }

    pub fn load_program(
        &mut self,
        segments: HashMap<String, Vec<BytecodeInstruction>>,
        main_segment_name: &str,
    ) -> Result<(), NsError> {
        if !segments.contains_key(main_segment_name) {
            return Err(NsError::Vm(format!(
                "Main segment '{}' not found.",
                main_segment_name
            )));
        }
        self.bytecode_segments.clear();
        self.label_to_pc.clear();
        for (name, instructions) in segments {
            let segment_name_rc = Rc::new(name);
            let mut current_segment_labels = HashMap::new();
            let mut processed_instructions = Vec::new();
            let mut effective_pc = 0;
            for instr in instructions.iter() {
                if let BytecodeInstruction::LabelDef(label_name) = instr {
                    if current_segment_labels
                        .insert(label_name.clone(), effective_pc)
                        .is_some()
                    {
                        return Err(NsError::Vm(format!(
                            "Duplicate label '{}' in segment '{}'",
                            label_name, segment_name_rc
                        )));
                    }
                } else {
                    processed_instructions.push(instr.clone());
                    effective_pc += 1;
                }
            }
            self.bytecode_segments
                .insert(segment_name_rc.clone(), Rc::new(processed_instructions));
            self.label_to_pc
                .insert(segment_name_rc, current_segment_labels);
        }
        let main_segment_rc = Rc::new(main_segment_name.to_string());
        self.current_segment_name = main_segment_rc.clone();
        self.current_bytecode = self
            .bytecode_segments
            .get(&main_segment_rc)
            .ok_or_else(|| {
                NsError::Vm(format!("Main segment '{}' disappeared.", main_segment_name))
            })?
            .clone();
        self.ip = 0;
        self.operand_stack.clear();
        self.call_stack.clear();
        let global_scope_map = Rc::new(RefCell::new(HashMap::new()));
        let main_module_locals = Scope::Locals(RefCell::new(Vec::new()));
        self.current_env_chain = vec![Scope::Lexical(global_scope_map), main_module_locals];
        Ok(())
    }

    #[allow(dead_code)]
    fn current_indexed_locals_mut(&mut self) -> Option<std::cell::RefMut<Vec<Value>>> {
        if let Some(Scope::Locals(locals_vec_rc)) = self.current_env_chain.last() {
            Some(locals_vec_rc.borrow_mut())
        } else {
            None
        }
    }

    fn lookup_variable(&self, name: &Rc<String>) -> Option<Value> {
        for scope_variant in self.current_env_chain.iter().rev() {
            if let Scope::Lexical(scope_map_rc) = scope_variant {
                let scope_map = scope_map_rc.borrow();
                if let Some(value) = scope_map.get(name) {
                    return Some(value.clone());
                }
            }
        }
        None
    }

    pub fn run(&mut self) -> Result<Option<Value>, NsError> {
        loop {
            if self.ip >= self.current_bytecode.len() {
                if self.call_stack.is_empty() && self.current_segment_name.ends_with("_main") {
                    break;
                } else if !self.call_stack.is_empty() {
                    return Err(NsError::Vm(format!(
                        "Reached end of function segment '{}' (IP: {}, len: {}) without Return.",
                        self.current_segment_name,
                        self.ip,
                        self.current_bytecode.len()
                    )));
                } else {
                    return Err(NsError::Vm(format!(
                        "IP out of bounds in segment '{}' (IP: {}, len: {}). Expected Return or Halt.",
                        self.current_segment_name, self.ip, self.current_bytecode.len()
                    )));
                }
            }

            let instruction = self.current_bytecode[self.ip].clone();
            self.ip += 1;

            // println!("[VM] IP: {}, Seg: '{}', Instr: {:?}, Stack: {:?}", self.ip - 1, self.current_segment_name, instruction, self.operand_stack.iter().map(|v| format!("{}", v)).collect::<Vec<_>>());

            match instruction {
                BytecodeInstruction::Operation(op) => match op {
                    OpCode::Halt => {
                        println!("Execution halted.");
                        break;
                    }
                    OpCode::Pop => {
                        self.operand_stack
                            .pop()
                            .ok_or_else(|| NsError::Vm("POP from empty stack".to_string()))?;
                    }
                    OpCode::Add
                    | OpCode::Sub
                    | OpCode::Mul
                    | OpCode::Div
                    | OpCode::Modulo
                    | OpCode::Eq
                    | OpCode::Lt
                    | OpCode::Gt
                    | OpCode::GreaterThanOrEqual
                    | OpCode::LessThanOrEqual
                    | OpCode::NotEqual => {
                        let right_op = self.operand_stack.pop().ok_or_else(|| {
                            NsError::Vm(format!("{:?} needs 2 operands (missing 1st)", op))
                        })?;
                        let left_op = self.operand_stack.pop().ok_or_else(|| {
                            NsError::Vm(format!("{:?} needs 2 operands (missing 2nd)", op))
                        })?;

                        match (left_op, right_op) {
                            (Value::Number(l), Value::Number(r)) => {
                                let result = match op {
                                    OpCode::Add => Value::Number(l + r),
                                    OpCode::Sub => Value::Number(l - r),
                                    OpCode::Mul => Value::Number(l * r),
                                    OpCode::Div => {
                                        if r == 0.0 {
                                            return Err(NsError::Vm(
                                                "Division by zero".to_string(),
                                            ));
                                        } else {
                                            Value::Number(l / r)
                                        }
                                    }
                                    OpCode::Modulo => {
                                        if r == 0.0 {
                                            return Err(NsError::Vm("Modulo by zero".to_string()));
                                        } else {
                                            Value::Number(l % r)
                                        }
                                    }
                                    OpCode::Eq => Value::Boolean(l == r),
                                    OpCode::Lt => Value::Boolean(l < r),
                                    OpCode::Gt => Value::Boolean(l > r),
                                    OpCode::GreaterThanOrEqual => Value::Boolean(l >= r),
                                    OpCode::LessThanOrEqual => Value::Boolean(l <= r),
                                    OpCode::NotEqual => Value::Boolean(l != r),
                                    _ => unreachable!("Binary op {:?} not handled for numbers", op),
                                };
                                self.operand_stack.push(result);
                            }
                            (Value::String(ref l_s), Value::String(ref r_s))
                                if op == OpCode::Add =>
                            {
                                self.operand_stack
                                    .push(Value::String(Rc::new(format!("{}{}", l_s, r_s))));
                            }
                            (v1, v2) if op == OpCode::Eq => {
                                self.operand_stack.push(Value::Boolean(v1 == v2));
                            }
                            (v1, v2) if op == OpCode::NotEqual => {
                                self.operand_stack.push(Value::Boolean(v1 != v2));
                            }
                            (l_val, r_val) => {
                                return Err(NsError::Vm(format!(
                                    "Type error for {:?}: Cannot operate on {:?} and {:?}",
                                    op,
                                    l_val.type_name(),
                                    r_val.type_name()
                                )));
                            }
                        }
                    }
                    OpCode::Not => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or_else(|| NsError::Vm("NOT requires one operand".to_string()))?;
                        self.operand_stack.push(Value::Boolean(!val.is_truthy()));
                    }
                    OpCode::Print => {
                        println!(
                            "Output: {}",
                            self.operand_stack
                                .pop()
                                .ok_or_else(|| NsError::Vm("PRINT requires a value".to_string()))?
                        );
                    }
                    OpCode::ThrowError => {
                        let msg_val = self.operand_stack.pop().ok_or_else(|| {
                            NsError::Vm("ERROR primitive requires a message string".to_string())
                        })?;
                        if let Value::String(s) = msg_val {
                            return Err(NsError::Vm(format!("User Error: {}", s)));
                        } else {
                            return Err(NsError::Vm(format!(
                                "ERROR primitive expects a string argument, got {}",
                                msg_val.type_name()
                            )));
                        }
                    }
                    OpCode::Return => {
                        if let Some(frame) = self.call_stack.pop() {
                            self.ip = frame.return_pc;
                            self.current_segment_name = frame.caller_segment_name;
                            self.current_bytecode = self
                                .bytecode_segments
                                .get(&self.current_segment_name)
                                .expect("Caller segment")
                                .clone();
                            self.current_env_chain = frame.previous_env_chain;
                        } else {
                            break;
                        }
                    }
                    OpCode::IsNone => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_NONE needs operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(val, Value::NoneValue)));
                    }
                    OpCode::Cons => {
                        let item = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("CONS needs item".to_string()))?;
                        let list_val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("CONS needs list".to_string()))?;
                        match list_val {
                            Value::List(list_rc) => {
                                let mut new_list = vec![item];
                                new_list.extend_from_slice(&list_rc);
                                self.operand_stack.push(Value::List(Rc::new(new_list)));
                            }
                            Value::NoneValue => {
                                self.operand_stack.push(Value::List(Rc::new(vec![item])));
                            }
                            _ => {
                                return Err(NsError::Vm(format!(
                                    "CONS expects list/none, got {}",
                                    list_val.type_name()
                                )));
                            }
                        }
                    }
                    OpCode::First => {
                        let list_val = self
                            .operand_stack
                            .pop()
                            .ok_or_else(|| NsError::Vm("FIRST requires a list".to_string()))?;
                        match list_val {
                            Value::List(list_rc) => {
                                if list_rc.is_empty() {
                                    return Err(NsError::Vm(
                                        "FIRST cannot operate on an empty list".to_string(),
                                    ));
                                }
                                self.operand_stack.push(list_rc[0].clone());
                            }
                            _ => {
                                return Err(NsError::Vm(format!(
                                    "FIRST expects a list, got {}",
                                    list_val.type_name()
                                )))
                            }
                        }
                    }
                    OpCode::Rest => {
                        let list_val = self
                            .operand_stack
                            .pop()
                            .ok_or_else(|| NsError::Vm("REST requires a list".to_string()))?;
                        match list_val {
                            Value::List(list_rc) => {
                                if list_rc.is_empty() {
                                    return Err(NsError::Vm(
                                        "REST cannot operate on an empty list".to_string(),
                                    ));
                                }
                                if list_rc.len() == 1 {
                                    self.operand_stack.push(Value::NoneValue);
                                } else {
                                    self.operand_stack
                                        .push(Value::List(Rc::new(list_rc[1..].to_vec())));
                                }
                            }
                            _ => {
                                return Err(NsError::Vm(format!(
                                    "REST expects a list, got {}",
                                    list_val.type_name()
                                )))
                            }
                        }
                    }
                    OpCode::IsBoolean => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_BOOLEAN needs operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(val, Value::Boolean(_))));
                    }
                    OpCode::IsNumber => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_NUMBER needs operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(val, Value::Number(_))));
                    }
                    OpCode::IsString => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_STRING needs operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(val, Value::String(_))));
                    }
                    OpCode::IsList => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_LIST needs operand".to_string()))?;
                        self.operand_stack.push(Value::Boolean(matches!(
                            val,
                            Value::List(_) | Value::NoneValue
                        )));
                    }
                    OpCode::IsStruct => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_STRUCT needs operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(val, Value::StructInstance(_))));
                    }
                    OpCode::IsFunction => {
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_FUNCTION needs operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(val, Value::Closure(_))));
                    }
                    _ => return Err(NsError::Vm(format!("Op Op {:?} not implemented", op))),
                },
                BytecodeInstruction::Push(value) => self.operand_stack.push(value),
                BytecodeInstruction::StoreGlobal(name) => {
                    let value = self.operand_stack.pop().ok_or_else(|| {
                        NsError::Vm(format!("STORE_GLOBAL '{}' needs value", name))
                    })?;
                    if let Some(Scope::Lexical(global_scope_rc)) = self.current_env_chain.first() {
                        global_scope_rc.borrow_mut().insert(Rc::new(name), value);
                    } else {
                        return Err(NsError::Vm(
                            "Global lexical scope missing for StoreGlobal".to_string(),
                        ));
                    }
                }
                BytecodeInstruction::LoadGlobal(name) => {
                    let name_rc = Rc::new(name);
                    match self.lookup_variable(&name_rc) {
                        Some(value) => self.operand_stack.push(value),
                        None => {
                            return Err(NsError::Vm(format!("Variable '{}' not defined.", name_rc)))
                        }
                    }
                }
                BytecodeInstruction::MakeClosure { label, arity } => {
                    let closure_name_opt = if label.starts_with("fn_body_") {
                        label.split('_').last().map(|s| Rc::new(s.to_string()))
                    } else {
                        None
                    };
                    let closure = Closure {
                        name: closure_name_opt,
                        arity,
                        code_label: label,
                        defining_env: self.current_env_chain.clone(),
                    };
                    self.operand_stack.push(Value::Closure(Rc::new(closure)));
                }
                BytecodeInstruction::Call { arity } => {
                    if self.operand_stack.len() < arity + 1 {
                        return Err(NsError::Vm(format!(
                            "Stack underflow for CALL: need {} args + closure, found {}",
                            arity,
                            self.operand_stack.len()
                        )));
                    }
                    let callee_val = self.operand_stack.pop().unwrap();

                    match callee_val {
                        Value::Closure(closure_rc) => {
                            let closure = &*closure_rc;
                            if closure.arity != arity {
                                return Err(NsError::Vm(format!(
                                    "Function '{}' called with incorrect arity. Expected {}, got {}.",
                                    closure
                                        .name
                                        .as_ref()
                                        .map_or_else(|| closure.code_label.as_str(), |n| n.as_str()),
                                    closure.arity,
                                    arity
                                )));
                            }

                            let frame = CallFrame {
                                return_pc: self.ip,
                                caller_segment_name: self.current_segment_name.clone(),
                                previous_env_chain: self.current_env_chain.clone(),
                            };
                            self.call_stack.push(frame);

                            let new_local_values_vec = Vec::with_capacity(arity);
                            let new_local_scope = Scope::Locals(RefCell::new(new_local_values_vec));

                            self.current_env_chain = closure.defining_env.clone();
                            self.current_env_chain.push(new_local_scope);

                            let target_segment_name_rc = Rc::new(closure.code_label.clone());
                            self.current_segment_name = target_segment_name_rc.clone();
                            self.current_bytecode = self
                                .bytecode_segments
                                .get(&target_segment_name_rc)
                                .ok_or_else(|| {
                                    NsError::Vm(format!(
                                        "Fn segment '{}' not found.",
                                        target_segment_name_rc
                                    ))
                                })?
                                .clone();
                            self.ip = 0;
                        }
                        _ => {
                            return Err(NsError::Vm(format!(
                                "Cannot call non-function value: {:?}",
                                callee_val.type_name()
                            )));
                        }
                    }
                }
                BytecodeInstruction::StoreLocal(index) => {
                    let value = self.operand_stack.pop().ok_or_else(|| {
                        NsError::Vm(format!("STORE_LOCAL index {} needs value", index))
                    })?;
                    if let Some(Scope::Locals(locals_rc)) = self.current_env_chain.last() {
                        let mut locals_vec = locals_rc.borrow_mut();
                        if index >= locals_vec.len() {
                            locals_vec.resize(index + 1, Value::NoneValue);
                        }
                        locals_vec[index] = value;
                    } else {
                        let last_scope_type_dbg = self
                            .current_env_chain
                            .last()
                            .map(|s| format!("{:?}", s))
                            .unwrap_or_else(|| "None (empty chain)".to_string());
                        return Err(NsError::Vm(format!(
                            "No local scope (Scope::Locals) for StoreLocal. Innermost scope is: {}",
                            last_scope_type_dbg
                        )));
                    }
                }
                BytecodeInstruction::LoadLocal(index) => {
                    if let Some(Scope::Locals(locals_rc)) = self.current_env_chain.last() {
                        let locals_vec = locals_rc.borrow();
                        let value = locals_vec.get(index).cloned().ok_or_else(|| {
                            NsError::Vm(format!(
                                "Invalid local index {} (len {})",
                                index,
                                locals_vec.len()
                            ))
                        })?;
                        self.operand_stack.push(value);
                    } else {
                        let last_scope_type_dbg = self
                            .current_env_chain
                            .last()
                            .map(|s| format!("{:?}", s))
                            .unwrap_or_else(|| "None (empty chain)".to_string());
                        return Err(NsError::Vm(format!(
                            "No local scope (Scope::Locals) for LoadLocal. Innermost scope is: {}",
                            last_scope_type_dbg
                        )));
                    }
                }
                BytecodeInstruction::Jump(ref target) => {
                    self.ip = self.resolve_jump_target(target)?;
                }
                BytecodeInstruction::JumpIfFalse(ref target) => {
                    let val = self
                        .operand_stack
                        .pop()
                        .ok_or_else(|| NsError::Vm("JUMP_IF_FALSE requires value".to_string()))?;
                    if !val.is_truthy() {
                        self.ip = self.resolve_jump_target(target)?;
                    }
                }
                BytecodeInstruction::LabelDef(_) => {
                    return Err(NsError::Vm(format!(
                        "LabelDef at runtime IP {}",
                        self.ip - 1
                    )));
                }
                BytecodeInstruction::MakeList { count } => {
                    if self.operand_stack.len() < count {
                        return Err(NsError::Vm(format!(
                            "MAKE_LIST needs {} items, found {}",
                            count,
                            self.operand_stack.len()
                        )));
                    }
                    let mut new_list = Vec::with_capacity(count);
                    for _ in 0..count {
                        new_list.push(self.operand_stack.pop().unwrap());
                    }
                    new_list.reverse();
                    self.operand_stack.push(Value::List(Rc::new(new_list)));
                }
                BytecodeInstruction::MakeStruct {
                    type_name,
                    field_names,
                } => {
                    if self.operand_stack.len() < field_names.len() {
                        return Err(NsError::Vm(format!(
                            "MAKE_STRUCT '{}' requires {} values, found {}",
                            type_name,
                            field_names.len(),
                            self.operand_stack.len()
                        )));
                    }
                    let mut fields_map = HashMap::with_capacity(field_names.len());
                    for field_name_str in field_names.iter().rev() {
                        let value = self.operand_stack.pop().unwrap();
                        fields_map.insert(Rc::new(field_name_str.clone()), value);
                    }
                    let struct_data = StructData {
                        type_name: Rc::new(type_name),
                        fields: fields_map,
                    };
                    self.operand_stack
                        .push(Value::StructInstance(Rc::new(RefCell::new(struct_data))));
                }
                BytecodeInstruction::GetField(field_name_str) => {
                    let instance_val = self.operand_stack.pop().ok_or_else(|| {
                        NsError::Vm(format!(
                            "GET_FIELD '{}' requires a struct instance",
                            field_name_str
                        ))
                    })?;
                    match instance_val {
                        Value::StructInstance(struct_data_rc) => {
                            let struct_data = struct_data_rc.borrow();
                            let field_name_rc = Rc::new(field_name_str);
                            match struct_data.fields.get(&field_name_rc) {
                                Some(value) => self.operand_stack.push(value.clone()),
                                None => {
                                    return Err(NsError::Vm(format!(
                                        "Struct '{}' has no field '{}'",
                                        struct_data.type_name, field_name_rc
                                    )))
                                }
                            }
                        }
                        _ => {
                            return Err(NsError::Vm(format!(
                                "GET_FIELD expects a struct instance, got {}",
                                instance_val.type_name()
                            )))
                        }
                    }
                }
                BytecodeInstruction::SetField(field_name_str) => {
                    if self.operand_stack.len() < 2 {
                        return Err(NsError::Vm(format!(
                            "SET_FIELD '{}' requires instance and value",
                            field_name_str
                        )));
                    }
                    let new_value = self.operand_stack.pop().unwrap();
                    let instance_val = self.operand_stack.pop().unwrap();
                    match instance_val {
                        Value::StructInstance(struct_data_rc) => {
                            let mut struct_data = struct_data_rc.borrow_mut();
                            let field_name_rc = Rc::new(field_name_str.clone());
                            if struct_data.fields.contains_key(&field_name_rc) {
                                struct_data.fields.insert(field_name_rc, new_value);
                                self.operand_stack
                                    .push(Value::StructInstance(struct_data_rc.clone()));
                            } else {
                                return Err(NsError::Vm(format!(
                                    "Struct '{}' has no field '{}' to set",
                                    struct_data.type_name, field_name_str
                                )));
                            }
                        }
                        _ => {
                            return Err(NsError::Vm(format!(
                                "SET_FIELD expects a struct instance, got {}",
                                instance_val.type_name()
                            )))
                        }
                    }
                }
            }
        }
        Ok(self.operand_stack.pop())
    }

    fn resolve_jump_target(&self, target: &StringOrPc) -> Result<usize, NsError> {
        match target {
            StringOrPc::Label(label_name) => {
                return self
                    .label_to_pc
                    .get(&self.current_segment_name)
                    .and_then(|labels_in_segment| labels_in_segment.get(label_name))
                    .copied()
                    .ok_or_else(|| {
                        NsError::Vm(format!(
                            "Undefined label '{}' in seg '{}'",
                            label_name, self.current_segment_name
                        ))
                    });
            }
            StringOrPc::Pc(pc_value) => {
                return Ok(*pc_value);
            }
        }
    }
}

// Add a debug helper for Scope if it's not already deriving Debug
// (It is deriving Debug in value.rs, so this is not strictly needed here but shown for completeness if it wasn't)
impl Scope {
    #[allow(dead_code)]
    fn type_name_debug(&self) -> &'static str {
        match self {
            Scope::Locals(_) => "Scope::Locals",
            Scope::Lexical(_) => "Scope::Lexical",
        }
    }
}
