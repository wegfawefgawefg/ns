// ns_project_root/ns_core/src/vm.rs
use crate::opcode::{BytecodeInstruction, OpCode, StringOrPc};
// Commenting out StructData as it's not used yet, but we'll need it for IsStruct
use crate::error::NsError;
use crate::value::{Closure, EnvironmentChain, StructData, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Represents a frame on the call stack
#[derive(Debug, Clone)]
struct CallFrame {
    return_pc: usize,
    caller_segment_name: Rc<String>, // Name of the segment the caller was in
    previous_env_chain: EnvironmentChain,
}

pub struct VirtualMachine {
    bytecode_segments: HashMap<Rc<String>, Rc<Vec<BytecodeInstruction>>>,
    current_segment_name: Rc<String>,
    current_bytecode: Rc<Vec<BytecodeInstruction>>,
    ip: usize,
    operand_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    globals: HashMap<Rc<String>, Value>,
    current_env_chain: EnvironmentChain,
    label_to_pc: HashMap<Rc<String>, HashMap<String, usize>>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        let global_scope = Rc::new(RefCell::new(HashMap::new()));
        let initial_env_chain = vec![global_scope.clone()];

        VirtualMachine {
            bytecode_segments: HashMap::new(),
            current_segment_name: Rc::new("".to_string()),
            current_bytecode: Rc::new(Vec::new()),
            ip: 0,
            operand_stack: Vec::new(),
            call_stack: Vec::new(),
            globals: HashMap::new(),
            current_env_chain: initial_env_chain,
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
                "Main segment '{}' not found in provided bytecode segments.",
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
                NsError::Vm(format!(
                    "Main segment '{}' disappeared after loading.",
                    main_segment_name
                ))
            })?
            .clone();
        self.ip = 0;
        self.operand_stack.clear();
        self.call_stack.clear();
        self.globals.clear();
        let global_scope = Rc::new(RefCell::new(HashMap::new()));
        self.current_env_chain = vec![global_scope];

        Ok(())
    }

    #[allow(dead_code)]
    fn current_local_scope_mut(&mut self) -> std::cell::RefMut<HashMap<Rc<String>, Value>> {
        self.current_env_chain
            .last()
            .expect("Environment chain should never be empty")
            .borrow_mut()
    }

    fn lookup_variable(&self, name: &Rc<String>) -> Option<Value> {
        for scope_rc in self.current_env_chain.iter().rev() {
            let scope = scope_rc.borrow();
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
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
                        "Reached end of function segment '{}' (IP: {}, len: {}) without a Return instruction.",
                        self.current_segment_name, self.ip, self.current_bytecode.len()
                    )));
                } else {
                    return Err(NsError::Vm(format!(
                        "Instruction pointer out of bounds in segment '{}' (IP: {}, len: {}). Expected Return or Halt.",
                        self.current_segment_name, self.ip, self.current_bytecode.len()
                    )));
                }
            }

            let instruction = self.current_bytecode[self.ip].clone();
            self.ip += 1;

            // For debugging:
            // println!("IP: {}, Seg: '{}', Instr: {:?}, Stack: {:?}", self.ip -1, self.current_segment_name, instruction, self.operand_stack);

            match instruction {
                BytecodeInstruction::Operation(op) => {
                    match op {
                        OpCode::Halt => {
                            println!("Execution halted.");
                            break;
                        }
                        OpCode::Pop => {
                            if self.operand_stack.pop().is_none() {
                                return Err(NsError::Vm("POP from empty stack".to_string()));
                            }
                        }
                        OpCode::Add
                        | OpCode::Sub
                        | OpCode::Mul
                        | OpCode::Div
                        | OpCode::Eq
                        | OpCode::Lt
                        | OpCode::Gt => {
                            if self.operand_stack.len() < 2 {
                                return Err(NsError::Vm(format!("{:?} requires two operands", op)));
                            }
                            let right = self.operand_stack.pop().unwrap();
                            let left = self.operand_stack.pop().unwrap();

                            match (left, right) {
                                (Value::Number(l), Value::Number(r)) => {
                                    let result = match op {
                                    OpCode::Add => Value::Number(l + r),
                                    OpCode::Sub => Value::Number(l - r),
                                    OpCode::Mul => Value::Number(l * r),
                                    OpCode::Div => {
                                        if r == 0.0 { return Err(NsError::Vm("Division by zero".to_string())); }
                                        Value::Number(l / r)
                                    }
                                    OpCode::Eq => Value::Boolean(l == r),
                                    OpCode::Lt => Value::Boolean(l < r),
                                    OpCode::Gt => Value::Boolean(l > r),
                                    _ => unreachable!("Binary op {:?} not handled for numbers after outer match", op),
                                };
                                    self.operand_stack.push(result);
                                }
                                (Value::String(ref l_s), Value::String(ref r_s))
                                    if op == OpCode::Add =>
                                {
                                    let mut new_s = String::with_capacity(l_s.len() + r_s.len());
                                    new_s.push_str(l_s);
                                    new_s.push_str(r_s);
                                    self.operand_stack.push(Value::String(Rc::new(new_s)));
                                }
                                (v1, v2) if op == OpCode::Eq => {
                                    // Generic equality for other types
                                    self.operand_stack.push(Value::Boolean(v1 == v2));
                                }
                                (l_val, r_val) => {
                                    return Err(NsError::Vm(format!(
                                        "Type error for {:?}: Cannot operate on {:?} and {:?}",
                                        op,
                                        l_val.type_name(),
                                        r_val.type_name()
                                    )))
                                }
                            }
                        }
                        OpCode::Not => {
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("NOT requires one operand".to_string())
                            })?;
                            match val {
                                Value::Boolean(b) => self.operand_stack.push(Value::Boolean(!b)),
                                Value::NoneValue => self.operand_stack.push(Value::Boolean(true)), // not none -> true
                                _ => self.operand_stack.push(Value::Boolean(false)), // not <truthy_value> -> false
                            }
                        }
                        OpCode::Print => {
                            let val = self
                                .operand_stack
                                .pop()
                                .ok_or_else(|| NsError::Vm("PRINT requires a value".to_string()))?;
                            println!("Output: {}", val);
                        }
                        OpCode::Return => {
                            if let Some(frame) = self.call_stack.pop() {
                                self.ip = frame.return_pc;
                                self.current_segment_name = frame.caller_segment_name;
                                self.current_bytecode = self
                                    .bytecode_segments
                                    .get(&self.current_segment_name)
                                    .expect("Caller segment bytecode should exist")
                                    .clone();
                                self.current_env_chain = frame.previous_env_chain;
                            } else {
                                break;
                            }
                        }
                        // List Primitives
                        OpCode::IsNone => {
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("IS_NONE requires one operand".to_string())
                            })?;
                            self.operand_stack
                                .push(Value::Boolean(matches!(val, Value::NoneValue)));
                        }
                        OpCode::Cons => {
                            if self.operand_stack.len() < 2 {
                                return Err(NsError::Vm("CONS requires item and list".to_string()));
                            }
                            let item = self.operand_stack.pop().unwrap(); // item is popped first
                            let list_val = self.operand_stack.pop().unwrap(); // then list

                            match list_val {
                                Value::List(list_rc) => {
                                    let mut new_list_vec = vec![item];
                                    new_list_vec.extend_from_slice(&list_rc);
                                    self.operand_stack.push(Value::List(Rc::new(new_list_vec)));
                                }
                                Value::NoneValue => {
                                    // cons onto 'none' creates a new list with one item
                                    self.operand_stack.push(Value::List(Rc::new(vec![item])));
                                }
                                _ => return Err(NsError::Vm(format!(
                                    "CONS expects a list or 'none' as its second argument, got {}",
                                    list_val.type_name()
                                ))),
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
                                        // Rest of a single-item list is 'none'
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
                        // Type Predicates
                        OpCode::IsBoolean => {
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("IS_BOOLEAN requires one operand".to_string())
                            })?;
                            self.operand_stack
                                .push(Value::Boolean(matches!(val, Value::Boolean(_))));
                        }
                        OpCode::IsNumber => {
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("IS_NUMBER requires one operand".to_string())
                            })?;
                            self.operand_stack
                                .push(Value::Boolean(matches!(val, Value::Number(_))));
                        }
                        OpCode::IsString => {
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("IS_STRING requires one operand".to_string())
                            })?;
                            self.operand_stack
                                .push(Value::Boolean(matches!(val, Value::String(_))));
                        }
                        OpCode::IsList => {
                            // As per language_definition.MD: is_list checks for Value::List or Value::NoneValue
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("IS_LIST requires one operand".to_string())
                            })?;
                            self.operand_stack.push(Value::Boolean(matches!(
                                val,
                                Value::List(_) | Value::NoneValue
                            )));
                        }
                        OpCode::IsStruct => {
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("IS_STRUCT requires one operand".to_string())
                            })?;
                            self.operand_stack
                                .push(Value::Boolean(matches!(val, Value::StructInstance(_))));
                        }
                        OpCode::IsFunction => {
                            let val = self.operand_stack.pop().ok_or_else(|| {
                                NsError::Vm("IS_FUNCTION requires one operand".to_string())
                            })?;
                            self.operand_stack
                                .push(Value::Boolean(matches!(val, Value::Closure(_))));
                        }
                        _ => {
                            return Err(NsError::Vm(format!(
                                "OpCode::Operation {:?} not yet implemented in VM run loop",
                                op
                            )))
                        }
                    }
                }

                BytecodeInstruction::Push(value) => {
                    self.operand_stack.push(value);
                }
                BytecodeInstruction::StoreGlobal(name) => {
                    let value = self.operand_stack.pop().ok_or_else(|| {
                        NsError::Vm(format!("STORE_GLOBAL '{}' requires a value on stack", name))
                    })?;
                    if let Some(global_scope_rc) = self.current_env_chain.first() {
                        global_scope_rc.borrow_mut().insert(Rc::new(name), value);
                    } else {
                        return Err(NsError::Vm(
                            "Critical: Global environment scope missing".to_string(),
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
                BytecodeInstruction::MakeClosure { label, .. } => {
                    let closure_name_opt = if label.starts_with("fn_body_") {
                        label.split('_').last().map(|s| Rc::new(s.to_string()))
                    } else if label.contains("_lambda_body_") {
                        None
                    } else {
                        None
                    };

                    let closure = Closure {
                        name: closure_name_opt,
                        arity: 0, // Placeholder: Arity should be set by codegen based on FnNode/LambdaNode
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
                            let closure = (*closure_rc).clone();
                            // TODO: Arity check: if closure.arity != 0 && closure.arity != arity { error }

                            let frame = CallFrame {
                                return_pc: self.ip,
                                caller_segment_name: self.current_segment_name.clone(),
                                previous_env_chain: self.current_env_chain.clone(),
                            };
                            self.call_stack.push(frame);

                            let new_local_scope = Rc::new(RefCell::new(HashMap::new()));
                            self.current_env_chain = closure.defining_env;
                            self.current_env_chain.push(new_local_scope);

                            let target_segment_name_rc = Rc::new(closure.code_label);
                            self.current_segment_name = target_segment_name_rc.clone();
                            self.current_bytecode = self
                                .bytecode_segments
                                .get(&target_segment_name_rc)
                                .ok_or_else(|| {
                                    NsError::Vm(format!(
                                        "Function body segment '{}' not found.",
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
                            )))
                        }
                    }
                }
                BytecodeInstruction::Jump(ref target) => {
                    self.ip = self.resolve_jump_target(target)?;
                }
                BytecodeInstruction::JumpIfFalse(ref target) => {
                    let val = self
                        .operand_stack
                        .pop()
                        .ok_or_else(|| NsError::Vm("JUMP_IF_FALSE requires a value".to_string()))?;
                    let condition_is_false = match val {
                        Value::Boolean(false) => true,
                        Value::NoneValue => true,
                        _ => false,
                    };
                    if condition_is_false {
                        self.ip = self.resolve_jump_target(target)?;
                    }
                }
                BytecodeInstruction::LabelDef(_) => {
                    return Err(NsError::Vm(format!("Encountered LabelDef instruction during execution in segment '{}' at IP {}. Labels should be pre-processed.", self.current_segment_name, self.ip -1)));
                }
                BytecodeInstruction::StoreLocal(index) => {
                    if self.operand_stack.is_empty() {
                        return Err(NsError::Vm(format!(
                            "STORE_LOCAL index {} requires a value on stack",
                            index
                        )));
                    }
                    let value = self.operand_stack.pop().unwrap();
                    let var_name = Rc::new(format!("__local_{}", index));
                    self.current_local_scope_mut().insert(var_name, value);
                }
                BytecodeInstruction::LoadLocal(index) => {
                    let var_name = Rc::new(format!("__local_{}", index));
                    match self.lookup_variable(&var_name) {
                        Some(value) => self.operand_stack.push(value),
                        None => {
                            return Err(NsError::Vm(format!(
                                "Local variable (index {}) as '{}' not defined.",
                                index, var_name
                            )))
                        }
                    }
                }
                BytecodeInstruction::MakeList { count } => {
                    if self.operand_stack.len() < count {
                        return Err(NsError::Vm(format!(
                            "MAKE_LIST requires {} items on stack, found {}",
                            count,
                            self.operand_stack.len()
                        )));
                    }
                    let mut new_list = Vec::with_capacity(count);
                    for _ in 0..count {
                        new_list.push(self.operand_stack.pop().unwrap()); // Pop items
                    }
                    new_list.reverse(); // Items are popped in reverse order of how they were pushed for the list
                    self.operand_stack.push(Value::List(Rc::new(new_list)));
                }
                // TODO: Implement MakeStruct, GetField, SetField
                _ => {
                    return Err(NsError::Vm(format!(
                        "BytecodeInstruction {:?} not yet implemented in VM run loop",
                        instruction
                    )))
                }
            }
        }
        Ok(self.operand_stack.pop())
    }

    fn resolve_jump_target(&self, target: &StringOrPc) -> Result<usize, NsError> {
        match target {
            StringOrPc::Label(label_name) => self
                .label_to_pc
                .get(&self.current_segment_name)
                .and_then(|labels_in_segment| labels_in_segment.get(label_name))
                .copied()
                .ok_or_else(|| {
                    NsError::Vm(format!(
                        "Undefined label '{}' in segment '{}'",
                        label_name, self.current_segment_name
                    ))
                }),
            StringOrPc::Pc(pc_value) => Ok(*pc_value),
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::NoneValue => "none",
            Value::QuotedSymbol(_) => "quoted_symbol",
            Value::List(_) => "list",
            Value::Closure(_) => "closure",
            Value::StructInstance(_) => "struct_instance", // Need to uncomment StructData import for this
        }
    }
}
