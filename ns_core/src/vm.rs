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
    current_env_chain: EnvironmentChain, // Vec<Scope>, where Scope is now only Scope::Lexical
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
        // Initialize with one global lexical scope
        self.current_env_chain = vec![Scope::Lexical(Rc::new(RefCell::new(HashMap::new())))];

        Ok(())
    }

    // `name_to_find` is &String for lookup in HashMap<String, Value>
    fn lookup_variable(&self, name_to_find: &String) -> Option<Value> {
        for scope_variant in self.current_env_chain.iter().rev() {
            match scope_variant {
                // Scope is always Scope::Lexical now
                Scope::Lexical(scope_map_rc) => {
                    let scope_map = scope_map_rc.borrow();
                    if let Some(value) = scope_map.get(name_to_find) {
                        return Some(value.clone());
                    }
                } // Scope::Locals case removed
            }
        }
        None
    }

    pub fn run(&mut self) -> Result<Option<Value>, NsError> {
        loop {
            if self.ip >= self.current_bytecode.len() {
                if self.call_stack.is_empty() {
                    break;
                } else {
                    return Err(NsError::Vm(format!(
                        "Reached end of function segment '{}' without Return.",
                        self.current_segment_name
                    )));
                }
            }

            let instruction = self.current_bytecode[self.ip].clone();
            self.ip += 1;

            match instruction {
                BytecodeInstruction::Operation(op) => match op {
                    OpCode::Halt => {
                        println!("Execution halted.");
                        return Ok(self.operand_stack.pop());
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
                        match (left_op.clone(), right_op.clone()) {
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
                                    _ => unreachable!(),
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
                            _ => {
                                return Err(NsError::Vm(format!(
                                    "Type error for {:?}: Cannot operate on {:?} and {:?}",
                                    op,
                                    left_op.type_name(),
                                    right_op.type_name()
                                )))
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
                        let val = self
                            .operand_stack
                            .pop()
                            .ok_or_else(|| NsError::Vm("PRINT requires a value".to_string()))?;
                        println!("Output: {}", val);
                        self.operand_stack.push(Value::NoneValue);
                    }
                    OpCode::ThrowError => {
                        let msg = self.operand_stack.pop().ok_or_else(|| {
                            NsError::Vm("ERROR primitive requires message".to_string())
                        })?;
                        if let Value::String(s) = msg {
                            return Err(NsError::Vm(format!("User Error: {}", s)));
                        } else {
                            return Err(NsError::Vm(format!(
                                "ERROR expects string, got {}",
                                msg.type_name()
                            )));
                        }
                    }
                    OpCode::Return => {
                        if let Some(f) = self.call_stack.pop() {
                            self.ip = f.return_pc;
                            self.current_segment_name = f.caller_segment_name;
                            self.current_bytecode = self
                                .bytecode_segments
                                .get(&self.current_segment_name)
                                .unwrap()
                                .clone();
                            self.current_env_chain = f.previous_env_chain;
                        } else {
                            return Ok(self.operand_stack.pop());
                        }
                    }
                    OpCode::EnterScope => {
                        self.current_env_chain
                            .push(Scope::Lexical(Rc::new(RefCell::new(HashMap::new()))));
                    }
                    OpCode::ExitScope => {
                        if self.current_env_chain.len() > 1 {
                            let p = self.current_env_chain.pop().unwrap();
                            if !matches!(p, Scope::Lexical(_)) {
                                return Err(NsError::Vm(format!(
                                    "ExitScope popped non-lexical: {:?}",
                                    p.type_name_debug()
                                )));
                            }
                        } else {
                            return Err(NsError::Vm("ExitScope beyond global".to_string()));
                        }
                    }
                    OpCode::IsNone => {
                        let v = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_NONE needs operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(v, Value::NoneValue)));
                    }
                    OpCode::Cons => {
                        let i = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("CONS item".to_string()))?;
                        let l = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("CONS list".to_string()))?;
                        match l {
                            Value::List(lr) => {
                                let mut n = vec![i];
                                n.extend_from_slice(&lr);
                                self.operand_stack.push(Value::List(Rc::new(n)));
                            }
                            Value::NoneValue => {
                                self.operand_stack.push(Value::List(Rc::new(vec![i])));
                            }
                            _ => {
                                return Err(NsError::Vm(format!(
                                    "CONS expects list/none, got {}",
                                    l.type_name()
                                )))
                            }
                        }
                    }
                    OpCode::First => {
                        match self
                            .operand_stack
                            .pop()
                            .ok_or_else(|| NsError::Vm("FIRST list".to_string()))?
                        {
                            Value::List(l) if !l.is_empty() => {
                                self.operand_stack.push(l[0].clone())
                            }
                            Value::List(_) => {
                                return Err(NsError::Vm("FIRST on empty list".to_string()))
                            }
                            v => {
                                return Err(NsError::Vm(format!(
                                    "FIRST expects list, got {}",
                                    v.type_name()
                                )))
                            }
                        }
                    }
                    OpCode::Rest => {
                        match self
                            .operand_stack
                            .pop()
                            .ok_or_else(|| NsError::Vm("REST list".to_string()))?
                        {
                            Value::List(l) if !l.is_empty() => {
                                self.operand_stack.push(if l.len() == 1 {
                                    Value::NoneValue
                                } else {
                                    Value::List(Rc::new(l[1..].to_vec()))
                                })
                            }
                            Value::List(_) => {
                                return Err(NsError::Vm("REST on empty list".to_string()))
                            }
                            v => {
                                return Err(NsError::Vm(format!(
                                    "REST expects list, got {}",
                                    v.type_name()
                                )))
                            }
                        }
                    }
                    OpCode::IsBoolean => {
                        let v = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_BOOLEAN operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(v, Value::Boolean(_))));
                    }
                    OpCode::IsNumber => {
                        let v = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_NUMBER operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(v, Value::Number(_))));
                    }
                    OpCode::IsString => {
                        let v = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_STRING operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(v, Value::String(_))));
                    }
                    OpCode::IsList => {
                        let v = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_LIST operand".to_string()))?;
                        self.operand_stack.push(Value::Boolean(matches!(
                            v,
                            Value::List(_) | Value::NoneValue
                        )));
                    }
                    OpCode::IsStruct => {
                        let v = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_STRUCT operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(v, Value::StructInstance(_))));
                    }
                    OpCode::IsFunction => {
                        let v = self
                            .operand_stack
                            .pop()
                            .ok_or(NsError::Vm("IS_FUNCTION operand".to_string()))?;
                        self.operand_stack
                            .push(Value::Boolean(matches!(v, Value::Closure(_))));
                    }
                    _ => return Err(NsError::Vm(format!("Unhandled Op: {:?}", op))),
                },
                BytecodeInstruction::Push(value) => self.operand_stack.push(value),
                BytecodeInstruction::StoreGlobal(name_str) => {
                    let value = self.operand_stack.pop().ok_or_else(|| {
                        NsError::Vm(format!("STORE_GLOBAL '{}' needs value", name_str))
                    })?;
                    let mut stored = false;
                    // Store in the innermost lexical scope.
                    for scope in self.current_env_chain.iter_mut().rev() {
                        if let Scope::Lexical(map_rc) = scope {
                            map_rc.borrow_mut().insert(name_str.clone(), value.clone());
                            stored = true;
                            break;
                        }
                    }
                    if !stored {
                        return Err(NsError::Vm(format!(
                            "No lexical scope for StoreGlobal '{}'",
                            name_str
                        )));
                    }
                }
                BytecodeInstruction::LoadGlobal(name_str) => {
                    match self.lookup_variable(&name_str) {
                        // Pass &String
                        Some(value) => self.operand_stack.push(value),
                        None => {
                            return Err(NsError::Vm(format!(
                                "Variable '{}' not defined.",
                                name_str
                            )))
                        }
                    }
                }
                BytecodeInstruction::MakeClosure {
                    label,
                    arity,
                    param_names,
                } => {
                    let name_opt = if label.starts_with("fn_body_") {
                        label.rsplit('_').next().map(|s| Rc::new(s.to_string()))
                    } else {
                        None
                    };
                    self.operand_stack.push(Value::Closure(Rc::new(Closure {
                        name: name_opt,
                        arity,
                        code_label: label,
                        defining_env: self.current_env_chain.clone(),
                        param_names, // Store param_names in the Closure
                    })));
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
                                    "Function '{}' arity mismatch. Expected {}, got {}.",
                                    closure.name.as_ref().map_or_else(
                                        || closure.code_label.as_str(),
                                        |n| n.as_str()
                                    ),
                                    closure.arity,
                                    arity
                                )));
                            }

                            let mut fn_args_values = Vec::with_capacity(arity);
                            if self.operand_stack.len() < arity {
                                return Err(NsError::Vm(format!(
                                    "Stack underflow for CALL args: need {}, found {}",
                                    arity,
                                    self.operand_stack.len()
                                )));
                            }
                            for _ in 0..arity {
                                fn_args_values.push(self.operand_stack.pop().unwrap());
                            }
                            fn_args_values.reverse(); // Arguments are popped in reverse order of pushing

                            self.call_stack.push(CallFrame {
                                return_pc: self.ip,
                                caller_segment_name: self.current_segment_name.clone(),
                                previous_env_chain: self.current_env_chain.clone(),
                            });

                            // Set up new environment for the function call
                            self.current_env_chain = closure.defining_env.clone(); // Start with defining (lexical) environment
                            let mut new_fn_scope_map = HashMap::new();
                            for (i, param_name_rc) in closure.param_names.iter().enumerate() {
                                new_fn_scope_map.insert(
                                    param_name_rc.as_ref().clone(),
                                    fn_args_values[i].clone(),
                                ); // param_name_rc is Rc<String>, get String
                            }
                            self.current_env_chain
                                .push(Scope::Lexical(Rc::new(RefCell::new(new_fn_scope_map)))); // Add new lexical scope for params

                            let target_segment_rc = Rc::new(closure.code_label.clone());
                            self.current_segment_name = target_segment_rc.clone();
                            self.current_bytecode = self
                                .bytecode_segments
                                .get(&target_segment_rc)
                                .ok_or_else(|| {
                                    NsError::Vm(format!(
                                        "Fn segment '{}' not found.",
                                        target_segment_rc
                                    ))
                                })?
                                .clone();
                            self.ip = 0;
                        }
                        _ => {
                            return Err(NsError::Vm(format!(
                                "Cannot call non-function: {}",
                                callee_val.type_name()
                            )))
                        }
                    }
                }
                // StoreLocal and LoadLocal are removed
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
                    )))
                }
                BytecodeInstruction::MakeList { count } => {
                    if self.operand_stack.len() < count {
                        return Err(NsError::Vm(format!(
                            "MAKE_LIST needs {} items, found {}",
                            count,
                            self.operand_stack.len()
                        )));
                    }
                    let mut nl = Vec::with_capacity(count);
                    for _ in 0..count {
                        nl.push(self.operand_stack.pop().unwrap());
                    }
                    nl.reverse();
                    self.operand_stack.push(Value::List(Rc::new(nl)));
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
                    let mut fm = HashMap::new();
                    for name_str in field_names.iter().rev() {
                        fm.insert(Rc::new(name_str.clone()), self.operand_stack.pop().unwrap());
                    }
                    self.operand_stack
                        .push(Value::StructInstance(Rc::new(RefCell::new(StructData {
                            type_name: Rc::new(type_name),
                            fields: fm,
                        }))));
                }
                BytecodeInstruction::GetField(field_name_str) => {
                    match self.operand_stack.pop().ok_or_else(|| {
                        NsError::Vm(format!("GET_FIELD '{}' requires instance", field_name_str))
                    })? {
                        Value::StructInstance(sd_rc) => {
                            let sd = sd_rc.borrow();
                            let fn_rc = Rc::new(field_name_str);
                            match sd.fields.get(&fn_rc) {
                                Some(v) => self.operand_stack.push(v.clone()),
                                None => {
                                    return Err(NsError::Vm(format!(
                                        "Struct '{}' has no field '{}'",
                                        sd.type_name, fn_rc
                                    )))
                                }
                            }
                        }
                        v => {
                            return Err(NsError::Vm(format!(
                                "GET_FIELD expects struct, got {}",
                                v.type_name()
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
                    let new_val = self.operand_stack.pop().unwrap();
                    let inst_val = self.operand_stack.pop().unwrap();
                    match inst_val {
                        Value::StructInstance(sd_rc) => {
                            let mut sd = sd_rc.borrow_mut();
                            let fn_rc = Rc::new(field_name_str.clone());
                            if let std::collections::hash_map::Entry::Occupied(mut e) =
                                sd.fields.entry(fn_rc)
                            {
                                e.insert(new_val);
                                self.operand_stack
                                    .push(Value::StructInstance(sd_rc.clone()));
                            } else {
                                return Err(NsError::Vm(format!(
                                    "Struct '{}' has no field '{}' to set",
                                    sd.type_name, field_name_str
                                )));
                            }
                        }
                        v => {
                            return Err(NsError::Vm(format!(
                                "SET_FIELD expects struct, got {}",
                                v.type_name()
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
            StringOrPc::Label(label_name) => self
                .label_to_pc
                .get(&self.current_segment_name)
                .and_then(|labels_in_segment| labels_in_segment.get(label_name))
                .copied()
                .ok_or_else(|| {
                    NsError::Vm(format!(
                        "Undefined label '{}' in seg '{}'",
                        label_name, self.current_segment_name
                    ))
                }),
            StringOrPc::Pc(pc_value) => Ok(*pc_value),
        }
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new()
    }
}
