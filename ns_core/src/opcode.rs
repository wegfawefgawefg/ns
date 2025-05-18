// ns_project_root/ns_core/src/opcode.rs
use crate::value::Value;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    Push = 1,
    Pop = 2,
    Add = 10,
    Sub = 11,
    Mul = 12,
    Div = 13,
    Modulo = 14,
    Eq = 20,
    Lt = 21,
    Gt = 22,
    Not = 23,
    GreaterThanOrEqual = 24,
    LessThanOrEqual = 25,
    NotEqual = 26,

    // Load and Store (now OpCode::LoadGlobal and OpCode::StoreGlobal) are handled by BytecodeInstruction variants
    Jump = 40,
    JumpIfFalse = 41,
    MakeClosure = 45,
    Call = 50,
    Return = 51,
    MakeStruct = 55,
    GetField = 56,
    SetField = 57,
    Halt = 60,
    Print = 61,
    ThrowError = 62,

    IsNone = 70,
    Cons = 71,
    First = 72,
    Rest = 73,
    MakeList = 74,
    IsBoolean = 80,
    IsNumber = 81,
    IsString = 82,
    IsList = 83,
    IsStruct = 84,
    IsFunction = 85,

    EnterScope = 90,
    ExitScope = 91,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BytecodeInstruction {
    Operation(OpCode),
    Push(Value),
    LoadGlobal(String),  // Load from lexical environment (innermost to global)
    StoreGlobal(String), // Store to innermost lexical environment
    // LoadLocal(usize),   // REMOVED
    // StoreLocal(usize),  // REMOVED
    Jump(StringOrPc),
    JumpIfFalse(StringOrPc),
    MakeClosure {
        label: String,
        arity: usize,
        param_names: Vec<Rc<String>>, // Added: names of parameters for the new scope
    },
    Call {
        arity: usize,
    },
    MakeStruct {
        type_name: String,
        field_names: Vec<String>,
    },
    GetField(String),
    SetField(String),
    MakeList {
        count: usize,
    },
    LabelDef(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringOrPc {
    Label(String),
    Pc(usize),
}
