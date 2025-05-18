// ns_project_root/ns_core/src/opcode.rs
use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    Push = 1,
    Pop = 2,
    Add = 10,
    Sub = 11,
    Mul = 12,
    Div = 13,
    Modulo = 14, // New: Modulo
    Eq = 20,
    Lt = 21,
    Gt = 22,
    Not = 23,
    GreaterThanOrEqual = 24,
    LessThanOrEqual = 25,
    NotEqual = 26, // New comparison ops

    Load = 30,
    Store = 31,
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
    ThrowError = 62, // New: ThrowError

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
}

#[derive(Debug, Clone, PartialEq)]
pub enum BytecodeInstruction {
    Operation(OpCode),
    Push(Value),
    LoadGlobal(String),
    StoreGlobal(String),
    LoadLocal(usize),
    StoreLocal(usize),
    Jump(StringOrPc),
    JumpIfFalse(StringOrPc),
    MakeClosure {
        label: String,
        arity: usize,
    },
    Call {
        arity: usize,
    },
    MakeStruct {
        type_name: String,
        field_names: Vec<String>, // Order of fields for initialization
    },
    GetField(String), // Field name
    SetField(String), // Field name
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
