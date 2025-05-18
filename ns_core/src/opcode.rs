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
    Eq = 20,
    Lt = 21,
    Gt = 22,
    Not = 23,
    Load = 30,
    Store = 31, // These might become LoadGlobal/StoreGlobal or be differentiated
    Jump = 40,
    JumpIfFalse = 41,
    MakeClosure = 45, // The BytecodeInstruction variant will carry more info
    Call = 50,        // The BytecodeInstruction variant will carry arity
    Return = 51,
    MakeStruct = 55,
    GetField = 56,
    SetField = 57,
    Halt = 60,
    Print = 61,
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
    LoadLocal(usize),  // Index into current frame's local values vector
    StoreLocal(usize), // Index into current frame's local values vector
    Jump(StringOrPc),
    JumpIfFalse(StringOrPc),
    MakeClosure {
        label: String, // Label of the function's code segment
        arity: usize,  // Number of parameters the closure expects
                       // Future: upvalue_capture_info: Vec<UpvalueCaptureDesc>,
    },
    Call {
        arity: usize, // Number of arguments being passed
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
