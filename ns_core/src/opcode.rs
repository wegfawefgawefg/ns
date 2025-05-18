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
    Modulo = 14,
    Eq = 20,
    Lt = 21,
    Gt = 22,
    Not = 23,
    GreaterThanOrEqual = 24,
    LessThanOrEqual = 25,
    NotEqual = 26,

    Load = 30,  // To be deprecated or clarified (use LoadGlobal/LoadLocal)
    Store = 31, // To be deprecated or clarified (use StoreGlobal/StoreLocal)
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

    EnterScope = 90, // New: For lexical scopes like 'let'
    ExitScope = 91,  // New: To pop lexical scopes
}

#[derive(Debug, Clone, PartialEq)]
pub enum BytecodeInstruction {
    Operation(OpCode),
    Push(Value),
    LoadGlobal(String),      // Load from global/module or outer lexical scope
    StoreGlobal(String),     // Store to global/module or innermost lexical scope
    LoadLocal(usize), // Load from function's indexed local slot (parameters, locals from simple let)
    StoreLocal(usize), // Store to function's indexed local slot
    Jump(StringOrPc), // Target label name or direct PC
    JumpIfFalse(StringOrPc), // Target label name or direct PC
    MakeClosure {
        label: String, // Label of the function body's bytecode segment
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
    LabelDef(String), // Definition of a label, removed before VM execution
}

/// Represents a jump target that can either be a symbolic label (resolved by VM loader)
/// or a direct program counter (PC) value (if pre-resolved, though current VM resolves labels).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringOrPc {
    Label(String),
    Pc(usize), // Currently not used by codegen, labels are preferred
}
