// ns_project_root/ns_core/src/opcode.rs

// We'll need the Value type for PUSH, so we'll define it in value.rs next.
// The `crate::` prefix means it's looking for a module in the current crate (ns_core).
use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)] // Optional: ensures it's a single byte, useful for compact bytecode
pub enum OpCode {
    // Stack Manipulation
    Push = 1, // Argument will be a Value, handled by BytecodeInstruction::PushValue
    Pop = 2,

    // Arithmetic / Logical Operations
    Add = 10,
    Sub = 11,
    Mul = 12,
    Div = 13,
    Eq = 20,
    Lt = 21,
    Gt = 22,
    Not = 23,

    // Variable Access
    Load = 30,  // Argument: variable name (String)
    Store = 31, // Argument: variable name (String)

    // Control Flow
    Jump = 40,        // Argument: label name (String) or target IP (usize)
    JumpIfFalse = 41, // Argument: label name (String) or target IP (usize)

    // Function Calls & Closures
    MakeClosure = 45, // Argument: code label (String) for the function body
    Call = 50,        // Argument: number of arguments (usize)
    Return = 51,

    // Struct Operations
    MakeStruct = 55, // Arguments: struct type name (String), field names (Vec<String>)
    GetField = 56,   // Argument: field name (String)
    SetField = 57,   // Argument: field name (String)

    // VM Control
    Halt = 60,
    Print = 61,

    // List Primitives (as per your language_definition.MD v20)
    IsNone = 70, // Checks if top of stack is Value::NoneValue
    Cons = 71,
    First = 72,
    Rest = 73,
    MakeList = 74, // Argument: number of items to take from stack (usize)

    // Type Predicates (as per your language_definition.MD v20)
    IsBoolean = 80,
    IsNumber = 81,
    IsString = 82,
    IsList = 83, // Checks for Value::List or Value::NoneValue
    IsStruct = 84,
    IsFunction = 85, // Checks for Value::Closure (or a future Value::BuiltinFunction)
}

/// Represents a single bytecode instruction, including its arguments.
/// This provides a more type-safe way to handle instructions than OpCode + raw args.
#[derive(Debug, Clone, PartialEq)]
pub enum BytecodeInstruction {
    // Instructions without immediate arguments (beyond what's on the stack)
    Operation(OpCode), // For Pop, Add, Sub, Mul, Div, Eq, Lt, Gt, Not, Return, Halt, Print, IsNone, Cons, First, Rest, IsBoolean, IsNumber, IsString, IsList, IsStruct, IsFunction

    // Instructions with immediate arguments
    Push(Value),         // Pushes a literal value onto the stack
    LoadGlobal(String),  // Loads a global variable by name
    StoreGlobal(String), // Stores into a global variable by name
    LoadLocal(usize),    // Loads a local variable by stack frame offset or index
    StoreLocal(usize),   // Stores into a local variable by stack frame offset or index
    Jump(StringOrPc),    // Jumps to a label (String) or PC offset (isize/usize)
    JumpIfFalse(StringOrPc),
    MakeClosure {
        label: String, // Label of the function's code
                       // arity: usize,       // Could also store arity here
                       // upvalue_count: usize // Info about captured variables if using a different upvalue strategy
    },
    Call {
        arity: usize,
    },
    MakeStruct {
        type_name: String,
        field_names: Vec<String>, // Order of fields matters for initialization
    },
    GetField(String),
    SetField(String),
    MakeList {
        count: usize,
    },
    LabelDef(String),
}

/// Used for jump targets which can either be a label (to be resolved) or a direct PC.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringOrPc {
    Label(String),
    Pc(usize),
}
