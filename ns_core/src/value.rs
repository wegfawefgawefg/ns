// ns_project_root/ns_core/src/value.rs
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// Represents a dynamic value in the "ns" language.
#[derive(Clone, Debug)]
pub enum Value {
    Number(f64), // Using f64 for all numbers (integers and floats)
    Boolean(bool),
    String(Rc<String>),       // Rc for cheap cloning of strings
    NoneValue,                // Represents "ns" language's 'none'
    QuotedSymbol(Rc<String>), // For 'symbol literals
    List(Rc<Vec<Value>>),     // Immutable list, Rc for sharing
    Closure(Rc<Closure>),
    StructInstance(Rc<RefCell<StructData>>), // Shared and mutable struct instances
                                             // Future: BuiltinFunction(BuiltinFn),
}

/// Represents a closure in "ns".
#[derive(Clone, Debug)]
pub struct Closure {
    pub name: Option<Rc<String>>, // Optional name, e.g., for debugging or from (fn ...)
    pub arity: usize,
    pub code_label: String, // Label or starting PC of the function's bytecode
    pub defining_env: EnvironmentChain, // Captured lexical environment
                            // Future: upvalues if you optimize closure environments
}

/// Represents the actual data of a struct instance.
#[derive(Clone, Debug, PartialEq)] // PartialEq for testing, might need custom impl if Value doesn't derive it simply
pub struct StructData {
    pub type_name: Rc<String>, // The name of the struct type (e.g., "Point")
    pub fields: HashMap<Rc<String>, Value>, // Field names to their values
}

/// An environment chain, where each element is a scope (a HashMap of name to Value).
/// Rc<RefCell<...>> allows shared, mutable access to scopes, which is needed for closures
/// that can modify variables in outer scopes (if your language supports this directly,
/// or if environments are set up this way for defining_env).
/// Simpler: Vec<HashMap<String, Value>> if envs are cloned per closure or always immutable.
/// For lexical scoping and closures capturing environments that might persist, sharing is common.
pub type EnvironmentChain = Vec<Rc<RefCell<HashMap<Rc<String>, Value>>>>;

// --- PartialEq Implementation ---
// Implementing PartialEq manually because f64 does not have total equality (NaN != NaN),
// and closures/structs are typically compared by reference or not at all for value equality.
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => {
                // Handle NaN: if both are NaN, they are not equal by IEEE 754.
                // If you want them to be equal in your language, you'd add:
                // (n1.is_nan() && n2.is_nan()) || n1 == n2
                n1 == n2
            }
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::String(s1), Value::String(s2)) => Rc::ptr_eq(s1, s2) || s1 == s2, // Compare content if not same Rc
            (Value::NoneValue, Value::NoneValue) => true,
            (Value::QuotedSymbol(s1), Value::QuotedSymbol(s2)) => Rc::ptr_eq(s1, s2) || s1 == s2,
            (Value::List(l1), Value::List(l2)) => Rc::ptr_eq(l1, l2) || l1 == l2, // Deep comparison if needed
            (Value::Closure(c1), Value::Closure(c2)) => Rc::ptr_eq(c1, c2), // Closures are generally compared by reference
            (Value::StructInstance(si1), Value::StructInstance(si2)) => Rc::ptr_eq(si1, si2), // Structs also by reference for mutable ones
            _ => false, // Different types
        }
    }
}
// Note: For lists and structs, if you need value equality (deep comparison),
// the PartialEq impl would need to iterate through elements/fields.
// For now, Rc::ptr_eq implies reference equality for shared data, or full comparison if not shared.

// --- Display Implementation (for PRINT opcode) ---
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => {
                // Attempt to print integers without .0
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s), // Consider printing with quotes for debug clarity if needed
            Value::NoneValue => write!(f, "none"),
            Value::QuotedSymbol(s) => write!(f, "'{}", s),
            Value::List(items_rc) => {
                let items = &**items_rc; // Dereference Rc<Vec<Value>> to &Vec<Value>
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Closure(closure_rc) => {
                if let Some(name) = &closure_rc.name {
                    write!(f, "<closure:{}>", name)
                } else {
                    write!(f, "<closure_anon:{}>", closure_rc.code_label)
                }
            }
            Value::StructInstance(struct_data_rc) => {
                let struct_data = struct_data_rc.borrow();
                write!(f, "(struct {} {{ ", struct_data.type_name)?;
                let mut first = true;
                for (key, val) in &struct_data.fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, val)?;
                    first = false;
                }
                write!(f, " }})")
            }
        }
    }
}
