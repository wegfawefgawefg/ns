// ns_project_root/ns_core/src/value.rs
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// Represents a dynamic value in the "ns" language.
#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Rc<String>),
    NoneValue,
    QuotedSymbol(Rc<String>),
    List(Rc<Vec<Value>>),
    Closure(Rc<Closure>),
    StructInstance(Rc<RefCell<StructData>>), // StructData import will be used later
}

/// Represents a closure in "ns".
#[derive(Clone, Debug)]
pub struct Closure {
    pub name: Option<Rc<String>>,
    pub arity: usize,
    pub code_label: String,
    pub defining_env: EnvironmentChain,
}

/// Represents the actual data of a struct instance.
#[derive(Clone, Debug, PartialEq)]
pub struct StructData {
    pub type_name: Rc<String>,
    pub fields: HashMap<Rc<String>, Value>,
}

#[derive(Clone, Debug)]
pub enum Scope {
    Locals(RefCell<Vec<Value>>),
    Lexical(Rc<RefCell<HashMap<Rc<String>, Value>>>),
}

pub type EnvironmentChain = Vec<Scope>;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::String(s1), Value::String(s2)) => Rc::ptr_eq(s1, s2) || s1 == s2,
            (Value::NoneValue, Value::NoneValue) => true,
            (Value::QuotedSymbol(s1), Value::QuotedSymbol(s2)) => Rc::ptr_eq(s1, s2) || s1 == s2,
            (Value::List(l1), Value::List(l2)) => Rc::ptr_eq(l1, l2) || l1 == l2, // Basic reference equality for lists
            (Value::Closure(c1), Value::Closure(c2)) => Rc::ptr_eq(c1, c2), // Closures by reference
            (Value::StructInstance(si1), Value::StructInstance(si2)) => Rc::ptr_eq(si1, si2), // Structs by reference
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s), // Consider f.write_str(s) for directness
            Value::NoneValue => write!(f, "none"),
            Value::QuotedSymbol(s) => write!(f, "'{}", s),
            Value::List(items_rc) => {
                let items = &**items_rc;
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
                    write!(f, "<closure:{}:{}>", name, closure_rc.arity)
                } else {
                    write!(f, "<lambda:{}:{}>", closure_rc.code_label, closure_rc.arity)
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

// Methods for Value
impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::NoneValue => "none",
            Value::QuotedSymbol(_) => "quoted-symbol", // Lispier name
            Value::List(_) => "list",
            Value::Closure(_) => "closure",
            Value::StructInstance(_) => "struct-instance",
        }
    }

    // Helper for truthiness, as per Lisp conventions (false and 'none' are falsy)
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::NoneValue => false,
            _ => true, // All other values (numbers, strings, lists, closures, structs) are truthy
        }
    }
}
