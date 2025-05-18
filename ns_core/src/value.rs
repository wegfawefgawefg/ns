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
    StructInstance(Rc<RefCell<StructData>>),
}

/// Represents a closure in "ns".
#[derive(Clone, Debug)]
pub struct Closure {
    pub name: Option<Rc<String>>, // Optional name for debugging/reflection
    pub arity: usize,
    pub code_label: String, // Label of the bytecode segment for this closure's body
    pub defining_env: EnvironmentChain, // The lexical environment captured at definition time
    pub param_names: Vec<Rc<String>>, // Names of the parameters, in order
}

/// Represents the actual data of a struct instance.
#[derive(Clone, Debug, PartialEq)]
pub struct StructData {
    pub type_name: Rc<String>,
    pub fields: HashMap<Rc<String>, Value>,
}

// Scope is now always a lexical scope (HashMap for named bindings)
#[derive(Clone, Debug)]
pub enum Scope {
    // Locals(RefCell<Vec<Value>>), // REMOVED
    Lexical(Rc<RefCell<HashMap<String, Value>>>), // Key is String (variable name)
}

pub type EnvironmentChain = Vec<Scope>; // A list of lexical scopes

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::String(s1), Value::String(s2)) => Rc::ptr_eq(s1, s2) || **s1 == **s2,
            (Value::NoneValue, Value::NoneValue) => true,
            (Value::QuotedSymbol(s1), Value::QuotedSymbol(s2)) => {
                Rc::ptr_eq(s1, s2) || **s1 == **s2
            }
            (Value::List(l1), Value::List(l2)) => {
                if Rc::ptr_eq(l1, l2) {
                    return true;
                }
                if l1.len() != l2.len() {
                    return false;
                }
                l1.iter().zip(l2.iter()).all(|(a, b)| a == b)
            }
            (Value::Closure(c1), Value::Closure(c2)) => Rc::ptr_eq(c1, c2),
            (Value::StructInstance(si1_rc), Value::StructInstance(si2_rc)) => {
                if Rc::ptr_eq(si1_rc, si2_rc) {
                    return true;
                }
                si1_rc.borrow().eq(&si2_rc.borrow())
            }
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
            Value::String(s) => write!(f, "{}", s),
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
                    let params_str = closure_rc
                        .param_names
                        .iter()
                        .map(|p| p.as_str())
                        .collect::<Vec<&str>>()
                        .join(" ");
                    write!(
                        f,
                        "<lambda:({})#{}:{}>",
                        params_str, closure_rc.code_label, closure_rc.arity
                    )
                }
            }
            Value::StructInstance(struct_data_rc) => {
                let struct_data = struct_data_rc.borrow();
                write!(f, "(struct {} {{ ", struct_data.type_name)?;
                let mut field_items: Vec<String> = struct_data
                    .fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                field_items.sort();
                write!(f, "{}", field_items.join(", "))?;
                write!(f, " }})")
            }
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
            Value::QuotedSymbol(_) => "quoted-symbol",
            Value::List(_) => "list",
            Value::Closure(_) => "closure",
            Value::StructInstance(_) => "struct-instance",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::NoneValue => false,
            _ => true,
        }
    }
}

// Ensure Scope also implements PartialEq if it's used in comparisons, though unlikely directly.
// For debugging, ensure it can be printed.
impl Scope {
    #[allow(dead_code)]
    pub fn type_name_debug(&self) -> &'static str {
        match self {
            Scope::Lexical(_) => "Scope::Lexical",
        }
    }
}
