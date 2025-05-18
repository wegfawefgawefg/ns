// ns_project_root/ns_core/src/error.rs
use std::fmt;

#[derive(Debug)]
pub enum NsError {
    Lexer(String),
    Parser(String),
    Codegen(String),
    Vm(String),         // For runtime errors
    Resolution(String), // For errors like undefined variables or labels
    Io(std::io::Error),
    Argument(String), // For incorrect function arguments, etc.
    Type(String),     // For type mismatches
}

impl fmt::Display for NsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NsError::Lexer(msg) => write!(f, "Lexer Error: {}", msg),
            NsError::Parser(msg) => write!(f, "Parser Error: {}", msg),
            NsError::Codegen(msg) => write!(f, "Code Generation Error: {}", msg),
            NsError::Vm(msg) => write!(f, "VM Runtime Error: {}", msg),
            NsError::Resolution(msg) => write!(f, "Resolution Error: {}", msg),
            NsError::Io(err) => write!(f, "IO Error: {}", err),
            NsError::Argument(msg) => write!(f, "Argument Error: {}", msg),
            NsError::Type(msg) => write!(f, "Type Error: {}", msg),
        }
    }
}

// This allows NsError to be treated as a standard Rust error.
impl std::error::Error for NsError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            NsError::Io(err) => Some(err),
            _ => None,
        }
    }
}

// Convenience From implementations
impl From<std::io::Error> for NsError {
    fn from(err: std::io::Error) -> NsError {
        NsError::Io(err)
    }
}

// We'll likely add more specific error types or From implementations as we go.
// For example, for parser errors from a parsing library if you use one.
