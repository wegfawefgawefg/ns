// ns_project_root/ns_core/src/lexer.rs
use crate::error::NsError;
use lazy_static::lazy_static;
use regex::Regex;
use std::rc::Rc; // For string sharing in TokenValue if needed

#[derive(Debug, Clone, PartialEq, Eq, Copy)] // Copy if all variants are simple
pub enum TokenType {
    // Single-character tokens
    Lparen, // (
    Rparen, // )
    Quote,  // ' (apostrophe for quoting)

    // Literals
    Symbol,  // e.g., my_var, +, -, etc.
    Number,  // e.g., 123, 3.14, -10
    String,  // e.g., "hello world"
    Boolean, // true, false
    NoneLit, // none

    // Keywords (can also be handled as Symbols then differentiated by parser if preferred)
    // For now, 'none', 'true', 'false' are distinct TokenTypes for literals.
    Eof, // End of File
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenLiteralValue {
    Number(f64),
    String(Rc<String>), // Using Rc for efficient string cloning if strings are large/reused
    Boolean(bool),
    // None is represented by TokenType::NoneLit and doesn't need a value here.
    // Symbols carry their "value" in the lexeme.
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String, // The actual text slice from the source
    pub literal: Option<TokenLiteralValue>, // For number, string, boolean literals
    pub line: usize,    // 1-indexed line number
    pub col: usize,     // 1-indexed column number (character offset from start of line)
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<TokenLiteralValue>,
        line: usize,
        col: usize,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
            col,
        }
    }
}

// Using lazy_static to compile regexes only once
lazy_static! {
    // Order matters for matching! Keywords before general symbols, float before int.
    static ref TOKEN_REGEX_PATTERNS: Vec<(TokenType, Regex)> = vec![
        // Comments (will be skipped, not tokenized)
        // Whitespace (will be skipped)
        (TokenType::Lparen, Regex::new(r"^\(").unwrap()),
        (TokenType::Rparen, Regex::new(r"^\)").unwrap()),
        (TokenType::Quote, Regex::new(r"^'").unwrap()),

        // Literals - Keywords first
        (TokenType::Boolean, Regex::new(r"^\b(true|false)\b").unwrap()),
        (TokenType::NoneLit, Regex::new(r"^\bnone\b").unwrap()), // "none" for ns language

        // String literals: "(?:\\.|[^"\\])*"
        (TokenType::String, Regex::new(r#"^"(?:\\.|[^"\\])*""#).unwrap()),

        // Numbers: float before integer to ensure correct matching
        (TokenType::Number, Regex::new(r"^-?\d+\.\d+").unwrap()), // Floats
        (TokenType::Number, Regex::new(r"^-?\d+").unwrap()),      // Integers

        // Symbols: (should be fairly broad, includes operators)
        // Must not start with a digit if it's not a number.
        // Allows underscores, alphanumeric, and typical operator characters.
        (TokenType::Symbol, Regex::new(r"^[a-zA-Z_+\-*/%<>=!?&|^~@][a-zA-Z0-9_+\-*/%<>=!?&|^~@]*").unwrap()),
        // Simpler symbol regex (if the above is too complex or has issues with Rust regex engine quirks):
        // (TokenType::Symbol, Regex::new(r"^[^\s\(\)'""\d][^\s\(\)'""]*").unwrap()), // Avoids whitespace, parens, quotes, and doesn't start with digit
    ];
    static ref COMMENT_REGEX: Regex = Regex::new(r"^//[^\n]*").unwrap();
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s+").unwrap(); // Matches one or more whitespace characters
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, NsError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut current_offset = 0; // Byte offset in the source string
    let mut line: usize = 1;
    // Byte offset of the beginning of the current line from the start of the source string
    let mut line_start_offset: usize = 0;

    while current_offset < source.len() {
        let remaining_source = &source[current_offset..];
        let current_char_col = source[line_start_offset..current_offset].chars().count() + 1;

        // 1. Skip Whitespace
        if let Some(mat) = WHITESPACE_REGEX.find(remaining_source) {
            // mat.start() should be 0 if it matches at the current position
            if mat.start() == 0 {
                let matched_str = mat.as_str();
                for ch in matched_str.chars() {
                    if ch == '\n' {
                        line += 1;
                        line_start_offset = current_offset + ch.len_utf8(); // Next char after \n
                    }
                }
                current_offset += matched_str.len();
                continue;
            }
        }

        // 2. Skip Comments
        if let Some(mat) = COMMENT_REGEX.find(remaining_source) {
            if mat.start() == 0 {
                // Comments run to EOL but don't consume the newline itself usually
                // The newline will be handled by whitespace skipping next iteration.
                current_offset += mat.as_str().len();
                continue;
            }
        }

        // 3. Match Tokens
        let mut found_match = false;
        for (token_type, regex) in TOKEN_REGEX_PATTERNS.iter() {
            if let Some(mat) = regex.find(remaining_source) {
                // Ensure the match is at the beginning of the current slice
                if mat.start() == 0 {
                    let lexeme = mat.as_str().to_string();
                    let mut literal_value: Option<TokenLiteralValue> = None;

                    match token_type {
                        TokenType::Number => match lexeme.parse::<f64>() {
                            Ok(num) => literal_value = Some(TokenLiteralValue::Number(num)),
                            Err(_) => {
                                return Err(NsError::Lexer(format!(
                                    "Invalid number format: '{}' at line {}, col {}",
                                    lexeme, line, current_char_col
                                )))
                            }
                        },
                        TokenType::String => {
                            // Remove surrounding quotes and unescape
                            let s_val = lexeme[1..lexeme.len() - 1]
                                .to_string() // Basic unescaping, can be expanded
                                .replace("\\n", "\n")
                                .replace("\\\"", "\"")
                                .replace("\\t", "\t")
                                .replace("\\\\", "\\");
                            literal_value = Some(TokenLiteralValue::String(Rc::new(s_val)));
                        }
                        TokenType::Boolean => {
                            literal_value = Some(TokenLiteralValue::Boolean(lexeme == "true"));
                        }
                        TokenType::NoneLit => {
                            // No specific literal value needed beyond the type
                        }
                        _ => {} // Lparen, Rparen, Quote, Symbol
                    }

                    tokens.push(Token::new(
                        *token_type,
                        lexeme,
                        literal_value,
                        line,
                        current_char_col,
                    ));
                    current_offset += mat.as_str().len();
                    found_match = true;
                    break; // Found the longest match from the ordered patterns
                }
            }
        }

        if !found_match {
            let offending_char = remaining_source.chars().next().unwrap_or('?');
            return Err(NsError::Lexer(format!(
                "Unexpected character: '{}' at line {}, col {}",
                offending_char, line, current_char_col
            )));
        }
    }

    tokens.push(Token::new(
        TokenType::Eof,
        String::new(), // Empty lexeme for EOF
        None,
        line,
        source[line_start_offset..].chars().count() + 1, // Column after last char or 1 if line is empty
    ));
    Ok(tokens)
}
