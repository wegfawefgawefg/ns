// ns_project_root/ns_core/src/parser.rs
use crate::ast::{
    BeginNode, CallNode, Expression, FnNode, GetNode, IfNode, LambdaNode, LetBinding, LetNode,
    ProgramNode, QuotedData, SetNode, StaticNode, StructDefNode, TopLevelForm, UseItems, UseNode,
    WhileNode,
};
use crate::error::NsError;
use crate::lexer::{Token, TokenLiteralValue, TokenType};
use std::rc::Rc;

pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
    // Store the filename or source identifier for better error messages
    // source_name: &'a str, // Optional
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    // --- Helper Methods ---

    fn current_token(&self) -> Result<&'a Token, NsError> {
        self.tokens.get(self.position).ok_or_else(|| {
            let last_token_if_any = self.tokens.last(); // Get reference to last token
            NsError::Parser(format!(
                "Unexpected end of input (attempted to read past EOF). Position: {}. Last token: {:?}",
                self.position,
                last_token_if_any.map(|t| t.token_type) 
            ))
        })
    }

    fn previous_token(&self) -> &'a Token {
        // Assumes position > 0
        &self.tokens[self.position - 1]
    }

    fn consume_token(&mut self) -> Result<&'a Token, NsError> {
        let token = self.current_token()?;
        if token.token_type != TokenType::Eof {
            // Don't advance past EOF
            self.position += 1;
        }
        Ok(token)
    }

    fn expect_token_type(&mut self, expected_type: TokenType) -> Result<&'a Token, NsError> {
        let token = self.consume_token()?;
        if token.token_type == expected_type {
            Ok(token)
        } else {
            Err(NsError::Parser(format!(
                "Expected token type {:?} but got {:?} ('{}') at line {}, col {}",
                expected_type, token.token_type, token.lexeme, token.line, token.col
            )))
        }
    }

    fn check_current_token_type(&self, expected_type: TokenType) -> bool {
        self.current_token()
            .map_or(false, |t| t.token_type == expected_type)
    }

    // --- Parsing Methods ---

    pub fn parse_program(&mut self) -> Result<ProgramNode, NsError> {
        let mut forms: Vec<TopLevelForm> = Vec::new();
        while !self.check_current_token_type(TokenType::Eof) {
            forms.push(self.parse_top_level_form()?);
        }
        Ok(ProgramNode { forms })
    }

    fn parse_top_level_form(&mut self) -> Result<TopLevelForm, NsError> {
        // In ns, definitions like (static ...) or (fn ...) look like calls at the token level initially.
        // We peek to see if the first element of a list is a defining keyword.
        if self.check_current_token_type(TokenType::Lparen) {
            // Peek ahead to see if it's a definition form
            if let Some(token_after_lparen) = self.tokens.get(self.position + 1) {
                if token_after_lparen.token_type == TokenType::Symbol {
                    match token_after_lparen.lexeme.as_str() {
                        "static" => {
                            return self.parse_static_definition().map(TopLevelForm::StaticDef)
                        }
                        "fn" => return self.parse_fn_definition().map(TopLevelForm::FnDef),
                        "struct" => {
                            return self.parse_struct_definition().map(TopLevelForm::StructDef)
                        }
                        "use" => return self.parse_use_statement().map(TopLevelForm::Use),
                        _ => {} // Fall through to parse_expression
                    }
                }
            }
            // If not a special definition keyword, or if peeking failed, parse as a general expression
            Ok(TopLevelForm::Expression(self.parse_expression()?))
        } else {
            // Top-level atoms or quotes (less common as standalone top-level forms but possible)
            Ok(TopLevelForm::Expression(self.parse_expression()?))
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, NsError> {
        let token = self.current_token()?;
        match token.token_type {
            TokenType::Lparen => self.parse_list_or_call_expr(),
            TokenType::Quote => self.parse_quote(),
            TokenType::Number
            | TokenType::String
            | TokenType::Boolean
            | TokenType::NoneLit
            | TokenType::Symbol => self.parse_atom(),
            _ => Err(NsError::Parser(format!(
                "Unexpected token {:?} ('{}') when expecting an expression at line {}, col {}",
                token.token_type, token.lexeme, token.line, token.col
            ))),
        }
    }

    fn parse_atom(&mut self) -> Result<Expression, NsError> {
        let token = self.consume_token()?;
        match token.token_type {
            TokenType::Number => {
                if let Some(TokenLiteralValue::Number(n)) = &token.literal {
                    Ok(Expression::Number(*n))
                } else {
                    Err(NsError::Parser(format!("Lexer produced Number token without Number literal: {:?} at line {}, col {}", token, token.line, token.col)))
                }
            }
            TokenType::String => {
                if let Some(TokenLiteralValue::String(s)) = &token.literal {
                    Ok(Expression::String(s.clone()))
                } else {
                    Err(NsError::Parser(format!("Lexer produced String token without String literal: {:?} at line {}, col {}", token, token.line, token.col)))
                }
            }
            TokenType::Boolean => {
                if let Some(TokenLiteralValue::Boolean(b)) = &token.literal {
                    Ok(Expression::Boolean(*b))
                } else {
                    Err(NsError::Parser(format!("Lexer produced Boolean token without Boolean literal: {:?} at line {}, col {}", token, token.line, token.col)))
                }
            }
            TokenType::NoneLit => Ok(Expression::NoneLiteral),
            TokenType::Symbol => Ok(Expression::Symbol(Rc::new(token.lexeme.clone()))),
            _ => Err(NsError::Parser(format!(
                "Unexpected token {:?} ('{}') when expecting an atom at line {}, col {}",
                token.token_type, token.lexeme, token.line, token.col
            ))),
        }
    }

    fn parse_quote(&mut self) -> Result<Expression, NsError> {
        self.expect_token_type(TokenType::Quote)?; // Consume '
        let quoted_data = self.parse_quoted_s_expression()?;
        Ok(Expression::Quote(Box::new(quoted_data)))
    }

    // Parses the S-expression data *after* a quote character.
    fn parse_quoted_s_expression(&mut self) -> Result<QuotedData, NsError> {
        let token = self.current_token()?;
        match token.token_type {
            TokenType::Lparen => {
                self.consume_token()?; // Consume Lparen
                let mut elements = Vec::new();
                while !self.check_current_token_type(TokenType::Rparen) {
                    if self.check_current_token_type(TokenType::Eof) {
                        return Err(NsError::Parser(format!(
                            "Unexpected EOF inside quoted list, expecting ')' at line {}, col {}",
                            self.current_token()?.line,
                            self.current_token()?.col
                        )));
                    }
                    elements.push(self.parse_quoted_s_expression()?);
                }
                self.consume_token()?; // Consume Rparen
                Ok(QuotedData::List(elements))
            }
            TokenType::Quote => {
                // Handles nested quotes like ' 'foo or '(a 'b)
                self.consume_token()?; // Consume inner '
                Ok(QuotedData::Quote(Box::new(
                    self.parse_quoted_s_expression()?,
                )))
            }
            TokenType::Symbol => {
                let t = self.consume_token()?;
                Ok(QuotedData::Symbol(Rc::new(t.lexeme.clone())))
            }
            TokenType::Number => {
                let t = self.consume_token()?;
                if let Some(TokenLiteralValue::Number(n)) = t.literal {
                    Ok(QuotedData::Number(n))
                } else {
                    unreachable!("Number token must have number literal")
                }
            }
            TokenType::String => {
                let t = self.consume_token()?;
                if let Some(TokenLiteralValue::String(s)) = &t.literal {
                    Ok(QuotedData::String(s.clone()))
                } else {
                    unreachable!("String token must have string literal")
                }
            }
            TokenType::Boolean => {
                let t = self.consume_token()?;
                if let Some(TokenLiteralValue::Boolean(b)) = t.literal {
                    Ok(QuotedData::Boolean(b))
                } else {
                    unreachable!("Boolean token must have boolean literal")
                }
            }
            TokenType::NoneLit => {
                self.consume_token()?;
                Ok(QuotedData::None)
            }
            _ => Err(NsError::Parser(format!(
                "Unexpected token {:?} ('{}') inside quoted expression at line {}, col {}",
                token.token_type, token.lexeme, token.line, token.col
            ))),
        }
    }

    fn parse_list_or_call_expr(&mut self) -> Result<Expression, NsError> {
        let lparen_token = self.expect_token_type(TokenType::Lparen)?; // Consume Lparen

        if self.check_current_token_type(TokenType::Rparen) {
            self.consume_token()?; // Consume Rparen
            return Err(NsError::Parser(format!(
                "Empty list '()' is not a valid expression directly at line {}, col {}. Use '(list)' or 'none'.",
                lparen_token.line, lparen_token.col
            )));
        }

        // Peek at the first element to see if it's a special form keyword
        let first_expr_token = self.current_token()?;
        if first_expr_token.token_type == TokenType::Symbol {
            match first_expr_token.lexeme.as_str() {
                // These are expression-producing special forms
                "if" => return self.parse_if_expression(),
                "let" => {
                    return self
                        .parse_let_expression()
                        .map(|n| Expression::Let(Box::new(n)))
                }
                "lambda" => {
                    return self
                        .parse_lambda_expression()
                        .map(|n| Expression::Lambda(Box::new(n)))
                }
                "get" => {
                    return self
                        .parse_get_expression()
                        .map(|n| Expression::Get(Box::new(n)))
                }
                "set" => {
                    return self
                        .parse_set_expression()
                        .map(|n| Expression::Set(Box::new(n)))
                }
                "while" => {
                    return self
                        .parse_while_expression()
                        .map(|n| Expression::While(Box::new(n)))
                }
                "begin" => {
                    return self
                        .parse_begin_expression()
                        .map(|n| Expression::Begin(Box::new(n)))
                }
                // "static", "fn", "struct", "use" were handled in parse_top_level_form
                // If they appear here, they are being called or are part of a non-top-level expression context
                // which might be an error, or they could be treated as regular symbols.
                // For now, we assume they are regular symbols if not at top level.
                _ => {} // Fall through to generic call
            }
        }

        // Generic call or callable expression
        let callable_expr = self.parse_expression()?;
        let mut arguments = Vec::new();
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in call arguments, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            arguments.push(self.parse_expression()?);
        }
        self.expect_token_type(TokenType::Rparen)?; // Consume Rparen
        Ok(Expression::Call(Box::new(CallNode {
            callable_expr,
            arguments,
        })))
    }

    // --- Definition Parsers (called from parse_top_level_form) ---

    fn parse_static_definition(&mut self) -> Result<StaticNode, NsError> {
        self.expect_token_type(TokenType::Lparen)?;
        self.expect_token_type(TokenType::Symbol)?; // Consume 'static' keyword (lexeme checked by caller)
        let name_token = self.expect_token_type(TokenType::Symbol)?;
        let name = Rc::new(name_token.lexeme.clone());
        let value = self.parse_expression()?;
        self.expect_token_type(TokenType::Rparen)?;
        Ok(StaticNode { name, value })
    }

    fn parse_fn_definition(&mut self) -> Result<FnNode, NsError> {
        self.expect_token_type(TokenType::Lparen)?;
        self.expect_token_type(TokenType::Symbol)?; // Consume 'fn'
        let name_token = self.expect_token_type(TokenType::Symbol)?;
        let name = Rc::new(name_token.lexeme.clone());

        self.expect_token_type(TokenType::Lparen)?; // For parameter list
        let mut params = Vec::new();
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in fn parameters, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            let param_token = self.expect_token_type(TokenType::Symbol)?;
            params.push(Rc::new(param_token.lexeme.clone()));
        }
        self.expect_token_type(TokenType::Rparen)?; // End parameter list

        let mut body = Vec::new();
        let start_body_token = self.current_token()?;
        if self.check_current_token_type(TokenType::Rparen) {
            // Check for empty body before parsing expressions
            return Err(NsError::Parser(format!(
                "Function body cannot be empty for 'fn {}' at line {}, col {}",
                name, start_body_token.line, start_body_token.col
            )));
        }
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in fn body, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            body.push(self.parse_expression()?);
        }
        if body.is_empty() {
            // Should have been caught above, but as a safeguard
            return Err(NsError::Parser(format!(
                "Function body cannot be empty for 'fn {}' at line {}, col {}",
                name, start_body_token.line, start_body_token.col
            )));
        }
        self.expect_token_type(TokenType::Rparen)?; // End fn definition
        Ok(FnNode { name, params, body })
    }

    fn parse_struct_definition(&mut self) -> Result<StructDefNode, NsError> {
        self.expect_token_type(TokenType::Lparen)?;
        self.expect_token_type(TokenType::Symbol)?; // Consume 'struct'
        let name_token = self.expect_token_type(TokenType::Symbol)?;
        let name = Rc::new(name_token.lexeme.clone());

        self.expect_token_type(TokenType::Lparen)?; // For field list
        let mut fields = Vec::new();
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in struct fields, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            let field_token = self.expect_token_type(TokenType::Symbol)?;
            fields.push(Rc::new(field_token.lexeme.clone()));
        }
        // Empty field list is allowed by this logic.
        self.expect_token_type(TokenType::Rparen)?; // End field list
        self.expect_token_type(TokenType::Rparen)?; // End struct definition
        Ok(StructDefNode { name, fields })
    }

    fn parse_use_statement(&mut self) -> Result<UseNode, NsError> {
        self.expect_token_type(TokenType::Lparen)?;
        self.expect_token_type(TokenType::Symbol)?; // Consume 'use'
        let module_name_token = self.expect_token_type(TokenType::Symbol)?;
        let module_name = Rc::new(module_name_token.lexeme.clone());

        let items = if self.check_current_token_type(TokenType::Lparen) {
            self.consume_token()?; // Consume LPAREN for item list
            let mut specific_items = Vec::new();
            while !self.check_current_token_type(TokenType::Rparen) {
                if self.check_current_token_type(TokenType::Eof) {
                    return Err(NsError::Parser(format!(
                        "Unexpected EOF in use items list, expecting ')' at line {}, col {}",
                        self.current_token()?.line,
                        self.current_token()?.col
                    )));
                }
                let item_token = self.expect_token_type(TokenType::Symbol)?;
                specific_items.push(Rc::new(item_token.lexeme.clone()));
            }
            self.expect_token_type(TokenType::Rparen)?; // Consume RPAREN for item list
            if specific_items.is_empty() {
                return Err(NsError::Parser(format!(
                    "Specific import list for 'use {}' cannot be empty at line {}, col {}",
                    module_name, module_name_token.line, module_name_token.col
                )));
            }
            UseItems::Specific(specific_items)
        } else if self.check_current_token_type(TokenType::Symbol)
            && self.current_token()?.lexeme == "*"
        {
            self.consume_token()?; // Consume '*'
            UseItems::All
        } else {
            let current = self.current_token()?;
            return Err(NsError::Parser(format!(
                "Expected item list '(item1 ...)' or '*' for 'use {}', but got {:?} at line {}, col {}",
                module_name, current.token_type, current.line, current.col
            )));
        };
        self.expect_token_type(TokenType::Rparen)?; // End use statement
        Ok(UseNode { module_name, items })
    }

    // --- Expression-Producing Special Form Parsers (called from parse_list_or_call_expr) ---

    fn parse_if_expression(&mut self) -> Result<Expression, NsError> {
        // LPAREN already consumed by caller
        self.expect_token_type(TokenType::Symbol)?; // Consume 'if'
        let condition = self.parse_expression()?;
        let then_branch = self.parse_expression()?;

        // Check if else branch exists before consuming RPAREN
        if self.check_current_token_type(TokenType::Rparen) {
            let prev_tok = self.previous_token(); // then_branch's last token or surrounding
            return Err(NsError::Parser(format!(
                "'if' expression requires an else branch. Missing after then-branch ending near line {}, col {}.",
                 prev_tok.line, prev_tok.col // This position might not be ideal
            )));
        }
        let else_branch = self.parse_expression()?;
        self.expect_token_type(TokenType::Rparen)?;
        Ok(Expression::If(Box::new(IfNode {
            condition,
            then_branch,
            else_branch,
        })))
    }

    fn parse_let_expression(&mut self) -> Result<LetNode, NsError> {
        // LPAREN already consumed by caller
        let let_keyword_token = self.expect_token_type(TokenType::Symbol)?; // Consume 'let'
        let mut bindings = Vec::new();

        // Check for single binding `(let var expr ...)` or multi-binding `(let ((v1 e1) ...) ...)`
        if self.check_current_token_type(TokenType::Lparen) {
            // Multi-binding form: (let ((var1 val1) (var2 val2) ...) body...)
            self.expect_token_type(TokenType::Lparen)?; // Consume outer LPAREN of binding list
            while !self.check_current_token_type(TokenType::Rparen) {
                if self.check_current_token_type(TokenType::Eof) {
                    return Err(NsError::Parser(format!(
                        "Unexpected EOF in let multi-bindings, expecting ')' at line {}, col {}",
                        self.current_token()?.line,
                        self.current_token()?.col
                    )));
                }
                self.expect_token_type(TokenType::Lparen)?; // LPAREN for individual binding
                let name_token = self.expect_token_type(TokenType::Symbol)?;
                let name = Rc::new(name_token.lexeme.clone());
                let value = self.parse_expression()?;
                bindings.push(LetBinding { name, value });
                self.expect_token_type(TokenType::Rparen)?; // RPAREN for individual binding
            }
            self.expect_token_type(TokenType::Rparen)?; // Consume outer RPAREN of binding list
        } else if self.check_current_token_type(TokenType::Symbol) {
            // Single binding form: (let var val body...)
            // This was changed in Python from (let var val) to (let ((var val)) ...).
            // Let's stick to the AST structure: bindings list, then body.
            // (let var val body...) means one binding: (var val)
            let name_token = self.expect_token_type(TokenType::Symbol)?;
            let name = Rc::new(name_token.lexeme.clone());
            let value = self.parse_expression()?;
            bindings.push(LetBinding { name, value });
        } else {
            let current = self.current_token()?;
            return Err(NsError::Parser(format!(
                "Invalid 'let' binding structure. Expected symbol or list of bindings, got {:?} at line {}, col {}",
                current.token_type, current.line, current.col
            )));
        }

        if bindings.is_empty() {
            return Err(NsError::Parser(format!(
                "'let' expression must have at least one binding. Started at line {}, col {}",
                let_keyword_token.line, let_keyword_token.col
            )));
        }

        let mut body = Vec::new();
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in let body, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            body.push(self.parse_expression()?);
        }
        // If body is empty, it's fine. `(let x 10)` is valid.
        self.expect_token_type(TokenType::Rparen)?;
        Ok(LetNode { bindings, body })
    }

    fn parse_lambda_expression(&mut self) -> Result<LambdaNode, NsError> {
        // LPAREN already consumed
        self.expect_token_type(TokenType::Symbol)?; // Consume 'lambda'
        self.expect_token_type(TokenType::Lparen)?; // For parameter list
        let mut params = Vec::new();
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in lambda parameters, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            let param_token = self.expect_token_type(TokenType::Symbol)?;
            params.push(Rc::new(param_token.lexeme.clone()));
        }
        self.expect_token_type(TokenType::Rparen)?; // End parameter list

        let mut body = Vec::new();
        let start_body_token = self.current_token()?;
        if self.check_current_token_type(TokenType::Rparen) {
            return Err(NsError::Parser(format!(
                "Lambda body cannot be empty at line {}, col {}",
                start_body_token.line, start_body_token.col
            )));
        }
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in lambda body, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            body.push(self.parse_expression()?);
        }
        if body.is_empty() {
            return Err(NsError::Parser(format!(
                "Lambda body cannot be empty at line {}, col {}",
                start_body_token.line, start_body_token.col
            )));
        }
        self.expect_token_type(TokenType::Rparen)?;
        Ok(LambdaNode { params, body })
    }

    fn parse_get_expression(&mut self) -> Result<GetNode, NsError> {
        // LPAREN consumed
        self.expect_token_type(TokenType::Symbol)?; // Consume 'get'
        let instance = self.parse_expression()?;
        let field_name_token = self.expect_token_type(TokenType::Symbol)?;
        let field_name = Rc::new(field_name_token.lexeme.clone());
        self.expect_token_type(TokenType::Rparen)?;
        Ok(GetNode {
            instance,
            field_name,
        })
    }

    fn parse_set_expression(&mut self) -> Result<SetNode, NsError> {
        // LPAREN consumed
        self.expect_token_type(TokenType::Symbol)?; // Consume 'set'
        let instance = self.parse_expression()?;
        let field_name_token = self.expect_token_type(TokenType::Symbol)?;
        let field_name = Rc::new(field_name_token.lexeme.clone());
        let value = self.parse_expression()?;
        self.expect_token_type(TokenType::Rparen)?;
        Ok(SetNode {
            instance,
            field_name,
            value,
        })
    }

    fn parse_while_expression(&mut self) -> Result<WhileNode, NsError> {
        // LPAREN consumed
        self.expect_token_type(TokenType::Symbol)?; // Consume 'while'
        let condition = self.parse_expression()?;
        let mut body = Vec::new();
        // Body can be empty for (while false)
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in while body, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            body.push(self.parse_expression()?);
        }
        self.expect_token_type(TokenType::Rparen)?;
        Ok(WhileNode { condition, body })
    }

    fn parse_begin_expression(&mut self) -> Result<BeginNode, NsError> {
        // LPAREN consumed
        self.expect_token_type(TokenType::Symbol)?; // Consume 'begin'
        let mut expressions = Vec::new();
        // Body can be empty for (begin) which evaluates to 'none'
        while !self.check_current_token_type(TokenType::Rparen) {
            if self.check_current_token_type(TokenType::Eof) {
                return Err(NsError::Parser(format!(
                    "Unexpected EOF in begin body, expecting ')' at line {}, col {}",
                    self.current_token()?.line,
                    self.current_token()?.col
                )));
            }
            expressions.push(self.parse_expression()?);
        }
        self.expect_token_type(TokenType::Rparen)?;
        Ok(BeginNode { expressions })
    }
}
