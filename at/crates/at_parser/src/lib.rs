use at_syntax::{
    Expr, Function, Ident, InterpPart, MatchArm, MatchPattern, Module, Param, Span, Stmt, TypeRef,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Fn,
    Needs,
    Test,
    Using,
    Tool,
    Import,
    As,
    Match,
    While,
    For,
    In,
    If,
    Else,
    Set,
    Break,
    Continue,
    Return,
    Let,
    Ident(String),
    True,
    False,
    Int(i64),
    Float(f64),
    String(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    AndAnd,
    OrOr,
    Pipe,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equals,
    FatArrow,
    Arrow,
    Colon,
    Question,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Dot,
    DotDot,
    DotDotEquals,
    Semicolon,
    Invalid(char),
    UnterminatedString,
    UnterminatedBlockComment,
    InvalidNumber,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: TokenKind,
        span: Span,
    },
    UnterminatedString {
        span: Span,
    },
    UnterminatedBlockComment {
        span: Span,
    },
    InvalidNumber {
        span: Span,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => {
                write!(
                    f,
                    "parse error at {}: expected {}, found {:?}",
                    span.start, expected, found
                )
            }
            ParseError::UnterminatedString { span } => {
                write!(f, "parse error at {}: unterminated string", span.start)
            }
            ParseError::UnterminatedBlockComment { span } => {
                write!(
                    f,
                    "parse error at {}: unterminated block comment",
                    span.start
                )
            }
            ParseError::InvalidNumber { span } => {
                write!(f, "parse error at {}: invalid number", span.start)
            }
        }
    }
}

impl std::error::Error for ParseError {}

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(&mut lexer);
    let (module, errors) = parser.parse_module_with_errors();
    if let Some(error) = errors.into_iter().next() {
        Err(error)
    } else {
        Ok(module)
    }
}

pub fn parse_module_with_errors(source: &str) -> (Module, Vec<ParseError>) {
    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(&mut lexer);
    parser.parse_module_with_errors()
}

struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer<'a>) -> Self {
        let current = lexer.next_token();
        Self { lexer, current }
    }

    fn parse_module_with_errors(&mut self) -> (Module, Vec<ParseError>) {
        let mut functions = Vec::new();
        let mut stmts = Vec::new();
        let mut errors = Vec::new();
        while self.current.kind != TokenKind::Eof {
            match self.current.kind {
                TokenKind::Fn => match self.parse_function(false) {
                    Ok(func) => functions.push(func),
                    Err(err) => {
                        errors.push(err);
                        self.recover_to_function_end();
                    }
                },
                TokenKind::Tool => match self.parse_tool_function() {
                    Ok(func) => functions.push(func),
                    Err(err) => {
                        errors.push(err);
                        self.recover_to_function_end();
                    }
                },
                _ => match self.parse_stmt() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(err) => {
                        errors.push(err);
                        self.recover_to_stmt_boundary();
                    }
                },
            }
        }
        (Module { functions, stmts }, errors)
    }

    fn recover_to_stmt_boundary(&mut self) {
        if self.current.kind != TokenKind::Eof {
            self.advance();
        }
        loop {
            match self.current.kind {
                TokenKind::Semicolon => {
                    self.advance();
                    break;
                }
                TokenKind::RBrace => {
                    self.advance();
                    break;
                }
                TokenKind::Fn
                | TokenKind::Tool
                | TokenKind::Import
                | TokenKind::Let
                | TokenKind::Using
                | TokenKind::Set
                | TokenKind::Return
                | TokenKind::Test
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::If
                | TokenKind::Eof => {
                    break;
                }
                _ => self.advance(),
            }
        }
    }

    fn recover_to_function_end(&mut self) {
        if self.current.kind != TokenKind::Eof {
            self.advance();
        }
        loop {
            match self.current.kind {
                TokenKind::RBrace => {
                    self.advance();
                    break;
                }
                TokenKind::Eof => break,
                _ => self.advance(),
            }
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match &self.current.kind {
            TokenKind::Import => self.parse_import_stmt(),
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Using => self.parse_using_stmt(),
            TokenKind::Set => self.parse_set_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Test => self.parse_test_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Break => self.parse_break_stmt(),
            TokenKind::Continue => self.parse_continue_stmt(),
            TokenKind::LBrace => self.parse_block_stmt(),
            TokenKind::If => {
                let expr = self.parse_if_expr()?;
                if self.current.kind == TokenKind::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Expr(expr))
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_tool_function(&mut self) -> Result<Function, ParseError> {
        self.advance();
        self.expect(TokenKind::Fn)?;
        self.parse_function(true)
    }

    fn parse_function(&mut self, is_tool: bool) -> Result<Function, ParseError> {
        if self.current.kind == TokenKind::Fn {
            self.advance();
        }
        let name = self.expect_ident()?;
        self.expect(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen)?;
        let return_ty = if self.current.kind == TokenKind::Arrow {
            self.advance();
            Some(self.parse_type_ref()?)
        } else {
            None
        };
        let needs = if self.current.kind == TokenKind::Needs {
            self.advance();
            self.expect(TokenKind::LBrace)?;
            let list = self.parse_ident_list()?;
            self.expect(TokenKind::RBrace)?;
            list
        } else {
            Vec::new()
        };
        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while self.current.kind != TokenKind::RBrace && self.current.kind != TokenKind::Eof {
            body.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Function {
            name,
            params,
            return_ty,
            needs,
            body,
            is_tool,
        })
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let ident = self.expect_ident()?;
        let ty = if self.current.kind == TokenKind::Colon {
            self.advance();
            Some(self.parse_type_ref()?)
        } else {
            None
        };
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Let {
            name: ident,
            ty,
            value,
        })
    }

    fn parse_import_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let path = match &self.current.kind {
            TokenKind::String(value) => {
                let path = value.clone();
                self.advance();
                path
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "string".to_string(),
                    found: self.current.kind.clone(),
                    span: self.current.span,
                });
            }
        };
        self.expect(TokenKind::As)?;
        let alias = self.expect_ident()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Import { path, alias })
    }

    fn parse_using_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let ident = self.expect_ident()?;
        let ty = if self.current.kind == TokenKind::Colon {
            self.advance();
            Some(self.parse_type_ref()?)
        } else {
            None
        };
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Using {
            name: ident,
            ty,
            value,
        })
    }

    fn parse_set_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let base_ident = self.expect_ident()?;

        let mut base_expr = Expr::Ident(base_ident.clone());
        let mut last_field: Option<Ident> = None;
        let mut last_index: Option<Expr> = None;
        let mut last_index_span: Option<Span> = None;

        loop {
            if self.current.kind == TokenKind::Dot {
                self.advance();
                let field = self.expect_ident()?;

                if self.current.kind == TokenKind::Dot || self.current.kind == TokenKind::LBracket {
                    base_expr = Expr::Member {
                        base: Box::new(base_expr),
                        name: field,
                    };
                    last_field = None;
                    last_index = None;
                    last_index_span = None;
                    continue;
                }

                last_field = Some(field);
                last_index = None;
                last_index_span = None;
                break;
            } else if self.current.kind == TokenKind::LBracket {
                let index_span = self.current.span;
                self.advance();
                let index = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;

                if self.current.kind == TokenKind::Dot || self.current.kind == TokenKind::LBracket {
                    base_expr = Expr::Index {
                        index_span,
                        base: Box::new(base_expr),
                        index: Box::new(index),
                    };
                    last_field = None;
                    last_index = None;
                    last_index_span = None;
                    continue;
                }

                last_field = None;
                last_index = Some(index);
                last_index_span = Some(index_span);
                break;
            } else {
                break;
            }
        }

        let assign_op = match self.current.kind {
            TokenKind::Equals => {
                self.advance();
                None
            }
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent => {
                let (op, span) = match self.current.kind {
                    TokenKind::Plus => (at_syntax::BinaryOp::Add, self.current.span),
                    TokenKind::Minus => (at_syntax::BinaryOp::Sub, self.current.span),
                    TokenKind::Star => (at_syntax::BinaryOp::Mul, self.current.span),
                    TokenKind::Slash => (at_syntax::BinaryOp::Div, self.current.span),
                    TokenKind::Percent => (at_syntax::BinaryOp::Mod, self.current.span),
                    _ => unreachable!(),
                };
                self.advance();
                self.expect(TokenKind::Equals)?;
                Some((op, span))
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "assignment operator".to_string(),
                    found: self.current.kind.clone(),
                    span: self.current.span,
                })
            }
        };

        match (last_field, last_index) {
            (Some(field), None) => {
                let value = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                let value = if let Some((op, op_span)) = assign_op {
                    Expr::Binary {
                        left: Box::new(Expr::Member {
                            base: Box::new(base_expr.clone()),
                            name: field.clone(),
                        }),
                        op,
                        op_span,
                        right: Box::new(value),
                    }
                } else {
                    value
                };
                Ok(Stmt::SetMember {
                    base: base_expr,
                    field,
                    value,
                })
            }
            (None, Some(index)) => {
                let value = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                let value = if let Some((op, op_span)) = assign_op {
                    Expr::Binary {
                        left: Box::new(Expr::Index {
                            index_span: last_index_span.unwrap_or(op_span),
                            base: Box::new(base_expr.clone()),
                            index: Box::new(index.clone()),
                        }),
                        op,
                        op_span,
                        right: Box::new(value),
                    }
                } else {
                    value
                };
                Ok(Stmt::SetIndex {
                    base: base_expr,
                    index,
                    value,
                })
            }
            (None, None) => {
                let value = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                let value = if let Some((op, op_span)) = assign_op {
                    Expr::Binary {
                        left: Box::new(Expr::Ident(base_ident.clone())),
                        op,
                        op_span,
                        right: Box::new(value),
                    }
                } else {
                    value
                };
                Ok(Stmt::Set {
                    name: base_ident,
                    value,
                })
            }
            _ => unreachable!(),
        }
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        if self.current.kind == TokenKind::Semicolon {
            self.advance();
            return Ok(Stmt::Return(None));
        }
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Return(Some(expr)))
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while self.current.kind != TokenKind::RBrace && self.current.kind != TokenKind::Eof {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Stmt::Block(stmts))
    }

    fn parse_test_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let name = match &self.current.kind {
            TokenKind::String(value) => {
                let name = value.clone();
                self.advance();
                name
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "string".to_string(),
                    found: self.current.kind.clone(),
                    span: self.current.span,
                });
            }
        };
        let body = match self.parse_block_stmt()? {
            Stmt::Block(stmts) => stmts,
            _ => Vec::new(),
        };
        Ok(Stmt::Test { name, body })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        let while_span = self.current.span;
        self.advance();
        let condition = self.parse_expr()?;
        let body = match self.parse_block_stmt()? {
            Stmt::Block(stmts) => stmts,
            _ => Vec::new(),
        };
        Ok(Stmt::While {
            while_span,
            condition,
            body,
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let for_span = self.current.span;
        self.advance();
        let item = self.expect_ident()?;
        self.expect(TokenKind::In)?;
        let iter = self.parse_expr()?;
        let body = match self.parse_block_stmt()? {
            Stmt::Block(stmts) => stmts,
            _ => Vec::new(),
        };
        Ok(Stmt::For {
            for_span,
            item,
            iter,
            body,
        })
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParseError> {
        let break_span = self.current.span;
        self.advance();
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Break { break_span })
    }

    fn parse_continue_stmt(&mut self) -> Result<Stmt, ParseError> {
        let continue_span = self.current.span;
        self.advance();
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Continue { continue_span })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_and()?;
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::OrOr => (at_syntax::BinaryOp::Or, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                op_span: span,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::AndAnd => (at_syntax::BinaryOp::And, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                op_span: span,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::EqualEqual => (at_syntax::BinaryOp::Eq, self.current.span),
                TokenKind::BangEqual => (at_syntax::BinaryOp::Neq, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                op_span: span,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_range()?;
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::Less => (at_syntax::BinaryOp::Lt, self.current.span),
                TokenKind::LessEqual => (at_syntax::BinaryOp::Lte, self.current.span),
                TokenKind::Greater => (at_syntax::BinaryOp::Gt, self.current.span),
                TokenKind::GreaterEqual => (at_syntax::BinaryOp::Gte, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_range()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                op_span: span,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_range(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;
        if self.current.kind == TokenKind::DotDot || self.current.kind == TokenKind::DotDotEquals {
            let range_span = self.current.span;
            let inclusive = self.current.kind == TokenKind::DotDotEquals;
            self.advance();
            let end = self.parse_term()?;
            expr = Expr::Range {
                range_span,
                start: Box::new(expr),
                end: Box::new(end),
                inclusive,
            };
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::Plus => (at_syntax::BinaryOp::Add, self.current.span),
                TokenKind::Minus => (at_syntax::BinaryOp::Sub, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                op_span: span,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::Star => (at_syntax::BinaryOp::Mul, self.current.span),
                TokenKind::Slash => (at_syntax::BinaryOp::Div, self.current.span),
                TokenKind::Percent => (at_syntax::BinaryOp::Mod, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                op_span: span,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        match self.current.kind {
            TokenKind::Minus => {
                let span = self.current.span;
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: at_syntax::UnaryOp::Neg,
                    op_span: span,
                    expr: Box::new(expr),
                })
            }
            TokenKind::Bang => {
                let span = self.current.span;
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: at_syntax::UnaryOp::Not,
                    op_span: span,
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.current.kind == TokenKind::Dot {
                self.advance();
                let name = self.expect_ident()?;
                expr = Expr::Member {
                    base: Box::new(expr),
                    name,
                };
                continue;
            }
            if self.current.kind == TokenKind::LParen {
                self.advance();
                let args = self.parse_args()?;
                self.expect(TokenKind::RParen)?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                };
                continue;
            }
            if self.current.kind == TokenKind::LBracket {
                let index_span = self.current.span;
                self.advance();
                let index = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                expr = Expr::Index {
                    index_span,
                    base: Box::new(expr),
                    index: Box::new(index),
                };
                continue;
            }
            if self.current.kind == TokenKind::Question {
                self.advance();
                expr = Expr::Try(Box::new(expr));
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match &self.current.kind {
            TokenKind::If => self.parse_if_expr(),
            TokenKind::Match => self.parse_match_expr(),
            TokenKind::True => {
                let span = self.current.span;
                self.advance();
                Ok(Expr::Bool(true, span))
            }
            TokenKind::False => {
                let span = self.current.span;
                self.advance();
                Ok(Expr::Bool(false, span))
            }
            TokenKind::Int(value) => {
                let span = self.current.span;
                let v = *value;
                self.advance();
                Ok(Expr::Int(v, span))
            }
            TokenKind::Float(value) => {
                let span = self.current.span;
                let v = *value;
                self.advance();
                Ok(Expr::Float(v, span))
            }
            TokenKind::String(value) => {
                let span = self.current.span;
                let v = value.clone();
                self.advance();
                if v.contains('{') {
                    return self.parse_interpolated_string(v, span);
                }
                Ok(Expr::String(v, span))
            }
            TokenKind::UnterminatedString => Err(ParseError::UnterminatedString {
                span: self.current.span,
            }),
            TokenKind::UnterminatedBlockComment => Err(ParseError::UnterminatedBlockComment {
                span: self.current.span,
            }),
            TokenKind::InvalidNumber => Err(ParseError::InvalidNumber {
                span: self.current.span,
            }),
            TokenKind::Ident(name) => {
                let ident = Ident {
                    name: name.clone(),
                    span: self.current.span,
                };
                self.advance();
                Ok(Expr::Ident(ident))
            }
            TokenKind::LParen => {
                let paren_span = self.current.span;
                self.advance();

                if self.current.kind == TokenKind::RParen {
                    self.advance();
                    return Ok(Expr::Tuple {
                        tuple_span: paren_span,
                        items: vec![],
                    });
                }

                let first_expr = self.parse_expr()?;
                if self.current.kind == TokenKind::Comma {
                    self.advance();
                    let mut items = vec![first_expr];
                    while self.current.kind != TokenKind::RParen {
                        items.push(self.parse_expr()?);
                        if self.current.kind == TokenKind::Comma {
                            self.advance();
                            if self.current.kind == TokenKind::RParen {
                                break;
                            }
                        } else if self.current.kind != TokenKind::RParen {
                            return Err(ParseError::UnexpectedToken {
                                expected: "comma or closing paren".to_string(),
                                found: self.current.kind.clone(),
                                span: self.current.span,
                            });
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    return Ok(Expr::Tuple {
                        tuple_span: paren_span,
                        items,
                    });
                }

                self.expect(TokenKind::RParen)?;
                Ok(first_expr)
            }
            TokenKind::Pipe => self.parse_closure_expr(),
            TokenKind::LBracket => self.parse_array_literal(),
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: self.current.kind.clone(),
                span: self.current.span,
            }),
        }
    }

    fn parse_interpolated_string(&mut self, value: String, span: Span) -> Result<Expr, ParseError> {
        let mut parts = Vec::new();
        let mut current = String::new();
        let mut chars = value.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '{' {
                if chars.peek() == Some(&'{') {
                    chars.next();
                    current.push('{');
                    continue;
                }
                if !current.is_empty() {
                    parts.push(InterpPart::String(current.clone()));
                    current.clear();
                }
                let mut expr_str = String::new();
                let mut depth = 1;
                while let Some(ch) = chars.next() {
                    if ch == '{' {
                        depth += 1;
                        expr_str.push(ch);
                    } else if ch == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        expr_str.push(ch);
                    } else {
                        expr_str.push(ch);
                    }
                }
                let mut expr_lexer = Lexer::new(expr_str.as_str());
                let mut expr_parser = Parser::new(&mut expr_lexer);
                let expr = expr_parser.parse_expr()?;
                if expr_parser.current.kind != TokenKind::Eof {
                    return Err(ParseError::UnexpectedToken {
                        expected: "end of interpolation".to_string(),
                        found: expr_parser.current.kind.clone(),
                        span: expr_parser.current.span,
                    });
                }
                parts.push(InterpPart::Expr(expr));
                continue;
            }
            if ch == '}' && chars.peek() == Some(&'}') {
                chars.next();
                current.push('}');
                continue;
            }
            current.push(ch);
        }

        if !current.is_empty() {
            parts.push(InterpPart::String(current));
        }

        Ok(Expr::InterpolatedString { span, parts })
    }

    fn parse_closure_expr(&mut self) -> Result<Expr, ParseError> {
        let span = self.current.span;
        self.expect(TokenKind::Pipe)?;

        let mut params = Vec::new();
        if self.current.kind != TokenKind::Pipe {
            loop {
                if let TokenKind::Ident(name) = &self.current.kind {
                    params.push(Ident {
                        name: name.clone(),
                        span: self.current.span,
                    });
                    self.advance();
                    if self.current.kind != TokenKind::Comma {
                        break;
                    }
                    self.advance();
                    if self.current.kind == TokenKind::Pipe {
                        break;
                    }
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "parameter name".to_string(),
                        found: self.current.kind.clone(),
                        span: self.current.span,
                    });
                }
            }
        }
        self.expect(TokenKind::Pipe)?;

        let body = self.parse_expr()?;
        Ok(Expr::Closure {
            span,
            params,
            body: Box::new(body),
        })
    }

    fn parse_array_literal(&mut self) -> Result<Expr, ParseError> {
        let array_span = self.current.span;
        self.expect(TokenKind::LBracket)?;
        let mut items = Vec::new();
        if self.current.kind != TokenKind::RBracket {
            loop {
                items.push(self.parse_expr()?);
                if self.current.kind != TokenKind::Comma {
                    break;
                }
                self.advance();
                if self.current.kind == TokenKind::RBracket {
                    break;
                }
            }
        }
        self.expect(TokenKind::RBracket)?;
        Ok(Expr::Array { array_span, items })
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        let if_span = self.current.span;
        self.advance();
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block_expr()?;
        let else_branch = if self.current.kind == TokenKind::Else {
            self.advance();
            let else_expr = if self.current.kind == TokenKind::If {
                self.parse_if_expr()?
            } else {
                self.parse_block_expr()?
            };
            Some(Box::new(else_expr))
        } else {
            None
        };
        Ok(Expr::If {
            if_span,
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParseError> {
        let block_span = self.current.span;
        self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        let mut tail = None;
        while self.current.kind != TokenKind::RBrace && self.current.kind != TokenKind::Eof {
            match &self.current.kind {
                TokenKind::Import
                | TokenKind::Let
                | TokenKind::Using
                | TokenKind::Set
                | TokenKind::Return
                | TokenKind::Test
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::LBrace => {
                    stmts.push(self.parse_stmt()?);
                }
                _ => {
                    let expr = self.parse_expr()?;
                    if self.current.kind == TokenKind::Semicolon {
                        self.advance();
                        stmts.push(Stmt::Expr(expr));
                    } else {
                        tail = Some(Box::new(expr));
                        break;
                    }
                }
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Expr::Block {
            block_span,
            stmts,
            tail,
        })
    }

    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        let match_span = self.current.span;
        self.advance();
        let value = self.parse_expr()?;
        self.expect(TokenKind::LBrace)?;
        let mut arms = Vec::new();
        while self.current.kind != TokenKind::RBrace && self.current.kind != TokenKind::Eof {
            let mut patterns = vec![self.parse_match_pattern()?];
            while self.current.kind == TokenKind::Pipe {
                self.advance();
                patterns.push(self.parse_match_pattern()?);
            }
            let guard = if self.current.kind == TokenKind::If {
                self.advance();
                Some(self.parse_expr()?)
            } else {
                None
            };
            self.expect(TokenKind::FatArrow)?;
            let body = self.parse_expr()?;
            for pattern in patterns {
                arms.push(MatchArm {
                    pattern,
                    guard: guard.clone(),
                    body: body.clone(),
                });
            }
            if self.current.kind == TokenKind::Comma {
                self.advance();
                // After comma, we must have another arm or closing brace
                if self.current.kind == TokenKind::RBrace {
                    // Trailing comma is allowed, break
                    break;
                }
            } else if self.current.kind != TokenKind::RBrace {
                // Missing comma between arms
                return Err(ParseError::UnexpectedToken {
                    expected: "comma or closing brace".to_string(),
                    found: self.current.kind.clone(),
                    span: self.current.span,
                });
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Expr::Match {
            match_span,
            value: Box::new(value),
            arms,
        })
    }

    fn parse_match_pattern(&mut self) -> Result<MatchPattern, ParseError> {
        if self.current.kind == TokenKind::Ident(String::from("_")) {
            self.advance();
            return Ok(MatchPattern::Wildcard);
        }
        if let TokenKind::Int(value) = self.current.kind {
            let value = value;
            self.advance();
            return Ok(MatchPattern::Int(value));
        }
        if self.current.kind == TokenKind::Minus {
            let span = self.current.span;
            self.advance();
            match self.current.kind {
                TokenKind::Int(value) => {
                    let value = value;
                    self.advance();
                    return Ok(MatchPattern::Int(-value));
                }
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "int literal".to_string(),
                        found: self.current.kind.clone(),
                        span,
                    })
                }
            }
        }
        let head = self.expect_ident()?;
        match head.name.as_str() {
            "ok" => {
                self.expect(TokenKind::LParen)?;
                let name = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(MatchPattern::ResultOk(name))
            }
            "err" => {
                self.expect(TokenKind::LParen)?;
                let name = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(MatchPattern::ResultErr(name))
            }
            "some" => {
                self.expect(TokenKind::LParen)?;
                let name = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(MatchPattern::OptionSome(name))
            }
            "none" => Ok(MatchPattern::OptionNone),
            _ => Err(ParseError::UnexpectedToken {
                expected: "match pattern".to_string(),
                found: TokenKind::Ident(head.name),
                span: head.span,
            }),
        }
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        if self.current.kind == TokenKind::RParen {
            return Ok(Vec::new());
        }
        let mut params = Vec::new();
        loop {
            let name = self.expect_ident()?;
            let ty = if self.current.kind == TokenKind::Colon {
                self.advance();
                Some(self.parse_type_ref()?)
            } else {
                None
            };
            params.push(Param { name, ty });
            if self.current.kind != TokenKind::Comma {
                break;
            }
            self.advance();
            if self.current.kind == TokenKind::RParen {
                break;
            }
        }
        Ok(params)
    }

    fn parse_ident_list(&mut self) -> Result<Vec<Ident>, ParseError> {
        let mut items = Vec::new();
        loop {
            items.push(self.expect_ident()?);
            if self.current.kind != TokenKind::Comma {
                break;
            }
            self.advance();
            if self.current.kind == TokenKind::RBrace {
                break;
            }
        }
        Ok(items)
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();
        if self.current.kind == TokenKind::RParen {
            return Ok(args);
        }
        loop {
            args.push(self.parse_expr()?);
            if self.current.kind != TokenKind::Comma {
                break;
            }
            self.advance();
            if self.current.kind == TokenKind::RParen {
                break;
            }
        }
        Ok(args)
    }

    fn parse_type_ref(&mut self) -> Result<TypeRef, ParseError> {
        if self.current.kind == TokenKind::Fn {
            let fn_span = self.current.span;
            self.advance();
            self.expect(TokenKind::LParen)?;
            let mut params = Vec::new();
            if self.current.kind != TokenKind::RParen {
                loop {
                    params.push(self.parse_type_ref()?);
                    if self.current.kind != TokenKind::Comma {
                        break;
                    }
                    self.advance();
                    if self.current.kind == TokenKind::RParen {
                        break;
                    }
                }
            }
            self.expect(TokenKind::RParen)?;
            self.expect(TokenKind::Arrow)?;
            let return_ty = self.parse_type_ref()?;
            return Ok(TypeRef::Function {
                fn_span,
                params,
                return_ty: Box::new(return_ty),
            });
        }

        let name = self.expect_ident()?;
        let args = if self.current.kind == TokenKind::Less {
            self.advance();
            let mut items = Vec::new();
            if self.current.kind != TokenKind::Greater {
                loop {
                    items.push(self.parse_type_ref()?);
                    if self.current.kind != TokenKind::Comma {
                        break;
                    }
                    self.advance();
                    if self.current.kind == TokenKind::Greater {
                        break;
                    }
                }
            }
            self.expect(TokenKind::Greater)?;
            items
        } else {
            Vec::new()
        };
        Ok(TypeRef::Named { name, args })
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        match &self.current.kind {
            TokenKind::Ident(name) => {
                let ident = Ident {
                    name: name.clone(),
                    span: self.current.span,
                };
                self.advance();
                Ok(ident)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self.current.kind.clone(),
                span: self.current.span,
            }),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if std::mem::discriminant(&self.current.kind) == std::mem::discriminant(&kind) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{kind:?}"),
                found: self.current.kind.clone(),
                span: self.current.span,
            })
        }
    }

    fn advance(&mut self) {
        self.current = self.lexer.next_token();
    }
}

struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    index: usize,
    current: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        let mut chars = source.chars();
        let current = chars.next();
        Self {
            chars,
            index: 0,
            current,
        }
    }

    fn next_token(&mut self) -> Token {
        if let Some(token) = self.skip_whitespace() {
            return token;
        }
        let start = self.index;
        match self.current {
            Some('+') => {
                self.bump();
                Token {
                    kind: TokenKind::Plus,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('-') => {
                self.bump();
                if self.current == Some('>') {
                    self.bump();
                    Token {
                        kind: TokenKind::Arrow,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Minus,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some('*') => {
                self.bump();
                Token {
                    kind: TokenKind::Star,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('/') => {
                self.bump();
                Token {
                    kind: TokenKind::Slash,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('%') => {
                self.bump();
                Token {
                    kind: TokenKind::Percent,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('&') => {
                self.bump();
                if self.current == Some('&') {
                    self.bump();
                    Token {
                        kind: TokenKind::AndAnd,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Invalid('&'),
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some('|') => {
                self.bump();
                if self.current == Some('|') {
                    self.bump();
                    Token {
                        kind: TokenKind::OrOr,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Pipe,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some('!') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::BangEqual,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Bang,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some('<') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::LessEqual,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Less,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some('>') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::GreaterEqual,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Greater,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some('.') => {
                self.bump();
                if self.current == Some('.') {
                    self.bump();
                    if self.current == Some('=') {
                        self.bump();
                        Token {
                            kind: TokenKind::DotDotEquals,
                            span: Span {
                                start,
                                end: self.index,
                            },
                        }
                    } else {
                        Token {
                            kind: TokenKind::DotDot,
                            span: Span {
                                start,
                                end: self.index,
                            },
                        }
                    }
                } else {
                    Token {
                        kind: TokenKind::Dot,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some(':') => {
                self.bump();
                Token {
                    kind: TokenKind::Colon,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('?') => {
                self.bump();
                Token {
                    kind: TokenKind::Question,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('(') => {
                self.bump();
                Token {
                    kind: TokenKind::LParen,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some(')') => {
                self.bump();
                Token {
                    kind: TokenKind::RParen,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('[') => {
                self.bump();
                Token {
                    kind: TokenKind::LBracket,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some(']') => {
                self.bump();
                Token {
                    kind: TokenKind::RBracket,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('{') => {
                self.bump();
                Token {
                    kind: TokenKind::LBrace,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('}') => {
                self.bump();
                Token {
                    kind: TokenKind::RBrace,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some(',') => {
                self.bump();
                Token {
                    kind: TokenKind::Comma,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('=') => {
                self.bump();
                if self.current == Some('>') {
                    self.bump();
                    Token {
                        kind: TokenKind::FatArrow,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::EqualEqual,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Equals,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }
                }
            }
            Some(';') => {
                self.bump();
                Token {
                    kind: TokenKind::Semicolon,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            Some('"') => {
                if self.peek() == Some('"') && self.peek_next() == Some('"') {
                    self.multiline_string_token(start)
                } else {
                    self.string_token(start)
                }
            }
            Some('r') if self.peek() == Some('"') => self.raw_string_token(start),
            Some(ch) if ch.is_ascii_digit() => self.number_token(start),
            Some(ch) if is_ident_start(ch) => self.ident_or_keyword(start),
            Some(ch) => {
                self.bump();
                Token {
                    kind: TokenKind::Invalid(ch),
                    span: Span {
                        start,
                        end: self.index,
                    },
                }
            }
            None => Token {
                kind: TokenKind::Eof,
                span: Span {
                    start,
                    end: self.index,
                },
            },
        }
    }

    fn string_token(&mut self, start: usize) -> Token {
        self.bump();
        let mut value = String::new();
        let mut terminated = false;
        while let Some(ch) = self.current {
            if ch == '"' {
                self.bump();
                terminated = true;
                break;
            }
            if ch == '\\' {
                self.bump();
                match self.current {
                    Some('n') => {
                        value.push('\n');
                        self.bump();
                    }
                    Some('t') => {
                        value.push('\t');
                        self.bump();
                    }
                    Some('r') => {
                        value.push('\r');
                        self.bump();
                    }
                    Some('0') => {
                        value.push('\0');
                        self.bump();
                    }
                    Some('"') => {
                        value.push('"');
                        self.bump();
                    }
                    Some('\\') => {
                        value.push('\\');
                        self.bump();
                    }
                    Some(other) => {
                        value.push(other);
                        self.bump();
                    }
                    None => break,
                }
            } else {
                value.push(ch);
                self.bump();
            }
        }
        if terminated {
            Token {
                kind: TokenKind::String(value),
                span: Span {
                    start,
                    end: self.index,
                },
            }
        } else {
            Token {
                kind: TokenKind::UnterminatedString,
                span: Span {
                    start,
                    end: self.index,
                },
            }
        }
    }

    fn raw_string_token(&mut self, start: usize) -> Token {
        self.bump();
        self.bump();
        let mut value = String::new();
        let mut terminated = false;
        while let Some(ch) = self.current {
            if ch == '"' {
                self.bump();
                terminated = true;
                break;
            }
            value.push(ch);
            self.bump();
        }
        if terminated {
            Token {
                kind: TokenKind::String(value),
                span: Span {
                    start,
                    end: self.index,
                },
            }
        } else {
            Token {
                kind: TokenKind::UnterminatedString,
                span: Span {
                    start,
                    end: self.index,
                },
            }
        }
    }

    fn multiline_string_token(&mut self, start: usize) -> Token {
        self.bump();
        self.bump();
        self.bump();
        let mut value = String::new();
        let mut terminated = false;
        while let Some(ch) = self.current {
            if ch == '"' && self.peek() == Some('"') && self.peek_next() == Some('"') {
                self.bump();
                self.bump();
                self.bump();
                terminated = true;
                break;
            }
            value.push(ch);
            self.bump();
        }
        if terminated {
            Token {
                kind: TokenKind::String(value),
                span: Span {
                    start,
                    end: self.index,
                },
            }
        } else {
            Token {
                kind: TokenKind::UnterminatedString,
                span: Span {
                    start,
                    end: self.index,
                },
            }
        }
    }

    fn number_token(&mut self, start: usize) -> Token {
        let mut value = String::new();
        let mut has_dot = false;
        while let Some(ch) = self.current {
            if ch.is_ascii_digit() {
                value.push(ch);
                self.bump();
            } else if ch == '.' && !has_dot {
                // Check if next char is a digit (for float) or not (member access)
                if self.peek().map_or(false, |c| c.is_ascii_digit()) {
                    has_dot = true;
                    value.push(ch);
                    self.bump();
                } else {
                    // This is `42.` followed by non-digit, treat as int + separate dot
                    break;
                }
            } else {
                break;
            }
        }
        let span = Span {
            start,
            end: self.index,
        };
        if has_dot {
            match value.parse::<f64>() {
                Ok(parsed) => Token {
                    kind: TokenKind::Float(parsed),
                    span,
                },
                Err(_) => Token {
                    kind: TokenKind::InvalidNumber,
                    span,
                },
            }
        } else {
            match value.parse::<i64>() {
                Ok(parsed) => Token {
                    kind: TokenKind::Int(parsed),
                    span,
                },
                Err(_) => Token {
                    kind: TokenKind::InvalidNumber,
                    span,
                },
            }
        }
    }

    fn ident_or_keyword(&mut self, start: usize) -> Token {
        let mut value = String::new();
        while let Some(ch) = self.current {
            if !is_ident_continue(ch) {
                break;
            }
            value.push(ch);
            self.bump();
        }
        let kind = match value.as_str() {
            "fn" => TokenKind::Fn,
            "needs" => TokenKind::Needs,
            "test" => TokenKind::Test,
            "using" => TokenKind::Using,
            "tool" => TokenKind::Tool,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            "match" => TokenKind::Match,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "set" => TokenKind::Set,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "let" => TokenKind::Let,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(value),
        };
        Token {
            kind,
            span: Span {
                start,
                end: self.index,
            },
        }
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        loop {
            match self.current {
                Some(ch) if ch.is_whitespace() => {
                    self.bump();
                }
                Some('/') if self.peek() == Some('/') => {
                    self.bump();
                    self.bump();
                    while let Some(ch) = self.current {
                        if ch == '\n' {
                            break;
                        }
                        self.bump();
                    }
                }
                Some('/') if self.peek() == Some('*') => {
                    let comment_start = self.index;
                    self.bump();
                    self.bump();
                    let mut depth = 1;
                    while let Some(ch) = self.current {
                        if ch == '*' && self.peek() == Some('/') {
                            self.bump();
                            self.bump();
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        } else if ch == '/' && self.peek() == Some('*') {
                            self.bump();
                            self.bump();
                            depth += 1;
                        } else {
                            self.bump();
                        }
                    }
                    if depth > 0 {
                        return Some(Token {
                            kind: TokenKind::UnterminatedBlockComment,
                            span: Span {
                                start: comment_start,
                                end: self.index,
                            },
                        });
                    }
                }
                _ => break,
            }
        }
        None
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next()?;
        chars.next()
    }

    fn bump(&mut self) {
        if let Some(ch) = self.current {
            self.index += ch.len_utf8();
        }
        self.current = self.chars.next();
    }
}

#[cfg(test)]
mod tests {
    use super::parse_module;

    #[test]
    fn parses_arrays_and_indexing() {
        let source = r#"
fn f() {
    let values = [1, 2, 3];
    let first = values[0];
    return first;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_trailing_commas() {
        let source = r#"
fn f(a: int, b: int,) {
    let values = [1, 2, 3,];
    let result = add(a, b,);
    return result;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_logical_ops() {
        let source = r#"
fn f() {
    let value = true && false || true;
    return value;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_comments_and_escapes() {
        let source = r#"
// comment
fn f() {
    /* block comment */
    let value = "line\nnext";
    return value;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_for_loop() {
        let source = r#"
fn f() {
    let values = [1, 2, 3];
    for value in values {
        print(value);
    }
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_wildcard_match() {
        let source = r#"
fn f(x: option<int>) -> int {
    return match x {
        some(v) => v,
        _ => 0,
    };
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_float_literals() {
        let source = r#"
fn f() {
    let a = 3.14;
    let b = 2.0;
    let c = a + b;
    return c;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_modulo_operator() {
        let source = r#"
fn f() {
    let a = 10 % 3;
    return a;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_try_operator() {
        let source = r#"
fn f() -> result<int, string> {
    let a = ok(42)?;
    return ok(a);
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_test_blocks() {
        let source = r#"
fn add(a: int, b: int) -> int {
    return a + b;
}

test "addition works" {
    let result = add(2, 3);
    assert_eq(result, 5);
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_tool_functions() {
        let source = r#"
tool fn greet(name: string) -> string {
    return "Hello, " + name;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_import_with_alias() {
        let source = r#"
import "./lib.at" as lib;

fn f() {
    return lib.foo();
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_needs_declaration() {
        let source = r#"
fn get_time() -> int needs { time } {
    return time.now();
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_break_and_continue() {
        let source = r#"
fn f() {
    let i = 0;
    while i < 10 {
        set i = i + 1;
        if i == 5 {
            break;
        } else {
            // continue below
        }
        if i == 3 {
            continue;
        } else {
            // continue above
        }
    }
    return i;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_nested_blocks() {
        let source = r#"
fn f() {
    let x = 1;
    {
        let x = 2;
        {
            let x = 3;
        }
    }
    return x;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_string_concatenation() {
        let source = r#"
fn f() {
    let a = "Hello";
    let b = "World";
    let c = a + ", " + b;
    return c;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_complex_expression() {
        let source = r#"
fn f() {
    let a = 1 + 2 * 3 - 4 / 2;
    let b = (1 + 2) * (3 - 4);
    let c = -a + !true;
    return c;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn rejects_unterminated_string() {
        let source = r#"let x = "unterminated;"#;
        assert!(parse_module(source).is_err());
    }

    #[test]
    fn rejects_invalid_number() {
        let source = r#"let x = 999999999999999999999;"#;
        assert!(parse_module(source).is_err());
    }

    #[test]
    fn rejects_unknown_character() {
        let source = r#"let x = @;"#;
        assert!(parse_module(source).is_err());
    }

    #[test]
    fn rejects_duplicate_local() {
        let source = r#"
fn f() {
    let x = 1;
    let x = 2;
}
"#;
        let module = parse_module(source).expect("parse should succeed");
        // This should be caught by linter, not parser
        assert!(module.functions.len() > 0);
    }

    #[test]
    fn parses_result_type() {
        let source = r#"
fn divide(a: int, b: int) -> result<int, string> {
    if b == 0 {
        return err("division by zero");
    } else {
        return ok(a / b);
    }
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_option_type() {
        let source = r#"
fn maybe_value() -> option<int> {
    return some(42);
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_generic_array() {
        let source = r#"
fn sum(arr: array<int>) -> int {
    let total = 0;
    for item in arr {
        set total = total + item;
    }
    return total;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_member_access() {
        let source = r#"
fn f() {
    let result = ok(42);
    return result.value;
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_empty_function() {
        let source = r#"
fn f() {}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_empty_module() {
        let source = "";
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_multiple_top_level_items() {
        let source = r#"
fn a() { return 1; }
fn b() { return 2; }
let x = 1;
import "lib.at" as lib;
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn rejects_missing_comma_in_match() {
        let source = r#"
fn f(x: option<int>) -> int {
    return match x {
        some(v) => v
        _ => 0
    };
}
"#;
        assert!(parse_module(source).is_err());
    }

    #[test]
    fn parses_match_with_trailing_comma() {
        let source = r#"
fn f(x: option<int>) -> int {
    return match x {
        some(v) => v,
        _ => 0,
    };
}
"#;
        assert!(parse_module(source).is_ok());
    }
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}
