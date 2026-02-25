use at_syntax::{
    Comment, EnumVariant, Expr, Function, Ident, InterpPart, MatchArm, MatchPattern, Module, Param,
    Span, Stmt, StructField, StructLiteralField, StructPatternField, TypeRef,
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
    Is,
    Try,
    Await,
    Async,
    Catch,
    Finally,
    Match,
    While,
    For,
    In,
    If,
    Else,
    Set,
    Struct,
    Type,
    Enum,
    Break,
    Continue,
    Return,
    Throw,
    Raise,
    Defer,
    With,
    Yield,
    Let,
    Const,
    Mut,
    Pub,
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
    Ampersand,
    Pipe,
    Caret,
    Shl,
    Shr,
    AmpersandEquals,
    PipeEquals,
    CaretEquals,
    ShlEquals,
    ShrEquals,
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
    ColonColon,
    Question,
    At,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Dot,
    DotDotDot,
    DotDot,
    DotDotEquals,
    Semicolon,
    Comment,
    Invalid(char),
    UnterminatedString,
    UnterminatedBlockComment,
    InvalidNumber,
    Eof,
}

impl TokenKind {
    fn is_comment(&self) -> bool {
        matches!(self, TokenKind::Comment)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Keywords
            TokenKind::Fn => write!(f, "`fn`"),
            TokenKind::Needs => write!(f, "`needs`"),
            TokenKind::Test => write!(f, "`test`"),
            TokenKind::Using => write!(f, "`using`"),
            TokenKind::Tool => write!(f, "`tool`"),
            TokenKind::Import => write!(f, "`import`"),
            TokenKind::As => write!(f, "`as`"),
            TokenKind::Is => write!(f, "`is`"),
            TokenKind::Try => write!(f, "`try`"),
            TokenKind::Await => write!(f, "`await`"),
            TokenKind::Async => write!(f, "`async`"),
            TokenKind::Catch => write!(f, "`catch`"),
            TokenKind::Finally => write!(f, "`finally`"),
            TokenKind::Match => write!(f, "`match`"),
            TokenKind::While => write!(f, "`while`"),
            TokenKind::For => write!(f, "`for`"),
            TokenKind::In => write!(f, "`in`"),
            TokenKind::If => write!(f, "`if`"),
            TokenKind::Else => write!(f, "`else`"),
            TokenKind::Set => write!(f, "`set`"),
            TokenKind::Struct => write!(f, "`struct`"),
            TokenKind::Type => write!(f, "`type`"),
            TokenKind::Enum => write!(f, "`enum`"),
            TokenKind::Break => write!(f, "`break`"),
            TokenKind::Continue => write!(f, "`continue`"),
            TokenKind::Return => write!(f, "`return`"),
            TokenKind::Throw => write!(f, "`throw`"),
            TokenKind::Raise => write!(f, "`raise`"),
            TokenKind::Defer => write!(f, "`defer`"),
            TokenKind::With => write!(f, "`with`"),
            TokenKind::Yield => write!(f, "`yield`"),
            TokenKind::Let => write!(f, "`let`"),
            TokenKind::Const => write!(f, "`const`"),
            TokenKind::Mut => write!(f, "`mut`"),
            TokenKind::Pub => write!(f, "`pub`"),
            // Boolean literals
            TokenKind::True => write!(f, "`true`"),
            TokenKind::False => write!(f, "`false`"),
            // Data-carrying literals
            TokenKind::Ident(name) => write!(f, "identifier `{name}`"),
            TokenKind::Int(value) => write!(f, "integer literal `{value}`"),
            TokenKind::Float(value) => write!(f, "float literal `{value}`"),
            TokenKind::String(value) => write!(f, "string literal \"{value}\""),
            // Operators
            TokenKind::Plus => write!(f, "`+`"),
            TokenKind::Minus => write!(f, "`-`"),
            TokenKind::Star => write!(f, "`*`"),
            TokenKind::Slash => write!(f, "`/`"),
            TokenKind::Percent => write!(f, "`%`"),
            TokenKind::AndAnd => write!(f, "`&&`"),
            TokenKind::OrOr => write!(f, "`||`"),
            TokenKind::Ampersand => write!(f, "`&`"),
            TokenKind::Pipe => write!(f, "`|`"),
            TokenKind::Caret => write!(f, "`^`"),
            TokenKind::Shl => write!(f, "`<<`"),
            TokenKind::Shr => write!(f, "`>>`"),
            TokenKind::AmpersandEquals => write!(f, "`&=`"),
            TokenKind::PipeEquals => write!(f, "`|=`"),
            TokenKind::CaretEquals => write!(f, "`^=`"),
            TokenKind::ShlEquals => write!(f, "`<<=`"),
            TokenKind::ShrEquals => write!(f, "`>>=`"),
            TokenKind::EqualEqual => write!(f, "`==`"),
            TokenKind::Bang => write!(f, "`!`"),
            TokenKind::BangEqual => write!(f, "`!=`"),
            TokenKind::Less => write!(f, "`<`"),
            TokenKind::LessEqual => write!(f, "`<=`"),
            TokenKind::Greater => write!(f, "`>`"),
            TokenKind::GreaterEqual => write!(f, "`>=`"),
            TokenKind::Equals => write!(f, "`=`"),
            TokenKind::FatArrow => write!(f, "`=>`"),
            TokenKind::Arrow => write!(f, "`->`"),
            // Punctuation
            TokenKind::Colon => write!(f, "`:`"),
            TokenKind::ColonColon => write!(f, "`::`"),
            TokenKind::Question => write!(f, "`?`"),
            TokenKind::At => write!(f, "`@`"),
            TokenKind::LParen => write!(f, "`(`"),
            TokenKind::RParen => write!(f, "`)`"),
            TokenKind::LBracket => write!(f, "`[`"),
            TokenKind::RBracket => write!(f, "`]`"),
            TokenKind::LBrace => write!(f, "`{{`"),
            TokenKind::RBrace => write!(f, "`}}`"),
            TokenKind::Comma => write!(f, "`,`"),
            TokenKind::Dot => write!(f, "`.`"),
            TokenKind::DotDotDot => write!(f, "`...`"),
            TokenKind::DotDot => write!(f, "`..`"),
            TokenKind::DotDotEquals => write!(f, "`..=`"),
            TokenKind::Semicolon => write!(f, "`;`"),
            // Special
            TokenKind::Comment => write!(f, "comment"),
            TokenKind::Invalid(ch) => write!(f, "invalid token `{ch}`"),
            TokenKind::UnterminatedString => write!(f, "unterminated string"),
            TokenKind::UnterminatedBlockComment => write!(f, "unterminated block comment"),
            TokenKind::InvalidNumber => write!(f, "invalid number"),
            TokenKind::Eof => write!(f, "end of file"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
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
                    "parse error at {}: expected {}, found {}",
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
    parser.parse_module()
}

pub fn parse_module_with_errors(source: &str) -> (Module, Vec<ParseError>) {
    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(&mut lexer);
    parser.parse_module_with_errors()
}

struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current: Token,
    comments: Vec<Comment>,
    pending_token: Option<Token>,
    next_id: u32,
    collect_errors: bool,
    recovered_errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            current: Token {
                kind: TokenKind::Eof,
                span: Span::new(0, 0),
            },
            comments: Vec::new(),
            pending_token: None,
            next_id: 1,
            collect_errors: false,
            recovered_errors: Vec::new(),
        };
        parser.advance();
        parser
    }

    fn alloc_id(&mut self) -> at_syntax::NodeId {
        let id = self.next_id;
        self.next_id = self.next_id.saturating_add(1);
        at_syntax::NodeId(id)
    }

    fn parse_module_with_errors(&mut self) -> (Module, Vec<ParseError>) {
        self.collect_errors = true;
        let mut functions = Vec::new();
        let mut stmts = Vec::new();
        let mut errors = Vec::new();
        while self.current.kind != TokenKind::Eof {
            match self.current.kind {
                TokenKind::Fn | TokenKind::Async => match self.parse_function(false) {
                    Ok(func) => functions.push(func),
                    Err(err) => {
                        errors.push(err);
                        self.recover_to_function_end();
                    }
                },
                TokenKind::Pub => match self.peek_kind() {
                    TokenKind::Fn | TokenKind::Async => match self.parse_function(false) {
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
        errors.append(&mut self.recovered_errors);
        (
            Module {
                id: self.alloc_id(),
                functions,
                stmts,
                comments: std::mem::take(&mut self.comments),
                source_path: None,
            },
            errors,
        )
    }

    fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut functions = Vec::new();
        let mut stmts = Vec::new();
        while self.current.kind != TokenKind::Eof {
            match self.current.kind {
                TokenKind::Fn | TokenKind::Async => functions.push(self.parse_function(false)?),
                TokenKind::Pub => {
                    if matches!(self.peek_kind(), TokenKind::Fn | TokenKind::Async) {
                        functions.push(self.parse_function(false)?);
                    } else {
                        let stmt = self.parse_stmt()?;
                        stmts.push(stmt);
                    }
                }
                TokenKind::Tool => functions.push(self.parse_tool_function()?),
                _ => {
                    let stmt = self.parse_stmt()?;
                    stmts.push(stmt);
                }
            }
        }
        Ok(Module {
            id: self.alloc_id(),
            functions,
            stmts,
            comments: std::mem::take(&mut self.comments),
            source_path: None,
        })
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
                | TokenKind::Pub
                | TokenKind::Tool
                | TokenKind::Import
                | TokenKind::Let
                | TokenKind::Const
                | TokenKind::Using
                | TokenKind::Set
                | TokenKind::Return
                | TokenKind::Test
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::If
                | TokenKind::Struct
                | TokenKind::Type
                | TokenKind::Enum
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
            TokenKind::Pub => self.parse_pub_stmt(),
            TokenKind::Import => self.parse_import_stmt(),
            TokenKind::Type => self.parse_type_alias_stmt(),
            TokenKind::Enum => self.parse_enum_stmt(),
            TokenKind::Struct => self.parse_struct_stmt(),
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Const => self.parse_const_stmt(),
            TokenKind::Using => self.parse_using_stmt(),
            TokenKind::Set => self.parse_set_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Throw => self.parse_throw_stmt(),
            TokenKind::Raise => self.parse_throw_stmt(),
            TokenKind::Defer => self.parse_defer_stmt(),
            TokenKind::With => self.parse_with_stmt(),
            TokenKind::Yield => self.parse_yield_stmt(),
            TokenKind::Test => self.parse_test_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Break => self.parse_break_stmt(),
            TokenKind::Continue => self.parse_continue_stmt(),
            TokenKind::LBrace => self.parse_block_stmt(),
            TokenKind::If => {
                let stmt = self.parse_if_stmt()?;
                Ok(stmt)
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt::Expr {
                    id: self.alloc_id(),
                    expr,
                })
            }
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let if_span = self.current.span;
        self.advance();
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block_stmt_list()?;
        let else_branch = if self.current.kind == TokenKind::Else {
            self.advance();
            if self.current.kind == TokenKind::If {
                let nested = self.parse_if_stmt()?;
                Some(vec![nested])
            } else {
                Some(self.parse_block_stmt_list()?)
            }
        } else {
            None
        };
        Ok(Stmt::If {
            id: self.alloc_id(),
            if_span,
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_block_stmt_list(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while self.current.kind != TokenKind::RBrace && self.current.kind != TokenKind::Eof {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(stmts)
    }

    fn parse_pub_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        match self.current.kind {
            TokenKind::Struct => self.parse_struct_stmt_with_pub(true),
            TokenKind::Enum => self.parse_enum_stmt_with_pub(true),
            TokenKind::Type => self.parse_type_alias_stmt_with_pub(true),
            TokenKind::Import => self.parse_import_stmt_with_pub(true),
            _ => Err(ParseError::UnexpectedToken {
                expected: "declaration after pub".to_string(),
                found: self.current.kind.clone(),
                span: self.current.span,
            }),
        }
    }

    fn parse_tool_function(&mut self) -> Result<Function, ParseError> {
        self.advance();
        let mut async_span = None;
        let is_async = if self.current.kind == TokenKind::Async {
            async_span = Some(self.current.span);
            self.advance();
            true
        } else {
            false
        };
        self.expect(TokenKind::Fn)?;
        if is_async {
            return Err(ParseError::UnexpectedToken {
                expected: "async tool fn is not supported".to_string(),
                found: TokenKind::Async,
                span: async_span.unwrap_or(self.current.span),
            });
        }
        self.parse_function(true)
    }

    fn parse_function(&mut self, is_tool: bool) -> Result<Function, ParseError> {
        let mut is_pub = false;
        if self.current.kind == TokenKind::Pub {
            is_pub = true;
            self.advance();
        }
        let mut is_async = false;
        if self.current.kind == TokenKind::Async {
            is_async = true;
            self.advance();
        }
        if self.current.kind == TokenKind::Fn {
            self.advance();
        }
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
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
        let mut errors = Vec::new();
        while self.current.kind != TokenKind::RBrace && self.current.kind != TokenKind::Eof {
            match self.parse_stmt() {
                Ok(stmt) => body.push(stmt),
                Err(err) => {
                    errors.push(err);
                    self.recover_to_stmt_boundary();
                }
            }
        }
        self.expect(TokenKind::RBrace)?;
        if !errors.is_empty() {
            if self.collect_errors {
                self.recovered_errors.extend(errors);
            } else if let Some(err) = errors.first().cloned() {
                return Err(err);
            }
        }
        Ok(Function {
            id: self.alloc_id(),
            name,
            is_pub,
            is_async,
            type_params,
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
            id: self.alloc_id(),
            name: ident,
            ty,
            value,
        })
    }

    fn parse_const_stmt(&mut self) -> Result<Stmt, ParseError> {
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
        Ok(Stmt::Const {
            id: self.alloc_id(),
            name: ident,
            ty,
            value,
        })
    }

    fn parse_import_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.parse_import_stmt_with_pub(false)
    }

    fn parse_import_stmt_with_pub(&mut self, is_pub: bool) -> Result<Stmt, ParseError> {
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
        Ok(Stmt::Import {
            id: self.alloc_id(),
            path,
            alias,
            is_pub,
        })
    }

    fn parse_type_alias_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.parse_type_alias_stmt_with_pub(false)
    }

    fn parse_type_alias_stmt_with_pub(&mut self, is_pub: bool) -> Result<Stmt, ParseError> {
        self.advance();
        let name = self.expect_ident()?;
        self.expect(TokenKind::Equals)?;
        let ty = self.parse_type_ref()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::TypeAlias {
            id: self.alloc_id(),
            name,
            ty,
            is_pub,
        })
    }

    fn parse_enum_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.parse_enum_stmt_with_pub(false)
    }

    fn parse_enum_stmt_with_pub(&mut self, is_pub: bool) -> Result<Stmt, ParseError> {
        self.advance();
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(TokenKind::LBrace)?;
        let mut variants = Vec::new();
        if self.current.kind != TokenKind::RBrace {
            loop {
                let variant_name = self.expect_ident()?;
                let payload = if self.current.kind == TokenKind::LParen {
                    self.advance();
                    let ty = self.parse_type_ref()?;
                    self.expect(TokenKind::RParen)?;
                    Some(ty)
                } else {
                    None
                };
                variants.push(EnumVariant {
                    id: self.alloc_id(),
                    name: variant_name,
                    payload,
                });
                if self.current.kind != TokenKind::Comma {
                    break;
                }
                self.advance();
                if self.current.kind == TokenKind::RBrace {
                    break;
                }
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Stmt::Enum {
            id: self.alloc_id(),
            name,
            type_params,
            variants,
            is_pub,
        })
    }

    fn parse_struct_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.parse_struct_stmt_with_pub(false)
    }

    fn parse_struct_stmt_with_pub(&mut self, is_pub: bool) -> Result<Stmt, ParseError> {
        self.advance();
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();
        if self.current.kind != TokenKind::RBrace {
            loop {
                let field_name = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type_ref()?;
                fields.push(StructField {
                    id: self.alloc_id(),
                    name: field_name,
                    ty,
                });
                if self.current.kind != TokenKind::Comma {
                    break;
                }
                self.advance();
                if self.current.kind == TokenKind::RBrace {
                    break;
                }
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Stmt::Struct {
            id: self.alloc_id(),
            name,
            type_params,
            fields,
            is_pub,
        })
    }

    fn parse_type_params(&mut self) -> Result<Vec<Ident>, ParseError> {
        let mut params = Vec::new();
        if self.current.kind != TokenKind::Less {
            return Ok(params);
        }
        self.advance();
        if self.current.kind != TokenKind::Greater {
            loop {
                params.push(self.expect_ident()?);
                if self.current.kind != TokenKind::Comma {
                    break;
                }
                self.advance();
                if self.current.kind == TokenKind::Greater {
                    break;
                }
            }
        }
        self.consume_type_greater()?;
        Ok(params)
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
            id: self.alloc_id(),
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
                        id: self.alloc_id(),
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
                        id: self.alloc_id(),
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
            | TokenKind::Percent
            | TokenKind::Ampersand
            | TokenKind::Pipe
            | TokenKind::Caret
            | TokenKind::Shl
            | TokenKind::Shr => {
                let (op, span) = match self.current.kind {
                    TokenKind::Plus => (at_syntax::BinaryOp::Add, self.current.span),
                    TokenKind::Minus => (at_syntax::BinaryOp::Sub, self.current.span),
                    TokenKind::Star => (at_syntax::BinaryOp::Mul, self.current.span),
                    TokenKind::Slash => (at_syntax::BinaryOp::Div, self.current.span),
                    TokenKind::Percent => (at_syntax::BinaryOp::Mod, self.current.span),
                    TokenKind::Ampersand => (at_syntax::BinaryOp::BitAnd, self.current.span),
                    TokenKind::Pipe => (at_syntax::BinaryOp::BitOr, self.current.span),
                    TokenKind::Caret => (at_syntax::BinaryOp::BitXor, self.current.span),
                    TokenKind::Shl => (at_syntax::BinaryOp::Shl, self.current.span),
                    TokenKind::Shr => (at_syntax::BinaryOp::Shr, self.current.span),
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "compound assignment operator".to_string(),
                            found: self.current.kind.clone(),
                            span: self.current.span,
                        });
                    }
                };
                self.advance();
                self.expect(TokenKind::Equals)?;
                Some((op, span))
            }
            TokenKind::AmpersandEquals
            | TokenKind::PipeEquals
            | TokenKind::CaretEquals
            | TokenKind::ShlEquals
            | TokenKind::ShrEquals => {
                let (op, span) = match self.current.kind {
                    TokenKind::AmpersandEquals => (at_syntax::BinaryOp::BitAnd, self.current.span),
                    TokenKind::PipeEquals => (at_syntax::BinaryOp::BitOr, self.current.span),
                    TokenKind::CaretEquals => (at_syntax::BinaryOp::BitXor, self.current.span),
                    TokenKind::ShlEquals => (at_syntax::BinaryOp::Shl, self.current.span),
                    TokenKind::ShrEquals => (at_syntax::BinaryOp::Shr, self.current.span),
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "bitwise assignment operator".to_string(),
                            found: self.current.kind.clone(),
                            span: self.current.span,
                        });
                    }
                };
                self.advance();
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
                            id: self.alloc_id(),
                            base: Box::new(base_expr.clone()),
                            name: field.clone(),
                        }),
                        op,
                        op_span,
                        id: self.alloc_id(),
                        right: Box::new(value),
                    }
                } else {
                    value
                };
                Ok(Stmt::SetMember {
                    id: self.alloc_id(),
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
                            id: self.alloc_id(),
                            base: Box::new(base_expr.clone()),
                            index: Box::new(index.clone()),
                        }),
                        op,
                        op_span,
                        id: self.alloc_id(),
                        right: Box::new(value),
                    }
                } else {
                    value
                };
                Ok(Stmt::SetIndex {
                    id: self.alloc_id(),
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
                        id: self.alloc_id(),
                        right: Box::new(value),
                    }
                } else {
                    value
                };
                Ok(Stmt::Set {
                    id: self.alloc_id(),
                    name: base_ident,
                    value,
                })
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "set target".to_string(),
                found: self.current.kind.clone(),
                span: self.current.span,
            }),
        }
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        if self.current.kind == TokenKind::Semicolon {
            self.advance();
            return Ok(Stmt::Return {
                id: self.alloc_id(),
                expr: None,
            });
        }
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Return {
            id: self.alloc_id(),
            expr: Some(expr),
        })
    }

    fn parse_throw_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Throw {
            id: self.alloc_id(),
            expr,
        })
    }

    fn parse_defer_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Defer {
            id: self.alloc_id(),
            expr,
        })
    }

    fn parse_with_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let name = self.expect_ident()?;
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expr()?;
        let block = self.parse_block_stmt()?;
        let body = match block {
            Stmt::Block { stmts, .. } => stmts,
            _ => Vec::new(),
        };
        Ok(Stmt::With {
            id: self.alloc_id(),
            name,
            value,
            body,
        })
    }

    fn parse_yield_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Yield {
            id: self.alloc_id(),
            expr,
        })
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while self.current.kind != TokenKind::RBrace && self.current.kind != TokenKind::Eof {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Stmt::Block {
            id: self.alloc_id(),
            stmts,
        })
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
            Stmt::Block { stmts, .. } => stmts,
            _ => Vec::new(),
        };
        Ok(Stmt::Test {
            id: self.alloc_id(),
            name,
            body,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        let while_span = self.current.span;
        self.advance();
        let condition = self.parse_expr()?;
        let body = match self.parse_block_stmt()? {
            Stmt::Block { stmts, .. } => stmts,
            _ => Vec::new(),
        };
        Ok(Stmt::While {
            id: self.alloc_id(),
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
            Stmt::Block { stmts, .. } => stmts,
            _ => Vec::new(),
        };
        Ok(Stmt::For {
            id: self.alloc_id(),
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
        Ok(Stmt::Break {
            id: self.alloc_id(),
            break_span,
        })
    }

    fn parse_continue_stmt(&mut self) -> Result<Stmt, ParseError> {
        let continue_span = self.current.span;
        self.advance();
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Continue {
            id: self.alloc_id(),
            continue_span,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_ternary()
    }

    fn parse_ternary(&mut self) -> Result<Expr, ParseError> {
        let condition = self.parse_or()?;
        if self.current.kind != TokenKind::Question {
            return Ok(condition);
        }
        let start = expr_span_start(&condition).unwrap_or(self.current.span.start);
        self.advance();
        let then_branch = self.parse_expr()?;
        self.expect(TokenKind::Colon)?;
        let else_branch = self.parse_ternary()?;
        let end = expr_span_end(&else_branch).unwrap_or(self.current.span.end);
        Ok(Expr::Ternary {
            span: Span::new(start, end),
            id: self.alloc_id(),
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_and()?;
        while matches!(self.current.kind, TokenKind::OrOr) {
            let span = self.current.span;
            self.advance();
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: at_syntax::BinaryOp::Or,
                op_span: span,
                id: self.alloc_id(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        while matches!(self.current.kind, TokenKind::AndAnd) {
            let span = self.current.span;
            self.advance();
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: at_syntax::BinaryOp::And,
                op_span: span,
                id: self.alloc_id(),
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
                id: self.alloc_id(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_bitwise_or()?;
        let mut items = vec![expr];
        let mut ops = Vec::new();
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::Less => (at_syntax::BinaryOp::Lt, self.current.span),
                TokenKind::LessEqual => (at_syntax::BinaryOp::Lte, self.current.span),
                TokenKind::Greater => (at_syntax::BinaryOp::Gt, self.current.span),
                TokenKind::GreaterEqual => (at_syntax::BinaryOp::Gte, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_bitwise_or()?;
            ops.push((op, span));
            items.push(right);
        }
        if ops.is_empty() {
            Ok(items.remove(0))
        } else {
            let start = expr_span_start(&items[0]).unwrap_or(self.current.span.start);
            let end = expr_span_end(items.last().expect("items non-empty after ops"))
                .unwrap_or(self.current.span.end);
            Ok(Expr::ChainedComparison {
                span: Span::new(start, end),
                id: self.alloc_id(),
                items,
                ops,
            })
        }
    }

    fn parse_bitwise_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_bitwise_xor()?;
        while matches!(self.current.kind, TokenKind::Pipe) {
            let span = self.current.span;
            self.advance();
            let right = self.parse_bitwise_xor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: at_syntax::BinaryOp::BitOr,
                op_span: span,
                id: self.alloc_id(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_bitwise_and()?;
        while matches!(self.current.kind, TokenKind::Caret) {
            let span = self.current.span;
            self.advance();
            let right = self.parse_bitwise_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: at_syntax::BinaryOp::BitXor,
                op_span: span,
                id: self.alloc_id(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_shift()?;
        while matches!(self.current.kind, TokenKind::Ampersand) {
            let span = self.current.span;
            self.advance();
            let right = self.parse_shift()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: at_syntax::BinaryOp::BitAnd,
                op_span: span,
                id: self.alloc_id(),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_shift(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_range()?;
        loop {
            let (op, span) = match self.current.kind {
                TokenKind::Shl => (at_syntax::BinaryOp::Shl, self.current.span),
                TokenKind::Shr => (at_syntax::BinaryOp::Shr, self.current.span),
                _ => break,
            };
            self.advance();
            let right = self.parse_range()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                op_span: span,
                id: self.alloc_id(),
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
                id: self.alloc_id(),
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
                id: self.alloc_id(),
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
                id: self.alloc_id(),
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
                    id: self.alloc_id(),
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
                    id: self.alloc_id(),
                    expr: Box::new(expr),
                })
            }
            TokenKind::Await => {
                let span = self.current.span;
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Await {
                    await_span: span,
                    id: self.alloc_id(),
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
                    id: self.alloc_id(),
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
                    id: self.alloc_id(),
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
                    id: self.alloc_id(),
                    base: Box::new(expr),
                    index: Box::new(index),
                };
                continue;
            }
            if self.current.kind == TokenKind::Question {
                if self.question_starts_ternary() {
                    break;
                }
                self.advance();
                expr = Expr::Try(Box::new(expr), self.alloc_id());
                continue;
            }
            if self.current.kind == TokenKind::Is {
                let span = self.current.span;
                self.advance();
                let ty = self.parse_type_ref()?;
                expr = Expr::Is {
                    expr: Box::new(expr),
                    ty,
                    span,
                    id: self.alloc_id(),
                };
                continue;
            }
            if self.current.kind == TokenKind::As {
                let span = self.current.span;
                self.advance();
                let ty = self.parse_type_ref()?;
                expr = Expr::As {
                    expr: Box::new(expr),
                    ty,
                    span,
                    id: self.alloc_id(),
                };
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
            TokenKind::Try => self.parse_try_expr(),
            TokenKind::True => {
                let span = self.current.span;
                self.advance();
                Ok(Expr::Bool(true, span, self.alloc_id()))
            }
            TokenKind::False => {
                let span = self.current.span;
                self.advance();
                Ok(Expr::Bool(false, span, self.alloc_id()))
            }
            TokenKind::Int(value) => {
                let span = self.current.span;
                let v = *value;
                self.advance();
                Ok(Expr::Int(v, span, self.alloc_id()))
            }
            TokenKind::Float(value) => {
                let span = self.current.span;
                let v = *value;
                self.advance();
                Ok(Expr::Float(v, span, self.alloc_id()))
            }
            TokenKind::String(value) => {
                let span = self.current.span;
                let v = value.clone();
                self.advance();
                if v.contains('{') {
                    return self.parse_interpolated_string(v, span);
                }
                Ok(Expr::String(v, span, self.alloc_id()))
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
                    id: self.alloc_id(),
                };
                self.advance();
                if ident.name == "map" && self.current.kind == TokenKind::LBrace {
                    return self.parse_map_literal(ident.span);
                }
                if self.current.kind == TokenKind::ColonColon {
                    return self.parse_enum_literal(ident);
                }
                let is_type_name = ident
                    .name
                    .chars()
                    .next()
                    .map(|ch| ch.is_uppercase())
                    .unwrap_or(false);
                if is_type_name && self.current.kind == TokenKind::LBrace {
                    return self.parse_struct_literal(ident);
                }
                Ok(Expr::Ident(ident))
            }
            TokenKind::LParen => {
                let paren_span = self.current.span;
                self.advance();

                if self.current.kind == TokenKind::RParen {
                    self.advance();
                    return Ok(Expr::Tuple {
                        tuple_span: paren_span,
                        id: self.alloc_id(),
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
                        id: self.alloc_id(),
                        items,
                    });
                }

                let end_span = self.current.span;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Group {
                    span: Span::new(paren_span.start, end_span.end),
                    id: self.alloc_id(),
                    expr: Box::new(first_expr),
                })
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

    fn parse_map_literal(&mut self, start_span: Span) -> Result<Expr, ParseError> {
        let span_start = start_span.start;
        self.expect(TokenKind::LBrace)?;
        let mut entries = Vec::new();
        if self.current.kind != TokenKind::RBrace {
            loop {
                if self.current.kind == TokenKind::DotDotDot {
                    let spread_span = self.current.span;
                    self.advance();
                    let expr = self.parse_expr()?;
                    let key = Expr::MapSpread {
                        spread_span,
                        id: self.alloc_id(),
                        expr: Box::new(expr),
                    };
                    let placeholder = Expr::Bool(true, spread_span, self.alloc_id());
                    entries.push((key, placeholder));
                } else {
                    let key = self.parse_expr()?;
                    self.expect(TokenKind::Colon)?;
                    let value = self.parse_expr()?;
                    entries.push((key, value));
                }
                if self.current.kind != TokenKind::Comma {
                    break;
                }
                self.advance();
                if self.current.kind == TokenKind::RBrace {
                    break;
                }
            }
        }
        let end_span = self.current.span;
        self.expect(TokenKind::RBrace)?;
        Ok(Expr::MapLiteral {
            span: Span::new(span_start, end_span.end),
            id: self.alloc_id(),
            entries,
        })
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
                    parts.push(InterpPart::String(current.clone(), self.alloc_id()));
                    current.clear();
                }
                let mut expr_str = String::new();
                let mut depth = 1;
                for ch in chars.by_ref() {
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
                parts.push(InterpPart::Expr(expr, self.alloc_id()));
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
            parts.push(InterpPart::String(current, self.alloc_id()));
        }

        Ok(Expr::InterpolatedString {
            span,
            id: self.alloc_id(),
            parts,
        })
    }

    fn parse_closure_expr(&mut self) -> Result<Expr, ParseError> {
        let span = self.current.span;
        self.expect(TokenKind::Pipe)?;

        let mut params = Vec::new();
        if self.current.kind != TokenKind::Pipe {
            loop {
                if matches!(self.current.kind, TokenKind::Ident(_)) {
                    params.push(self.expect_ident()?);
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
            id: self.alloc_id(),
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
                if self.current.kind == TokenKind::DotDotDot {
                    let spread_span = self.current.span;
                    self.advance();
                    let expr = self.parse_expr()?;
                    items.push(Expr::ArraySpread {
                        spread_span,
                        id: self.alloc_id(),
                        expr: Box::new(expr),
                    });
                } else {
                    items.push(self.parse_expr()?);
                }
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
        Ok(Expr::Array {
            array_span,
            id: self.alloc_id(),
            items,
        })
    }

    fn parse_struct_literal(&mut self, name: Ident) -> Result<Expr, ParseError> {
        let span = name.span;
        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();
        if self.current.kind != TokenKind::RBrace {
            loop {
                let field_name = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let value = self.parse_expr()?;
                fields.push(StructLiteralField {
                    id: self.alloc_id(),
                    name: field_name,
                    value,
                });
                if self.current.kind != TokenKind::Comma {
                    break;
                }
                self.advance();
                if self.current.kind == TokenKind::RBrace {
                    break;
                }
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Expr::StructLiteral {
            span,
            id: self.alloc_id(),
            name,
            fields,
        })
    }

    fn parse_enum_literal(&mut self, name: Ident) -> Result<Expr, ParseError> {
        let span = name.span;
        self.expect(TokenKind::ColonColon)?;
        let variant = self.expect_ident()?;
        let payload = if self.current.kind == TokenKind::LParen {
            self.advance();
            let expr = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            Some(Box::new(expr))
        } else {
            None
        };
        Ok(Expr::EnumLiteral {
            span,
            id: self.alloc_id(),
            name,
            variant,
            payload,
        })
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
            id: self.alloc_id(),
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
                | TokenKind::Pub
                | TokenKind::Type
                | TokenKind::Enum
                | TokenKind::Let
                | TokenKind::Const
                | TokenKind::Using
                | TokenKind::Set
                | TokenKind::Struct
                | TokenKind::Return
                | TokenKind::Throw
                | TokenKind::Raise
                | TokenKind::Defer
                | TokenKind::With
                | TokenKind::Yield
                | TokenKind::Test
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::LBrace
                | TokenKind::If => {
                    stmts.push(self.parse_stmt()?);
                }
                _ => {
                    let expr = self.parse_expr()?;
                    if self.current.kind == TokenKind::Semicolon {
                        self.advance();
                        stmts.push(Stmt::Expr {
                            id: self.alloc_id(),
                            expr,
                        });
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
            id: self.alloc_id(),
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
                    id: self.alloc_id(),
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
            id: self.alloc_id(),
            value: Box::new(value),
            arms,
        })
    }

    fn parse_try_expr(&mut self) -> Result<Expr, ParseError> {
        let try_span = self.current.span;
        self.advance();
        let try_block = self.parse_block_expr()?;

        let catch_block = if self.current.kind == TokenKind::Catch {
            self.advance();
            Some(Box::new(self.parse_block_expr()?))
        } else {
            None
        };

        let finally_block = if self.current.kind == TokenKind::Finally {
            self.advance();
            Some(Box::new(self.parse_block_expr()?))
        } else {
            None
        };

        if catch_block.is_none() && finally_block.is_none() {
            return Err(ParseError::UnexpectedToken {
                expected: "catch or finally".to_string(),
                found: self.current.kind.clone(),
                span: self.current.span,
            });
        }

        Ok(Expr::TryCatch {
            try_span,
            id: self.alloc_id(),
            try_block: Box::new(try_block),
            catch_block,
            finally_block,
        })
    }

    fn parse_match_pattern(&mut self) -> Result<MatchPattern, ParseError> {
        if self.current.kind == TokenKind::Ident(String::from("_")) {
            self.advance();
            return Ok(MatchPattern::Wildcard(self.alloc_id()));
        }
        if self.current.kind == TokenKind::True {
            self.advance();
            return Ok(MatchPattern::Bool(true, self.alloc_id()));
        }
        if self.current.kind == TokenKind::False {
            self.advance();
            return Ok(MatchPattern::Bool(false, self.alloc_id()));
        }
        if let TokenKind::Int(value) = self.current.kind {
            self.advance();
            return Ok(MatchPattern::Int(value, self.alloc_id()));
        }
        if let TokenKind::String(value) = &self.current.kind {
            let value = value.clone();
            self.advance();
            return Ok(MatchPattern::String(value, self.alloc_id()));
        }
        if self.current.kind == TokenKind::Minus {
            let span = self.current.span;
            self.advance();
            match self.current.kind {
                TokenKind::Int(value) => {
                    self.advance();
                    return Ok(MatchPattern::Int(-value, self.alloc_id()));
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
        if self.current.kind == TokenKind::LParen {
            self.advance();
            let first = self.parse_match_pattern()?;
            if self.current.kind != TokenKind::Comma {
                self.expect(TokenKind::RParen)?;
                return Ok(first);
            }
            let mut items = Vec::new();
            items.push(first);
            while self.current.kind == TokenKind::Comma {
                self.advance();
                if self.current.kind == TokenKind::RParen {
                    break;
                }
                items.push(self.parse_match_pattern()?);
            }
            self.expect(TokenKind::RParen)?;
            return Ok(MatchPattern::Tuple {
                id: self.alloc_id(),
                items,
            });
        }
        let head = self.expect_ident()?;
        if self.current.kind == TokenKind::At {
            self.advance();
            let pattern = self.parse_match_pattern()?;
            return Ok(MatchPattern::Binding {
                id: self.alloc_id(),
                name: head,
                pattern: Box::new(pattern),
            });
        }
        if self.current.kind == TokenKind::ColonColon {
            self.advance();
            let variant = self.expect_ident()?;
            let binding = if self.current.kind == TokenKind::LParen {
                self.advance();
                let binding = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Some(binding)
            } else {
                None
            };
            return Ok(MatchPattern::Enum {
                id: self.alloc_id(),
                name: head,
                variant,
                binding,
            });
        }
        match head.name.as_str() {
            "ok" => {
                self.expect(TokenKind::LParen)?;
                let name = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(MatchPattern::ResultOk(name, self.alloc_id()))
            }
            "err" => {
                self.expect(TokenKind::LParen)?;
                let name = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(MatchPattern::ResultErr(name, self.alloc_id()))
            }
            "some" => {
                self.expect(TokenKind::LParen)?;
                let name = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(MatchPattern::OptionSome(name, self.alloc_id()))
            }
            "none" => Ok(MatchPattern::OptionNone(self.alloc_id())),
            _ => {
                let is_type_name = head
                    .name
                    .chars()
                    .next()
                    .map(|ch| ch.is_uppercase())
                    .unwrap_or(false);
                if is_type_name && self.current.kind == TokenKind::LBrace {
                    self.advance();
                    let mut fields = Vec::new();
                    if self.current.kind != TokenKind::RBrace {
                        loop {
                            let field_name = self.expect_ident()?;
                            let binding = if self.current.kind == TokenKind::Colon {
                                self.advance();
                                Some(self.expect_ident()?)
                            } else {
                                None
                            };
                            fields.push(StructPatternField {
                                name: field_name,
                                binding,
                                id: self.alloc_id(),
                            });
                            if self.current.kind != TokenKind::Comma {
                                break;
                            }
                            self.advance();
                            if self.current.kind == TokenKind::RBrace {
                                break;
                            }
                        }
                    }
                    self.expect(TokenKind::RBrace)?;
                    Ok(MatchPattern::Struct {
                        id: self.alloc_id(),
                        name: head,
                        fields,
                    })
                } else {
                    Ok(MatchPattern::Binding {
                        id: self.alloc_id(),
                        name: head,
                        pattern: Box::new(MatchPattern::Wildcard(self.alloc_id())),
                    })
                }
            }
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
            params.push(Param {
                id: self.alloc_id(),
                name,
                ty,
            });
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
        let qualifier = match self.current.kind {
            TokenKind::Const => {
                self.advance();
                Some(at_syntax::TypeQualifier::Const)
            }
            TokenKind::Mut => {
                self.advance();
                Some(at_syntax::TypeQualifier::Mut)
            }
            _ => None,
        };
        let ty = self.parse_type_union()?;
        if let Some(qualifier) = qualifier {
            Ok(TypeRef::Qualified {
                qualifier,
                ty: Box::new(ty),
            })
        } else {
            Ok(ty)
        }
    }

    fn parse_type_union(&mut self) -> Result<TypeRef, ParseError> {
        let ty = self.parse_type_intersection()?;
        let mut items = Vec::new();
        items.push(ty);
        while self.current.kind == TokenKind::Pipe {
            self.advance();
            items.push(self.parse_type_intersection()?);
        }
        if items.len() == 1 {
            Ok(items.remove(0))
        } else {
            Ok(TypeRef::Union { types: items })
        }
    }

    fn parse_type_intersection(&mut self) -> Result<TypeRef, ParseError> {
        let ty = self.parse_type_primary()?;
        let mut items = Vec::new();
        items.push(ty);
        while self.current.kind == TokenKind::Ampersand {
            self.advance();
            items.push(self.parse_type_primary()?);
        }
        if items.len() == 1 {
            Ok(items.remove(0))
        } else {
            Ok(TypeRef::Intersection { types: items })
        }
    }

    fn parse_type_primary(&mut self) -> Result<TypeRef, ParseError> {
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
                id: self.alloc_id(),
                params,
                return_ty: Box::new(return_ty),
            });
        }

        if self.current.kind == TokenKind::LParen {
            let tuple_span = self.current.span;
            self.advance();
            let mut items = Vec::new();
            if self.current.kind != TokenKind::RParen {
                loop {
                    items.push(self.parse_type_ref()?);
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
            return Ok(TypeRef::Tuple {
                tuple_span,
                id: self.alloc_id(),
                items,
            });
        }

        if self.current.kind == TokenKind::LBracket {
            let start_span = self.current.span;
            self.advance();
            let inner = self.parse_type_ref()?;
            if self.current.kind != TokenKind::RBracket {
                return Err(ParseError::UnexpectedToken {
                    expected: "]".to_string(),
                    found: self.current.kind.clone(),
                    span: self.current.span,
                });
            }
            let end_span = self.current.span;
            self.advance();
            let array_name = Ident {
                name: "array".to_string(),
                span: Span::new(start_span.start, end_span.end),
                id: self.alloc_id(),
            };
            let ty = TypeRef::Named {
                name: array_name,
                args: vec![inner],
            };
            return Ok(self.apply_type_postfix(ty));
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
                    if self.current.kind == TokenKind::Greater
                        || self.current.kind == TokenKind::Shr
                    {
                        break;
                    }
                }
            }
            self.consume_type_greater()?;
            items
        } else {
            Vec::new()
        };
        let ty = TypeRef::Named { name, args };
        Ok(self.apply_type_postfix(ty))
    }

    fn apply_type_postfix(&mut self, mut ty: TypeRef) -> TypeRef {
        loop {
            if self.current.kind == TokenKind::Question {
                let span = self.current.span;
                self.advance();
                let name = Ident {
                    name: "option".to_string(),
                    span,
                    id: self.alloc_id(),
                };
                ty = TypeRef::Named {
                    name,
                    args: vec![ty],
                };
                continue;
            }
            if self.current.kind == TokenKind::LBracket {
                let start_span = self.current.span;
                self.advance();
                if self.current.kind == TokenKind::RBracket {
                    let end_span = self.current.span;
                    self.advance();
                    let name = Ident {
                        name: "array".to_string(),
                        span: Span::new(start_span.start, end_span.end),
                        id: self.alloc_id(),
                    };
                    ty = TypeRef::Named {
                        name,
                        args: vec![ty],
                    };
                    continue;
                }
            }
            break;
        }
        ty
    }

    fn consume_type_greater(&mut self) -> Result<(), ParseError> {
        match &self.current.kind {
            TokenKind::Greater => {
                self.advance();
                Ok(())
            }
            TokenKind::Shr => {
                let span = self.current.span;
                let mid = span.start.saturating_add(1).min(span.end);
                self.pending_token = Some(Token {
                    kind: TokenKind::Greater,
                    span: Span::new(mid, span.end),
                });
                self.current = Token {
                    kind: TokenKind::Greater,
                    span: Span::new(span.start, mid),
                };
                self.advance();
                Ok(())
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "Greater".to_string(),
                found: self.current.kind.clone(),
                span: self.current.span,
            }),
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        match &self.current.kind {
            TokenKind::Ident(name) => {
                let ident = Ident {
                    name: name.clone(),
                    span: self.current.span,
                    id: self.alloc_id(),
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
        if let Some(token) = self.pending_token.take() {
            self.current = token;
            return;
        }
        loop {
            self.current = self.lexer.next_token();
            if self.current.kind.is_comment() {
                self.capture_comment();
                continue;
            }
            break;
        }
    }

    fn peek_kind(&self) -> TokenKind {
        let mut lexer = self.lexer.clone();
        let mut pending_token = self.pending_token.clone();
        loop {
            let token = if let Some(token) = pending_token.take() {
                token
            } else {
                lexer.next_token()
            };
            if token.kind.is_comment() {
                continue;
            }
            return token.kind;
        }
    }

    fn capture_comment(&mut self) {
        let span = self.current.span;
        let text = self.lexer.source[span.start..span.end].to_string();
        let id = self.alloc_id();
        self.comments.push(Comment { span, text, id });
    }

    fn question_starts_ternary(&self) -> bool {
        let mut lexer = self.lexer.clone();
        let mut pending_token = self.pending_token.clone();
        let mut depth_paren = 0usize;
        let mut depth_bracket = 0usize;
        let mut depth_brace = 0usize;

        let next_token = |lexer: &mut Lexer<'_>, pending: &mut Option<Token>| -> Token {
            if let Some(token) = pending.take() {
                return token;
            }
            loop {
                let token = lexer.next_token();
                if token.kind.is_comment() {
                    continue;
                }
                return token;
            }
        };

        let mut token = next_token(&mut lexer, &mut pending_token);
        loop {
            match token.kind {
                TokenKind::LParen => depth_paren += 1,
                TokenKind::RParen => {
                    if depth_paren > 0 {
                        depth_paren -= 1;
                    } else if depth_bracket == 0 && depth_brace == 0 {
                        return false;
                    }
                }
                TokenKind::LBracket => depth_bracket += 1,
                TokenKind::RBracket => {
                    if depth_bracket > 0 {
                        depth_bracket -= 1;
                    } else if depth_paren == 0 && depth_brace == 0 {
                        return false;
                    }
                }
                TokenKind::LBrace => depth_brace += 1,
                TokenKind::RBrace => {
                    if depth_brace > 0 {
                        depth_brace -= 1;
                    } else if depth_paren == 0 && depth_bracket == 0 {
                        return false;
                    }
                }
                TokenKind::Colon => {
                    if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 {
                        return true;
                    }
                }
                TokenKind::Semicolon | TokenKind::Comma | TokenKind::Eof => {
                    if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 {
                        return false;
                    }
                }
                _ => {}
            }
            token = next_token(&mut lexer, &mut pending_token);
        }
    }
}

#[derive(Clone)]
struct Lexer<'a> {
    source: &'a str,
    chars: std::str::Chars<'a>,
    index: usize,
    current: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        let mut chars = source.chars();
        let current = chars.next();
        Self {
            source,
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
                    span: Span::new(start, self.index),
                }
            }
            Some('-') => {
                self.bump();
                if self.current == Some('>') {
                    self.bump();
                    Token {
                        kind: TokenKind::Arrow,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Minus,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('*') => {
                self.bump();
                Token {
                    kind: TokenKind::Star,
                    span: Span::new(start, self.index),
                }
            }
            Some('/') => {
                self.bump();
                Token {
                    kind: TokenKind::Slash,
                    span: Span::new(start, self.index),
                }
            }
            Some('%') => {
                self.bump();
                Token {
                    kind: TokenKind::Percent,
                    span: Span::new(start, self.index),
                }
            }
            Some('&') => {
                self.bump();
                if self.current == Some('&') {
                    self.bump();
                    Token {
                        kind: TokenKind::AndAnd,
                        span: Span::new(start, self.index),
                    }
                } else if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::AmpersandEquals,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Ampersand,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('|') => {
                self.bump();
                if self.current == Some('|') {
                    self.bump();
                    Token {
                        kind: TokenKind::OrOr,
                        span: Span::new(start, self.index),
                    }
                } else if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::PipeEquals,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Pipe,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('^') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::CaretEquals,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Caret,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('!') => {
                self.bump();
                if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::BangEqual,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Bang,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('<') => {
                self.bump();
                if self.current == Some('<') {
                    self.bump();
                    if self.current == Some('=') {
                        self.bump();
                        Token {
                            kind: TokenKind::ShlEquals,
                            span: Span::new(start, self.index),
                        }
                    } else {
                        Token {
                            kind: TokenKind::Shl,
                            span: Span::new(start, self.index),
                        }
                    }
                } else if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::LessEqual,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Less,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('>') => {
                self.bump();
                if self.current == Some('>') {
                    self.bump();
                    if self.current == Some('=') {
                        self.bump();
                        Token {
                            kind: TokenKind::ShrEquals,
                            span: Span::new(start, self.index),
                        }
                    } else {
                        Token {
                            kind: TokenKind::Shr,
                            span: Span::new(start, self.index),
                        }
                    }
                } else if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::GreaterEqual,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Greater,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('.') => {
                self.bump();
                if self.current == Some('.') {
                    self.bump();
                    if self.current == Some('.') {
                        self.bump();
                        Token {
                            kind: TokenKind::DotDotDot,
                            span: Span::new(start, self.index),
                        }
                    } else if self.current == Some('=') {
                        self.bump();
                        Token {
                            kind: TokenKind::DotDotEquals,
                            span: Span::new(start, self.index),
                        }
                    } else {
                        Token {
                            kind: TokenKind::DotDot,
                            span: Span::new(start, self.index),
                        }
                    }
                } else {
                    Token {
                        kind: TokenKind::Dot,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some(':') => {
                self.bump();
                if self.current == Some(':') {
                    self.bump();
                    Token {
                        kind: TokenKind::ColonColon,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Colon,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('?') => {
                self.bump();
                if self.current == Some('.') {
                    self.bump();
                    Token {
                        kind: TokenKind::Dot,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Question,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('(') => {
                self.bump();
                Token {
                    kind: TokenKind::LParen,
                    span: Span::new(start, self.index),
                }
            }
            Some(')') => {
                self.bump();
                Token {
                    kind: TokenKind::RParen,
                    span: Span::new(start, self.index),
                }
            }
            Some('[') => {
                self.bump();
                Token {
                    kind: TokenKind::LBracket,
                    span: Span::new(start, self.index),
                }
            }
            Some(']') => {
                self.bump();
                Token {
                    kind: TokenKind::RBracket,
                    span: Span::new(start, self.index),
                }
            }
            Some('{') => {
                self.bump();
                Token {
                    kind: TokenKind::LBrace,
                    span: Span::new(start, self.index),
                }
            }
            Some('}') => {
                self.bump();
                Token {
                    kind: TokenKind::RBrace,
                    span: Span::new(start, self.index),
                }
            }
            Some(',') => {
                self.bump();
                Token {
                    kind: TokenKind::Comma,
                    span: Span::new(start, self.index),
                }
            }
            Some('=') => {
                self.bump();
                if self.current == Some('>') {
                    self.bump();
                    Token {
                        kind: TokenKind::FatArrow,
                        span: Span::new(start, self.index),
                    }
                } else if self.current == Some('=') {
                    self.bump();
                    Token {
                        kind: TokenKind::EqualEqual,
                        span: Span::new(start, self.index),
                    }
                } else {
                    Token {
                        kind: TokenKind::Equals,
                        span: Span::new(start, self.index),
                    }
                }
            }
            Some('@') => {
                self.bump();
                Token {
                    kind: TokenKind::At,
                    span: Span::new(start, self.index),
                }
            }
            Some(';') => {
                self.bump();
                Token {
                    kind: TokenKind::Semicolon,
                    span: Span::new(start, self.index),
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
                    span: Span::new(start, self.index),
                }
            }
            None => Token {
                kind: TokenKind::Eof,
                span: Span::new(start, self.index),
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
                span: Span::new(start, self.index),
            }
        } else {
            Token {
                kind: TokenKind::UnterminatedString,
                span: Span::new(start, self.index),
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
                span: Span::new(start, self.index),
            }
        } else {
            Token {
                kind: TokenKind::UnterminatedString,
                span: Span::new(start, self.index),
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
                span: Span::new(start, self.index),
            }
        } else {
            Token {
                kind: TokenKind::UnterminatedString,
                span: Span::new(start, self.index),
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
                if self.peek().is_some_and(|c| c.is_ascii_digit()) {
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
        let span = Span::new(start, self.index);
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
            "is" => TokenKind::Is,
            "try" => TokenKind::Try,
            "await" => TokenKind::Await,
            "async" => TokenKind::Async,
            "catch" => TokenKind::Catch,
            "finally" => TokenKind::Finally,
            "match" => TokenKind::Match,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "set" => TokenKind::Set,
            "struct" => TokenKind::Struct,
            "type" => TokenKind::Type,
            "enum" => TokenKind::Enum,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "throw" => TokenKind::Throw,
            "raise" => TokenKind::Raise,
            "defer" => TokenKind::Defer,
            "with" => TokenKind::With,
            "yield" => TokenKind::Yield,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "mut" => TokenKind::Mut,
            "pub" => TokenKind::Pub,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(value),
        };
        Token {
            kind,
            span: Span::new(start, self.index),
        }
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        loop {
            match self.current {
                Some(ch) if ch.is_whitespace() => {
                    self.bump();
                }
                Some('/') if self.peek() == Some('/') => {
                    let comment_start = self.index;
                    self.bump();
                    self.bump();
                    while let Some(ch) = self.current {
                        if ch == '\n' {
                            break;
                        }
                        self.bump();
                    }
                    return Some(Token {
                        kind: TokenKind::Comment,
                        span: Span::new(comment_start, self.index),
                    });
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
                            span: Span::new(comment_start, self.index),
                        });
                    }
                    return Some(Token {
                        kind: TokenKind::Comment,
                        span: Span::new(comment_start, self.index),
                    });
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

fn expr_span_start(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Int(_, span, _)
        | Expr::Float(_, span, _)
        | Expr::String(_, span, _)
        | Expr::Bool(_, span, _) => Some(span.start),
        Expr::Ident(ident) => Some(ident.span.start),
        Expr::Unary { op_span, .. } => Some(op_span.start),
        Expr::Binary { left, .. } => expr_span_start(left),
        Expr::Ternary { span, .. } => Some(span.start),
        Expr::ChainedComparison { span, .. } => Some(span.start),
        Expr::If { if_span, .. } => Some(if_span.start),
        Expr::Member { base, .. } => expr_span_start(base),
        Expr::Call { callee, .. } => expr_span_start(callee),
        Expr::Try(expr, _) => expr_span_start(expr),
        Expr::Await { await_span, .. } => Some(await_span.start),
        Expr::TryCatch { try_span, .. } => Some(try_span.start),
        Expr::Match { match_span, .. } => Some(match_span.start),
        Expr::Block { block_span, .. } => Some(block_span.start),
        Expr::Array { array_span, .. } => Some(array_span.start),
        Expr::ArraySpread { spread_span, .. } => Some(spread_span.start),
        Expr::Index { index_span, .. } => Some(index_span.start),
        Expr::Tuple { tuple_span, .. } => Some(tuple_span.start),
        Expr::Range { range_span, .. } => Some(range_span.start),
        Expr::InterpolatedString { span, .. } => Some(span.start),
        Expr::Closure { span, .. } => Some(span.start),
        Expr::StructLiteral { span, .. } => Some(span.start),
        Expr::EnumLiteral { span, .. } => Some(span.start),
        Expr::MapLiteral { span, .. } => Some(span.start),
        Expr::MapSpread { spread_span, .. } => Some(spread_span.start),
        Expr::As { span, .. } => Some(span.start),
        Expr::Is { span, .. } => Some(span.start),
        Expr::Group { span, .. } => Some(span.start),
    }
}

fn expr_span_end(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Int(_, span, _)
        | Expr::Float(_, span, _)
        | Expr::String(_, span, _)
        | Expr::Bool(_, span, _) => Some(span.end),
        Expr::Ident(ident) => Some(ident.span.end),
        Expr::Unary { op_span, .. } => Some(op_span.end),
        Expr::Binary { right, .. } => expr_span_end(right),
        Expr::Ternary { span, .. } => Some(span.end),
        Expr::ChainedComparison { span, .. } => Some(span.end),
        Expr::If { if_span, .. } => Some(if_span.end),
        Expr::Member { name, .. } => Some(name.span.end),
        Expr::Call { callee, args, .. } => args
            .last()
            .and_then(expr_span_end)
            .or_else(|| expr_span_end(callee)),
        Expr::Try(expr, _) => expr_span_end(expr),
        Expr::Await { await_span, .. } => Some(await_span.end),
        Expr::TryCatch { try_span, .. } => Some(try_span.end),
        Expr::Match { match_span, .. } => Some(match_span.end),
        Expr::Block { block_span, .. } => Some(block_span.end),
        Expr::Array { array_span, .. } => Some(array_span.end),
        Expr::ArraySpread { spread_span, .. } => Some(spread_span.end),
        Expr::Index { index_span, .. } => Some(index_span.end),
        Expr::Tuple { tuple_span, .. } => Some(tuple_span.end),
        Expr::Range { range_span, .. } => Some(range_span.end),
        Expr::InterpolatedString { span, .. } => Some(span.end),
        Expr::Closure { span, .. } => Some(span.end),
        Expr::StructLiteral { span, .. } => Some(span.end),
        Expr::EnumLiteral { span, .. } => Some(span.end),
        Expr::MapLiteral { span, .. } => Some(span.end),
        Expr::MapSpread { spread_span, .. } => Some(spread_span.end),
        Expr::As { span, .. } => Some(span.end),
        Expr::Is { span, .. } => Some(span.end),
        Expr::Group { span, .. } => Some(span.end),
    }
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

#[cfg(test)]
mod tests {
    use super::{parse_module, parse_module_with_errors};

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
    fn parses_async_fn_and_await() {
        let source = "async fn fetch() {\n    return await load();\n}\n";
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_with_and_yield() {
        let source = r#"
fn f() {
    with time = time.fixed("2026-01-01T00:00:00Z") {
        yield time.now();
    }
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_statement_keywords_inside_block_expressions() {
        let source = r#"
fn f() {
    let a = if true {
        defer print("cleanup");
        throw err("boom");
        0
    } else {
        0
    };
    let b = if true {
        with time = time.fixed("2026-01-01T00:00:00Z") {
            yield time.now();
        }
        1
    } else {
        1
    };
    return a + b;
}
"#;
        let result = parse_module(source);
        assert!(result.is_ok(), "{result:?}");
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
    fn rejects_async_tool_function_with_async_span() {
        let source = r#"
tool async fn greet(name: string) -> string {
    return name;
}
"#;
        let (_module, errors) = parse_module_with_errors(source);
        let err = errors
            .into_iter()
            .find(|err| matches!(err, super::ParseError::UnexpectedToken { expected, .. } if expected.contains("async tool fn is not supported")))
            .expect("expected async tool parse error");
        let super::ParseError::UnexpectedToken { span, .. } = err else {
            panic!("unexpected parse error");
        };
        let async_offset = source.find("async").expect("async keyword");
        assert_eq!(span.start, async_offset);
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
    fn parses_pub_functions() {
        let source = r#"
pub fn add(a: int, b: int) -> int {
    return a + b;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].is_pub);
    }

    #[test]
    fn parses_pub_async_functions() {
        let source = r#"
pub async fn fetch() -> int {
    return 1;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].is_pub);
        assert!(module.functions[0].is_async);
    }

    #[test]
    fn parses_mixed_pub_declarations() {
        let source = r#"
pub import "./lib.at" as lib;

pub fn call() {
    return lib.foo();
}
"#;
        let module = parse_module(source).expect("parse module");
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].is_pub);
        assert_eq!(module.stmts.len(), 1);
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
        assert!(!module.functions.is_empty());
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
    fn parses_const_stmt() {
        let source = r#"
const max_retries: int = 3;

fn f() {
    const label = "ok";
    let retries = max_retries;
    return label + retries;
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
    fn parses_map_literal_and_casts() {
        let source = r#"
fn f() {
    let grades = map { "taylor": 3, "casey": 5 };
    let taylor = grades["taylor"];
    let count = taylor as int;
    if taylor is int {
        print(count);
    }
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_type_sugar() {
        let source = r#"
fn f(a: [int], b: string[], c: int?) {
    let d: [string?] = ["ok"];
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_try_catch_and_union_types() {
        let source = r#"
fn f() {
    let value: int | string = 1;
    let result = try { ok(1) } catch { 0 } finally { print(value); };
    print(result);
}
"#;
        assert!(parse_module(source).is_ok());
    }

    #[test]
    fn parses_match_patterns() {
        let source = r#"
fn f(value: (int, bool)) {
    match value {
        (x, y) => print(x),
        t @ (a, b) => print(a),
        (_, _) => print(0),
    };
}
"#;
        let parsed = parse_module(source);
        assert!(parsed.is_ok(), "{}", parsed.unwrap_err());
    }

    #[test]
    fn parses_ternary_and_chained_comparison() {
        let source = r#"
fn f(a: int, b: int, c: int) {
    let x = a < b < c;
    let y = a ? b : c;
}
"#;
        let parsed = parse_module(source);
        assert!(parsed.is_ok(), "{}", parsed.unwrap_err());
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

    #[test]
    fn parse_module_collects_multiple_errors() {
        let source = r#"
fn f() {
    let x = ;
    let y = ;
    return 1;
}
"#;
        let (_module, errors) = parse_module_with_errors(source);
        assert!(
            errors.len() >= 2,
            "expected multiple errors, got {}",
            errors.len()
        );
    }

    #[test]
    fn error_recovery_skips_invalid_stmt_without_placeholder_nodes() {
        let source = r#"
fn f() {
    let broken = ;
    let ok = 1;
    ok;
}
"#;
        let (module, errors) = parse_module_with_errors(source);
        assert!(!errors.is_empty());
        assert_eq!(module.functions.len(), 1);
        let body = &module.functions[0].body;
        assert_eq!(body.len(), 2);
    }
}
