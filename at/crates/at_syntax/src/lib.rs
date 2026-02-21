#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRef {
    pub name: Ident,
    pub args: Vec<TypeRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64, Span),
    Float(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    Ident(Ident),
    Unary {
        op: UnaryOp,
        op_span: Span,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        op_span: Span,
        right: Box<Expr>,
    },
    If {
        if_span: Span,
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Member {
        base: Box<Expr>,
        name: Ident,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Try(Box<Expr>),
    Match {
        match_span: Span,
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Block {
        block_span: Span,
        stmts: Vec<Stmt>,
        tail: Option<Box<Expr>>,
    },
    Array {
        array_span: Span,
        items: Vec<Expr>,
    },
    Index {
        index_span: Span,
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Tuple {
        tuple_span: Span,
        items: Vec<Expr>,
    },
    Range {
        range_span: Span,
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },
    InterpolatedString {
        span: Span,
        parts: Vec<InterpPart>,
    },
    Closure {
        span: Span,
        params: Vec<Ident>,
        body: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Or,
    And,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpPart {
    String(String),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchPattern {
    ResultOk(Ident),
    ResultErr(Ident),
    OptionSome(Ident),
    OptionNone,
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Import {
        path: String,
        alias: Ident,
    },
    Let {
        name: Ident,
        ty: Option<TypeRef>,
        value: Expr,
    },
    Using {
        name: Ident,
        ty: Option<TypeRef>,
        value: Expr,
    },
    Set {
        name: Ident,
        value: Expr,
    },
    SetMember {
        base: Expr,
        field: Ident,
        value: Expr,
    },
    SetIndex {
        base: Expr,
        index: Expr,
        value: Expr,
    },
    While {
        while_span: Span,
        condition: Expr,
        body: Vec<Stmt>,
    },
    For {
        for_span: Span,
        item: Ident,
        iter: Expr,
        body: Vec<Stmt>,
    },
    Break {
        break_span: Span,
    },
    Continue {
        continue_span: Span,
    },
    Expr(Expr),
    Return(Option<Expr>),
    Block(Vec<Stmt>),
    Test {
        name: String,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<Param>,
    pub return_ty: Option<TypeRef>,
    pub needs: Vec<Ident>,
    pub body: Vec<Stmt>,
    pub is_tool: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Ident,
    pub ty: Option<TypeRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub functions: Vec<Function>,
    pub stmts: Vec<Stmt>,
}
