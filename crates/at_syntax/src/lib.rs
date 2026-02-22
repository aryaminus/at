#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub id: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct NodeId(pub u32);

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        let hash = (start as u64).wrapping_mul(11400714819323198485u64) ^ (end as u64);
        let id = (hash ^ (hash >> 32)) as u32;
        Self { start, end, id }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Ident {
    pub name: String,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TypeRef {
    Named {
        name: Ident,
        args: Vec<TypeRef>,
    },
    Union {
        types: Vec<TypeRef>,
    },
    Intersection {
        types: Vec<TypeRef>,
    },
    Tuple {
        tuple_span: Span,
        id: NodeId,
        items: Vec<TypeRef>,
    },
    Function {
        fn_span: Span,
        id: NodeId,
        params: Vec<TypeRef>,
        return_ty: Box<TypeRef>,
    },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Expr {
    Int(i64, Span, NodeId),
    Float(f64, Span, NodeId),
    String(String, Span, NodeId),
    Bool(bool, Span, NodeId),
    Ident(Ident),
    Unary {
        op: UnaryOp,
        op_span: Span,
        id: NodeId,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        op_span: Span,
        id: NodeId,
        right: Box<Expr>,
    },
    Ternary {
        span: Span,
        id: NodeId,
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    ChainedComparison {
        span: Span,
        id: NodeId,
        items: Vec<Expr>,
        ops: Vec<(BinaryOp, Span)>,
    },
    If {
        if_span: Span,
        id: NodeId,
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Member {
        base: Box<Expr>,
        name: Ident,
        id: NodeId,
    },
    Call {
        callee: Box<Expr>,
        id: NodeId,
        args: Vec<Expr>,
    },
    Try(Box<Expr>, NodeId),
    Match {
        match_span: Span,
        id: NodeId,
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    TryCatch {
        try_span: Span,
        id: NodeId,
        try_block: Box<Expr>,
        catch_block: Option<Box<Expr>>,
        finally_block: Option<Box<Expr>>,
    },
    Block {
        block_span: Span,
        id: NodeId,
        stmts: Vec<Stmt>,
        tail: Option<Box<Expr>>,
    },
    Array {
        array_span: Span,
        id: NodeId,
        items: Vec<Expr>,
    },
    Index {
        index_span: Span,
        id: NodeId,
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Tuple {
        tuple_span: Span,
        id: NodeId,
        items: Vec<Expr>,
    },
    Range {
        range_span: Span,
        id: NodeId,
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },
    InterpolatedString {
        span: Span,
        id: NodeId,
        parts: Vec<InterpPart>,
    },
    Closure {
        span: Span,
        id: NodeId,
        params: Vec<Ident>,
        body: Box<Expr>,
    },
    StructLiteral {
        span: Span,
        id: NodeId,
        name: Ident,
        fields: Vec<StructLiteralField>,
    },
    EnumLiteral {
        span: Span,
        id: NodeId,
        name: Ident,
        variant: Ident,
        payload: Option<Box<Expr>>,
    },
    MapLiteral {
        span: Span,
        id: NodeId,
        entries: Vec<(Expr, Expr)>,
    },
    As {
        expr: Box<Expr>,
        ty: TypeRef,
        span: Span,
        id: NodeId,
    },
    Is {
        expr: Box<Expr>,
        ty: TypeRef,
        span: Span,
        id: NodeId,
    },
    Group {
        span: Span,
        id: NodeId,
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct StructField {
    pub name: Ident,
    pub ty: TypeRef,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct StructLiteralField {
    pub name: Ident,
    pub value: Expr,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct EnumVariant {
    pub name: Ident,
    pub payload: Option<TypeRef>,
    pub id: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum BinaryOp {
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum InterpPart {
    String(String, NodeId),
    Expr(Expr, NodeId),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MatchArm {
    pub id: NodeId,
    pub pattern: MatchPattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MatchPattern {
    Int(i64, NodeId),
    Bool(bool, NodeId),
    String(String, NodeId),
    ResultOk(Ident, NodeId),
    ResultErr(Ident, NodeId),
    OptionSome(Ident, NodeId),
    OptionNone(NodeId),
    Tuple {
        id: NodeId,
        items: Vec<MatchPattern>,
    },
    Struct {
        id: NodeId,
        name: Ident,
        fields: Vec<StructPatternField>,
    },
    Enum {
        id: NodeId,
        name: Ident,
        variant: Ident,
        binding: Option<Ident>,
    },
    Binding {
        id: NodeId,
        name: Ident,
        pattern: Box<MatchPattern>,
    },
    Wildcard(NodeId),
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct StructPatternField {
    pub name: Ident,
    pub binding: Option<Ident>,
    pub id: NodeId,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Stmt {
    Import {
        id: NodeId,
        path: String,
        alias: Ident,
        is_pub: bool,
    },
    TypeAlias {
        id: NodeId,
        name: Ident,
        ty: TypeRef,
        is_pub: bool,
    },
    Enum {
        id: NodeId,
        name: Ident,
        type_params: Vec<Ident>,
        variants: Vec<EnumVariant>,
        is_pub: bool,
    },
    Struct {
        id: NodeId,
        name: Ident,
        type_params: Vec<Ident>,
        fields: Vec<StructField>,
        is_pub: bool,
    },
    Const {
        id: NodeId,
        name: Ident,
        ty: Option<TypeRef>,
        value: Expr,
    },
    Let {
        id: NodeId,
        name: Ident,
        ty: Option<TypeRef>,
        value: Expr,
    },
    Using {
        id: NodeId,
        name: Ident,
        ty: Option<TypeRef>,
        value: Expr,
    },
    Set {
        id: NodeId,
        name: Ident,
        value: Expr,
    },
    SetMember {
        id: NodeId,
        base: Expr,
        field: Ident,
        value: Expr,
    },
    SetIndex {
        id: NodeId,
        base: Expr,
        index: Expr,
        value: Expr,
    },
    While {
        id: NodeId,
        while_span: Span,
        condition: Expr,
        body: Vec<Stmt>,
    },
    If {
        id: NodeId,
        if_span: Span,
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    For {
        id: NodeId,
        for_span: Span,
        item: Ident,
        iter: Expr,
        body: Vec<Stmt>,
    },
    Break {
        id: NodeId,
        break_span: Span,
    },
    Continue {
        id: NodeId,
        continue_span: Span,
    },
    Expr {
        id: NodeId,
        expr: Expr,
    },
    Return {
        id: NodeId,
        expr: Option<Expr>,
    },
    Block {
        id: NodeId,
        stmts: Vec<Stmt>,
    },
    Test {
        id: NodeId,
        name: String,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Function {
    pub id: NodeId,
    pub name: Ident,
    pub is_pub: bool,
    pub type_params: Vec<Ident>,
    pub params: Vec<Param>,
    pub return_ty: Option<TypeRef>,
    pub needs: Vec<Ident>,
    pub body: Vec<Stmt>,
    pub is_tool: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Param {
    pub id: NodeId,
    pub name: Ident,
    pub ty: Option<TypeRef>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Module {
    pub id: NodeId,
    pub functions: Vec<Function>,
    pub stmts: Vec<Stmt>,
    pub comments: Vec<Comment>,
    pub source_path: Option<String>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Comment {
    pub span: Span,
    pub text: String,
    pub id: NodeId,
}

pub trait AstVisitor {
    fn visit_module(&mut self, module: &Module) {
        walk_module(self, module);
    }
    fn visit_function(&mut self, function: &Function) {
        walk_function(self, function);
    }
    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }
    fn visit_type_ref(&mut self, ty: &TypeRef) {
        walk_type_ref(self, ty);
    }
    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        walk_match_pattern(self, pattern);
    }
}

pub fn walk_module<V: AstVisitor + ?Sized>(visitor: &mut V, module: &Module) {
    for function in &module.functions {
        visitor.visit_function(function);
    }
    for stmt in &module.stmts {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_function<V: AstVisitor + ?Sized>(visitor: &mut V, function: &Function) {
    for ty in function.params.iter().filter_map(|param| param.ty.as_ref()) {
        visitor.visit_type_ref(ty);
    }
    if let Some(ty) = &function.return_ty {
        visitor.visit_type_ref(ty);
    }
    for stmt in &function.body {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_stmt<V: AstVisitor + ?Sized>(visitor: &mut V, stmt: &Stmt) {
    match stmt {
        Stmt::Import { .. } => {}
        Stmt::TypeAlias { ty, .. } => visitor.visit_type_ref(ty),
        Stmt::Enum { variants, .. } => {
            for variant in variants {
                if let Some(payload) = &variant.payload {
                    visitor.visit_type_ref(payload);
                }
            }
        }
        Stmt::Struct { fields, .. } => {
            for field in fields {
                visitor.visit_type_ref(&field.ty);
            }
        }
        Stmt::Const { ty, value, .. }
        | Stmt::Let { ty, value, .. }
        | Stmt::Using { ty, value, .. } => {
            if let Some(ty) = ty {
                visitor.visit_type_ref(ty);
            }
            visitor.visit_expr(value);
        }
        Stmt::Set { value, .. } => visitor.visit_expr(value),
        Stmt::SetMember { base, value, .. } => {
            visitor.visit_expr(base);
            visitor.visit_expr(value);
        }
        Stmt::SetIndex {
            base, index, value, ..
        } => {
            visitor.visit_expr(base);
            visitor.visit_expr(index);
            visitor.visit_expr(value);
        }
        Stmt::While {
            condition, body, ..
        } => {
            visitor.visit_expr(condition);
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            visitor.visit_expr(condition);
            for stmt in then_branch {
                visitor.visit_stmt(stmt);
            }
            if let Some(branch) = else_branch {
                for stmt in branch {
                    visitor.visit_stmt(stmt);
                }
            }
        }
        Stmt::For { iter, body, .. } => {
            visitor.visit_expr(iter);
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Expr { expr, .. } => visitor.visit_expr(expr),
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                visitor.visit_expr(expr);
            }
        }
        Stmt::Block { stmts, .. } => {
            for stmt in stmts {
                visitor.visit_stmt(stmt);
            }
        }
        Stmt::Test { body, .. } => {
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
    }
}

pub fn walk_expr<V: AstVisitor + ?Sized>(visitor: &mut V, expr: &Expr) {
    match expr {
        Expr::Int(..) | Expr::Float(..) | Expr::String(..) | Expr::Bool(..) | Expr::Ident(..) => {}
        Expr::Unary { expr, .. } => visitor.visit_expr(expr),
        Expr::Binary { left, right, .. } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(then_branch);
            visitor.visit_expr(else_branch);
        }
        Expr::ChainedComparison { items, .. } => {
            for item in items {
                visitor.visit_expr(item);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(then_branch);
            if let Some(branch) = else_branch {
                visitor.visit_expr(branch);
            }
        }
        Expr::Member { base, .. } => visitor.visit_expr(base),
        Expr::Call { callee, args, .. } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        Expr::Try(expr, _) => visitor.visit_expr(expr),
        Expr::Match { value, arms, .. } => {
            visitor.visit_expr(value);
            for arm in arms {
                visitor.visit_match_pattern(&arm.pattern);
                if let Some(guard) = &arm.guard {
                    visitor.visit_expr(guard);
                }
                visitor.visit_expr(&arm.body);
            }
        }
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            visitor.visit_expr(try_block);
            if let Some(block) = catch_block {
                visitor.visit_expr(block);
            }
            if let Some(block) = finally_block {
                visitor.visit_expr(block);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            for stmt in stmts {
                visitor.visit_stmt(stmt);
            }
            if let Some(expr) = tail {
                visitor.visit_expr(expr);
            }
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                visitor.visit_expr(item);
            }
        }
        Expr::Index { base, index, .. } => {
            visitor.visit_expr(base);
            visitor.visit_expr(index);
        }
        Expr::Range { start, end, .. } => {
            visitor.visit_expr(start);
            visitor.visit_expr(end);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
                    visitor.visit_expr(expr);
                }
            }
        }
        Expr::Closure { body, .. } => visitor.visit_expr(body),
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                visitor.visit_expr(&field.value);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                visitor.visit_expr(expr);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                visitor.visit_expr(key);
                visitor.visit_expr(value);
            }
        }
        Expr::As { expr, ty, .. } | Expr::Is { expr, ty, .. } => {
            visitor.visit_expr(expr);
            visitor.visit_type_ref(ty);
        }
        Expr::Group { expr, .. } => visitor.visit_expr(expr),
    }
}

pub fn walk_type_ref<V: AstVisitor + ?Sized>(visitor: &mut V, ty: &TypeRef) {
    match ty {
        TypeRef::Named { args, .. } => {
            for arg in args {
                visitor.visit_type_ref(arg);
            }
        }
        TypeRef::Union { types } | TypeRef::Intersection { types } => {
            for ty in types {
                visitor.visit_type_ref(ty);
            }
        }
        TypeRef::Tuple { items, .. } => {
            for item in items {
                visitor.visit_type_ref(item);
            }
        }
        TypeRef::Function {
            params, return_ty, ..
        } => {
            for param in params {
                visitor.visit_type_ref(param);
            }
            visitor.visit_type_ref(return_ty);
        }
    }
}

pub fn walk_match_pattern<V: AstVisitor + ?Sized>(visitor: &mut V, pattern: &MatchPattern) {
    match pattern {
        MatchPattern::Int(..)
        | MatchPattern::Bool(..)
        | MatchPattern::String(..)
        | MatchPattern::ResultOk(..)
        | MatchPattern::ResultErr(..)
        | MatchPattern::OptionSome(..)
        | MatchPattern::OptionNone(..)
        | MatchPattern::Wildcard(..) => {}
        MatchPattern::Tuple { items, .. } => {
            for item in items {
                visitor.visit_match_pattern(item);
            }
        }
        MatchPattern::Struct { .. } => {}
        MatchPattern::Enum { .. } => {}
        MatchPattern::Binding { pattern, .. } => visitor.visit_match_pattern(pattern),
    }
}
