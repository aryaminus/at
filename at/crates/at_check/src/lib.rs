use std::collections::{HashMap, HashSet};

use at_syntax::{
    BinaryOp, Expr, Function, Ident, Module, Span, Stmt, StructField, TypeRef, UnaryOp,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum SimpleType {
    Int,
    Float,
    Bool,
    String,
    Array,
    Option,
    Result,
    Unit,
    Function(usize),
    Custom(String),
    Unknown,
}

#[derive(Debug, Clone)]
struct FuncSig {
    params: Vec<SimpleType>,
    return_ty: SimpleType,
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Option<Span>,
}

pub fn typecheck_module(module: &Module) -> Result<(), Vec<TypeError>> {
    let mut checker = TypeChecker::new();
    checker.load_functions(module);
    checker.check_module(module);
    if checker.errors.is_empty() {
        Ok(())
    } else {
        Err(checker.errors)
    }
}

pub fn infer_function_returns(module: &Module) -> HashMap<String, String> {
    let mut checker = TypeChecker::new();
    checker.load_functions(module);
    for func in &module.functions {
        checker.check_function(func);
        if func.return_ty.is_none() {
            if let Some(inferred) = checker.inferred_return_string() {
                checker
                    .inferred_returns
                    .insert(func.name.name.clone(), inferred);
            }
        }
    }
    checker.inferred_returns
}

struct TypeChecker {
    functions: HashMap<String, FuncSig>,
    function_needs: HashMap<String, Vec<String>>,
    structs: HashMap<String, Vec<StructField>>,
    type_aliases: HashMap<String, TypeRef>,
    locals: Vec<HashMap<String, SimpleType>>,
    option_inner: Vec<HashMap<String, SimpleType>>,
    result_ok: Vec<HashMap<String, SimpleType>>,
    result_err: Vec<HashMap<String, SimpleType>>,
    capabilities: Vec<HashSet<String>>,
    current_return: SimpleType,
    current_return_ref: Option<TypeRef>,
    return_option_inner: Option<SimpleType>,
    return_result_ok: Option<SimpleType>,
    return_result_err: Option<SimpleType>,
    inferred_returns: HashMap<String, String>,
    errors: Vec<TypeError>,
    last_option_inner: Option<SimpleType>,
    last_result_ok: Option<SimpleType>,
    last_result_err: Option<SimpleType>,
    loop_depth: usize,
}

impl TypeChecker {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            function_needs: HashMap::new(),
            structs: HashMap::new(),
            type_aliases: HashMap::new(),
            locals: Vec::new(),
            option_inner: Vec::new(),
            result_ok: Vec::new(),
            result_err: Vec::new(),
            capabilities: Vec::new(),
            current_return: SimpleType::Unknown,
            current_return_ref: None,
            return_option_inner: None,
            return_result_ok: None,
            return_result_err: None,
            inferred_returns: HashMap::new(),
            errors: Vec::new(),
            last_option_inner: None,
            last_result_ok: None,
            last_result_err: None,
            loop_depth: 0,
        }
    }

    fn load_functions(&mut self, module: &Module) {
        for func in &module.functions {
            if self.functions.contains_key(&func.name.name) {
                self.push_error(
                    format!("duplicate function: {}", func.name.name),
                    Some(func.name.span),
                );
                continue;
            }
            self.function_needs.insert(
                func.name.name.clone(),
                func.needs.iter().map(|ident| ident.name.clone()).collect(),
            );
            let params = func
                .params
                .iter()
                .map(|param| {
                    param
                        .ty
                        .as_ref()
                        .map(|ty| self.type_from_ref(ty))
                        .unwrap_or(SimpleType::Unknown)
                })
                .collect();
            let return_ty = func
                .return_ty
                .as_ref()
                .map(|ty| self.type_from_ref(ty))
                .unwrap_or(SimpleType::Unknown);
            self.functions
                .insert(func.name.name.clone(), FuncSig { params, return_ty });
        }
    }

    fn check_module(&mut self, module: &Module) {
        self.check_duplicate_import_aliases(module);
        self.load_type_aliases(module);
        self.load_structs(module);
        for func in &module.functions {
            self.check_function(func);
        }

        self.locals.clear();
        self.capabilities.clear();
        self.push_scope();
        for stmt in &module.stmts {
            self.check_stmt(stmt);
        }
        self.pop_scope();
    }

    fn check_function(&mut self, func: &Function) {
        self.locals.clear();
        self.option_inner.clear();
        self.result_ok.clear();
        self.result_err.clear();
        self.capabilities.clear();
        self.push_scope();
        for need in &func.needs {
            self.insert_capability(&need.name);
        }
        for param in &func.params {
            if self.is_local_in_current_scope(&param.name.name) {
                self.push_error(
                    format!("duplicate local: {}", param.name.name),
                    Some(param.name.span),
                );
            }
            let ty = param
                .ty
                .as_ref()
                .map(|ty| self.type_from_ref(ty))
                .unwrap_or(SimpleType::Unknown);
            self.bind_local(&param.name, ty);
        }
        self.current_return = func
            .return_ty
            .as_ref()
            .map(|ty| self.type_from_ref(ty))
            .unwrap_or(SimpleType::Unknown);
        self.current_return_ref = func.return_ty.clone();
        self.return_option_inner = None;
        self.return_result_ok = None;
        self.return_result_err = None;
        for stmt in &func.body {
            self.check_stmt(stmt);
        }
        // Check for missing return statement
        if self.current_return != SimpleType::Unit && self.current_return != SimpleType::Unknown {
            let has_return = func.body.iter().any(|stmt| matches!(stmt, Stmt::Return(_)));
            if !has_return {
                self.push_error(
                    format!(
                        "function '{}' declares return type '{}' but has no return statement",
                        func.name.name,
                        format_type(&self.current_return)
                    ),
                    Some(func.name.span),
                );
            }
        }
        self.pop_scope();
    }

    fn check_duplicate_import_aliases(&mut self, module: &Module) {
        let mut seen = HashMap::new();
        for stmt in &module.stmts {
            if let Stmt::Import { alias, .. } = stmt {
                if seen.insert(alias.name.clone(), alias.span).is_some() {
                    self.push_error(
                        format!("duplicate import alias: {}", alias.name),
                        Some(alias.span),
                    );
                }
            }
        }
    }

    fn load_structs(&mut self, module: &Module) {
        self.structs.clear();
        for stmt in &module.stmts {
            if let Stmt::Struct { name, fields } = stmt {
                if self.structs.contains_key(&name.name) {
                    self.push_error(format!("duplicate struct: {}", name.name), Some(name.span));
                    continue;
                }
                let mut seen = HashSet::new();
                for field in fields {
                    if !seen.insert(field.name.name.clone()) {
                        self.push_error(
                            format!("duplicate struct field: {}", field.name.name),
                            Some(field.name.span),
                        );
                    }
                }
                self.structs.insert(name.name.clone(), fields.clone());
            }
        }
    }

    fn load_type_aliases(&mut self, module: &Module) {
        self.type_aliases.clear();
        for stmt in &module.stmts {
            if let Stmt::TypeAlias { name, ty } = stmt {
                if self.type_aliases.contains_key(&name.name) {
                    self.push_error(
                        format!("duplicate type alias: {}", name.name),
                        Some(name.span),
                    );
                    continue;
                }
                self.type_aliases.insert(name.name.clone(), ty.clone());
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Import { .. } => {}
            Stmt::TypeAlias { .. } => {}
            Stmt::Struct { .. } => {}
            Stmt::Let { name, ty, value } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                if self.is_local_in_current_scope(&name.name) {
                    self.push_error(format!("duplicate local: {}", name.name), Some(name.span));
                }
                let value_ty = self.check_expr(value);
                let declared = ty.as_ref().map(|ty| self.type_from_ref(ty));
                if let Some(expected) = declared.clone() {
                    self.check_compatible(
                        &expected,
                        &value_ty,
                        &format!("type mismatch for {}", name.name),
                        Some(name.span),
                    );
                    self.bind_local(name, expected);
                } else {
                    self.bind_or_refine_local(name, value_ty.clone());
                    self.infer_inner_from_expr(value, &value_ty);
                    self.bind_inner_types(name, &value_ty);
                }
            }
            Stmt::Using { name, ty, value } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                if self.is_local_in_current_scope(&name.name) {
                    self.push_error(format!("duplicate local: {}", name.name), Some(name.span));
                }
                let value_ty = self.check_expr(value);
                let declared = ty.as_ref().map(|ty| self.type_from_ref(ty));
                if let Some(expected) = declared.clone() {
                    self.check_compatible(
                        &expected,
                        &value_ty,
                        &format!("type mismatch for {}", name.name),
                        Some(name.span),
                    );
                    self.bind_local(name, expected);
                } else {
                    self.bind_or_refine_local(name, value_ty.clone());
                    self.infer_inner_from_expr(value, &value_ty);
                    self.bind_inner_types(name, &value_ty);
                }
                self.insert_capability(&name.name);
            }
            Stmt::Set { name, value } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                let existing = self.resolve_local(name);
                if existing.is_none() {
                    self.push_error(
                        format!("unknown identifier: {}", name.name),
                        Some(name.span),
                    );
                }
                let value_ty = self.check_expr(value);
                if let Some(expected) = existing {
                    if self.check_compatible(
                        &expected,
                        &value_ty,
                        &format!("type mismatch for {}", name.name),
                        Some(name.span),
                    ) {
                        // Only rebind on successful type check
                        self.merge_assigned_inners(name, &expected, name.span);
                        self.infer_inner_from_expr(value, &value_ty);
                        self.bind_inner_types_for_assignment(name, &value_ty);
                    }
                }
            }
            Stmt::SetMember { base, value, .. } => {
                let _ = self.check_expr(base);
                self.check_expr(value);
            }
            Stmt::SetIndex { base, index, value } => {
                let _ = self.check_expr(base);
                let _ = self.check_expr(index);
                self.check_expr(value);
            }
            Stmt::While {
                while_span,
                condition,
                body,
            } => {
                let cond_ty = self.check_expr(condition);
                if cond_ty != SimpleType::Bool
                    && cond_ty != SimpleType::Int
                    && cond_ty != SimpleType::Unknown
                {
                    self.push_error(
                        format!("while expects bool or int, got {}", format_type(&cond_ty)),
                        Some(*while_span),
                    );
                }
                self.loop_depth += 1;
                self.push_scope();
                for stmt in body {
                    self.check_stmt(stmt);
                }
                self.pop_scope();
                self.loop_depth -= 1;
            }
            Stmt::For {
                for_span,
                item,
                iter,
                body,
            } => {
                let iter_ty = self.check_expr(iter);
                if iter_ty != SimpleType::Array && iter_ty != SimpleType::Unknown {
                    self.push_error(
                        format!("for expects array, got {}", format_type(&iter_ty)),
                        Some(*for_span),
                    );
                }
                self.loop_depth += 1;
                self.push_scope();
                self.bind_local(item, SimpleType::Unknown);
                for stmt in body {
                    self.check_stmt(stmt);
                }
                self.pop_scope();
                self.loop_depth -= 1;
            }
            Stmt::Break { break_span } => {
                if self.loop_depth == 0 {
                    self.push_error("break used outside of loop".to_string(), Some(*break_span));
                }
            }
            Stmt::Continue { continue_span } => {
                if self.loop_depth == 0 {
                    self.push_error(
                        "continue used outside of loop".to_string(),
                        Some(*continue_span),
                    );
                }
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr);
            }
            Stmt::Return(expr) => match (expr, self.current_return.clone()) {
                (None, SimpleType::Unit) => {}
                (None, SimpleType::Unknown) => {
                    self.current_return = SimpleType::Unit;
                }
                (None, expected) => {
                    self.push_error(
                        format!(
                            "return type mismatch: expected {} but got unit",
                            format_type(&expected)
                        ),
                        None,
                    );
                }
                (Some(expr), SimpleType::Unknown) => {
                    self.last_result_ok = None;
                    self.last_result_err = None;
                    self.last_option_inner = None;
                    let found = self.check_expr(expr);
                    if matches!(found, SimpleType::Result) {
                        self.current_return = SimpleType::Result;
                        self.update_return_result_inners(expr_span(expr));
                        self.update_return_from_ident(expr, expr_span(expr));
                    } else if matches!(found, SimpleType::Option) {
                        self.current_return = SimpleType::Option;
                        self.update_return_option_inner(expr_span(expr));
                        self.update_return_from_ident(expr, expr_span(expr));
                    } else if found != SimpleType::Unknown {
                        self.current_return = found;
                    }
                }
                (Some(expr), expected) => {
                    self.last_result_ok = None;
                    self.last_result_err = None;
                    self.last_option_inner = None;
                    let found = self.check_expr(expr);
                    self.check_compatible(
                        &expected,
                        &found,
                        "return type mismatch",
                        expr_span(expr),
                    );
                    if matches!(expected, SimpleType::Result) {
                        self.update_return_result_inners(expr_span(expr));
                        self.update_return_from_ident(expr, expr_span(expr));
                    }
                    if matches!(expected, SimpleType::Option) {
                        self.update_return_option_inner(expr_span(expr));
                        self.update_return_from_ident(expr, expr_span(expr));
                    }
                }
            },
            Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
                self.push_scope();
                for stmt in stmts {
                    self.check_stmt(stmt);
                }
                self.pop_scope();
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> SimpleType {
        match expr {
            Expr::Int(_, _) => SimpleType::Int,
            Expr::Float(_, _) => SimpleType::Float,
            Expr::String(_, _) => SimpleType::String,
            Expr::Bool(_, _) => SimpleType::Bool,
            Expr::Ident(ident) => {
                let ty = if let Some(ty) = self.resolve_local(ident) {
                    ty
                } else if self.has_capability(&ident.name) {
                    SimpleType::Unknown
                } else {
                    self.push_error(
                        format!("unknown identifier: {}", ident.name),
                        Some(ident.span),
                    );
                    SimpleType::Unknown
                };
                if matches!(ty, SimpleType::Option) {
                    self.last_option_inner = self.lookup_option_inner(&ident.name);
                }
                if matches!(ty, SimpleType::Result) {
                    self.last_result_ok = self.lookup_result_ok(&ident.name);
                    self.last_result_err = self.lookup_result_err(&ident.name);
                }
                ty
            }
            Expr::Binary {
                left,
                op,
                op_span,
                right,
            } => self.check_binary(left, *op, *op_span, right),
            Expr::Unary { op, op_span, expr } => self.check_unary(*op, *op_span, expr),
            Expr::If {
                if_span,
                condition,
                then_branch,
                else_branch,
            } => self.check_if(*if_span, condition, then_branch, else_branch.as_deref()),
            Expr::Member { base, name } => {
                let base_ty = self.check_expr(base);
                if let SimpleType::Custom(struct_name) = base_ty {
                    if let Some(fields) = self.structs.get(&struct_name) {
                        let field_ty = fields
                            .iter()
                            .find(|field| field.name.name == name.name)
                            .map(|field| field.ty.clone());
                        if let Some(field_ty) = field_ty {
                            return self.type_from_ref(&field_ty);
                        }
                        self.push_error(
                            format!("unknown field {} on struct {}", name.name, struct_name),
                            Some(name.span),
                        );
                        return SimpleType::Unknown;
                    }
                }
                SimpleType::Unknown
            }
            Expr::Call { callee, args } => {
                let result = self.check_call(callee, args);
                result
            }
            Expr::Try(expr) => {
                let inner = self.check_expr(expr);
                if inner != SimpleType::Result && inner != SimpleType::Unknown {
                    self.push_error(
                        format!("? expects result, got {}", format_type(&inner)),
                        expr_span(expr),
                    );
                    SimpleType::Unknown
                } else {
                    self.last_result_ok.clone().unwrap_or(SimpleType::Unknown)
                }
            }
            Expr::Match {
                match_span,
                value,
                arms,
            } => self.check_match(*match_span, value, arms),
            Expr::Block { stmts, tail, .. } => {
                self.push_scope();
                for stmt in stmts {
                    self.check_stmt(stmt);
                }
                let ty = if let Some(expr) = tail {
                    self.check_expr(expr)
                } else {
                    SimpleType::Unit
                };
                self.pop_scope();
                ty
            }
            Expr::Array { items, .. } => {
                let mut array_ty = SimpleType::Unknown;
                for item in items {
                    let item_ty = self.check_expr(item);
                    if array_ty == SimpleType::Unknown {
                        array_ty = item_ty;
                    } else if item_ty != SimpleType::Unknown && item_ty != array_ty {
                        self.push_error(
                            format!(
                                "array element type mismatch: expected {}, got {}",
                                format_type(&array_ty),
                                format_type(&item_ty)
                            ),
                            expr_span(item),
                        );
                    }
                }
                SimpleType::Array
            }
            Expr::Index { base, index, .. } => {
                let base_ty = self.check_expr(base);
                if base_ty != SimpleType::Array && base_ty != SimpleType::Unknown {
                    self.push_error(
                        format!("index expects array, got {}", format_type(&base_ty)),
                        expr_span(base),
                    );
                }
                let index_ty = self.check_expr(index);
                if index_ty != SimpleType::Int && index_ty != SimpleType::Unknown {
                    self.push_error(
                        format!("index expects int, got {}", format_type(&index_ty)),
                        expr_span(index),
                    );
                }
                SimpleType::Unknown
            }
            Expr::Tuple { items, .. } => {
                for item in items {
                    self.check_expr(item);
                }
                SimpleType::Unknown
            }
            Expr::Range { start, end, .. } => {
                self.check_expr(start);
                self.check_expr(end);
                SimpleType::Array
            }
            Expr::InterpolatedString { parts, .. } => {
                for part in parts {
                    if let at_syntax::InterpPart::Expr(expr) = part {
                        self.check_expr(expr);
                    }
                }
                SimpleType::String
            }
            Expr::Closure { params, body, .. } => {
                self.push_scope();
                for param in params {
                    self.bind_local(param, SimpleType::Unknown);
                }
                self.check_expr(body);
                self.pop_scope();
                SimpleType::Function(params.len())
            }
            Expr::StructLiteral { name, fields, .. } => {
                let struct_fields = self.structs.get(&name.name).cloned();
                if struct_fields.is_none() {
                    self.push_error(format!("unknown struct: {}", name.name), Some(name.span));
                }
                let mut provided = HashSet::new();
                for field in fields {
                    if !provided.insert(field.name.name.clone()) {
                        self.push_error(
                            format!("duplicate field: {}", field.name.name),
                            Some(field.name.span),
                        );
                    }
                    let value_ty = self.check_expr(&field.value);
                    if let Some(struct_fields) = struct_fields.as_ref() {
                        if let Some(expected) = struct_fields
                            .iter()
                            .find(|entry| entry.name.name == field.name.name)
                        {
                            let expected_ty = self.type_from_ref(&expected.ty);
                            self.check_compatible(
                                &expected_ty,
                                &value_ty,
                                &format!("type mismatch for field {}", field.name.name),
                                Some(field.name.span),
                            );
                        } else {
                            self.push_error(
                                format!(
                                    "unknown field {} on struct {}",
                                    field.name.name, name.name
                                ),
                                Some(field.name.span),
                            );
                        }
                    }
                }
                if let Some(struct_fields) = struct_fields.as_ref() {
                    for field in struct_fields {
                        if !provided.contains(&field.name.name) {
                            self.push_error(
                                format!(
                                    "missing field {} for struct {}",
                                    field.name.name, name.name
                                ),
                                Some(name.span),
                            );
                        }
                    }
                }
                SimpleType::Custom(name.name.clone())
            }
        }
    }

    fn check_binary(
        &mut self,
        left: &Expr,
        op: BinaryOp,
        op_span: Span,
        right: &Expr,
    ) -> SimpleType {
        let left_ty = self.check_expr(left);
        let right_ty = self.check_expr(right);
        match op {
            BinaryOp::Add => {
                if left_ty == SimpleType::String && right_ty == SimpleType::String {
                    SimpleType::String
                } else if left_ty == SimpleType::Int && right_ty == SimpleType::Int {
                    SimpleType::Int
                } else if left_ty == SimpleType::Float && right_ty == SimpleType::Float {
                    SimpleType::Float
                } else if left_ty == SimpleType::Unknown || right_ty == SimpleType::Unknown {
                    SimpleType::Unknown
                } else {
                    self.push_error(
                        format!(
                            "operator expects int, float, or string, got {} and {}",
                            format_type(&left_ty),
                            format_type(&right_ty)
                        ),
                        Some(op_span),
                    );
                    SimpleType::Unknown
                }
            }
            BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                if left_ty == SimpleType::Int && right_ty == SimpleType::Int {
                    SimpleType::Int
                } else if left_ty == SimpleType::Float && right_ty == SimpleType::Float {
                    SimpleType::Float
                } else if left_ty == SimpleType::Unknown || right_ty == SimpleType::Unknown {
                    if left_ty == SimpleType::Float || right_ty == SimpleType::Float {
                        SimpleType::Float
                    } else {
                        SimpleType::Int
                    }
                } else {
                    self.push_error(
                        format!(
                            "operator expects int or float, got {} and {}",
                            format_type(&left_ty),
                            format_type(&right_ty)
                        ),
                        Some(op_span),
                    );
                    SimpleType::Unknown
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                let left_ok = left_ty == SimpleType::Bool
                    || left_ty == SimpleType::Int
                    || left_ty == SimpleType::Unknown;
                let right_ok = right_ty == SimpleType::Bool
                    || right_ty == SimpleType::Int
                    || right_ty == SimpleType::Unknown;
                if !left_ok {
                    self.push_error(
                        format!(
                            "operator expects bool or int, got {}",
                            format_type(&left_ty)
                        ),
                        Some(op_span),
                    );
                }
                if !right_ok {
                    self.push_error(
                        format!(
                            "operator expects bool or int, got {}",
                            format_type(&right_ty)
                        ),
                        Some(op_span),
                    );
                }
                SimpleType::Bool
            }
            BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                if (left_ty == SimpleType::Int && right_ty == SimpleType::Int)
                    || (left_ty == SimpleType::Float && right_ty == SimpleType::Float)
                {
                    SimpleType::Bool
                } else if left_ty == SimpleType::Unknown || right_ty == SimpleType::Unknown {
                    SimpleType::Bool
                } else {
                    self.push_error(
                        format!(
                            "comparison expects int or float, got {} and {}",
                            format_type(&left_ty),
                            format_type(&right_ty)
                        ),
                        Some(op_span),
                    );
                    SimpleType::Bool
                }
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if left_ty != SimpleType::Unknown
                    && right_ty != SimpleType::Unknown
                    && left_ty != right_ty
                {
                    self.push_error(
                        format!(
                            "equality expects matching types, got {} and {}",
                            format_type(&left_ty),
                            format_type(&right_ty)
                        ),
                        Some(op_span),
                    );
                }
                SimpleType::Bool
            }
        }
    }

    fn check_unary(&mut self, op: UnaryOp, op_span: Span, expr: &Expr) -> SimpleType {
        let ty = self.check_expr(expr);
        match op {
            UnaryOp::Neg => {
                if ty == SimpleType::Int {
                    SimpleType::Int
                } else if ty == SimpleType::Float {
                    SimpleType::Float
                } else if ty == SimpleType::Unknown {
                    SimpleType::Unknown
                } else {
                    self.push_error(
                        format!("- expects int or float, got {}", format_type(&ty)),
                        Some(op_span),
                    );
                    SimpleType::Unknown
                }
            }
            UnaryOp::Not => {
                if ty != SimpleType::Bool && ty != SimpleType::Int && ty != SimpleType::Unknown {
                    self.push_error(
                        format!("! expects bool or int, got {}", format_type(&ty)),
                        Some(op_span),
                    );
                }
                SimpleType::Bool
            }
        }
    }

    fn check_if(
        &mut self,
        if_span: Span,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
    ) -> SimpleType {
        let cond_ty = self.check_expr(condition);
        if cond_ty != SimpleType::Bool
            && cond_ty != SimpleType::Int
            && cond_ty != SimpleType::Unknown
        {
            self.push_error(
                format!("if expects bool or int, got {}", format_type(&cond_ty)),
                Some(if_span),
            );
        }
        self.last_option_inner = None;
        self.last_result_ok = None;
        self.last_result_err = None;
        let then_ty = self.check_expr(then_branch);
        let then_option = self.last_option_inner.clone();
        let then_ok = self.last_result_ok.clone();
        let then_err = self.last_result_err.clone();

        self.last_option_inner = None;
        self.last_result_ok = None;
        self.last_result_err = None;
        let else_ty = if let Some(else_expr) = else_branch {
            self.check_expr(else_expr)
        } else {
            SimpleType::Unit
        };
        let else_option = self.last_option_inner.clone();
        let else_ok = self.last_result_ok.clone();
        let else_err = self.last_result_err.clone();
        if then_ty == else_ty {
            self.merge_if_inners(
                if_span,
                &then_ty,
                then_option,
                then_ok,
                then_err,
                else_option,
                else_ok,
                else_err,
            );
            then_ty
        } else if then_ty == SimpleType::Unknown {
            self.merge_if_inners(
                if_span,
                &else_ty,
                then_option,
                then_ok,
                then_err,
                else_option,
                else_ok,
                else_err,
            );
            else_ty
        } else if else_ty == SimpleType::Unknown {
            self.merge_if_inners(
                if_span,
                &then_ty,
                then_option,
                then_ok,
                then_err,
                else_option,
                else_ok,
                else_err,
            );
            then_ty
        } else {
            self.push_error(
                format!(
                    "if branches must return same type, got {} and {}",
                    format_type(&then_ty),
                    format_type(&else_ty)
                ),
                Some(if_span),
            );
            self.last_option_inner = None;
            self.last_result_ok = None;
            self.last_result_err = None;
            SimpleType::Unknown
        }
    }

    fn merge_if_inners(
        &mut self,
        span: Span,
        ty: &SimpleType,
        then_option: Option<SimpleType>,
        then_ok: Option<SimpleType>,
        then_err: Option<SimpleType>,
        else_option: Option<SimpleType>,
        else_ok: Option<SimpleType>,
        else_err: Option<SimpleType>,
    ) {
        self.last_option_inner = None;
        self.last_result_ok = None;
        self.last_result_err = None;
        match ty {
            SimpleType::Option => {
                self.last_option_inner = self.merge_inner(
                    then_option,
                    else_option,
                    "if option inner mismatch",
                    Some(span),
                );
            }
            SimpleType::Result => {
                self.last_result_ok =
                    self.merge_inner(then_ok, else_ok, "if result ok inner mismatch", Some(span));
                self.last_result_err = self.merge_inner(
                    then_err,
                    else_err,
                    "if result err inner mismatch",
                    Some(span),
                );
            }
            _ => {}
        }
    }

    fn merge_inner(
        &mut self,
        left: Option<SimpleType>,
        right: Option<SimpleType>,
        message: &str,
        span: Option<Span>,
    ) -> Option<SimpleType> {
        match (left, right) {
            (Some(left), Some(right)) => {
                self.check_inner_compatible(&left, &right, message, span);
                Some(left)
            }
            (Some(left), None) => Some(left),
            (None, Some(right)) => Some(right),
            (None, None) => None,
        }
    }

    fn merge_assigned_inners(&mut self, name: &Ident, ty: &SimpleType, span: Span) {
        if matches!(ty, SimpleType::Option) {
            let existing = self.lookup_option_inner(&name.name);
            let current = self.last_option_inner.clone();
            self.last_option_inner = self.merge_inner(
                existing,
                current,
                &format!("option inner mismatch for {}", name.name),
                Some(span),
            );
        }
        if matches!(ty, SimpleType::Result) {
            let existing_ok = self.lookup_result_ok(&name.name);
            let current_ok = self.last_result_ok.clone();
            self.last_result_ok = self.merge_inner(
                existing_ok,
                current_ok,
                &format!("result ok inner mismatch for {}", name.name),
                Some(span),
            );
            let existing_err = self.lookup_result_err(&name.name);
            let current_err = self.last_result_err.clone();
            self.last_result_err = self.merge_inner(
                existing_err,
                current_err,
                &format!("result err inner mismatch for {}", name.name),
                Some(span),
            );
        }
    }

    fn check_match(
        &mut self,
        match_span: Span,
        value: &Expr,
        arms: &[at_syntax::MatchArm],
    ) -> SimpleType {
        let value_ty = self.check_expr(value);
        let mut option_inner = self.last_option_inner.take();
        let mut result_ok = self.last_result_ok.take();
        let mut result_err = self.last_result_err.take();

        if let Expr::Ident(ident) = value {
            if option_inner.is_none() {
                option_inner = self.lookup_option_inner(&ident.name);
            }
            if result_ok.is_none() {
                result_ok = self.lookup_result_ok(&ident.name);
            }
            if result_err.is_none() {
                result_err = self.lookup_result_err(&ident.name);
            }
        }
        let mut result_ty = SimpleType::Unknown;
        let mut has_ok = false;
        let mut has_err = false;
        let mut has_some = false;
        let mut has_none = false;
        let mut has_wildcard = false;
        let mut inferred_ok = None;
        let mut inferred_err = None;
        let mut inferred_opt = None;

        for arm in arms {
            self.push_scope();
            match &arm.pattern {
                at_syntax::MatchPattern::Int(_) => {
                    if value_ty != SimpleType::Int && value_ty != SimpleType::Unknown {
                        self.push_error("match expects int value".to_string(), Some(match_span));
                    }
                }
                at_syntax::MatchPattern::Struct { name, fields } => {
                    let struct_fields = self.structs.get(&name.name).cloned();
                    if struct_fields.is_none() {
                        self.push_error(format!("unknown struct: {}", name.name), Some(name.span));
                    }
                    if let SimpleType::Custom(struct_name) = &value_ty {
                        if struct_name != &name.name {
                            self.push_error(
                                format!("match expects struct {}, got {}", name.name, struct_name),
                                Some(match_span),
                            );
                        }
                    } else if value_ty != SimpleType::Unknown {
                        self.push_error(
                            format!(
                                "match expects struct {}, got {}",
                                name.name,
                                format_type(&value_ty)
                            ),
                            Some(match_span),
                        );
                    }

                    if let Some(struct_fields) = struct_fields {
                        for field in fields {
                            let binding = field.binding.as_ref().unwrap_or(&field.name);
                            if binding.name != "_" {
                                if let Some(expected) = struct_fields
                                    .iter()
                                    .find(|entry| entry.name.name == field.name.name)
                                {
                                    let expected_ty = self.type_from_ref(&expected.ty);
                                    self.bind_local(binding, expected_ty);
                                } else {
                                    self.push_error(
                                        format!(
                                            "unknown field {} on struct {}",
                                            field.name.name, name.name
                                        ),
                                        Some(field.name.span),
                                    );
                                }
                            }
                        }
                    }
                }
                at_syntax::MatchPattern::ResultOk(ident) => {
                    has_ok = true;
                    if value_ty != SimpleType::Result && value_ty != SimpleType::Unknown {
                        self.push_error("match expects result value".to_string(), Some(match_span));
                    }
                    let ty = result_ok.clone().unwrap_or(SimpleType::Unknown);
                    self.bind_local(ident, ty);
                }
                at_syntax::MatchPattern::ResultErr(ident) => {
                    has_err = true;
                    if value_ty != SimpleType::Result && value_ty != SimpleType::Unknown {
                        self.push_error("match expects result value".to_string(), Some(match_span));
                    }
                    let ty = result_err.clone().unwrap_or(SimpleType::Unknown);
                    self.bind_local(ident, ty);
                }
                at_syntax::MatchPattern::OptionSome(ident) => {
                    has_some = true;
                    if value_ty != SimpleType::Option && value_ty != SimpleType::Unknown {
                        self.push_error("match expects option value".to_string(), Some(match_span));
                    }
                    let ty = option_inner.clone().unwrap_or(SimpleType::Unknown);
                    self.bind_local(ident, ty);
                }
                at_syntax::MatchPattern::OptionNone => {
                    has_none = true;
                    if value_ty != SimpleType::Option && value_ty != SimpleType::Unknown {
                        self.push_error("match expects option value".to_string(), Some(match_span));
                    }
                }
                at_syntax::MatchPattern::Wildcard => {
                    has_wildcard = true;
                }
            }
            if let Some(guard) = &arm.guard {
                let guard_ty = self.check_expr(guard);
                if guard_ty != SimpleType::Bool && guard_ty != SimpleType::Unknown {
                    self.push_error(
                        format!("match guard expects bool, got {}", format_type(&guard_ty)),
                        expr_span(guard),
                    );
                }
            }
            self.last_result_ok = None;
            self.last_result_err = None;
            self.last_option_inner = None;
            let arm_ty = self.check_expr(&arm.body);
            if matches!(arm_ty, SimpleType::Result) {
                if let Some(ok_inner) = self.last_result_ok.clone() {
                    if let Some(existing) = inferred_ok.clone() {
                        self.check_inner_compatible(
                            &existing,
                            &ok_inner,
                            "match result ok inner mismatch",
                            expr_span(&arm.body),
                        );
                    } else {
                        inferred_ok = Some(ok_inner);
                    }
                }
                if let Some(err_inner) = self.last_result_err.clone() {
                    if let Some(existing) = inferred_err.clone() {
                        self.check_inner_compatible(
                            &existing,
                            &err_inner,
                            "match result err inner mismatch",
                            expr_span(&arm.body),
                        );
                    } else {
                        inferred_err = Some(err_inner);
                    }
                }
            }
            if matches!(arm_ty, SimpleType::Option) {
                if let Some(opt_inner) = self.last_option_inner.clone() {
                    if let Some(existing) = inferred_opt.clone() {
                        self.check_inner_compatible(
                            &existing,
                            &opt_inner,
                            "match option inner mismatch",
                            expr_span(&arm.body),
                        );
                    } else {
                        inferred_opt = Some(opt_inner);
                    }
                }
            }
            if result_ty == SimpleType::Unknown && arm_ty != SimpleType::Unknown {
                result_ty = arm_ty;
            } else if result_ty != SimpleType::Unknown && arm_ty != SimpleType::Unknown {
                self.check_compatible(
                    &result_ty,
                    &arm_ty,
                    "match arm type mismatch",
                    expr_span(&arm.body),
                );
            }
            self.pop_scope();
        }

        if !has_wildcard {
            if matches!(value_ty, SimpleType::Result) && !(has_ok && has_err) {
                self.push_error(
                    "non-exhaustive match for result".to_string(),
                    Some(match_span),
                );
            }
            if matches!(value_ty, SimpleType::Option) && !(has_some && has_none) {
                self.push_error(
                    "non-exhaustive match for option".to_string(),
                    Some(match_span),
                );
            }
        }

        if matches!(result_ty, SimpleType::Result) {
            self.last_result_ok = inferred_ok.clone();
            self.last_result_err = inferred_err.clone();
        }
        if matches!(result_ty, SimpleType::Option) {
            self.last_option_inner = inferred_opt.clone();
        }

        result_ty
    }

    fn check_call(&mut self, callee: &Expr, args: &[Expr]) -> SimpleType {
        if let Expr::Ident(ident) = callee {
            if let Some(result) = self.check_builtin_call(&ident.name, args, Some(ident.span)) {
                return result;
            }
            if let Some(sig) = self.functions.get(&ident.name).cloned() {
                self.check_function_needs(&ident.name, Some(ident.span));
                self.check_call_args(&ident.name, &sig, args, Some(ident.span));
                return sig.return_ty;
            }
            self.push_error(
                format!("unknown function: {}", ident.name),
                Some(ident.span),
            );
            for arg in args {
                self.check_expr(arg);
            }
            return SimpleType::Unknown;
        }

        if let Expr::Member { base, name } = callee {
            if let Expr::Ident(base_ident) = base.as_ref() {
                if !self.has_capability(&base_ident.name)
                    && ((base_ident.name == "time" && name.name == "now")
                        || (base_ident.name == "rng" && name.name == "uuid"))
                {
                    self.push_error(
                        format!("missing capability: {}", base_ident.name),
                        Some(base_ident.span),
                    );
                }
                if let Some(result) = self.check_member_builtin_call(
                    &base_ident.name,
                    &name.name,
                    args,
                    Some(name.span),
                ) {
                    return result;
                }
                let func_name = format!("{}.{}", base_ident.name, name.name);
                if let Some(sig) = self.functions.get(&func_name).cloned() {
                    self.check_function_needs(&func_name, Some(name.span));
                    self.check_call_args(&func_name, &sig, args, Some(name.span));
                    return sig.return_ty;
                }
                self.push_error(format!("unknown function: {func_name}"), Some(name.span));
                for arg in args {
                    self.check_expr(arg);
                }
                return SimpleType::Unknown;
            }
            self.push_error(
                "invalid call target: member base is not an identifier".to_string(),
                expr_span(callee),
            );
            for arg in args {
                self.check_expr(arg);
            }
            return SimpleType::Unknown;
        }

        let callee_ty = self.check_expr(callee);
        for arg in args {
            self.check_expr(arg);
        }
        match callee_ty {
            SimpleType::Function(params) => {
                if params != args.len() {
                    self.push_error(
                        format!(
                            "wrong arity for closure: expected {} args, got {}",
                            params,
                            args.len()
                        ),
                        expr_span(callee),
                    );
                }
                SimpleType::Unknown
            }
            SimpleType::Unknown => SimpleType::Unknown,
            _ => {
                self.push_error("invalid call target".to_string(), expr_span(callee));
                SimpleType::Unknown
            }
        }
    }

    fn check_call_args(&mut self, name: &str, sig: &FuncSig, args: &[Expr], span: Option<Span>) {
        if sig.params.len() != args.len() {
            self.push_error(
                format!(
                    "wrong arity for {}: expected {} args, got {}",
                    name,
                    sig.params.len(),
                    args.len()
                ),
                span,
            );
        }
        for (index, arg) in args.iter().enumerate() {
            let arg_ty = self.check_expr(arg);
            if let Some(expected) = sig.params.get(index) {
                self.check_compatible(
                    expected,
                    &arg_ty,
                    &format!("argument {} type mismatch for {}", index + 1, name),
                    expr_span(arg),
                );
            }
        }
    }

    fn bind_or_refine_local(&mut self, name: &Ident, ty: SimpleType) {
        for scope in self.locals.iter_mut().rev() {
            if let Some(existing) = scope.get_mut(&name.name) {
                if matches!(*existing, SimpleType::Unknown) && !matches!(ty, SimpleType::Unknown) {
                    *existing = ty;
                }
                return;
            }
        }
        self.bind_local(name, ty);
    }

    fn check_builtin_call(
        &mut self,
        name: &str,
        args: &[Expr],
        span: Option<Span>,
    ) -> Option<SimpleType> {
        self.last_option_inner = None;
        self.last_result_ok = None;
        self.last_result_err = None;
        match name {
            "assert" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Bool
                        && arg_ty != SimpleType::Int
                        && arg_ty != SimpleType::Unknown
                    {
                        self.push_error("assert expects bool or int".to_string(), expr_span(arg));
                    }
                }
                Some(SimpleType::Unit)
            }
            "assert_eq" => {
                self.check_arity(name, args, 2, span);
                if args.len() == 2 {
                    let left = self.check_expr(&args[0]);
                    let right = self.check_expr(&args[1]);
                    self.check_compatible(
                        &left,
                        &right,
                        "assert_eq type mismatch",
                        expr_span(&args[0]),
                    );
                }
                Some(SimpleType::Unit)
            }
            "len" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Array
                        && arg_ty != SimpleType::String
                        && arg_ty != SimpleType::Unknown
                    {
                        self.push_error("len expects array or string".to_string(), expr_span(arg));
                    }
                }
                Some(SimpleType::Int)
            }
            "append" => {
                self.check_arity(name, args, 2, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Array && arg_ty != SimpleType::Unknown {
                        self.push_error("append expects array".to_string(), expr_span(arg));
                    }
                }
                if args.len() == 2 {
                    self.check_expr(&args[1]);
                }
                Some(SimpleType::Array)
            }
            "contains" => {
                self.check_arity(name, args, 2, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Array && arg_ty != SimpleType::Unknown {
                        self.push_error("contains expects array".to_string(), expr_span(arg));
                    }
                }
                if args.len() == 2 {
                    self.check_expr(&args[1]);
                }
                Some(SimpleType::Bool)
            }
            "slice" => {
                self.check_arity(name, args, 3, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Array && arg_ty != SimpleType::Unknown {
                        self.push_error("slice expects array".to_string(), expr_span(arg));
                    }
                }
                if args.len() >= 2 {
                    let start_ty = self.check_expr(&args[1]);
                    if start_ty != SimpleType::Int && start_ty != SimpleType::Unknown {
                        self.push_error(
                            "slice expects int bounds".to_string(),
                            expr_span(&args[1]),
                        );
                    }
                }
                if args.len() >= 3 {
                    let end_ty = self.check_expr(&args[2]);
                    if end_ty != SimpleType::Int && end_ty != SimpleType::Unknown {
                        self.push_error(
                            "slice expects int bounds".to_string(),
                            expr_span(&args[2]),
                        );
                    }
                }
                Some(SimpleType::Array)
            }
            "map" => {
                self.check_arity(name, args, 2, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Array && arg_ty != SimpleType::Unknown {
                        self.push_error("map expects array".to_string(), expr_span(arg));
                    }
                }
                if args.len() == 2 {
                    self.check_expr(&args[1]);
                }
                Some(SimpleType::Array)
            }
            "filter" => {
                self.check_arity(name, args, 2, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Array && arg_ty != SimpleType::Unknown {
                        self.push_error("filter expects array".to_string(), expr_span(arg));
                    }
                }
                if args.len() == 2 {
                    self.check_expr(&args[1]);
                }
                Some(SimpleType::Array)
            }
            "reduce" => {
                self.check_arity(name, args, 3, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Array && arg_ty != SimpleType::Unknown {
                        self.push_error("reduce expects array".to_string(), expr_span(arg));
                    }
                }
                if args.len() >= 2 {
                    self.check_expr(&args[1]);
                }
                if args.len() >= 3 {
                    self.check_expr(&args[2]);
                }
                Some(SimpleType::Unknown)
            }
            "print" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    self.check_expr(arg);
                }
                Some(SimpleType::Unit)
            }
            "some" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let inner = self.check_expr(arg);
                    self.last_option_inner = Some(inner);
                    if matches!(self.current_return, SimpleType::Unknown) {
                        self.current_return = SimpleType::Option;
                    }
                }
                Some(SimpleType::Option)
            }
            "none" => {
                self.check_arity(name, args, 0, span);
                self.last_option_inner = None;
                if matches!(self.current_return, SimpleType::Unknown) {
                    self.current_return = SimpleType::Option;
                }
                Some(SimpleType::Option)
            }
            "ok" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let inner = self.check_expr(arg);
                    self.last_result_ok = Some(inner);
                    if matches!(self.current_return, SimpleType::Unknown) {
                        self.current_return = SimpleType::Result;
                    }
                }
                Some(SimpleType::Result)
            }
            "err" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let inner = self.check_expr(arg);
                    self.last_result_err = Some(inner);
                    if matches!(self.current_return, SimpleType::Unknown) {
                        self.current_return = SimpleType::Result;
                    }
                }
                Some(SimpleType::Result)
            }
            "is_some" | "is_none" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Option && arg_ty != SimpleType::Unknown {
                        self.push_error(
                            format!("{} expects option, got {}", name, format_type(&arg_ty)),
                            expr_span(arg),
                        );
                    }
                }
                Some(SimpleType::Bool)
            }
            "is_ok" | "is_err" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::Result && arg_ty != SimpleType::Unknown {
                        self.push_error(
                            format!("{} expects result, got {}", name, format_type(&arg_ty)),
                            expr_span(arg),
                        );
                    }
                }
                Some(SimpleType::Bool)
            }
            _ => None,
        }
    }

    fn check_member_builtin_call(
        &mut self,
        base: &str,
        name: &str,
        args: &[Expr],
        span: Option<Span>,
    ) -> Option<SimpleType> {
        match (base, name) {
            ("time", "now") => {
                self.check_arity("time.now", args, 0, span);
                Some(SimpleType::Int)
            }
            ("time", "fixed") => {
                self.check_arity("time.fixed", args, 1, span);
                if let Some(arg) = args.first() {
                    self.check_expr(arg);
                }
                Some(SimpleType::String)
            }
            ("rng", "deterministic") => {
                self.check_arity("rng.deterministic", args, 1, span);
                if let Some(arg) = args.first() {
                    self.check_expr(arg);
                }
                Some(SimpleType::Int)
            }
            ("rng", "uuid") => {
                self.check_arity("rng.uuid", args, 0, span);
                Some(SimpleType::String)
            }
            _ => None,
        }
    }

    fn check_arity(&mut self, name: &str, args: &[Expr], expected: usize, span: Option<Span>) {
        if args.len() != expected {
            self.push_error(
                format!(
                    "wrong arity for {}: expected {} args, got {}",
                    name,
                    expected,
                    args.len()
                ),
                span,
            );
        }
    }

    fn check_compatible(
        &mut self,
        expected: &SimpleType,
        found: &SimpleType,
        message: &str,
        span: Option<Span>,
    ) -> bool {
        if expected == found {
            return true;
        }
        if matches!(expected, SimpleType::Unknown) || matches!(found, SimpleType::Unknown) {
            return true;
        }
        self.push_error(
            format!(
                "{}: expected {}, got {}",
                message,
                format_type(expected),
                format_type(found)
            ),
            span,
        );
        false
    }

    fn check_inner_compatible(
        &mut self,
        expected: &SimpleType,
        found: &SimpleType,
        message: &str,
        span: Option<Span>,
    ) {
        if matches!(expected, SimpleType::Unknown) || matches!(found, SimpleType::Unknown) {
            return;
        }
        if expected != found {
            self.push_error(
                format!(
                    "{}: expected {}, got {}",
                    message,
                    format_type(expected),
                    format_type(found)
                ),
                span,
            );
        }
    }

    fn push_scope(&mut self) {
        self.locals.push(HashMap::new());
        self.option_inner.push(HashMap::new());
        self.result_ok.push(HashMap::new());
        self.result_err.push(HashMap::new());
        self.capabilities.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.locals.pop();
        self.option_inner.pop();
        self.result_ok.pop();
        self.result_err.pop();
        self.capabilities.pop();
    }

    fn insert_capability(&mut self, name: &str) {
        if let Some(scope) = self.capabilities.last_mut() {
            scope.insert(name.to_string());
        }
    }

    fn has_capability(&self, name: &str) -> bool {
        for scope in self.capabilities.iter().rev() {
            if scope.contains(name) {
                return true;
            }
        }
        false
    }

    fn check_function_needs(&mut self, name: &str, span: Option<Span>) {
        let needs = match self.function_needs.get(name).cloned() {
            Some(needs) => needs,
            None => return,
        };
        for need in needs {
            if !self.has_capability(&need) {
                self.push_error(format!("missing capability: {}", need), span);
            }
        }
    }

    fn bind_local(&mut self, name: &Ident, ty: SimpleType) {
        if let Some(scope) = self.locals.last_mut() {
            scope.insert(name.name.clone(), ty);
        }
    }

    fn bind_inner_types(&mut self, name: &Ident, ty: &SimpleType) {
        if matches!(ty, SimpleType::Option) {
            if let Some(inner) = self.last_option_inner.clone() {
                if let Some(scope) = self.option_inner.last_mut() {
                    scope.insert(name.name.clone(), inner);
                }
            }
        }
        if matches!(ty, SimpleType::Result) {
            if let Some(inner) = self.last_result_ok.clone() {
                if let Some(scope) = self.result_ok.last_mut() {
                    scope.insert(name.name.clone(), inner);
                }
            }
            if let Some(inner) = self.last_result_err.clone() {
                if let Some(scope) = self.result_err.last_mut() {
                    scope.insert(name.name.clone(), inner);
                }
            }
        }
    }

    fn bind_inner_types_for_assignment(&mut self, name: &Ident, ty: &SimpleType) {
        let scope_index = self.find_local_scope_index(&name.name);
        if matches!(ty, SimpleType::Option) {
            if let Some(inner) = self.last_option_inner.clone() {
                if let Some(index) = scope_index {
                    if let Some(scope) = self.option_inner.get_mut(index) {
                        scope.insert(name.name.clone(), inner);
                    }
                } else if let Some(scope) = self.option_inner.last_mut() {
                    scope.insert(name.name.clone(), inner);
                }
            }
        }
        if matches!(ty, SimpleType::Result) {
            if let Some(inner) = self.last_result_ok.clone() {
                if let Some(index) = scope_index {
                    if let Some(scope) = self.result_ok.get_mut(index) {
                        scope.insert(name.name.clone(), inner);
                    }
                } else if let Some(scope) = self.result_ok.last_mut() {
                    scope.insert(name.name.clone(), inner);
                }
            }
            if let Some(inner) = self.last_result_err.clone() {
                if let Some(index) = scope_index {
                    if let Some(scope) = self.result_err.get_mut(index) {
                        scope.insert(name.name.clone(), inner);
                    }
                } else if let Some(scope) = self.result_err.last_mut() {
                    scope.insert(name.name.clone(), inner);
                }
            }
        }
    }

    fn find_local_scope_index(&self, name: &str) -> Option<usize> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find_map(|(index, scope)| scope.contains_key(name).then_some(index))
    }

    fn lookup_option_inner(&self, name: &str) -> Option<SimpleType> {
        for scope in self.option_inner.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn lookup_result_ok(&self, name: &str) -> Option<SimpleType> {
        for scope in self.result_ok.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn lookup_result_err(&self, name: &str) -> Option<SimpleType> {
        for scope in self.result_err.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn update_return_option_inner(&mut self, span: Option<Span>) {
        if let Some(inner) = self.last_option_inner.clone() {
            if let Some(expected) = self.expected_option_inner() {
                self.check_inner_compatible(
                    &expected,
                    &inner,
                    "return option inner mismatch",
                    span,
                );
            }
            let existing = self.return_option_inner.clone();
            if let Some(existing) = existing {
                self.check_inner_compatible(
                    &existing,
                    &inner,
                    "return option inner mismatch",
                    span,
                );
            } else {
                self.return_option_inner = Some(inner);
            }
        }
    }

    fn update_return_result_inners(&mut self, span: Option<Span>) {
        if let Some(ok_inner) = self.last_result_ok.clone() {
            if let Some(expected) = self.expected_result_ok() {
                self.check_inner_compatible(
                    &expected,
                    &ok_inner,
                    "return result ok inner mismatch",
                    span,
                );
            }
            let existing = self.return_result_ok.clone();
            if let Some(existing) = existing {
                self.check_inner_compatible(
                    &existing,
                    &ok_inner,
                    "return result ok inner mismatch",
                    span,
                );
            } else {
                self.return_result_ok = Some(ok_inner);
            }
        }
        if let Some(err_inner) = self.last_result_err.clone() {
            if let Some(expected) = self.expected_result_err() {
                self.check_inner_compatible(
                    &expected,
                    &err_inner,
                    "return result err inner mismatch",
                    span,
                );
            }
            let existing = self.return_result_err.clone();
            if let Some(existing) = existing {
                self.check_inner_compatible(
                    &existing,
                    &err_inner,
                    "return result err inner mismatch",
                    span,
                );
            } else {
                self.return_result_err = Some(err_inner);
            }
        }
    }

    fn inferred_return_string(&self) -> Option<String> {
        match &self.current_return {
            SimpleType::Unknown => None,
            SimpleType::Result => {
                let ok = self.return_result_ok.as_ref().map(format_type);
                let err = self.return_result_err.as_ref().map(format_type);
                if let (Some(ok), Some(err)) = (ok, err) {
                    Some(format!("result<{ok}, {err}>"))
                } else {
                    Some("result".to_string())
                }
            }
            SimpleType::Option => {
                if let Some(inner) = &self.return_option_inner {
                    Some(format!("option<{}>", format_type(inner)))
                } else {
                    Some("option".to_string())
                }
            }
            other => Some(format_type(other)),
        }
    }

    fn update_return_from_ident(&mut self, expr: &Expr, span: Option<Span>) {
        let ident = match expr {
            Expr::Ident(ident) => ident,
            _ => return,
        };
        if matches!(self.current_return, SimpleType::Option) {
            if let Some(inner) = self.lookup_option_inner(&ident.name) {
                let existing = self.return_option_inner.clone();
                if let Some(existing) = existing {
                    self.check_inner_compatible(
                        &existing,
                        &inner,
                        "return option inner mismatch",
                        span,
                    );
                } else {
                    self.return_option_inner = Some(inner);
                }
            }
        }
        if matches!(self.current_return, SimpleType::Result) {
            if let Some(inner) = self.lookup_result_ok(&ident.name) {
                let existing = self.return_result_ok.clone();
                if let Some(existing) = existing {
                    self.check_inner_compatible(
                        &existing,
                        &inner,
                        "return result ok inner mismatch",
                        span,
                    );
                } else {
                    self.return_result_ok = Some(inner);
                }
            }
            if let Some(inner) = self.lookup_result_err(&ident.name) {
                let existing = self.return_result_err.clone();
                if let Some(existing) = existing {
                    self.check_inner_compatible(
                        &existing,
                        &inner,
                        "return result err inner mismatch",
                        span,
                    );
                } else {
                    self.return_result_err = Some(inner);
                }
            }
        }
    }

    fn expected_option_inner(&mut self) -> Option<SimpleType> {
        let ty = self.current_return_ref.clone()?;
        match ty {
            TypeRef::Named { name, args } => {
                if name.name != "option" || args.len() != 1 {
                    return None;
                }
                Some(self.type_from_ref(&args[0]))
            }
            _ => None,
        }
    }

    fn expected_result_ok(&mut self) -> Option<SimpleType> {
        let ty = self.current_return_ref.clone()?;
        match ty {
            TypeRef::Named { name, args } => {
                if name.name != "result" || args.len() != 2 {
                    return None;
                }
                Some(self.type_from_ref(&args[0]))
            }
            _ => None,
        }
    }

    fn expected_result_err(&mut self) -> Option<SimpleType> {
        let ty = self.current_return_ref.clone()?;
        match ty {
            TypeRef::Named { name, args } => {
                if name.name != "result" || args.len() != 2 {
                    return None;
                }
                Some(self.type_from_ref(&args[1]))
            }
            _ => None,
        }
    }

    fn infer_inner_from_expr(&mut self, expr: &Expr, ty: &SimpleType) {
        match expr {
            Expr::Ident(ident) => {
                if matches!(ty, SimpleType::Option) {
                    self.last_option_inner = self.lookup_option_inner(&ident.name);
                }
                if matches!(ty, SimpleType::Result) {
                    self.last_result_ok = self.lookup_result_ok(&ident.name);
                    self.last_result_err = self.lookup_result_err(&ident.name);
                }
            }
            Expr::Match { .. } => {
                // inner types may already be recorded by check_match
            }
            _ => {}
        }
    }

    fn resolve_local(&self, name: &Ident) -> Option<SimpleType> {
        for scope in self.locals.iter().rev() {
            if let Some(ty) = scope.get(&name.name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn is_local_in_current_scope(&self, name: &str) -> bool {
        self.locals
            .last()
            .map(|scope| scope.contains_key(name))
            .unwrap_or(false)
    }

    fn type_from_ref(&mut self, ty: &TypeRef) -> SimpleType {
        self.validate_type_ref(ty);
        match ty {
            TypeRef::Named { name, .. } => {
                if let Some(alias) = self.resolve_alias(name) {
                    return self.type_from_ref(&alias);
                }
                match name.name.as_str() {
                    "int" => SimpleType::Int,
                    "float" => SimpleType::Float,
                    "bool" => SimpleType::Bool,
                    "string" => SimpleType::String,
                    "array" => SimpleType::Array,
                    "option" => SimpleType::Option,
                    "result" => SimpleType::Result,
                    "unit" => SimpleType::Unit,
                    other => SimpleType::Custom(other.to_string()),
                }
            }
            TypeRef::Function { params, .. } => SimpleType::Function(params.len()),
        }
    }

    fn validate_type_ref(&mut self, ty: &TypeRef) {
        match ty {
            TypeRef::Named { name, args } => match name.name.as_str() {
                "array" => {
                    if args.len() != 1 {
                        self.push_error(
                            format!("type array expects 1 argument, got {}", args.len()),
                            Some(name.span),
                        );
                    }
                }
                "option" => {
                    if args.len() != 1 {
                        self.push_error(
                            format!("type option expects 1 argument, got {}", args.len()),
                            Some(name.span),
                        );
                    }
                }
                "result" => {
                    if args.len() != 2 {
                        self.push_error(
                            format!("type result expects 2 arguments, got {}", args.len()),
                            Some(name.span),
                        );
                    }
                }
                "int" | "float" | "bool" | "string" | "unit" => {}
                other => {
                    if self.type_aliases.contains_key(other) {
                        if let Some(alias) = self.resolve_alias(name) {
                            self.validate_type_ref(&alias);
                        }
                    } else if !self.structs.contains_key(other) {
                        self.push_error(format!("unknown type: {}", other), Some(name.span));
                    }
                }
            },
            TypeRef::Function {
                params, return_ty, ..
            } => {
                for param in params {
                    self.validate_type_ref(param);
                }
                self.validate_type_ref(return_ty);
            }
        }
    }

    fn resolve_alias(&mut self, name: &Ident) -> Option<TypeRef> {
        let mut current = name.name.clone();
        let mut seen = HashSet::new();
        while let Some(alias) = self.type_aliases.get(&current).cloned() {
            if !seen.insert(current.clone()) {
                self.push_error(format!("cyclic type alias: {}", name.name), Some(name.span));
                return None;
            }
            match alias {
                TypeRef::Named { name, args } if args.is_empty() => {
                    current = name.name;
                    continue;
                }
                other => return Some(other),
            }
        }
        None
    }

    fn push_error(&mut self, message: String, span: Option<Span>) {
        self.errors.push(TypeError { message, span });
    }
}

fn expr_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Int(_, span) => Some(*span),
        Expr::String(_, span) => Some(*span),
        Expr::Bool(_, span) => Some(*span),
        Expr::Ident(ident) => Some(ident.span),
        Expr::Unary { op_span, .. } => Some(*op_span),
        Expr::Binary { op_span, .. } => Some(*op_span),
        Expr::If { if_span, .. } => Some(*if_span),
        Expr::Member { name, .. } => Some(name.span),
        Expr::Call { callee, .. } => expr_span(callee),
        Expr::Try(expr) => expr_span(expr),
        Expr::Match { match_span, .. } => Some(*match_span),
        Expr::Block { block_span, .. } => Some(*block_span),
        Expr::Array { array_span, .. } => Some(*array_span),
        Expr::Index { index_span, .. } => Some(*index_span),
        Expr::Tuple { tuple_span, .. } => Some(*tuple_span),
        Expr::Range { range_span, .. } => Some(*range_span),
        Expr::InterpolatedString { span, .. } => Some(*span),
        Expr::Closure { span, .. } => Some(*span),
        Expr::StructLiteral { span, .. } => Some(*span),
        Expr::Float(_, span) => Some(*span),
    }
}

fn format_type(ty: &SimpleType) -> String {
    match ty {
        SimpleType::Int => "int".to_string(),
        SimpleType::Bool => "bool".to_string(),
        SimpleType::String => "string".to_string(),
        SimpleType::Array => "array".to_string(),
        SimpleType::Option => "option".to_string(),
        SimpleType::Result => "result".to_string(),
        SimpleType::Unit => "unit".to_string(),
        SimpleType::Float => "float".to_string(),
        SimpleType::Function(params) => format!("fn({params})"),
        SimpleType::Custom(name) => name.clone(),
        SimpleType::Unknown => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::{infer_function_returns, typecheck_module};
    use at_parser::parse_module;

    #[test]
    fn infers_option_return_inner() {
        let source = r#"
fn f() {
    return some(1);
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(inferred.get("f").map(String::as_str), Some("option<int>"));
    }

    #[test]
    fn infers_result_return_inner() {
        let source = r#"
fn g() {
    return ok(1);
    return err("no");
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(
            inferred.get("g").map(String::as_str),
            Some("result<int, string>")
        );
    }

    #[test]
    fn infers_match_return_inner() {
        let source = r#"
fn h() {
    let x = ok(1);
    return match x {
        ok(v) => ok(v),
        err(e) => err(e),
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(
            inferred.get("h").map(String::as_str),
            Some("result<int, unknown>")
        );
    }

    #[test]
    fn infers_option_match_return_inner() {
        let source = r#"
fn i() {
    let x = some(1);
    return match x {
        some(v) => some(v),
        none => none(),
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(inferred.get("i").map(String::as_str), Some("option<int>"));
    }

    #[test]
    fn infers_result_inner_from_set_in_while() {
        let source = r#"
fn f() {
    let x = err("no");
    while false {
        set x = ok(1);
        break;
    }
    return x;
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(
            inferred.get("f").map(String::as_str),
            Some("result<int, string>")
        );
    }

    #[test]
    fn errors_on_result_ok_inner_mismatch() {
        let source = r#"
fn f() -> result<int, string> {
    return ok("no");
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("return result ok inner mismatch")));
    }

    #[test]
    fn errors_on_result_err_inner_mismatch() {
        let source = r#"
fn f() -> result<int, string> {
    return err(1);
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("return result err inner mismatch")));
    }

    #[test]
    fn errors_on_option_inner_mismatch() {
        let source = r#"
fn f() -> option<int> {
    return some("no");
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("return option inner mismatch")));
    }

    #[test]
    fn errors_on_match_return_result_inner_mismatch() {
        let source = r#"
fn f() -> result<int, string> {
    let x = ok(1);
    return match x {
        ok(v) => ok("no"),
        err(e) => err(e),
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("return result ok inner mismatch")));
    }

    #[test]
    fn errors_on_match_return_option_inner_mismatch() {
        let source = r#"
fn f() -> option<int> {
    let x = some(1);
    return match x {
        some(v) => some("no"),
        none => none(),
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("return option inner mismatch")));
    }

    #[test]
    fn errors_on_return_ident_result_inner_mismatch() {
        let source = r#"
fn f() -> result<int, string> {
    let x = ok("no");
    return x;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("return result ok inner mismatch")));
    }

    #[test]
    fn errors_on_return_ident_option_inner_mismatch() {
        let source = r#"
fn f() -> option<int> {
    let x = some("no");
    return x;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("return option inner mismatch")));
    }

    #[test]
    fn errors_on_duplicate_function_name() {
        let source = r#"
fn f() {}
fn f() {}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate function")));
    }

    #[test]
    fn errors_on_duplicate_import_alias() {
        let source = r#"
import "./a.at" as foo;
import "./b.at" as foo;
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate import alias")));
    }

    #[test]
    fn errors_on_duplicate_local_same_scope() {
        let source = r#"
fn f() {
    let x = 1;
    let x = 2;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate local")));
    }

    #[test]
    fn allows_shadowing_in_inner_scope() {
        let source = r#"
fn f() {
    let x = 1;
    {
        let x = 2;
    }
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_duplicate_param() {
        let source = r#"
fn f(x: int, x: int) {}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate local")));
    }

    #[test]
    fn errors_on_builtin_wrong_arity() {
        let source = r#"
fn f() {
    assert();
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("wrong arity for assert")));
    }

    #[test]
    fn errors_on_member_builtin_wrong_arity() {
        let source = r#"
fn f() {
    time.now(1);
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("wrong arity for time.now")));
    }

    #[test]
    fn errors_on_missing_capability_for_time_now() {
        let source = r#"
fn f() -> int {
    return time.now();
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("missing capability: time")));
    }

    #[test]
    fn allows_calling_needs_function_with_using() {
        let source = r#"
fn f() -> int needs { time } {
    return time.now();
}

using time = time.fixed("2026-01-01T00:00:00Z");
f();
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_index_non_int() {
        let source = r#"
fn f() {
    let values = [1, 2, 3];
    let value = values["nope"];
    return value;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("index expects int")));
    }

    #[test]
    fn errors_on_len_non_collection() {
        let source = r#"
fn f() {
    let value = len(1);
    return value;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("len expects array or string")));
    }

    #[test]
    fn errors_on_contains_non_array() {
        let source = r#"
fn f() {
    let value = contains(1, 2);
    return value;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("contains expects array")));
    }

    #[test]
    fn errors_on_for_non_array() {
        let source = r#"
fn f() {
    for value in 1 {
        print(value);
    }
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("for expects array")));
    }

    #[test]
    fn allows_for_array() {
        let source = r#"
fn f() {
    let values = [1, 2, 3];
    for value in values {
        print(value);
    }
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn infers_option_inner_from_if() {
        let source = r#"
fn f() {
    return if true { some(1) } else { none() };
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(inferred.get("f").map(String::as_str), Some("option<int>"));
    }

    #[test]
    fn infers_result_inner_from_if() {
        let source = r#"
fn f() {
    return if true { ok(1) } else { err("no") };
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(
            inferred.get("f").map(String::as_str),
            Some("result<int, string>")
        );
    }

    #[test]
    fn infers_result_inner_from_set_in_if_branch() {
        let source = r#"
fn f() {
    let x = err("no");
    if true {
        set x = ok(1);
    } else {
        set x = err("no");
    }
    return x;
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(
            inferred.get("f").map(String::as_str),
            Some("result<int, string>")
        );
    }

    #[test]
    fn infers_option_inner_from_set_in_if_branch() {
        let source = r#"
fn f() {
    let x = none();
    if true {
        set x = some(1);
    } else {
        set x = none();
    }
    return x;
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(inferred.get("f").map(String::as_str), Some("option<int>"));
    }

    #[test]
    fn errors_on_if_result_inner_mismatch() {
        let source = r#"
fn f() -> result<int, string> {
    return if true { ok("no") } else { err("no") };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors.iter().any(|err| {
            err.message.contains("if result ok inner mismatch")
                || err.message.contains("return result ok inner mismatch")
        }));
    }

    #[test]
    fn errors_on_set_option_inner_mismatch() {
        let source = r#"
fn f() {
    let x = some(1);
    set x = some("no");
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("option inner mismatch for x")));
    }

    #[test]
    fn errors_on_set_result_ok_inner_mismatch() {
        let source = r#"
fn f() {
    let x = ok(1);
    set x = ok("no");
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("result ok inner mismatch for x")));
    }

    #[test]
    fn match_errors_include_span() {
        let source = r#"
fn f() {
    let x = some(1);
    return match x {
        some(v) => v,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        let err = errors
            .iter()
            .find(|err| err.message.contains("non-exhaustive match for option"))
            .expect("expected match error");
        assert!(err.span.is_some());
    }

    #[test]
    fn match_value_type_errors_include_span() {
        let source = r#"
fn f() {
    let x = 1;
    return match x {
        some(v) => v,
        none => 0,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        let err = errors
            .iter()
            .find(|err| err.message.contains("match expects option value"))
            .expect("expected match type error");
        assert!(err.span.is_some());
    }

    #[test]
    fn errors_on_break_outside_loop() {
        let source = r#"
fn f() {
    break;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("break used outside of loop")));
    }

    #[test]
    fn errors_on_continue_outside_loop() {
        let source = r#"
fn f() {
    continue;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("continue used outside of loop")));
    }

    #[test]
    fn allows_break_continue_in_loop() {
        let source = r#"
fn f() {
    let i: int = 0;
    while i < 10 {
        set i = i + 1;
        continue;
    }
    while i < 10 {
        break;
    }
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }
}
