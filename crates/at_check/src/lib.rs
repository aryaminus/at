use std::collections::{HashMap, HashSet};

use at_syntax::{
    BinaryOp, EnumVariant, Expr, Function, Ident, MapEntry, Module, Span, Stmt, StructField,
    TypeRef, UnaryOp,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum SimpleType {
    Int,
    Float,
    Bool,
    String,
    Unit,
    Array(Box<SimpleType>),
    Option(Box<SimpleType>),
    Result(Box<SimpleType>, Box<SimpleType>),
    Function(Vec<SimpleType>, Box<SimpleType>),
    Tuple(Vec<SimpleType>),
    Map(Box<SimpleType>, Box<SimpleType>),
    Future(Box<SimpleType>),
    Generator(Box<SimpleType>),
    Union(Vec<SimpleType>),
    Intersection(Vec<SimpleType>),
    Custom(String, Vec<SimpleType>),
    TypeParam(String),
    Unknown,
}

#[derive(Debug, Clone)]
struct FuncSig {
    params: Vec<SimpleType>,
    return_ty: SimpleType,
    is_async: bool,
    type_params: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypecheckMode {
    Default,
    Strict,
}

#[derive(Clone, Debug)]
struct OptionNarrow {
    ident: Ident,
    inner: SimpleType,
    then_branch: bool,
}

#[derive(Clone, Debug)]
struct ResultNarrow {
    ident: Ident,
    ok: SimpleType,
    err: SimpleType,
    then_branch: bool,
}

struct BranchInners {
    option: Option<SimpleType>,
    ok: Option<SimpleType>,
    err: Option<SimpleType>,
}

pub fn typecheck_module(module: &Module) -> Result<(), Vec<TypeError>> {
    let mut checker = TypeChecker::new();
    checker.load_declarations(module);
    checker.load_functions(module);
    checker.check_module(module);
    if checker.errors.is_empty() {
        Ok(())
    } else {
        Err(checker.errors)
    }
}

pub fn typecheck_module_with_mode(
    module: &Module,
    mode: TypecheckMode,
) -> Result<(), Vec<TypeError>> {
    let mut checker = TypeChecker::new_with_mode(mode);
    checker.load_declarations(module);
    checker.load_functions(module);
    checker.check_module(module);
    if checker.errors.is_empty() {
        Ok(())
    } else {
        Err(checker.errors)
    }
}

pub fn typecheck_modules(modules: &[Module]) -> Result<(), Vec<TypeError>> {
    let mut checker = TypeChecker::new();
    for module in modules {
        checker.load_declarations(module);
    }
    for module in modules {
        checker.load_functions(module);
    }
    for module in modules {
        checker.check_module(module);
    }
    if checker.errors.is_empty() {
        Ok(())
    } else {
        Err(checker.errors)
    }
}

pub fn typecheck_modules_with_mode(
    modules: &[Module],
    mode: TypecheckMode,
) -> Result<(), Vec<TypeError>> {
    let mut checker = TypeChecker::new_with_mode(mode);
    for module in modules {
        checker.load_declarations(module);
    }
    for module in modules {
        checker.load_functions(module);
    }
    for module in modules {
        checker.check_module(module);
    }
    if checker.errors.is_empty() {
        Ok(())
    } else {
        Err(checker.errors)
    }
}

pub fn infer_function_returns(module: &Module) -> HashMap<String, String> {
    let mut checker = TypeChecker::new();
    checker.load_declarations(module);
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
    structs: HashMap<String, (Vec<Ident>, Vec<StructField>)>,
    type_aliases: HashMap<String, TypeRef>,
    enums: HashMap<String, (Vec<Ident>, Vec<EnumVariant>)>,
    type_params: Vec<HashSet<String>>,
    locals: Vec<HashMap<String, SimpleType>>,
    consts: Vec<HashSet<String>>,
    capabilities: Vec<HashSet<String>>,
    current_return: SimpleType,
    current_return_ref: Option<TypeRef>,
    return_option_inner: Option<SimpleType>,
    return_result_ok: Option<SimpleType>,
    return_result_err: Option<SimpleType>,
    return_generator_inner: Option<SimpleType>,
    inferred_returns: HashMap<String, String>,
    errors: Vec<TypeError>,
    last_option_inner: Option<SimpleType>,
    last_result_ok: Option<SimpleType>,
    last_result_err: Option<SimpleType>,
    loop_depth: usize,
    current_is_async: bool,
    mode: TypecheckMode,
}

impl TypeChecker {
    fn new() -> Self {
        Self::new_with_mode(TypecheckMode::Default)
    }

    fn new_with_mode(mode: TypecheckMode) -> Self {
        Self {
            functions: HashMap::new(),
            function_needs: HashMap::new(),
            structs: HashMap::new(),
            type_aliases: HashMap::new(),
            enums: HashMap::new(),
            locals: Vec::new(),
            consts: Vec::new(),
            type_params: Vec::new(),
            capabilities: Vec::new(),
            current_return: SimpleType::Unknown,
            current_return_ref: None,
            return_option_inner: None,
            return_result_ok: None,
            return_result_err: None,
            return_generator_inner: None,
            inferred_returns: HashMap::new(),
            errors: Vec::new(),
            last_option_inner: None,
            last_result_ok: None,
            last_result_err: None,
            loop_depth: 0,
            current_is_async: false,
            mode,
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
            if !func.type_params.is_empty() {
                self.validate_type_params(&func.type_params, func.name.span, "function");
            }
            self.function_needs.insert(
                func.name.name.clone(),
                func.needs.iter().map(|ident| ident.name.clone()).collect(),
            );
            let tp: HashSet<String> = func.type_params.iter().map(|p| p.name.clone()).collect();
            let tp_ref = if tp.is_empty() { None } else { Some(&tp) };
            let params = func
                .params
                .iter()
                .map(|param| {
                    param
                        .ty
                        .as_ref()
                        .map(|ty| self.type_from_ref_with_env(ty, None, tp_ref))
                        .unwrap_or(SimpleType::Unknown)
                })
                .collect();
            let return_ty = func
                .return_ty
                .as_ref()
                .map(|ty| self.type_from_ref_with_env(ty, None, tp_ref))
                .unwrap_or(SimpleType::Unknown);
            self.functions.insert(
                func.name.name.clone(),
                FuncSig {
                    params,
                    return_ty,
                    is_async: func.is_async,
                    type_params: func
                        .type_params
                        .iter()
                        .map(|param| param.name.clone())
                        .collect(),
                },
            );
        }
    }

    fn load_declarations(&mut self, module: &Module) {
        self.load_type_aliases(module);
        self.load_structs(module);
        self.load_enums(module);
    }

    fn check_module(&mut self, module: &Module) {
        self.check_duplicate_import_aliases(module);
        self.type_params.clear();
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
        self.current_is_async = func.is_async;
        self.locals.clear();
        self.capabilities.clear();
        self.type_params.clear();
        self.push_scope();
        for need in &func.needs {
            self.insert_capability(&need.name);
        }
        if !func.type_params.is_empty() {
            let mut params = HashSet::new();
            for param in &func.type_params {
                params.insert(param.name.clone());
            }
            self.type_params.push(params);
        }
        let current_params = self.current_type_params().cloned();
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
                .map(|ty| self.type_from_ref_with_env(ty, None, current_params.as_ref()))
                .unwrap_or(SimpleType::Unknown);
            self.bind_local(&param.name, ty);
        }
        self.current_return = func
            .return_ty
            .as_ref()
            .map(|ty| self.type_from_ref_with_env(ty, None, current_params.as_ref()))
            .unwrap_or(SimpleType::Unknown);
        self.current_return_ref = func.return_ty.clone();
        self.return_option_inner = None;
        self.return_result_ok = None;
        self.return_result_err = None;
        self.return_generator_inner = None;
        for stmt in &func.body {
            self.check_stmt(stmt);
        }
        self.current_is_async = false;
        if let SimpleType::Generator(_) = &self.current_return {
            let inner = self
                .return_generator_inner
                .clone()
                .unwrap_or(SimpleType::Unknown);
            self.current_return = SimpleType::Generator(Box::new(inner));
        }
        // Check for missing return statement
        if self.current_return != SimpleType::Unit
            && self.current_return != SimpleType::Unknown
            && !matches!(self.current_return, SimpleType::Generator(_))
        {
            let has_return = Self::block_terminates(&func.body);
            if !has_return {
                self.push_error(
                    format!(
                        "function '{}' declares return type '{}' but not all paths return",
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

    fn stmt_returns(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return { .. } | Stmt::Throw { .. } => true,
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                let then_returns = Self::block_terminates(then_branch);
                let else_returns = else_branch
                    .as_ref()
                    .map(|branch| Self::block_terminates(branch))
                    .unwrap_or(false);
                then_returns && else_returns
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::With { body, .. }
            | Stmt::Block { stmts: body, .. }
            | Stmt::Test { body, .. } => Self::block_terminates(body),
            _ => false,
        }
    }

    fn block_terminates(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            if Self::stmt_returns(stmt) {
                return true;
            }
            if matches!(stmt, Stmt::Break { .. } | Stmt::Continue { .. }) {
                return false;
            }
        }
        false
    }

    fn load_structs(&mut self, module: &Module) {
        self.structs.clear();
        for stmt in &module.stmts {
            if let Stmt::Struct {
                name,
                type_params,
                fields,
                ..
            } = stmt
            {
                if self.structs.contains_key(&name.name) {
                    self.push_error(format!("duplicate struct: {}", name.name), Some(name.span));
                    continue;
                }
                self.validate_type_params(type_params, name.span, "struct");
                let mut seen = HashSet::new();
                for field in fields {
                    if !seen.insert(field.name.name.clone()) {
                        self.push_error(
                            format!("duplicate struct field: {}", field.name.name),
                            Some(field.name.span),
                        );
                    }
                }
                self.structs
                    .insert(name.name.clone(), (type_params.clone(), fields.clone()));
            }
        }
    }

    fn load_enums(&mut self, module: &Module) {
        self.enums.clear();
        for stmt in &module.stmts {
            if let Stmt::Enum {
                name,
                type_params,
                variants,
                ..
            } = stmt
            {
                if self.enums.contains_key(&name.name) {
                    self.push_error(format!("duplicate enum: {}", name.name), Some(name.span));
                    continue;
                }
                self.validate_type_params(type_params, name.span, "enum");
                let mut seen = HashSet::new();
                for variant in variants {
                    if !seen.insert(variant.name.name.clone()) {
                        self.push_error(
                            format!("duplicate enum variant: {}", variant.name.name),
                            Some(variant.name.span),
                        );
                    }
                }
                self.enums
                    .insert(name.name.clone(), (type_params.clone(), variants.clone()));
            }
        }
    }

    fn load_type_aliases(&mut self, module: &Module) {
        self.type_aliases.clear();
        for stmt in &module.stmts {
            if let Stmt::TypeAlias { name, ty, .. } = stmt {
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

    fn current_type_params(&self) -> Option<&HashSet<String>> {
        self.type_params.last()
    }

    fn validate_type_params(&mut self, params: &[Ident], span: Span, kind: &str) {
        let mut seen = HashSet::new();
        for param in params {
            if !seen.insert(param.name.clone()) {
                self.push_error(
                    format!("duplicate {kind} type parameter: {}", param.name),
                    Some(span),
                );
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Import { .. } => {}
            Stmt::TypeAlias { .. } => {}
            Stmt::Enum { .. } => {}
            Stmt::Struct { .. } => {}
            Stmt::Const {
                name, ty, value, ..
            } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                if self.is_local_in_current_scope(&name.name) {
                    self.push_error(format!("duplicate local: {}", name.name), Some(name.span));
                }
                let value_ty = self.check_expr(value);
                let declared = ty
                    .as_ref()
                    .map(|ty| self.type_from_ref_with_env(ty, None, None));
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
                }
                self.bind_const(name);
            }
            Stmt::Let {
                name, ty, value, ..
            } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                if self.is_local_in_current_scope(&name.name) {
                    self.push_error(format!("duplicate local: {}", name.name), Some(name.span));
                }
                let declared = ty
                    .as_ref()
                    .map(|ty| self.type_from_ref_with_env(ty, None, None));
                // When binding a closure to a typed `let`, infer param types
                // from the declared function type so the body is checked with
                // concrete types instead of Unknown.
                let value_ty = if let (
                    Some(SimpleType::Function(ref param_tys, _)),
                    Expr::Closure {
                        params: closure_params,
                        body,
                        ..
                    },
                ) = (&declared, value)
                {
                    let ret =
                        self.infer_closure_return_with_params(closure_params, body, param_tys);
                    SimpleType::Function(param_tys.clone(), Box::new(ret))
                } else {
                    self.check_expr_strict(value)
                };
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
                }
            }
            Stmt::Using {
                name, ty, value, ..
            } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                if self.is_local_in_current_scope(&name.name) {
                    self.push_error(format!("duplicate local: {}", name.name), Some(name.span));
                }
                let declared = ty
                    .as_ref()
                    .map(|ty| self.type_from_ref_with_env(ty, None, None));
                let value_ty = if let (
                    Some(SimpleType::Function(ref param_tys, _)),
                    Expr::Closure {
                        params: closure_params,
                        body,
                        ..
                    },
                ) = (&declared, value)
                {
                    let ret =
                        self.infer_closure_return_with_params(closure_params, body, param_tys);
                    SimpleType::Function(param_tys.clone(), Box::new(ret))
                } else {
                    self.check_expr_strict(value)
                };
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
                }
                self.insert_capability(&name.name);
            }
            Stmt::Set { name, value, .. } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                if self.is_const(name) {
                    self.push_error(
                        format!("cannot assign to const {}", name.name),
                        Some(name.span),
                    );
                }
                let existing = self.resolve_local(name);
                if existing.is_none() {
                    self.push_error(
                        format!("unknown identifier: {}", name.name),
                        Some(name.span),
                    );
                }
                let value_ty = self.check_expr_strict(value);
                if let Some(expected) = existing {
                    if let SimpleType::Option(inner) = &value_ty {
                        if self.last_option_inner.is_none()
                            && !matches!(**inner, SimpleType::Unknown)
                        {
                            self.last_option_inner = Some((**inner).clone());
                        }
                    }
                    if let SimpleType::Result(ok, err) = &value_ty {
                        if self.last_result_ok.is_none() && !matches!(**ok, SimpleType::Unknown) {
                            self.last_result_ok = Some((**ok).clone());
                        }
                        if self.last_result_err.is_none() && !matches!(**err, SimpleType::Unknown) {
                            self.last_result_err = Some((**err).clone());
                        }
                    }
                    let structurally_compatible = (matches!(expected, SimpleType::Option(_))
                        && matches!(value_ty, SimpleType::Option(_)))
                        || (matches!(expected, SimpleType::Result(_, _))
                            && matches!(value_ty, SimpleType::Result(_, _)));
                    if structurally_compatible
                        || self.check_compatible(
                            &expected,
                            &value_ty,
                            &format!("type mismatch for {}", name.name),
                            Some(name.span),
                        )
                    {
                        self.merge_assigned_inners(name, &expected, name.span);
                    }
                }
            }
            Stmt::SetMember { base, value, .. } => {
                if let Expr::Ident(ident) = base {
                    if self.is_const(ident) {
                        self.push_error(
                            format!("cannot assign to const {}", ident.name),
                            Some(ident.span),
                        );
                    }
                }
                let _ = self.check_expr_strict(base);
                self.check_expr_strict(value);
            }
            Stmt::SetIndex {
                base, index, value, ..
            } => {
                if let Expr::Ident(ident) = base {
                    if self.is_const(ident) {
                        self.push_error(
                            format!("cannot assign to const {}", ident.name),
                            Some(ident.span),
                        );
                    }
                }
                let base_ty = self.check_expr_strict(base);
                let index_ty = self.check_expr_strict(index);
                let value_ty = self.check_expr_strict(value);
                match base_ty {
                    SimpleType::Array(inner) => {
                        if index_ty != SimpleType::Int && !matches!(index_ty, SimpleType::Unknown) {
                            self.push_error(
                                format!("set index expects int, got {}", format_type(&index_ty)),
                                expr_span(index),
                            );
                        }
                        self.check_compatible(
                            &inner,
                            &value_ty,
                            "set index type mismatch",
                            expr_span(value),
                        );
                    }
                    SimpleType::Tuple(_) => {
                        if index_ty != SimpleType::Int && !matches!(index_ty, SimpleType::Unknown) {
                            self.push_error(
                                format!("set index expects int, got {}", format_type(&index_ty)),
                                expr_span(index),
                            );
                        }
                    }
                    SimpleType::Map(key, inner) => {
                        if !Self::types_compatible(&key, &index_ty) {
                            self.push_error(
                                format!(
                                    "map index expects {}, got {}",
                                    format_type(&key),
                                    format_type(&index_ty)
                                ),
                                expr_span(index),
                            );
                        }
                        self.check_compatible(
                            &inner,
                            &value_ty,
                            "set map value type mismatch",
                            expr_span(value),
                        );
                    }
                    SimpleType::Unknown => {}
                    _ => {
                        self.push_error(
                            format!(
                                "set index expects array, tuple, or map, got {}",
                                format_type(&base_ty)
                            ),
                            expr_span(base),
                        );
                    }
                }
            }
            Stmt::While {
                while_span,
                condition,
                body,
                ..
            } => {
                let cond_ty = self.check_expr_strict(condition);
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
                self.check_loop_body_fixpoint(body);
                self.pop_scope();
                self.loop_depth -= 1;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.check_expr_strict(condition);
                self.push_scope();
                for stmt in then_branch {
                    self.check_stmt(stmt);
                }
                self.pop_scope();
                if let Some(else_branch) = else_branch {
                    self.push_scope();
                    for stmt in else_branch {
                        self.check_stmt(stmt);
                    }
                    self.pop_scope();
                }
            }
            Stmt::For {
                for_span,
                item,
                iter,
                body,
                ..
            } => {
                let iter_ty = self.check_expr_strict(iter);
                if !matches!(iter_ty, SimpleType::Array(_)) && iter_ty != SimpleType::Unknown {
                    self.push_error(
                        format!("for expects array, got {}", format_type(&iter_ty)),
                        Some(*for_span),
                    );
                }
                self.loop_depth += 1;
                self.push_scope();
                let item_ty = match iter_ty {
                    SimpleType::Array(inner) => (*inner).clone(),
                    _ => SimpleType::Unknown,
                };
                self.bind_local(item, item_ty);
                for stmt in body {
                    self.check_stmt(stmt);
                }
                self.pop_scope();
                self.loop_depth -= 1;
            }
            Stmt::Break { break_span, .. } => {
                if self.loop_depth == 0 {
                    self.push_error("break used outside of loop".to_string(), Some(*break_span));
                }
            }
            Stmt::Continue { continue_span, .. } => {
                if self.loop_depth == 0 {
                    self.push_error(
                        "continue used outside of loop".to_string(),
                        Some(*continue_span),
                    );
                }
            }
            Stmt::Expr { expr, .. } => {
                self.check_expr_strict(expr);
            }
            Stmt::Return { expr, .. } => match (expr, self.current_return.clone()) {
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
                    let found = self.check_expr_strict(expr);
                    if matches!(found, SimpleType::Result(_, _)) {
                        self.current_return = found.clone();
                        self.update_return_result_inners(expr_span(expr));
                        self.update_return_from_ident(expr, expr_span(expr));
                    } else if matches!(found, SimpleType::Option(_)) {
                        self.current_return = found.clone();
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
                    let found = self.check_expr_strict(expr);
                    self.check_compatible(
                        &expected,
                        &found,
                        "return type mismatch",
                        expr_span(expr),
                    );
                    if matches!(expected, SimpleType::Result(_, _)) {
                        self.update_return_result_inners(expr_span(expr));
                        self.update_return_from_ident(expr, expr_span(expr));
                    }
                    if matches!(expected, SimpleType::Option(_)) {
                        self.update_return_option_inner(expr_span(expr));
                        self.update_return_from_ident(expr, expr_span(expr));
                    }
                }
            },
            Stmt::Throw { expr, .. } => {
                let ty = self.check_expr_strict(expr);
                if !matches!(ty, SimpleType::Result(_, _)) && ty != SimpleType::Unknown {
                    self.push_error(
                        format!("throw expects result, got {}", format_type(&ty)),
                        expr_span(expr),
                    );
                }
                if self.current_return == SimpleType::Unknown {
                    self.current_return = SimpleType::Result(
                        Box::new(SimpleType::Unknown),
                        Box::new(SimpleType::Unknown),
                    );
                }
                if matches!(self.current_return, SimpleType::Result(_, _)) {
                    self.last_result_ok = None;
                    self.last_result_err = None;
                    self.infer_inner_from_expr(expr, &ty);
                    self.update_return_result_inners(expr_span(expr));
                }
            }
            Stmt::Defer { expr, .. } => {
                self.check_expr_strict(expr);
            }
            Stmt::With {
                name, value, body, ..
            } => {
                self.last_option_inner = None;
                self.last_result_ok = None;
                self.last_result_err = None;
                if self.is_local_in_current_scope(&name.name) {
                    self.push_error(format!("duplicate local: {}", name.name), Some(name.span));
                }
                self.push_scope();
                let value_ty = self.check_expr_strict(value);
                self.bind_or_refine_local(name, value_ty.clone());
                self.infer_inner_from_expr(value, &value_ty);
                self.insert_capability(&name.name);
                for stmt in body {
                    self.check_stmt(stmt);
                }
                self.pop_scope();
            }
            Stmt::Yield { expr, .. } => {
                let value_ty = self.check_expr_strict(expr);
                if matches!(self.current_return, SimpleType::Unknown) {
                    self.current_return = SimpleType::Generator(Box::new(SimpleType::Unknown));
                }
                let mut effective_return = self.current_return.clone();
                if matches!(effective_return, SimpleType::Unknown) {
                    effective_return = SimpleType::Generator(Box::new(SimpleType::Unknown));
                    self.current_return = effective_return.clone();
                }
                if let SimpleType::Generator(inner) = effective_return {
                    let existing = self.return_generator_inner.clone();
                    match existing {
                        Some(existing) => {
                            if !Self::types_compatible(&existing, &value_ty)
                                && !matches!(value_ty, SimpleType::Unknown)
                            {
                                self.push_error(
                                    format!(
                                        "yield type mismatch: expected {}, got {}",
                                        format_type(&existing),
                                        format_type(&value_ty)
                                    ),
                                    expr_span(expr),
                                );
                            }
                        }
                        None => {
                            self.return_generator_inner = Some(value_ty.clone());
                        }
                    }
                    if matches!(*inner, SimpleType::Unknown)
                        && !matches!(value_ty, SimpleType::Unknown)
                    {
                        self.current_return = SimpleType::Generator(Box::new(value_ty));
                    }
                } else {
                    self.push_error(
                        "yield requires generator return type".to_string(),
                        expr_span(expr),
                    );
                }
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
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
            Expr::Int(_, _, _) => SimpleType::Int,
            Expr::Float(_, _, _) => SimpleType::Float,
            Expr::String(_, _, _) => SimpleType::String,
            Expr::Bool(_, _, _) => SimpleType::Bool,
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
                if let SimpleType::Option(inner) = &ty {
                    self.last_option_inner = Some((**inner).clone());
                }
                if let SimpleType::Result(ok, err) = &ty {
                    self.last_result_ok = Some((**ok).clone());
                    self.last_result_err = Some((**err).clone());
                }
                ty
            }
            Expr::Binary {
                left,
                op,
                op_span,
                right,
                ..
            } => self.check_binary(left, *op, *op_span, right),
            Expr::Ternary {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_ty = self.check_expr(condition);
                if cond_ty != SimpleType::Bool
                    && cond_ty != SimpleType::Int
                    && cond_ty != SimpleType::Unknown
                {
                    self.push_error(
                        format!("ternary expects bool or int, got {}", format_type(&cond_ty)),
                        expr_span(condition),
                    );
                }
                let then_ty = self.check_expr(then_branch);
                let else_ty = self.check_expr(else_branch);
                if Self::types_compatible(&then_ty, &else_ty) {
                    then_ty
                } else if Self::types_compatible(&else_ty, &then_ty)
                    || then_ty == SimpleType::Unknown
                {
                    else_ty
                } else if else_ty == SimpleType::Unknown {
                    then_ty
                } else {
                    self.push_error(
                        format!(
                            "ternary branch mismatch: {} vs {}",
                            format_type(&then_ty),
                            format_type(&else_ty)
                        ),
                        expr_span(then_branch),
                    );
                    SimpleType::Unknown
                }
            }
            Expr::ChainedComparison { items, .. } => {
                for item in items {
                    let ty = self.check_expr(item);
                    if ty != SimpleType::Int && ty != SimpleType::Float && ty != SimpleType::Unknown
                    {
                        self.push_error(
                            format!("comparison expects number, got {}", format_type(&ty)),
                            expr_span(item),
                        );
                    }
                }
                SimpleType::Bool
            }
            Expr::Unary {
                op, op_span, expr, ..
            } => self.check_unary(*op, *op_span, expr),
            Expr::If {
                if_span,
                condition,
                then_branch,
                else_branch,
                ..
            } => self.check_if(*if_span, condition, then_branch, else_branch.as_deref()),
            Expr::Member { base, name, .. } => {
                let base_ty = self.check_expr(base);
                if let SimpleType::Array(inner) = &base_ty {
                    if name.name == "len" {
                        return SimpleType::Int;
                    }
                    if name.name == "first" {
                        return SimpleType::Option(Box::new((**inner).clone()));
                    }
                    if name.name == "last" {
                        return SimpleType::Option(Box::new((**inner).clone()));
                    }
                }
                if let SimpleType::Tuple(items) = &base_ty {
                    if name.name == "len" {
                        return SimpleType::Int;
                    }
                    if name.name == "first" {
                        let inner = items.first().cloned().unwrap_or(SimpleType::Unknown);
                        return SimpleType::Option(Box::new(inner));
                    }
                    if name.name == "last" {
                        let inner = items.last().cloned().unwrap_or(SimpleType::Unknown);
                        return SimpleType::Option(Box::new(inner));
                    }
                }
                if let SimpleType::String = &base_ty {
                    if name.name == "len" {
                        return SimpleType::Int;
                    }
                }
                if let SimpleType::Map(_, value) = &base_ty {
                    if name.name == "len" {
                        return SimpleType::Int;
                    }
                    if name.name == "values" {
                        return SimpleType::Array(Box::new((**value).clone()));
                    }
                }
                if let SimpleType::Custom(struct_name, args) = base_ty {
                    if let Some((type_params, fields)) = self.structs.get(&struct_name).cloned() {
                        let param_names = self.type_param_set(&type_params);
                        let env = self.type_env_from_args(&type_params, &args, Some(name.span));
                        let field_ty = fields
                            .iter()
                            .find(|field| field.name.name == name.name)
                            .map(|field| field.ty.clone());
                        if let Some(field_ty) = field_ty {
                            return self.type_from_ref_with_env(
                                &field_ty,
                                Some(&env),
                                Some(&param_names),
                            );
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
            Expr::Call { callee, args, .. } => self.check_call(callee, args),
            Expr::Try(expr, _) => {
                let expects_result = matches!(self.current_return, SimpleType::Result(_, _))
                    || matches!(
                        self.current_return_ref,
                        Some(TypeRef::Named { ref name, .. }) if name.name == "result"
                    );
                if !expects_result {
                    self.push_error(
                        "? requires function returning result".to_string(),
                        expr_span(expr),
                    );
                }
                let inner = self.check_expr(expr);
                match &inner {
                    SimpleType::Result(ok, err) => {
                        if let SimpleType::Result(_, expected_err) = &self.current_return {
                            if !Self::types_compatible(expected_err, err)
                                && !matches!(**expected_err, SimpleType::Unknown)
                            {
                                self.push_error(
                                    format!(
                                        "? error type mismatch: expected {}, got {}",
                                        format_type(expected_err),
                                        format_type(err)
                                    ),
                                    expr_span(expr),
                                );
                            }
                        }
                        (**ok).clone()
                    }
                    SimpleType::Unknown => {
                        self.last_result_ok.clone().unwrap_or(SimpleType::Unknown)
                    }
                    _ => {
                        self.push_error(
                            format!("? expects result, got {}", format_type(&inner)),
                            expr_span(expr),
                        );
                        SimpleType::Unknown
                    }
                }
            }
            Expr::Await { expr, .. } => {
                if !self.current_is_async {
                    self.push_error("await requires async fn".to_string(), expr_span(expr));
                }
                let awaited = self.check_expr(expr);
                match awaited {
                    SimpleType::Future(inner) => (*inner).clone(),
                    SimpleType::Unknown => SimpleType::Unknown,
                    other => {
                        self.push_error(
                            format!("await expects future, got {}", format_type(&other)),
                            expr_span(expr),
                        );
                        SimpleType::Unknown
                    }
                }
            }
            Expr::TryCatch {
                try_block,
                catch_block,
                finally_block,
                ..
            } => {
                let try_ty = self.check_expr(try_block);
                let catch_block = match catch_block {
                    Some(catch_block) => catch_block,
                    None => {
                        self.push_error("try requires catch".to_string(), expr_span(try_block));
                        if let Some(finally_block) = finally_block {
                            let _ = self.check_expr(finally_block);
                        }
                        return SimpleType::Unknown;
                    }
                };
                let catch_ty = self.check_expr(catch_block);
                if let Some(finally_block) = finally_block {
                    let _ = self.check_expr(finally_block);
                }
                match try_ty {
                    SimpleType::Result(ok, _) => {
                        if !matches!(catch_ty, SimpleType::Unknown)
                            && !Self::types_compatible(&ok, &catch_ty)
                        {
                            self.push_error(
                                format!(
                                    "try/catch type mismatch: try is {}, catch is {}",
                                    format_type(&ok),
                                    format_type(&catch_ty)
                                ),
                                expr_span(catch_block),
                            );
                        }
                        (*ok).clone()
                    }
                    SimpleType::Unknown => SimpleType::Unknown,
                    other => {
                        self.push_error(
                            format!("try expects result, got {}", format_type(&other)),
                            expr_span(try_block),
                        );
                        SimpleType::Unknown
                    }
                }
            }
            Expr::Match {
                match_span,
                value,
                arms,
                ..
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
                let mut array_inner = SimpleType::Unknown;
                for item in items {
                    let item_ty = self.check_expr(item);
                    if matches!(item_ty, SimpleType::Array(_)) {
                        if let SimpleType::Array(inner) = item_ty {
                            let item_inner = (*inner).clone();
                            if matches!(array_inner, SimpleType::Unknown) {
                                array_inner = item_inner;
                            } else if !matches!(item_inner, SimpleType::Unknown)
                                && !Self::types_compatible(&array_inner, &item_inner)
                            {
                                self.push_error(
                                    format!(
                                        "array element type mismatch: expected {}, got {}",
                                        format_type(&array_inner),
                                        format_type(&item_inner)
                                    ),
                                    expr_span(item),
                                );
                            }
                        }
                        continue;
                    }
                    if matches!(array_inner, SimpleType::Unknown) {
                        array_inner = item_ty;
                    } else if !matches!(item_ty, SimpleType::Unknown)
                        && !Self::types_compatible(&array_inner, &item_ty)
                    {
                        self.push_error(
                            format!(
                                "array element type mismatch: expected {}, got {}",
                                format_type(&array_inner),
                                format_type(&item_ty)
                            ),
                            expr_span(item),
                        );
                    }
                }
                SimpleType::Array(Box::new(array_inner))
            }
            Expr::Index { base, index, .. } => {
                let base_ty = self.check_expr(base);
                let index_ty = self.check_expr(index);
                match base_ty {
                    SimpleType::Array(inner) => {
                        if index_ty != SimpleType::Int && !matches!(index_ty, SimpleType::Unknown) {
                            self.push_error(
                                format!("index expects int, got {}", format_type(&index_ty)),
                                expr_span(index),
                            );
                        }
                        (*inner).clone()
                    }
                    SimpleType::Tuple(items) => {
                        if index_ty != SimpleType::Int && !matches!(index_ty, SimpleType::Unknown) {
                            self.push_error(
                                format!("index expects int, got {}", format_type(&index_ty)),
                                expr_span(index),
                            );
                        }
                        if let Expr::Int(value, _, _) = index.as_ref() {
                            if *value < 0 {
                                self.push_error(
                                    format!("tuple index must be non-negative, got {}", value),
                                    expr_span(index),
                                );
                                return SimpleType::Unknown;
                            }
                            let idx = *value as usize;
                            if let Some(item_ty) = items.get(idx) {
                                item_ty.clone()
                            } else {
                                self.push_error(
                                    format!(
                                        "tuple index {} out of bounds (len {})",
                                        idx,
                                        items.len()
                                    ),
                                    expr_span(index),
                                );
                                SimpleType::Unknown
                            }
                        } else {
                            SimpleType::Unknown
                        }
                    }
                    SimpleType::Map(key, value) => {
                        if !Self::types_compatible(&key, &index_ty) {
                            self.push_error(
                                format!(
                                    "map index expects {}, got {}",
                                    format_type(&key),
                                    format_type(&index_ty)
                                ),
                                expr_span(index),
                            );
                        }
                        (*value).clone()
                    }
                    SimpleType::Unknown => SimpleType::Unknown,
                    _ => {
                        if index_ty != SimpleType::Int && !matches!(index_ty, SimpleType::Unknown) {
                            self.push_error(
                                format!("index expects int, got {}", format_type(&index_ty)),
                                expr_span(index),
                            );
                        }
                        self.push_error(
                            format!("index expects array, got {}", format_type(&base_ty)),
                            expr_span(base),
                        );
                        SimpleType::Unknown
                    }
                }
            }
            Expr::Tuple { items, .. } => {
                let mut tys = Vec::new();
                for item in items {
                    tys.push(self.check_expr(item));
                }
                SimpleType::Tuple(tys)
            }
            Expr::MapLiteral { entries, .. } => {
                let mut key_ty = SimpleType::Unknown;
                let mut value_ty = SimpleType::Unknown;
                for entry in entries {
                    match entry {
                        MapEntry::Spread(spread_expr) => {
                            let found = self.check_expr(spread_expr);
                            match found {
                                SimpleType::Map(inner_key, inner_value) => {
                                    let found_key = (*inner_key).clone();
                                    let found_value = (*inner_value).clone();
                                    if matches!(key_ty, SimpleType::Unknown) {
                                        key_ty = found_key.clone();
                                    } else if !matches!(found_key, SimpleType::Unknown)
                                        && !Self::types_compatible(&key_ty, &found_key)
                                    {
                                        self.push_error(
                                            format!(
                                                "map key type mismatch: expected {}, got {}",
                                                format_type(&key_ty),
                                                format_type(&found_key)
                                            ),
                                            expr_span(spread_expr),
                                        );
                                    }
                                    if matches!(value_ty, SimpleType::Unknown) {
                                        value_ty = found_value.clone();
                                    } else if !matches!(found_value, SimpleType::Unknown)
                                        && !Self::types_compatible(&value_ty, &found_value)
                                    {
                                        self.push_error(
                                            format!(
                                                "map value type mismatch: expected {}, got {}",
                                                format_type(&value_ty),
                                                format_type(&found_value)
                                            ),
                                            expr_span(spread_expr),
                                        );
                                    }
                                }
                                SimpleType::Unknown => {}
                                other => {
                                    self.push_error(
                                        format!("spread expects map, got {}", format_type(&other)),
                                        expr_span(spread_expr),
                                    );
                                }
                            }
                        }
                        MapEntry::KeyValue { key, value } => {
                            let found_key = self.check_expr(key);
                            if matches!(key_ty, SimpleType::Unknown) {
                                key_ty = found_key;
                            } else if !matches!(found_key, SimpleType::Unknown)
                                && !Self::types_compatible(&key_ty, &found_key)
                            {
                                self.push_error(
                                    format!(
                                        "map key type mismatch: expected {}, got {}",
                                        format_type(&key_ty),
                                        format_type(&found_key)
                                    ),
                                    expr_span(key),
                                );
                            }

                            let found_value = self.check_expr(value);
                            if matches!(value_ty, SimpleType::Unknown) {
                                value_ty = found_value;
                            } else if !matches!(found_value, SimpleType::Unknown)
                                && !Self::types_compatible(&value_ty, &found_value)
                            {
                                self.push_error(
                                    format!(
                                        "map value type mismatch: expected {}, got {}",
                                        format_type(&value_ty),
                                        format_type(&found_value)
                                    ),
                                    expr_span(value),
                                );
                            }
                        }
                    }
                }
                SimpleType::Map(Box::new(key_ty), Box::new(value_ty))
            }
            Expr::ArraySpread { expr, .. } => {
                let found = self.check_expr(expr);
                match found {
                    SimpleType::Array(inner) => SimpleType::Array(inner),
                    SimpleType::Tuple(items) => {
                        // Join all tuple element types into a single array element type.
                        let mut joined = SimpleType::Unknown;
                        for item in items {
                            joined = self.join_types(&joined, &item);
                        }
                        SimpleType::Array(Box::new(joined))
                    }
                    SimpleType::Unknown => SimpleType::Unknown,
                    other => {
                        self.push_error(
                            format!("spread expects array or tuple, got {}", format_type(&other)),
                            expr_span(expr),
                        );
                        SimpleType::Unknown
                    }
                }
            }
            Expr::As { expr, ty, .. } => {
                let _ = self.check_expr(expr);
                self.type_from_ref_with_env(ty, None, None)
            }
            Expr::Is { expr, ty, .. } => {
                let _ = self.check_expr(expr);
                self.validate_type_ref(ty);
                SimpleType::Bool
            }
            Expr::Range { start, end, .. } => {
                let start_ty = self.check_expr(start);
                self.check_compatible(
                    &SimpleType::Int,
                    &start_ty,
                    "range start expects int",
                    expr_span(start),
                );
                let end_ty = self.check_expr(end);
                self.check_compatible(
                    &SimpleType::Int,
                    &end_ty,
                    "range end expects int",
                    expr_span(end),
                );
                SimpleType::Array(Box::new(SimpleType::Int))
            }
            Expr::InterpolatedString { parts, .. } => {
                for part in parts {
                    if let at_syntax::InterpPart::Expr(expr, _) = part {
                        self.check_expr(expr);
                    }
                }
                SimpleType::String
            }
            Expr::Closure { params, body, .. } => {
                self.push_scope();
                let mut param_tys = Vec::new();
                for param in params {
                    let ty = SimpleType::Unknown;
                    self.bind_local(param, ty.clone());
                    param_tys.push(ty);
                }
                let return_ty = self.check_expr(body);
                self.pop_scope();
                SimpleType::Function(param_tys, Box::new(return_ty))
            }
            Expr::EnumLiteral {
                name,
                variant,
                payload,
                ..
            } => {
                let enum_variants = self.enums.get(&name.name).cloned();
                if enum_variants.is_none() {
                    self.push_error(format!("unknown enum: {}", name.name), Some(name.span));
                }
                let mut env = HashMap::new();
                let mut param_names = HashSet::new();
                if let Some((type_params, _)) = enum_variants.as_ref() {
                    param_names = self.type_param_set(type_params);
                    env = self.type_env_from_args(type_params, &[], Some(name.span));
                }
                if let Some((_, enum_variants)) = enum_variants {
                    if let Some(expected) = enum_variants
                        .iter()
                        .find(|entry| entry.name.name == variant.name)
                    {
                        match (&expected.payload, payload) {
                            (Some(expected_ty), Some(expr)) => {
                                let value_ty = self.check_expr(expr);
                                if let Some(param_name) =
                                    Self::infer_type_param_name(expected_ty, &param_names)
                                {
                                    let entry =
                                        env.entry(param_name).or_insert(SimpleType::Unknown);
                                    if matches!(entry, SimpleType::Unknown) {
                                        *entry = value_ty.clone();
                                    } else {
                                        self.check_compatible(
                                            entry,
                                            &value_ty,
                                            &format!(
                                                "type mismatch for enum {}::{}",
                                                name.name, variant.name
                                            ),
                                            Some(variant.span),
                                        );
                                    }
                                }
                                let expected_ty = self.type_from_ref_with_env(
                                    expected_ty,
                                    Some(&env),
                                    Some(&param_names),
                                );
                                self.check_compatible(
                                    &expected_ty,
                                    &value_ty,
                                    &format!(
                                        "type mismatch for enum {}::{}",
                                        name.name, variant.name
                                    ),
                                    Some(variant.span),
                                );
                            }
                            (None, Some(_)) => {
                                self.push_error(
                                    format!(
                                        "enum {}::{} does not take a value",
                                        name.name, variant.name
                                    ),
                                    Some(variant.span),
                                );
                            }
                            (Some(_), None) => {
                                self.push_error(
                                    format!("enum {}::{} expects a value", name.name, variant.name),
                                    Some(variant.span),
                                );
                            }
                            (None, None) => {}
                        }
                    } else {
                        self.push_error(
                            format!("unknown variant {}::{}", name.name, variant.name),
                            Some(variant.span),
                        );
                    }
                } else if let Some(expr) = payload {
                    self.check_expr(expr);
                }
                let mut enum_args = Vec::new();
                if !param_names.is_empty() {
                    if let Some((params, _)) = self.enums.get(&name.name) {
                        for param in params {
                            let ty = env.get(&param.name).cloned().unwrap_or(SimpleType::Unknown);
                            enum_args.push(ty);
                        }
                    }
                }
                SimpleType::Custom(name.name.clone(), enum_args)
            }
            Expr::Group { expr, .. } => self.check_expr(expr),
            Expr::StructLiteral { name, fields, .. } => {
                let struct_fields = self.structs.get(&name.name).cloned();
                if struct_fields.is_none() {
                    self.push_error(format!("unknown struct: {}", name.name), Some(name.span));
                }
                let mut provided = HashSet::new();
                let mut struct_args = Vec::new();
                let mut param_names = HashSet::new();
                let mut env = HashMap::new();
                if let Some((type_params, _)) = struct_fields.as_ref() {
                    param_names = self.type_param_set(type_params);
                    env = self.type_env_from_args(type_params, &[], Some(name.span));
                }
                for field in fields {
                    if !provided.insert(field.name.name.clone()) {
                        self.push_error(
                            format!("duplicate field: {}", field.name.name),
                            Some(field.name.span),
                        );
                    }
                    let value_ty = self.check_expr(&field.value);
                    if let Some((_, struct_fields)) = struct_fields.as_ref() {
                        if let Some(expected) = struct_fields
                            .iter()
                            .find(|entry| entry.name.name == field.name.name)
                        {
                            if let Some(param_name) =
                                Self::infer_type_param_name(&expected.ty, &param_names)
                            {
                                let entry = env.entry(param_name).or_insert(SimpleType::Unknown);
                                if matches!(entry, SimpleType::Unknown) {
                                    *entry = value_ty.clone();
                                } else {
                                    self.check_compatible(
                                        entry,
                                        &value_ty,
                                        &format!("type mismatch for field {}", field.name.name),
                                        Some(field.name.span),
                                    );
                                }
                            }
                            let expected_ty = self.type_from_ref_with_env(
                                &expected.ty,
                                Some(&env),
                                Some(&param_names),
                            );
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
                if let Some((_, struct_fields)) = struct_fields.as_ref() {
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
                if !param_names.is_empty() {
                    if let Some((params, _)) = struct_fields.as_ref() {
                        for param in params {
                            let ty = env.get(&param.name).cloned().unwrap_or(SimpleType::Unknown);
                            struct_args.push(ty);
                        }
                    }
                }
                SimpleType::Custom(name.name.clone(), struct_args)
            }
        }
    }

    fn check_expr_strict(&mut self, expr: &Expr) -> SimpleType {
        let ty = self.check_expr(expr);
        if self.mode == TypecheckMode::Strict {
            if matches!(ty, SimpleType::Unknown) {
                self.push_error(
                    "type is unknown in strict mode".to_string(),
                    expr_span(expr),
                );
            }
            if let SimpleType::Array(inner) = &ty {
                if matches!(inner.as_ref(), SimpleType::Unknown) {
                    self.push_error(
                        "array element type is unknown in strict mode".to_string(),
                        expr_span(expr),
                    );
                }
            }
            if let SimpleType::Option(inner) = &ty {
                if matches!(inner.as_ref(), SimpleType::Unknown) {
                    self.push_error(
                        "option inner type is unknown in strict mode".to_string(),
                        expr_span(expr),
                    );
                }
            }
            if let SimpleType::Result(ok, err) = &ty {
                if matches!(ok.as_ref(), SimpleType::Unknown)
                    || matches!(err.as_ref(), SimpleType::Unknown)
                {
                    self.push_error(
                        "result inner type is unknown in strict mode".to_string(),
                        expr_span(expr),
                    );
                }
            }
            if let SimpleType::Function(params, ret) = &ty {
                if params
                    .iter()
                    .any(|param| matches!(param, SimpleType::Unknown))
                    || matches!(ret.as_ref(), SimpleType::Unknown)
                {
                    self.push_error(
                        "function type contains unknown in strict mode".to_string(),
                        expr_span(expr),
                    );
                }
            }
        }
        ty
    }

    fn infer_closure_return_with_params(
        &mut self,
        params: &[Ident],
        body: &Expr,
        inferred_params: &[SimpleType],
    ) -> SimpleType {
        self.push_scope();
        for (index, param) in params.iter().enumerate() {
            let ty = inferred_params
                .get(index)
                .cloned()
                .unwrap_or(SimpleType::Unknown);
            self.bind_local(param, ty);
        }
        let return_ty = self.check_expr(body);
        self.pop_scope();
        return_ty
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
                } else if (left_ty == SimpleType::Int || left_ty == SimpleType::Float)
                    && right_ty == SimpleType::Float
                    || left_ty == SimpleType::Float && right_ty == SimpleType::Int
                {
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
                } else if (left_ty == SimpleType::Int || left_ty == SimpleType::Float)
                    && right_ty == SimpleType::Float
                    || left_ty == SimpleType::Float && right_ty == SimpleType::Int
                {
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
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr => {
                if left_ty == SimpleType::Int && right_ty == SimpleType::Int {
                    SimpleType::Int
                } else if left_ty == SimpleType::Unknown || right_ty == SimpleType::Unknown {
                    SimpleType::Unknown
                } else {
                    self.push_error(
                        format!(
                            "operator expects int, got {} and {}",
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
                let is_numeric = matches!(
                    (&left_ty, &right_ty),
                    (SimpleType::Int, SimpleType::Int)
                        | (SimpleType::Float, SimpleType::Float)
                        | (SimpleType::Int, SimpleType::Float)
                        | (SimpleType::Float, SimpleType::Int)
                );
                if !is_numeric && left_ty != SimpleType::Unknown && right_ty != SimpleType::Unknown
                {
                    self.push_error(
                        format!(
                            "comparison expects int or float, got {} and {}",
                            format_type(&left_ty),
                            format_type(&right_ty)
                        ),
                        Some(op_span),
                    );
                }
                SimpleType::Bool
            }
            BinaryOp::Eq | BinaryOp::Neq => {
                if left_ty != SimpleType::Unknown
                    && right_ty != SimpleType::Unknown
                    && !Self::types_compatible(&left_ty, &right_ty)
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

    fn types_compatible(expected: &SimpleType, found: &SimpleType) -> bool {
        if expected == found {
            return true;
        }
        if matches!(expected, SimpleType::Float) && matches!(found, SimpleType::Int) {
            return true;
        }
        if matches!(expected, SimpleType::Unknown) || matches!(found, SimpleType::Unknown) {
            return true;
        }
        if matches!(expected, SimpleType::TypeParam(_)) || matches!(found, SimpleType::TypeParam(_))
        {
            return true;
        }
        match (expected, found) {
            (SimpleType::Array(left), SimpleType::Array(right)) => {
                Self::types_compatible(left, right)
            }
            (SimpleType::Option(left), SimpleType::Option(right)) => {
                Self::types_compatible(left, right)
            }
            (SimpleType::Result(ok_left, err_left), SimpleType::Result(ok_right, err_right)) => {
                Self::types_compatible(ok_left, ok_right)
                    && Self::types_compatible(err_left, err_right)
            }
            (SimpleType::Union(left), other) => {
                left.iter().any(|ty| Self::types_compatible(ty, other))
            }
            (other, SimpleType::Union(right)) => {
                right.iter().any(|ty| Self::types_compatible(other, ty))
            }
            (SimpleType::Intersection(left), other) => {
                left.iter().all(|ty| Self::types_compatible(ty, other))
            }
            (other, SimpleType::Intersection(right)) => {
                right.iter().all(|ty| Self::types_compatible(other, ty))
            }
            (SimpleType::Map(left_key, left_val), SimpleType::Map(right_key, right_val)) => {
                Self::types_compatible(left_key, right_key)
                    && Self::types_compatible(left_val, right_val)
            }
            (SimpleType::Future(left), SimpleType::Future(right)) => {
                Self::types_compatible(left, right)
            }
            (SimpleType::Tuple(left), SimpleType::Tuple(right)) => {
                left.len() == right.len()
                    && left
                        .iter()
                        .zip(right.iter())
                        .all(|(l, r)| Self::types_compatible(l, r))
            }
            (
                SimpleType::Function(params_left, ret_left),
                SimpleType::Function(params_right, ret_right),
            ) => {
                params_left.len() == params_right.len()
                    && params_left
                        .iter()
                        .zip(params_right.iter())
                        .all(|(left, right)| Self::types_compatible(left, right))
                    && Self::types_compatible(ret_left, ret_right)
            }
            (SimpleType::Custom(left, left_args), SimpleType::Custom(right, right_args)) => {
                left == right
                    && left_args.len() == right_args.len()
                    && left_args
                        .iter()
                        .zip(right_args.iter())
                        .all(|(left, right)| Self::types_compatible(left, right))
            }
            _ => false,
        }
    }

    fn check_if(
        &mut self,
        if_span: Span,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
    ) -> SimpleType {
        let option_narrow = self
            .extract_option_narrow(condition, true)
            .or_else(|| self.extract_option_narrow(condition, false));
        let result_narrow = self
            .extract_result_narrow(condition, true)
            .or_else(|| self.extract_result_narrow(condition, false));
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
        self.push_scope();
        if let Some(narrow) = option_narrow.clone() {
            if narrow.then_branch {
                self.bind_or_refine_local(
                    &narrow.ident,
                    SimpleType::Option(Box::new(narrow.inner.clone())),
                );
            }
        }
        if let Some(narrow) = result_narrow.clone() {
            if narrow.then_branch {
                self.bind_or_refine_local(
                    &narrow.ident,
                    SimpleType::Result(Box::new(narrow.ok.clone()), Box::new(narrow.err.clone())),
                );
            }
        }
        let then_ty = self.check_expr(then_branch);
        self.pop_scope();
        let then_option = self.last_option_inner.clone();
        let then_ok = self.last_result_ok.clone();
        let then_err = self.last_result_err.clone();

        self.last_option_inner = None;
        self.last_result_ok = None;
        self.last_result_err = None;
        self.push_scope();
        if let Some(narrow) = option_narrow.clone() {
            if !narrow.then_branch {
                self.bind_or_refine_local(
                    &narrow.ident,
                    SimpleType::Option(Box::new(narrow.inner.clone())),
                );
            }
        }
        if let Some(narrow) = result_narrow.clone() {
            if !narrow.then_branch {
                self.bind_or_refine_local(
                    &narrow.ident,
                    SimpleType::Result(Box::new(narrow.ok.clone()), Box::new(narrow.err.clone())),
                );
            }
        }
        let else_ty = if let Some(else_expr) = else_branch {
            self.check_expr(else_expr)
        } else {
            SimpleType::Unit
        };
        self.pop_scope();
        let else_option = self.last_option_inner.clone();
        let else_ok = self.last_result_ok.clone();
        let else_err = self.last_result_err.clone();
        let then_inners = BranchInners {
            option: then_option,
            ok: then_ok,
            err: then_err,
        };
        let else_inners = BranchInners {
            option: else_option,
            ok: else_ok,
            err: else_err,
        };
        if Self::types_compatible(&then_ty, &else_ty) {
            self.merge_if_inners(if_span, &then_ty, then_inners, else_inners);
            then_ty
        } else if then_ty == SimpleType::Unknown {
            self.merge_if_inners(if_span, &else_ty, then_inners, else_inners);
            else_ty
        } else if else_ty == SimpleType::Unknown {
            self.merge_if_inners(if_span, &then_ty, then_inners, else_inners);
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
        then_inners: BranchInners,
        else_inners: BranchInners,
    ) {
        self.last_option_inner = None;
        self.last_result_ok = None;
        self.last_result_err = None;
        match ty {
            SimpleType::Option(_) => {
                self.last_option_inner = self.merge_inner(
                    then_inners.option,
                    else_inners.option,
                    "if option inner mismatch",
                    Some(span),
                );
            }
            SimpleType::Result(_, _) => {
                self.last_result_ok = self.merge_inner(
                    then_inners.ok,
                    else_inners.ok,
                    "if result ok inner mismatch",
                    Some(span),
                );
                self.last_result_err = self.merge_inner(
                    then_inners.err,
                    else_inners.err,
                    "if result err inner mismatch",
                    Some(span),
                );
            }
            _ => {}
        }
    }

    fn extract_option_narrow(&mut self, condition: &Expr, for_then: bool) -> Option<OptionNarrow> {
        if let Expr::Binary {
            op, left, right, ..
        } = condition
        {
            if for_then && *op == BinaryOp::And {
                return self
                    .extract_option_narrow(left, true)
                    .or_else(|| self.extract_option_narrow(right, true));
            }
            if !for_then && *op == BinaryOp::Or {
                return self
                    .extract_option_narrow(left, false)
                    .or_else(|| self.extract_option_narrow(right, false));
            }
        }

        let mut call_expr = condition;
        let mut is_some = None;
        if let Expr::Unary {
            op: UnaryOp::Not,
            expr,
            ..
        } = condition
        {
            call_expr = expr.as_ref();
            is_some = Some(false);
        }

        let Expr::Call { callee, args, .. } = call_expr else {
            return None;
        };
        if let Expr::Ident(ident) = callee.as_ref() {
            let mut is_some_call = match ident.name.as_str() {
                "is_some" => Some(true),
                "is_none" => Some(false),
                _ => None,
            }?;
            if let Some(negated_is_some) = is_some {
                is_some_call = negated_is_some;
            }
            let then_branch = if is_some_call { for_then } else { !for_then };
            if let Some(Expr::Ident(arg_ident)) = args.first() {
                if let Some(SimpleType::Option(inner)) = self.resolve_local(arg_ident) {
                    return Some(OptionNarrow {
                        ident: arg_ident.clone(),
                        inner: (*inner).clone(),
                        then_branch,
                    });
                }
            }
        }
        None
    }

    fn extract_result_narrow(&mut self, condition: &Expr, for_then: bool) -> Option<ResultNarrow> {
        if let Expr::Binary {
            op, left, right, ..
        } = condition
        {
            if for_then && *op == BinaryOp::And {
                return self
                    .extract_result_narrow(left, true)
                    .or_else(|| self.extract_result_narrow(right, true));
            }
            if !for_then && *op == BinaryOp::Or {
                return self
                    .extract_result_narrow(left, false)
                    .or_else(|| self.extract_result_narrow(right, false));
            }
        }

        let mut call_expr = condition;
        let mut is_ok = None;
        if let Expr::Unary {
            op: UnaryOp::Not,
            expr,
            ..
        } = condition
        {
            call_expr = expr.as_ref();
            is_ok = Some(false);
        }

        let Expr::Call { callee, args, .. } = call_expr else {
            return None;
        };
        if let Expr::Ident(ident) = callee.as_ref() {
            let mut is_ok_call = match ident.name.as_str() {
                "is_ok" => Some(true),
                "is_err" => Some(false),
                _ => None,
            }?;
            if let Some(negated_is_ok) = is_ok {
                is_ok_call = negated_is_ok;
            }
            let then_branch = if is_ok_call { for_then } else { !for_then };
            if let Some(Expr::Ident(arg_ident)) = args.first() {
                if let Some(SimpleType::Result(ok, err)) = self.resolve_local(arg_ident) {
                    return Some(ResultNarrow {
                        ident: arg_ident.clone(),
                        ok: (*ok).clone(),
                        err: (*err).clone(),
                        then_branch,
                    });
                }
            }
        }
        None
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
                if matches!(left, SimpleType::Unknown) && !matches!(right, SimpleType::Unknown) {
                    return Some(right);
                }
                if matches!(right, SimpleType::Unknown) && !matches!(left, SimpleType::Unknown) {
                    return Some(left);
                }
                if !Self::types_compatible(&left, &right) {
                    self.push_error(
                        format!(
                            "{}: expected {} but got {}",
                            message,
                            format_type(&left),
                            format_type(&right)
                        ),
                        span,
                    );
                }
                Some(left)
            }
            (Some(left), None) => Some(left),
            (None, Some(right)) => Some(right),
            (None, None) => None,
        }
    }

    fn merge_assigned_inners(&mut self, name: &Ident, ty: &SimpleType, span: Span) {
        if let SimpleType::Option(inner) = ty {
            let current = self.last_option_inner.clone();
            self.last_option_inner = self.merge_inner(
                Some((**inner).clone()),
                current,
                &format!("option inner mismatch for {}", name.name),
                Some(span),
            );
            if let Some(inner) = self.last_option_inner.clone() {
                self.bind_or_refine_local(name, SimpleType::Option(Box::new(inner)));
            }
        }
        if let SimpleType::Result(ok, err) = ty {
            let current_ok = self.last_result_ok.clone();
            self.last_result_ok = self.merge_inner(
                Some((**ok).clone()),
                current_ok,
                &format!("result ok inner mismatch for {}", name.name),
                Some(span),
            );
            let current_err = self.last_result_err.clone();
            self.last_result_err = self.merge_inner(
                Some((**err).clone()),
                current_err,
                &format!("result err inner mismatch for {}", name.name),
                Some(span),
            );
            if let (Some(ok_inner), Some(err_inner)) =
                (self.last_result_ok.clone(), self.last_result_err.clone())
            {
                self.bind_or_refine_local(
                    name,
                    SimpleType::Result(Box::new(ok_inner), Box::new(err_inner)),
                );
            }
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
        let enum_name = match &value_ty {
            SimpleType::Custom(name, _) => Some(name.clone()),
            _ => None,
        };
        let mut matched_enum_variants: HashSet<String> = HashSet::new();

        if let Expr::Ident(ident) = value {
            if option_inner.is_none() {
                if let Some(SimpleType::Option(inner)) = self.resolve_local(ident) {
                    option_inner = Some((*inner).clone());
                }
            }
            if result_ok.is_none() {
                if let Some(SimpleType::Result(ok, _)) = self.resolve_local(ident) {
                    result_ok = Some((*ok).clone());
                }
            }
            if result_err.is_none() {
                if let Some(SimpleType::Result(_, err)) = self.resolve_local(ident) {
                    result_err = Some((*err).clone());
                }
            }
        }
        let mut result_ty = SimpleType::Unknown;
        let mut has_ok = false;
        let mut has_err = false;
        let mut has_some = false;
        let mut has_none = false;
        let mut has_wildcard = false;
        let mut matched_ints = HashSet::new();
        let mut matched_strings = HashSet::new();
        let mut inferred_ok = None;
        let mut inferred_err = None;
        let mut inferred_opt = None;

        for arm in arms {
            self.push_scope();
            let coverage_pattern = Self::unwrap_binding_pattern(&arm.pattern);
            if arm.guard.is_none() {
                match coverage_pattern {
                    at_syntax::MatchPattern::Int(value, _) => {
                        matched_ints.insert(*value);
                    }
                    at_syntax::MatchPattern::String(value, _) => {
                        matched_strings.insert(value.clone());
                    }
                    at_syntax::MatchPattern::ResultOk(_, _) => {
                        has_ok = true;
                    }
                    at_syntax::MatchPattern::ResultErr(_, _) => {
                        has_err = true;
                    }
                    at_syntax::MatchPattern::OptionSome(_, _) => {
                        has_some = true;
                    }
                    at_syntax::MatchPattern::OptionNone(_) => {
                        has_none = true;
                    }
                    at_syntax::MatchPattern::Enum { variant, .. } => {
                        matched_enum_variants.insert(variant.name.clone());
                    }
                    at_syntax::MatchPattern::Wildcard(_) => {
                        has_wildcard = true;
                    }
                    at_syntax::MatchPattern::Tuple { items, .. } => {
                        if let SimpleType::Tuple(inner_items) = &value_ty {
                            if items.len() == inner_items.len()
                                && items.iter().all(|item| Self::pattern_is_catch_all(item))
                            {
                                has_wildcard = true;
                            }
                        }
                    }
                    _ => {}
                }
            }

            self.check_match_pattern(
                &arm.pattern,
                &value_ty,
                match_span,
                option_inner.as_ref(),
                result_ok.as_ref(),
                result_err.as_ref(),
            );
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
            if matches!(arm_ty, SimpleType::Result(_, _)) {
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
            if matches!(arm_ty, SimpleType::Option(_)) {
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
            if matches!(value_ty, SimpleType::Result(_, _)) && !(has_ok && has_err) {
                self.push_error(
                    "non-exhaustive match for result".to_string(),
                    Some(match_span),
                );
            }
            if matches!(value_ty, SimpleType::Option(_)) && !(has_some && has_none) {
                self.push_error(
                    "non-exhaustive match for option".to_string(),
                    Some(match_span),
                );
            }
            if let Some(enum_name) = enum_name {
                if let Some((_, variants)) = self.enums.get(&enum_name) {
                    if matched_enum_variants.len() != variants.len() {
                        self.push_error(
                            format!("non-exhaustive match for enum {}", enum_name),
                            Some(match_span),
                        );
                    }
                }
            }
            if value_ty == SimpleType::Bool {
                let mut has_true = false;
                let mut has_false = false;
                for arm in arms {
                    if arm.guard.is_none() {
                        let coverage_pattern = Self::unwrap_binding_pattern(&arm.pattern);
                        if let at_syntax::MatchPattern::Bool(value, _) = coverage_pattern {
                            if *value {
                                has_true = true;
                            } else {
                                has_false = true;
                            }
                        }
                    }
                }
                if !(has_true && has_false) {
                    self.push_error(
                        "non-exhaustive match for bool".to_string(),
                        Some(match_span),
                    );
                }
            }
            if matches!(value_ty, SimpleType::Int) {
                self.push_error("non-exhaustive match for int".to_string(), Some(match_span));
            }
            if matches!(value_ty, SimpleType::String) {
                self.push_error(
                    "non-exhaustive match for string".to_string(),
                    Some(match_span),
                );
            }
        }

        if let SimpleType::Result(ok, err) = &result_ty {
            self.last_result_ok = Some((**ok).clone());
            self.last_result_err = Some((**err).clone());
        } else {
            self.last_result_ok = inferred_ok.clone();
            self.last_result_err = inferred_err.clone();
        }
        if let SimpleType::Option(inner) = &result_ty {
            self.last_option_inner = Some((**inner).clone());
        } else {
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
                if !sig.is_async {
                    self.check_function_needs(&ident.name, Some(ident.span));
                }
                let mut env = HashMap::new();
                let precomputed: Option<Vec<SimpleType>> = if !sig.type_params.is_empty() {
                    let tys: Vec<SimpleType> = args.iter().map(|a| self.check_expr(a)).collect();
                    for (arg_ty, expected) in tys.iter().zip(sig.params.iter()) {
                        if let Some((param, ty)) = self.infer_type_param(expected, arg_ty) {
                            env.entry(param).or_insert(ty);
                        }
                    }
                    Some(tys)
                } else {
                    None
                };
                self.check_call_args(
                    &ident.name,
                    &sig,
                    args,
                    Some(ident.span),
                    &env,
                    precomputed.as_deref(),
                );
                let return_ty = if sig.type_params.is_empty() {
                    sig.return_ty
                } else {
                    Self::apply_type_params(&sig.return_ty, &env)
                };
                return if sig.is_async {
                    SimpleType::Future(Box::new(return_ty))
                } else {
                    return_ty
                };
            }
            if self.resolve_local(ident).is_none() {
                self.push_error(
                    format!("unknown function: {}", ident.name),
                    Some(ident.span),
                );
                for arg in args {
                    self.check_expr(arg);
                }
                return SimpleType::Unknown;
            }
        }

        if let Expr::Member { base, name, .. } = callee {
            if let Expr::Ident(base_ident) = base.as_ref() {
                if let Some(required) =
                    self.required_member_capability(&base_ident.name, &name.name)
                {
                    if !self.has_capability(required) {
                        self.push_error(
                            format!("missing capability: {}", required),
                            Some(base_ident.span),
                        );
                    }
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
                    if !sig.is_async {
                        self.check_function_needs(&func_name, Some(name.span));
                    }
                    let mut env = HashMap::new();
                    let precomputed: Option<Vec<SimpleType>> = if !sig.type_params.is_empty() {
                        let tys: Vec<SimpleType> =
                            args.iter().map(|a| self.check_expr(a)).collect();
                        for (arg_ty, expected) in tys.iter().zip(sig.params.iter()) {
                            if let Some((param, ty)) = self.infer_type_param(expected, arg_ty) {
                                env.entry(param).or_insert(ty);
                            }
                        }
                        Some(tys)
                    } else {
                        None
                    };
                    self.check_call_args(
                        &func_name,
                        &sig,
                        args,
                        Some(name.span),
                        &env,
                        precomputed.as_deref(),
                    );
                    let return_ty = if sig.type_params.is_empty() {
                        sig.return_ty
                    } else {
                        Self::apply_type_params(&sig.return_ty, &env)
                    };
                    return if sig.is_async {
                        SimpleType::Future(Box::new(return_ty))
                    } else {
                        return_ty
                    };
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
        let arg_types = args
            .iter()
            .map(|arg| self.check_expr(arg))
            .collect::<Vec<_>>();
        match callee_ty {
            SimpleType::Function(params, return_ty) => {
                let mut expected_params = params.clone();
                let mut inferred_return = (*return_ty).clone();
                for (index, arg_ty) in arg_types.iter().enumerate() {
                    if matches!(expected_params.get(index), Some(SimpleType::Unknown))
                        && !matches!(arg_ty, SimpleType::Unknown)
                    {
                        expected_params[index] = arg_ty.clone();
                    }
                }
                if let Expr::Closure {
                    params: closure_params,
                    body,
                    ..
                } = callee
                {
                    if closure_params.len() == expected_params.len() {
                        let closure_return = self.infer_closure_return_with_params(
                            closure_params,
                            body,
                            &expected_params,
                        );
                        if !matches!(closure_return, SimpleType::Unknown) {
                            inferred_return = closure_return;
                        }
                    }
                }
                if let Expr::Ident(name) = callee {
                    self.refine_local_function_signature(name, &expected_params, &inferred_return);
                }
                if expected_params.len() != args.len() {
                    self.push_error(
                        format!(
                            "wrong arity for closure: expected {} args, got {}",
                            expected_params.len(),
                            args.len()
                        ),
                        expr_span(callee),
                    );
                }
                for (index, arg_ty) in arg_types.iter().enumerate() {
                    if let Some(expected) = expected_params.get(index) {
                        if !matches!(arg_ty, SimpleType::Unknown)
                            && !matches!(expected, SimpleType::Unknown)
                        {
                            self.check_compatible(
                                expected,
                                arg_ty,
                                &format!("closure argument {} type mismatch", index + 1),
                                args.get(index).and_then(expr_span),
                            );
                        }
                    }
                }
                inferred_return
            }
            SimpleType::Unknown => SimpleType::Unknown,
            _ => {
                self.push_error("invalid call target".to_string(), expr_span(callee));
                SimpleType::Unknown
            }
        }
    }

    fn required_member_capability(&self, base: &str, name: &str) -> Option<&'static str> {
        at_syntax::builtin_capability(base, name)
    }

    fn unwrap_binding_pattern<'a>(
        pattern: &'a at_syntax::MatchPattern,
    ) -> &'a at_syntax::MatchPattern {
        match pattern {
            at_syntax::MatchPattern::Binding { pattern, .. } => {
                Self::unwrap_binding_pattern(pattern)
            }
            _ => pattern,
        }
    }

    fn pattern_is_catch_all(pattern: &at_syntax::MatchPattern) -> bool {
        match pattern {
            at_syntax::MatchPattern::Wildcard(_) => true,
            at_syntax::MatchPattern::Binding { pattern, .. } => Self::pattern_is_catch_all(pattern),
            at_syntax::MatchPattern::Tuple { items, .. } => {
                items.iter().all(|item| Self::pattern_is_catch_all(item))
            }
            _ => false,
        }
    }

    fn check_match_pattern(
        &mut self,
        pattern: &at_syntax::MatchPattern,
        expected: &SimpleType,
        match_span: Span,
        option_inner: Option<&SimpleType>,
        result_ok: Option<&SimpleType>,
        result_err: Option<&SimpleType>,
    ) {
        match pattern {
            at_syntax::MatchPattern::Int(_, _) => {
                if *expected != SimpleType::Int && *expected != SimpleType::Unknown {
                    self.push_error("match expects int value".to_string(), Some(match_span));
                }
            }
            at_syntax::MatchPattern::Bool(_, _) => {
                if *expected != SimpleType::Bool && *expected != SimpleType::Unknown {
                    self.push_error("match expects bool value".to_string(), Some(match_span));
                }
            }
            at_syntax::MatchPattern::String(_, _) => {
                if *expected != SimpleType::String && *expected != SimpleType::Unknown {
                    self.push_error("match expects string value".to_string(), Some(match_span));
                }
            }
            at_syntax::MatchPattern::ResultOk(ident, _) => {
                if !matches!(expected, SimpleType::Result(_, _)) && *expected != SimpleType::Unknown
                {
                    self.push_error("match expects result value".to_string(), Some(match_span));
                }
                let ty = match expected {
                    SimpleType::Result(ok, _) => (**ok).clone(),
                    SimpleType::Unknown => result_ok.cloned().unwrap_or(SimpleType::Unknown),
                    _ => SimpleType::Unknown,
                };
                self.bind_local(ident, ty);
            }
            at_syntax::MatchPattern::ResultErr(ident, _) => {
                if !matches!(expected, SimpleType::Result(_, _)) && *expected != SimpleType::Unknown
                {
                    self.push_error("match expects result value".to_string(), Some(match_span));
                }
                let ty = match expected {
                    SimpleType::Result(_, err) => (**err).clone(),
                    SimpleType::Unknown => result_err.cloned().unwrap_or(SimpleType::Unknown),
                    _ => SimpleType::Unknown,
                };
                self.bind_local(ident, ty);
            }
            at_syntax::MatchPattern::OptionSome(ident, _) => {
                if !matches!(expected, SimpleType::Option(_)) && *expected != SimpleType::Unknown {
                    self.push_error("match expects option value".to_string(), Some(match_span));
                }
                let ty = match expected {
                    SimpleType::Option(inner) => (**inner).clone(),
                    SimpleType::Unknown => option_inner.cloned().unwrap_or(SimpleType::Unknown),
                    _ => SimpleType::Unknown,
                };
                self.bind_local(ident, ty);
            }
            at_syntax::MatchPattern::OptionNone(_) => {
                if !matches!(expected, SimpleType::Option(_)) && *expected != SimpleType::Unknown {
                    self.push_error("match expects option value".to_string(), Some(match_span));
                }
            }
            at_syntax::MatchPattern::Tuple { items, .. } => {
                if let SimpleType::Tuple(inner_items) = expected {
                    if inner_items.len() != items.len() {
                        self.push_error(
                            "match expects tuple with matching length".to_string(),
                            Some(match_span),
                        );
                    } else {
                        for (item, inner_ty) in items.iter().zip(inner_items.iter()) {
                            self.check_match_pattern(item, inner_ty, match_span, None, None, None);
                        }
                    }
                } else if *expected != SimpleType::Unknown {
                    self.push_error("match expects tuple value".to_string(), Some(match_span));
                } else {
                    for item in items {
                        self.check_match_pattern(
                            item,
                            &SimpleType::Unknown,
                            match_span,
                            None,
                            None,
                            None,
                        );
                    }
                }
            }
            at_syntax::MatchPattern::Struct { name, fields, .. } => {
                let struct_fields = self.structs.get(&name.name).cloned();
                if struct_fields.is_none() {
                    self.push_error(format!("unknown struct: {}", name.name), Some(name.span));
                }
                let mut env = HashMap::new();
                let mut param_names = HashSet::new();
                if let (Some((type_params, _)), SimpleType::Custom(struct_name, args)) =
                    (&struct_fields, expected)
                {
                    if struct_name != &name.name {
                        self.push_error(
                            format!("match expects struct {}, got {}", name.name, struct_name),
                            Some(match_span),
                        );
                    }
                    param_names = self.type_param_set(type_params);
                    env = self.type_env_from_args(type_params, args, Some(name.span));
                } else if let SimpleType::Custom(struct_name, _) = expected {
                    if struct_name != &name.name {
                        self.push_error(
                            format!("match expects struct {}, got {}", name.name, struct_name),
                            Some(match_span),
                        );
                    }
                } else if *expected != SimpleType::Unknown {
                    self.push_error(
                        format!(
                            "match expects struct {}, got {}",
                            name.name,
                            format_type(expected)
                        ),
                        Some(match_span),
                    );
                }

                if let Some((type_params, struct_fields)) = struct_fields {
                    if param_names.is_empty() {
                        param_names = self.type_param_set(&type_params);
                    }
                    for field in fields {
                        let binding = field.binding.as_ref().unwrap_or(&field.name);
                        if binding.name == "_" {
                            continue;
                        }
                        if let Some(expected_field) = struct_fields
                            .iter()
                            .find(|entry| entry.name.name == field.name.name)
                        {
                            let expected_ty = self.type_from_ref_with_env(
                                &expected_field.ty,
                                Some(&env),
                                Some(&param_names),
                            );
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
            at_syntax::MatchPattern::Enum {
                name,
                variant,
                binding,
                ..
            } => {
                let enum_variants = self.enums.get(&name.name).cloned();
                if enum_variants.is_none() {
                    self.push_error(format!("unknown enum: {}", name.name), Some(name.span));
                }
                let mut env = HashMap::new();
                let mut param_names = HashSet::new();
                if let (Some((type_params, _)), SimpleType::Custom(enum_name, args)) =
                    (&enum_variants, expected)
                {
                    if enum_name != &name.name {
                        self.push_error(
                            format!("match expects enum {}, got {}", name.name, enum_name),
                            Some(match_span),
                        );
                    }
                    param_names = self.type_param_set(type_params);
                    env = self.type_env_from_args(type_params, args, Some(name.span));
                } else if let SimpleType::Custom(enum_name, _) = expected {
                    if enum_name != &name.name {
                        self.push_error(
                            format!("match expects enum {}, got {}", name.name, enum_name),
                            Some(match_span),
                        );
                    }
                } else if *expected != SimpleType::Unknown {
                    self.push_error(
                        format!(
                            "match expects enum {}, got {}",
                            name.name,
                            format_type(expected)
                        ),
                        Some(match_span),
                    );
                }

                if let Some((type_params, enum_variants)) = enum_variants {
                    if param_names.is_empty() {
                        param_names = self.type_param_set(&type_params);
                    }
                    if let Some(expected_variant) = enum_variants
                        .iter()
                        .find(|entry| entry.name.name == variant.name)
                    {
                        match (&expected_variant.payload, binding) {
                            (Some(expected_ty), Some(binding)) => {
                                let expected_ty = self.type_from_ref_with_env(
                                    expected_ty,
                                    Some(&env),
                                    Some(&param_names),
                                );
                                self.bind_local(binding, expected_ty);
                            }
                            (None, Some(binding)) => {
                                self.push_error(
                                    format!(
                                        "enum {}::{} does not take a value",
                                        name.name, variant.name
                                    ),
                                    Some(binding.span),
                                );
                            }
                            (Some(_), None) => {
                                self.push_error(
                                    format!("enum {}::{} expects a value", name.name, variant.name),
                                    Some(variant.span),
                                );
                            }
                            (None, None) => {}
                        }
                    } else {
                        self.push_error(
                            format!("unknown variant {}::{}", name.name, variant.name),
                            Some(variant.span),
                        );
                    }
                }
            }
            at_syntax::MatchPattern::Binding { name, pattern, .. } => {
                self.bind_local(name, expected.clone());
                self.check_match_pattern(
                    pattern,
                    expected,
                    match_span,
                    option_inner,
                    result_ok,
                    result_err,
                );
            }
            at_syntax::MatchPattern::Wildcard(_) => {}
        }
    }

    fn check_call_args(
        &mut self,
        name: &str,
        sig: &FuncSig,
        args: &[Expr],
        span: Option<Span>,
        env: &HashMap<String, SimpleType>,
        precomputed: Option<&[SimpleType]>,
    ) {
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
            let expected_raw = sig.params.get(index);
            // Resolve type parameters so closure inference sees concrete types.
            let expected_resolved = expected_raw.map(|e| Self::apply_type_params(e, env));

            // When the expected param is a function type and the argument is a
            // closure literal, infer the closure's param types from the expected
            // signature rather than checking with Unknown params.
            let arg_ty = if let (
                Some(SimpleType::Function(ref param_tys, _)),
                Expr::Closure {
                    params: closure_params,
                    body,
                    ..
                },
            ) = (&expected_resolved, arg)
            {
                let ret = self.infer_closure_return_with_params(closure_params, body, param_tys);
                SimpleType::Function(param_tys.clone(), Box::new(ret))
            } else if let Some(pre) = precomputed.and_then(|p| p.get(index)) {
                pre.clone()
            } else {
                self.check_expr(arg)
            };

            if let Some(expected) = expected_resolved.as_ref().or(expected_raw) {
                self.check_compatible(
                    expected,
                    &arg_ty,
                    &format!("argument {} type mismatch for {}", index + 1, name),
                    expr_span(arg),
                );
            }
        }
    }

    fn infer_type_param(
        &self,
        expected: &SimpleType,
        arg_ty: &SimpleType,
    ) -> Option<(String, SimpleType)> {
        match expected {
            SimpleType::TypeParam(name) => Some((name.clone(), arg_ty.clone())),
            SimpleType::Option(inner) => {
                if let SimpleType::Option(found_inner) = arg_ty {
                    if let SimpleType::TypeParam(name) = inner.as_ref() {
                        return Some((name.clone(), (**found_inner).clone()));
                    }
                }
                None
            }
            SimpleType::Result(ok, err) => {
                if let SimpleType::Result(found_ok, found_err) = arg_ty {
                    if let SimpleType::TypeParam(name) = ok.as_ref() {
                        return Some((name.clone(), (**found_ok).clone()));
                    }
                    if let SimpleType::TypeParam(name) = err.as_ref() {
                        return Some((name.clone(), (**found_err).clone()));
                    }
                }
                None
            }
            SimpleType::Array(inner) => {
                if let SimpleType::Array(found_inner) = arg_ty {
                    if let SimpleType::TypeParam(name) = inner.as_ref() {
                        return Some((name.clone(), (**found_inner).clone()));
                    }
                }
                None
            }
            SimpleType::Map(key, value) => {
                if let SimpleType::Map(found_key, found_value) = arg_ty {
                    if let SimpleType::TypeParam(name) = key.as_ref() {
                        return Some((name.clone(), (**found_key).clone()));
                    }
                    if let SimpleType::TypeParam(name) = value.as_ref() {
                        return Some((name.clone(), (**found_value).clone()));
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn apply_type_params(ty: &SimpleType, env: &HashMap<String, SimpleType>) -> SimpleType {
        match ty {
            SimpleType::TypeParam(name) => env.get(name).cloned().unwrap_or(SimpleType::Unknown),
            SimpleType::Array(inner) => {
                SimpleType::Array(Box::new(Self::apply_type_params(inner, env)))
            }
            SimpleType::Option(inner) => {
                SimpleType::Option(Box::new(Self::apply_type_params(inner, env)))
            }
            SimpleType::Result(ok, err) => SimpleType::Result(
                Box::new(Self::apply_type_params(ok, env)),
                Box::new(Self::apply_type_params(err, env)),
            ),
            SimpleType::Function(params, ret) => SimpleType::Function(
                params
                    .iter()
                    .map(|param| Self::apply_type_params(param, env))
                    .collect(),
                Box::new(Self::apply_type_params(ret, env)),
            ),
            SimpleType::Tuple(items) => SimpleType::Tuple(
                items
                    .iter()
                    .map(|item| Self::apply_type_params(item, env))
                    .collect(),
            ),
            SimpleType::Map(key, value) => SimpleType::Map(
                Box::new(Self::apply_type_params(key, env)),
                Box::new(Self::apply_type_params(value, env)),
            ),
            SimpleType::Future(inner) => {
                SimpleType::Future(Box::new(Self::apply_type_params(inner, env)))
            }
            SimpleType::Union(types) => SimpleType::Union(
                types
                    .iter()
                    .map(|ty| Self::apply_type_params(ty, env))
                    .collect(),
            ),
            SimpleType::Intersection(types) => SimpleType::Intersection(
                types
                    .iter()
                    .map(|ty| Self::apply_type_params(ty, env))
                    .collect(),
            ),
            SimpleType::Custom(name, args) => SimpleType::Custom(
                name.clone(),
                args.iter()
                    .map(|arg| Self::apply_type_params(arg, env))
                    .collect(),
            ),
            SimpleType::Generator(inner) => {
                SimpleType::Generator(Box::new(Self::apply_type_params(inner, env)))
            }
            SimpleType::Int
            | SimpleType::Float
            | SimpleType::Bool
            | SimpleType::String
            | SimpleType::Unit
            | SimpleType::Unknown => ty.clone(),
        }
    }

    fn bind_or_refine_local(&mut self, name: &Ident, ty: SimpleType) {
        for scope in self.locals.iter_mut().rev() {
            if let Some(existing) = scope.get_mut(&name.name) {
                if matches!(*existing, SimpleType::Unknown) && !matches!(ty, SimpleType::Unknown) {
                    *existing = ty;
                    return;
                }
                match (&mut *existing, &ty) {
                    (SimpleType::Option(existing_inner), SimpleType::Option(new_inner)) => {
                        if matches!(**existing_inner, SimpleType::Unknown)
                            && !matches!(**new_inner, SimpleType::Unknown)
                        {
                            *existing_inner = Box::new((**new_inner).clone());
                        }
                    }
                    (
                        SimpleType::Result(existing_ok, existing_err),
                        SimpleType::Result(new_ok, new_err),
                    ) => {
                        if matches!(**existing_ok, SimpleType::Unknown)
                            && !matches!(**new_ok, SimpleType::Unknown)
                        {
                            *existing_ok = Box::new((**new_ok).clone());
                        }
                        if matches!(**existing_err, SimpleType::Unknown)
                            && !matches!(**new_err, SimpleType::Unknown)
                        {
                            *existing_err = Box::new((**new_err).clone());
                        }
                    }
                    _ => {}
                }
                return;
            }
        }
        self.bind_local(name, ty);
    }

    fn refine_local_function_signature(
        &mut self,
        name: &Ident,
        params: &[SimpleType],
        return_ty: &SimpleType,
    ) {
        for scope in self.locals.iter_mut().rev() {
            let Some(existing) = scope.get_mut(&name.name) else {
                continue;
            };
            if let SimpleType::Function(existing_params, existing_return) = existing {
                if existing_params.len() == params.len() {
                    for (index, expected) in params.iter().enumerate() {
                        if matches!(existing_params[index], SimpleType::Unknown)
                            && !matches!(expected, SimpleType::Unknown)
                        {
                            existing_params[index] = expected.clone();
                        }
                    }
                }
                if matches!(existing_return.as_ref(), SimpleType::Unknown)
                    && !matches!(return_ty, SimpleType::Unknown)
                {
                    *existing_return = Box::new(return_ty.clone());
                }
            }
            break;
        }
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
            "next" => {
                self.check_arity(name, args, 1, span);
                let mut inner = SimpleType::Unknown;
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    match arg_ty {
                        SimpleType::Generator(gen_inner) => {
                            inner = (*gen_inner).clone();
                        }
                        SimpleType::Unknown => {}
                        other => {
                            self.push_error(
                                format!("next expects generator, got {}", format_type(&other)),
                                expr_span(arg),
                            );
                        }
                    }
                }
                Some(SimpleType::Option(Box::new(inner)))
            }
            "len" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Array(_))
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
                let mut array_inner = None;
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Array(_)) && arg_ty != SimpleType::Unknown {
                        self.push_error("append expects array".to_string(), expr_span(arg));
                    } else if let SimpleType::Array(inner) = arg_ty {
                        array_inner = Some((*inner).clone());
                    }
                }
                if let Some(value) = args.get(1) {
                    let value_ty = self.check_expr(value);
                    if let Some(inner) = array_inner.as_ref() {
                        if !matches!(value_ty, SimpleType::Unknown) {
                            self.check_compatible(
                                inner,
                                &value_ty,
                                "append value type mismatch",
                                expr_span(value),
                            );
                        }
                    }
                }
                Some(SimpleType::Array(Box::new(
                    array_inner.unwrap_or(SimpleType::Unknown),
                )))
            }
            "contains" => {
                self.check_arity(name, args, 2, span);
                let mut array_inner = None;
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Array(_)) && arg_ty != SimpleType::Unknown {
                        self.push_error("contains expects array".to_string(), expr_span(arg));
                    } else if let SimpleType::Array(inner) = arg_ty {
                        array_inner = Some((*inner).clone());
                    }
                }
                if let Some(value) = args.get(1) {
                    let value_ty = self.check_expr(value);
                    if let Some(inner) = array_inner.as_ref() {
                        if !matches!(value_ty, SimpleType::Unknown) {
                            self.check_compatible(
                                inner,
                                &value_ty,
                                "contains value type mismatch",
                                expr_span(value),
                            );
                        }
                    }
                }
                Some(SimpleType::Bool)
            }
            "slice" => {
                self.check_arity(name, args, 3, span);
                let mut array_inner = None;
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Array(_)) && arg_ty != SimpleType::Unknown {
                        self.push_error("slice expects array".to_string(), expr_span(arg));
                    } else if let SimpleType::Array(inner) = arg_ty {
                        array_inner = Some((*inner).clone());
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
                Some(SimpleType::Array(Box::new(
                    array_inner.unwrap_or(SimpleType::Unknown),
                )))
            }
            "split" => {
                self.check_arity(name, args, 2, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::String && arg_ty != SimpleType::Unknown {
                        self.push_error("split expects string".to_string(), expr_span(arg));
                    }
                }
                if let Some(delim) = args.get(1) {
                    let delim_ty = self.check_expr(delim);
                    if delim_ty != SimpleType::String && delim_ty != SimpleType::Unknown {
                        self.push_error(
                            "split expects string delimiter".to_string(),
                            expr_span(delim),
                        );
                    }
                }
                Some(SimpleType::Array(Box::new(SimpleType::String)))
            }
            "trim" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::String && arg_ty != SimpleType::Unknown {
                        self.push_error("trim expects string".to_string(), expr_span(arg));
                    }
                }
                Some(SimpleType::String)
            }
            "substring" => {
                self.check_arity(name, args, 3, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::String && arg_ty != SimpleType::Unknown {
                        self.push_error("substring expects string".to_string(), expr_span(arg));
                    }
                }
                if let Some(start) = args.get(1) {
                    let start_ty = self.check_expr(start);
                    if start_ty != SimpleType::Int && start_ty != SimpleType::Unknown {
                        self.push_error(
                            "substring expects int bounds".to_string(),
                            expr_span(start),
                        );
                    }
                }
                if let Some(end) = args.get(2) {
                    let end_ty = self.check_expr(end);
                    if end_ty != SimpleType::Int && end_ty != SimpleType::Unknown {
                        self.push_error("substring expects int bounds".to_string(), expr_span(end));
                    }
                }
                Some(SimpleType::String)
            }
            "char_at" => {
                self.check_arity(name, args, 2, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::String && arg_ty != SimpleType::Unknown {
                        self.push_error("char_at expects string".to_string(), expr_span(arg));
                    }
                }
                if let Some(index) = args.get(1) {
                    let index_ty = self.check_expr(index);
                    if index_ty != SimpleType::Int && index_ty != SimpleType::Unknown {
                        self.push_error("char_at expects int".to_string(), expr_span(index));
                    }
                }
                Some(SimpleType::String)
            }
            "to_upper" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::String && arg_ty != SimpleType::Unknown {
                        self.push_error("to_upper expects string".to_string(), expr_span(arg));
                    }
                }
                Some(SimpleType::String)
            }
            "to_lower" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::String && arg_ty != SimpleType::Unknown {
                        self.push_error("to_lower expects string".to_string(), expr_span(arg));
                    }
                }
                Some(SimpleType::String)
            }
            "parse_int" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if arg_ty != SimpleType::String && arg_ty != SimpleType::Unknown {
                        self.push_error("parse_int expects string".to_string(), expr_span(arg));
                    }
                }
                Some(SimpleType::Option(Box::new(SimpleType::Int)))
            }
            "to_string" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    self.check_expr(arg);
                }
                Some(SimpleType::String)
            }
            "map" => {
                self.check_arity(name, args, 2, span);
                let mut array_inner = None;
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Array(_)) && arg_ty != SimpleType::Unknown {
                        self.push_error("map expects array".to_string(), expr_span(arg));
                    } else if let SimpleType::Array(inner) = arg_ty {
                        array_inner = Some((*inner).clone());
                    }
                }
                let mut mapped_ty = SimpleType::Unknown;
                if let Some(mapper) = args.get(1) {
                    if let Expr::Closure { params, body, .. } = mapper {
                        if params.len() != 1 {
                            self.push_error(
                                format!(
                                    "map expects function with 1 argument, got {}",
                                    params.len()
                                ),
                                expr_span(mapper),
                            );
                        } else {
                            let inferred_param = array_inner.clone().unwrap_or(SimpleType::Unknown);
                            mapped_ty = self.infer_closure_return_with_params(
                                params,
                                body,
                                &[inferred_param],
                            );
                        }
                    } else {
                        let mapper_ty = self.check_expr(mapper);
                        match mapper_ty {
                            SimpleType::Function(params, return_ty) => {
                                if params.len() != 1 {
                                    self.push_error(
                                        format!(
                                            "map expects function with 1 argument, got {}",
                                            params.len()
                                        ),
                                        expr_span(mapper),
                                    );
                                } else if let Some(inner) = array_inner.as_ref() {
                                    let param_ty = &params[0];
                                    if !matches!(param_ty, SimpleType::Unknown) {
                                        self.check_compatible(
                                            param_ty,
                                            inner,
                                            "map function parameter mismatch",
                                            expr_span(mapper),
                                        );
                                    }
                                }
                                mapped_ty = (*return_ty).clone();
                            }
                            SimpleType::Unknown => {}
                            _ => {
                                self.push_error(
                                    "map expects function".to_string(),
                                    expr_span(mapper),
                                );
                            }
                        }
                    }
                }
                Some(SimpleType::Array(Box::new(mapped_ty)))
            }
            "filter" => {
                self.check_arity(name, args, 2, span);
                let mut array_inner = None;
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Array(_)) && arg_ty != SimpleType::Unknown {
                        self.push_error("filter expects array".to_string(), expr_span(arg));
                    } else if let SimpleType::Array(inner) = arg_ty {
                        array_inner = Some((*inner).clone());
                    }
                }
                if let Some(predicate) = args.get(1) {
                    if let Expr::Closure { params, body, .. } = predicate {
                        if params.len() != 1 {
                            self.push_error(
                                format!(
                                    "filter expects function with 1 argument, got {}",
                                    params.len()
                                ),
                                expr_span(predicate),
                            );
                        } else {
                            let inferred_param = array_inner.clone().unwrap_or(SimpleType::Unknown);
                            let return_ty = self.infer_closure_return_with_params(
                                params,
                                body,
                                &[inferred_param],
                            );
                            if !matches!(return_ty, SimpleType::Unknown)
                                && return_ty != SimpleType::Bool
                            {
                                self.push_error(
                                    "filter expects bool predicate".to_string(),
                                    expr_span(predicate),
                                );
                            }
                        }
                    } else {
                        let predicate_ty = self.check_expr(predicate);
                        match predicate_ty {
                            SimpleType::Function(params, return_ty) => {
                                if params.len() != 1 {
                                    self.push_error(
                                        format!(
                                            "filter expects function with 1 argument, got {}",
                                            params.len()
                                        ),
                                        expr_span(predicate),
                                    );
                                } else if let Some(inner) = array_inner.as_ref() {
                                    let param_ty = &params[0];
                                    if !matches!(param_ty, SimpleType::Unknown) {
                                        self.check_compatible(
                                            param_ty,
                                            inner,
                                            "filter function parameter mismatch",
                                            expr_span(predicate),
                                        );
                                    }
                                }
                                if !matches!(*return_ty, SimpleType::Unknown)
                                    && *return_ty != SimpleType::Bool
                                {
                                    self.push_error(
                                        "filter expects bool predicate".to_string(),
                                        expr_span(predicate),
                                    );
                                }
                            }
                            SimpleType::Unknown => {}
                            _ => {
                                self.push_error(
                                    "filter expects function".to_string(),
                                    expr_span(predicate),
                                );
                            }
                        }
                    }
                }
                Some(SimpleType::Array(Box::new(
                    array_inner.unwrap_or(SimpleType::Unknown),
                )))
            }
            "reduce" => {
                self.check_arity(name, args, 3, span);
                let mut array_inner = None;
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Array(_)) && arg_ty != SimpleType::Unknown {
                        self.push_error("reduce expects array".to_string(), expr_span(arg));
                    } else if let SimpleType::Array(inner) = arg_ty {
                        array_inner = Some((*inner).clone());
                    }
                }
                let mut acc_ty = SimpleType::Unknown;
                if let Some(init) = args.get(1) {
                    acc_ty = self.check_expr(init);
                }
                if let Some(reducer) = args.get(2) {
                    if let Expr::Closure { params, body, .. } = reducer {
                        if params.len() != 2 {
                            self.push_error(
                                format!(
                                    "reduce expects function with 2 arguments, got {}",
                                    params.len()
                                ),
                                expr_span(reducer),
                            );
                        } else {
                            let inferred_item = array_inner.clone().unwrap_or(SimpleType::Unknown);
                            let return_ty = self.infer_closure_return_with_params(
                                params,
                                body,
                                &[acc_ty.clone(), inferred_item],
                            );
                            if !matches!(return_ty, SimpleType::Unknown)
                                && !matches!(acc_ty, SimpleType::Unknown)
                            {
                                self.check_compatible(
                                    &acc_ty,
                                    &return_ty,
                                    "reduce return type mismatch",
                                    expr_span(reducer),
                                );
                            }
                            if matches!(acc_ty, SimpleType::Unknown)
                                && !matches!(return_ty, SimpleType::Unknown)
                            {
                                acc_ty = return_ty;
                            }
                        }
                    } else {
                        let reducer_ty = self.check_expr(reducer);
                        match reducer_ty {
                            SimpleType::Function(params, return_ty) => {
                                if params.len() != 2 {
                                    self.push_error(
                                        format!(
                                            "reduce expects function with 2 arguments, got {}",
                                            params.len()
                                        ),
                                        expr_span(reducer),
                                    );
                                } else {
                                    let acc_param = &params[0];
                                    if !matches!(acc_param, SimpleType::Unknown)
                                        && !matches!(acc_ty, SimpleType::Unknown)
                                    {
                                        self.check_compatible(
                                            acc_param,
                                            &acc_ty,
                                            "reduce accumulator parameter mismatch",
                                            expr_span(reducer),
                                        );
                                    }
                                    if let Some(inner) = array_inner.as_ref() {
                                        let item_param = &params[1];
                                        if !matches!(item_param, SimpleType::Unknown) {
                                            self.check_compatible(
                                                item_param,
                                                inner,
                                                "reduce item parameter mismatch",
                                                expr_span(reducer),
                                            );
                                        }
                                    }
                                    if !matches!(*return_ty, SimpleType::Unknown)
                                        && !matches!(acc_ty, SimpleType::Unknown)
                                    {
                                        self.check_compatible(
                                            &acc_ty,
                                            &return_ty,
                                            "reduce return type mismatch",
                                            expr_span(reducer),
                                        );
                                    }
                                    if matches!(acc_ty, SimpleType::Unknown)
                                        && !matches!(*return_ty, SimpleType::Unknown)
                                    {
                                        acc_ty = (*return_ty).clone();
                                    }
                                }
                            }
                            SimpleType::Unknown => {}
                            _ => {
                                self.push_error(
                                    "reduce expects function".to_string(),
                                    expr_span(reducer),
                                );
                            }
                        }
                    }
                }
                Some(acc_ty)
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
                    self.last_option_inner = Some(inner.clone());
                    if matches!(self.current_return, SimpleType::Unknown) {
                        self.current_return = SimpleType::Option(Box::new(inner.clone()));
                    }
                }
                Some(SimpleType::Option(Box::new(
                    self.last_option_inner
                        .clone()
                        .unwrap_or(SimpleType::Unknown),
                )))
            }
            "none" => {
                self.check_arity(name, args, 0, span);
                self.last_option_inner = None;
                if matches!(self.current_return, SimpleType::Unknown) {
                    self.current_return = SimpleType::Option(Box::new(SimpleType::Unknown));
                }
                Some(SimpleType::Option(Box::new(SimpleType::Unknown)))
            }
            "ok" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let inner = self.check_expr(arg);
                    self.last_result_ok = Some(inner.clone());
                    if matches!(self.current_return, SimpleType::Unknown) {
                        self.current_return = SimpleType::Result(
                            Box::new(inner.clone()),
                            Box::new(SimpleType::Unknown),
                        );
                    }
                }
                Some(SimpleType::Result(
                    Box::new(self.last_result_ok.clone().unwrap_or(SimpleType::Unknown)),
                    Box::new(SimpleType::Unknown),
                ))
            }
            "err" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let inner = self.check_expr(arg);
                    self.last_result_err = Some(inner.clone());
                    if matches!(self.current_return, SimpleType::Unknown) {
                        self.current_return = SimpleType::Result(
                            Box::new(SimpleType::Unknown),
                            Box::new(inner.clone()),
                        );
                    }
                }
                Some(SimpleType::Result(
                    Box::new(SimpleType::Unknown),
                    Box::new(self.last_result_err.clone().unwrap_or(SimpleType::Unknown)),
                ))
            }
            "is_some" | "is_none" => {
                self.check_arity(name, args, 1, span);
                if let Some(arg) = args.first() {
                    let arg_ty = self.check_expr(arg);
                    if !matches!(arg_ty, SimpleType::Option(_)) && arg_ty != SimpleType::Unknown {
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
                    if !matches!(arg_ty, SimpleType::Result(_, _)) && arg_ty != SimpleType::Unknown
                    {
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
                Some(SimpleType::Int)
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
        if Self::types_compatible(expected, found) {
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
        if !Self::types_compatible(expected, found) && !Self::types_compatible(found, expected) {
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
        self.consts.push(HashSet::new());
        self.capabilities.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.locals.pop();
        self.consts.pop();
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

    fn bind_const(&mut self, name: &Ident) {
        if let Some(scope) = self.consts.last_mut() {
            scope.insert(name.name.clone());
        }
    }

    fn is_const(&self, name: &Ident) -> bool {
        self.consts
            .iter()
            .rev()
            .any(|scope| scope.contains(&name.name))
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
            SimpleType::Result(_, _) => {
                let ok = self.return_result_ok.as_ref().map(format_type);
                let err = self.return_result_err.as_ref().map(format_type);
                if let (Some(ok), Some(err)) = (ok, err) {
                    Some(format!("result<{ok}, {err}>"))
                } else {
                    Some("result".to_string())
                }
            }
            SimpleType::Generator(_) => {
                if let Some(inner) = &self.return_generator_inner {
                    Some(format!("generator<{}>", format_type(inner)))
                } else {
                    Some("generator".to_string())
                }
            }
            SimpleType::Option(_) => {
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
        if matches!(self.current_return, SimpleType::Option(_)) {
            if let Some(inner) = self.resolve_local(ident).and_then(|ty| match ty {
                SimpleType::Option(inner) => Some((*inner).clone()),
                _ => None,
            }) {
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
        if matches!(self.current_return, SimpleType::Result(_, _)) {
            if let Some(inner) = self.resolve_local(ident).and_then(|ty| match ty {
                SimpleType::Result(ok, _) => Some((*ok).clone()),
                _ => None,
            }) {
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
            if let Some(inner) = self.resolve_local(ident).and_then(|ty| match ty {
                SimpleType::Result(_, err) => Some((*err).clone()),
                _ => None,
            }) {
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
            TypeRef::Qualified { ty, .. } => self.expected_option_inner_from_ref(ty.as_ref()),
            TypeRef::Named { name, args } => {
                if name.name != "option" || args.len() != 1 {
                    return None;
                }
                Some(self.type_from_ref_with_env(&args[0], None, None))
            }
            _ => None,
        }
    }

    fn expected_option_inner_from_ref(&mut self, ty: &TypeRef) -> Option<SimpleType> {
        match ty {
            TypeRef::Qualified { ty, .. } => self.expected_option_inner_from_ref(ty),
            TypeRef::Named { name, args } => {
                if name.name != "option" || args.len() != 1 {
                    return None;
                }
                Some(self.type_from_ref_with_env(&args[0], None, None))
            }
            _ => None,
        }
    }

    fn expected_result_ok(&mut self) -> Option<SimpleType> {
        let ty = self.current_return_ref.clone()?;
        match ty {
            TypeRef::Qualified { ty, .. } => self.expected_result_ok_from_ref(ty.as_ref()),
            TypeRef::Named { name, args } => {
                if name.name != "result" || args.len() != 2 {
                    return None;
                }
                Some(self.type_from_ref_with_env(&args[0], None, None))
            }
            _ => None,
        }
    }

    fn expected_result_ok_from_ref(&mut self, ty: &TypeRef) -> Option<SimpleType> {
        match ty {
            TypeRef::Qualified { ty, .. } => self.expected_result_ok_from_ref(ty),
            TypeRef::Named { name, args } => {
                if name.name != "result" || args.len() != 2 {
                    return None;
                }
                Some(self.type_from_ref_with_env(&args[0], None, None))
            }
            _ => None,
        }
    }

    fn expected_result_err(&mut self) -> Option<SimpleType> {
        let ty = self.current_return_ref.clone()?;
        match ty {
            TypeRef::Qualified { ty, .. } => self.expected_result_err_from_ref(ty.as_ref()),
            TypeRef::Named { name, args } => {
                if name.name != "result" || args.len() != 2 {
                    return None;
                }
                Some(self.type_from_ref_with_env(&args[1], None, None))
            }
            _ => None,
        }
    }

    fn expected_result_err_from_ref(&mut self, ty: &TypeRef) -> Option<SimpleType> {
        match ty {
            TypeRef::Qualified { ty, .. } => self.expected_result_err_from_ref(ty),
            TypeRef::Named { name, args } => {
                if name.name != "result" || args.len() != 2 {
                    return None;
                }
                Some(self.type_from_ref_with_env(&args[1], None, None))
            }
            _ => None,
        }
    }

    fn infer_inner_from_expr(&mut self, expr: &Expr, ty: &SimpleType) {
        match expr {
            Expr::Ident(ident) => {
                if let Some(ty) = self.resolve_local(ident) {
                    if let SimpleType::Option(ref inner) = ty {
                        self.last_option_inner = Some((**inner).clone());
                    }
                    if let SimpleType::Result(ref ok, ref err) = ty {
                        self.last_result_ok = Some((**ok).clone());
                        self.last_result_err = Some((**err).clone());
                    }
                }
            }
            Expr::Match { .. } => {
                // inner types may already be recorded by check_match
            }
            _ => {}
        }
        if self.last_option_inner.is_none() {
            if let SimpleType::Option(inner) = ty {
                if !matches!(**inner, SimpleType::Unknown) {
                    self.last_option_inner = Some((**inner).clone());
                }
            }
        }
        if self.last_result_ok.is_none() || self.last_result_err.is_none() {
            if let SimpleType::Result(ok, err) = ty {
                if self.last_result_ok.is_none() && !matches!(**ok, SimpleType::Unknown) {
                    self.last_result_ok = Some((**ok).clone());
                }
                if self.last_result_err.is_none() && !matches!(**err, SimpleType::Unknown) {
                    self.last_result_err = Some((**err).clone());
                }
            }
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

    fn type_from_ref_with_env(
        &mut self,
        ty: &TypeRef,
        env: Option<&HashMap<String, SimpleType>>,
        params: Option<&HashSet<String>>,
    ) -> SimpleType {
        self.validate_type_ref_with_params(ty, params);
        match ty {
            TypeRef::Qualified { ty, .. } => self.type_from_ref_with_env(ty, env, params),
            TypeRef::Named { name, args } => {
                if let Some(params) = params {
                    if params.contains(&name.name) {
                        if let Some(env) = env {
                            return env.get(&name.name).cloned().unwrap_or(SimpleType::Unknown);
                        }
                        return SimpleType::TypeParam(name.name.clone());
                    }
                }
                if let Some(alias) = self.resolve_alias(name) {
                    return self.type_from_ref_with_env(&alias, env, params);
                }
                if let Some(current) = self.current_type_params() {
                    if current.contains(&name.name) {
                        return SimpleType::TypeParam(name.name.clone());
                    }
                }
                match name.name.as_str() {
                    "int" => SimpleType::Int,
                    "float" => SimpleType::Float,
                    "bool" => SimpleType::Bool,
                    "string" => SimpleType::String,
                    "unit" => SimpleType::Unit,
                    "array" => {
                        let inner = args
                            .first()
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        SimpleType::Array(Box::new(inner))
                    }
                    "option" => {
                        let inner = args
                            .first()
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        SimpleType::Option(Box::new(inner))
                    }
                    "result" => {
                        let ok = args
                            .first()
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        let err = args
                            .get(1)
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        SimpleType::Result(Box::new(ok), Box::new(err))
                    }
                    "future" => {
                        let inner = args
                            .first()
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        SimpleType::Future(Box::new(inner))
                    }
                    "generator" => {
                        let inner = args
                            .first()
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        SimpleType::Generator(Box::new(inner))
                    }
                    "map" => {
                        let key = args
                            .first()
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        let value = args
                            .get(1)
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .unwrap_or(SimpleType::Unknown);
                        SimpleType::Map(Box::new(key), Box::new(value))
                    }
                    other => {
                        let args = args
                            .iter()
                            .map(|arg| self.type_from_ref_with_env(arg, env, params))
                            .collect();
                        SimpleType::Custom(other.to_string(), args)
                    }
                }
            }
            TypeRef::Union { types } => SimpleType::Union(
                types
                    .iter()
                    .map(|ty| self.type_from_ref_with_env(ty, env, params))
                    .collect(),
            ),
            TypeRef::Intersection { types } => SimpleType::Intersection(
                types
                    .iter()
                    .map(|ty| self.type_from_ref_with_env(ty, env, params))
                    .collect(),
            ),
            TypeRef::Tuple { items, .. } => SimpleType::Tuple(
                items
                    .iter()
                    .map(|item| self.type_from_ref_with_env(item, env, params))
                    .collect(),
            ),
            TypeRef::Function {
                params: params_ref,
                return_ty,
                ..
            } => SimpleType::Function(
                params_ref
                    .iter()
                    .map(|param| self.type_from_ref_with_env(param, env, params))
                    .collect(),
                Box::new(self.type_from_ref_with_env(return_ty, env, params)),
            ),
        }
    }

    fn type_param_set(&self, params: &[Ident]) -> HashSet<String> {
        params.iter().map(|param| param.name.clone()).collect()
    }

    fn type_env_from_args(
        &mut self,
        params: &[Ident],
        args: &[SimpleType],
        span: Option<Span>,
    ) -> HashMap<String, SimpleType> {
        let mut env = HashMap::new();
        if params.len() != args.len() && !args.is_empty() {
            self.push_error(
                format!(
                    "type argument count mismatch: expected {}, got {}",
                    params.len(),
                    args.len()
                ),
                span,
            );
        }
        for (param, arg) in params.iter().zip(args.iter()) {
            env.insert(param.name.clone(), arg.clone());
        }
        env
    }

    fn infer_type_param_name(ty: &TypeRef, params: &HashSet<String>) -> Option<String> {
        match ty {
            TypeRef::Named { name, .. } if params.contains(&name.name) => Some(name.name.clone()),
            TypeRef::Qualified { ty, .. } => Self::infer_type_param_name(ty, params),
            _ => None,
        }
    }

    fn validate_type_ref(&mut self, ty: &TypeRef) {
        self.validate_type_ref_with_params(ty, None);
    }

    fn validate_type_ref_with_params(
        &mut self,
        ty: &TypeRef,
        extra_params: Option<&HashSet<String>>,
    ) {
        match ty {
            TypeRef::Qualified { ty, .. } => self.validate_type_ref_with_params(ty, extra_params),
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
                "future" => {
                    if args.len() != 1 {
                        self.push_error(
                            format!("type future expects 1 argument, got {}", args.len()),
                            Some(name.span),
                        );
                    }
                }
                "generator" => {
                    if args.len() != 1 {
                        self.push_error(
                            format!("type generator expects 1 argument, got {}", args.len()),
                            Some(name.span),
                        );
                    }
                }
                "map" => {
                    if args.len() != 2 {
                        self.push_error(
                            format!("type map expects 2 arguments, got {}", args.len()),
                            Some(name.span),
                        );
                    }
                }
                "int" | "float" | "bool" | "string" | "unit" => {}
                other => {
                    if self.type_aliases.contains_key(other) {
                        if let Some(alias) = self.resolve_alias(name) {
                            self.validate_type_ref_with_params(&alias, extra_params);
                        }
                    } else if !self.structs.contains_key(other)
                        && !self.enums.contains_key(other)
                        && !self
                            .current_type_params()
                            .is_some_and(|tp| tp.contains(other))
                        && !extra_params.is_some_and(|ep| ep.contains(other))
                    {
                        self.push_error(format!("unknown type: {}", other), Some(name.span));
                    }
                }
            },
            TypeRef::Tuple { items, .. } => {
                for item in items {
                    self.validate_type_ref_with_params(item, extra_params);
                }
            }
            TypeRef::Union { types } | TypeRef::Intersection { types } => {
                for ty in types {
                    self.validate_type_ref_with_params(ty, extra_params);
                }
            }
            TypeRef::Function {
                params, return_ty, ..
            } => {
                for param in params {
                    self.validate_type_ref_with_params(param, extra_params);
                }
                self.validate_type_ref_with_params(return_ty, extra_params);
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
            if Self::type_ref_contains_name(&alias, &name.name) {
                self.push_error(
                    format!("recursive type alias: {}", name.name),
                    Some(name.span),
                );
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

    fn type_ref_contains_name(ty: &TypeRef, target: &str) -> bool {
        match ty {
            TypeRef::Qualified { ty, .. } => Self::type_ref_contains_name(ty, target),
            TypeRef::Named { name, args } => {
                if name.name == target {
                    return true;
                }
                args.iter()
                    .any(|arg| Self::type_ref_contains_name(arg, target))
            }
            TypeRef::Union { types }
            | TypeRef::Intersection { types }
            | TypeRef::Tuple { items: types, .. } => types
                .iter()
                .any(|ty| Self::type_ref_contains_name(ty, target)),
            TypeRef::Function {
                params, return_ty, ..
            } => {
                params
                    .iter()
                    .any(|ty| Self::type_ref_contains_name(ty, target))
                    || Self::type_ref_contains_name(return_ty, target)
            }
        }
    }

    fn check_loop_body_fixpoint(&mut self, body: &[Stmt]) {
        // Snapshot outer scopes preserving scope boundaries (everything
        // except the innermost loop-body scope pushed by the caller).
        let outer_len = self.locals.len().saturating_sub(1);
        let mut state: Vec<HashMap<String, SimpleType>> = self.locals[..outer_len].to_vec();

        for _ in 0..4 {
            // Replace outer scopes with current state, keeping the
            // caller's loop scope intact at the end.
            let loop_scope = self.locals.last().cloned().unwrap_or_default();
            self.locals.truncate(0);
            self.locals.extend(state.clone());
            self.locals.push(loop_scope);

            // Body scope
            self.push_scope();
            for stmt in body {
                self.check_stmt(stmt);
            }
            self.pop_scope();

            // Capture the post-iteration outer scopes (same depth as `state`).
            let current: Vec<HashMap<String, SimpleType>> = self.locals[..outer_len].to_vec();

            // Join per-scope: for each scope index, join each binding.
            let mut next = state.clone();
            for (i, (prev_scope, cur_scope)) in state.iter().zip(current.iter()).enumerate() {
                for (name, prev_ty) in prev_scope {
                    if let Some(cur_ty) = cur_scope.get(name) {
                        let joined = self.join_types(prev_ty, cur_ty);
                        next[i].insert(name.clone(), joined);
                    }
                }
                // Also pick up any new bindings introduced in the body
                // that wrote into an outer scope (e.g. set on an outer var).
                for (name, cur_ty) in cur_scope {
                    if !prev_scope.contains_key(name) {
                        next[i].insert(name.clone(), cur_ty.clone());
                    }
                }
            }

            if next == state {
                break;
            }
            state = next;
        }

        // Write the converged per-scope state back.
        self.merge_loop_scopes_back(&state);
    }

    fn merge_loop_scopes_back(&mut self, state: &[HashMap<String, SimpleType>]) {
        let outer_len = self.locals.len().saturating_sub(1);
        for (i, scope_state) in state.iter().enumerate() {
            if i >= outer_len {
                break;
            }
            for (name, ty) in scope_state {
                if let Some(existing) = self.locals[i].get(name) {
                    let merged = self.join_types(existing, ty);
                    self.locals[i].insert(name.clone(), merged);
                } else {
                    self.locals[i].insert(name.clone(), ty.clone());
                }
            }
        }
    }

    fn join_types(&self, left: &SimpleType, right: &SimpleType) -> SimpleType {
        if left == right {
            return left.clone();
        }
        if matches!(left, SimpleType::Unknown) {
            return right.clone();
        }
        if matches!(right, SimpleType::Unknown) {
            return left.clone();
        }
        if Self::types_compatible(left, right) {
            return right.clone();
        }
        if Self::types_compatible(right, left) {
            return left.clone();
        }
        match (left, right) {
            (SimpleType::Union(left_items), SimpleType::Union(right_items)) => {
                let mut items = left_items.clone();
                for item in right_items {
                    if !items.iter().any(|existing| existing == item) {
                        items.push(item.clone());
                    }
                }
                SimpleType::Union(items)
            }
            (SimpleType::Union(items), other) | (other, SimpleType::Union(items)) => {
                if items.iter().any(|existing| existing == other) {
                    SimpleType::Union(items.clone())
                } else {
                    let mut next = items.clone();
                    next.push(other.clone());
                    SimpleType::Union(next)
                }
            }
            _ => SimpleType::Union(vec![left.clone(), right.clone()]),
        }
    }

    fn push_error(&mut self, message: String, span: Option<Span>) {
        self.errors.push(TypeError { message, span });
    }
}

fn expr_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Int(_, span, _) => Some(*span),
        Expr::String(_, span, _) => Some(*span),
        Expr::Bool(_, span, _) => Some(*span),
        Expr::Ident(ident) => Some(ident.span),
        Expr::Unary { op_span, .. } => Some(*op_span),
        Expr::Binary { op_span, .. } => Some(*op_span),
        Expr::Ternary { span, .. } => Some(*span),
        Expr::ChainedComparison { span, .. } => Some(*span),
        Expr::If { if_span, .. } => Some(*if_span),
        Expr::Member { name, .. } => Some(name.span),
        Expr::Call { callee, .. } => expr_span(callee),
        Expr::Try(expr, _) => expr_span(expr),
        Expr::Await { await_span, .. } => Some(*await_span),
        Expr::TryCatch { try_span, .. } => Some(*try_span),
        Expr::Match { match_span, .. } => Some(*match_span),
        Expr::Block { block_span, .. } => Some(*block_span),
        Expr::Array { array_span, .. } => Some(*array_span),
        Expr::ArraySpread { spread_span, .. } => Some(*spread_span),
        Expr::Index { index_span, .. } => Some(*index_span),
        Expr::Tuple { tuple_span, .. } => Some(*tuple_span),
        Expr::Range { range_span, .. } => Some(*range_span),
        Expr::InterpolatedString { span, .. } => Some(*span),
        Expr::Closure { span, .. } => Some(*span),
        Expr::StructLiteral { span, .. } => Some(*span),
        Expr::EnumLiteral { span, .. } => Some(*span),
        Expr::MapLiteral { span, .. } => Some(*span),
        Expr::As { span, .. } => Some(*span),
        Expr::Is { span, .. } => Some(*span),
        Expr::Group { span, .. } => Some(*span),
        Expr::Float(_, span, _) => Some(*span),
    }
}

fn format_type(ty: &SimpleType) -> String {
    match ty {
        SimpleType::Int => "int".to_string(),
        SimpleType::Bool => "bool".to_string(),
        SimpleType::String => "string".to_string(),
        SimpleType::Array(inner) => format!("array<{}>", format_type(inner)),
        SimpleType::Option(inner) => format!("option<{}>", format_type(inner)),
        SimpleType::Result(ok, err) => format!("result<{}, {}>", format_type(ok), format_type(err)),
        SimpleType::Unit => "unit".to_string(),
        SimpleType::Float => "float".to_string(),
        SimpleType::Function(params, return_ty) => {
            let params = params
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({params}) -> {}", format_type(return_ty))
        }
        SimpleType::Tuple(items) => {
            let inner = items.iter().map(format_type).collect::<Vec<_>>().join(", ");
            format!("({inner})")
        }
        SimpleType::Union(items) => items
            .iter()
            .map(format_type)
            .collect::<Vec<_>>()
            .join(" | "),
        SimpleType::Intersection(items) => items
            .iter()
            .map(format_type)
            .collect::<Vec<_>>()
            .join(" & "),
        SimpleType::Map(key, value) => {
            format!("map<{}, {}>", format_type(key), format_type(value))
        }
        SimpleType::Future(inner) => format!("future<{}>", format_type(inner)),
        SimpleType::Generator(inner) => format!("generator<{}>", format_type(inner)),
        SimpleType::Custom(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let args = args.iter().map(format_type).collect::<Vec<_>>().join(", ");
                format!("{}<{}>", name, args)
            }
        }
        SimpleType::TypeParam(name) => name.clone(),
        SimpleType::Unknown => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        infer_function_returns, typecheck_module, typecheck_module_with_mode, TypecheckMode,
    };
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
    fn infers_map_closure_return_inner() {
        let source = r#"
fn f() {
    return map([1, 2, 3], |x| x + 1);
}
"#;
        let module = parse_module(source).expect("parse module");
        let inferred = infer_function_returns(&module);
        assert_eq!(inferred.get("f").map(String::as_str), Some("array<int>"));
    }

    #[test]
    fn strict_mode_reports_unknown_flow() {
        let source = r#"
fn f() {
    let id = |x| x;
    id(1);
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module_with_mode(&module, TypecheckMode::Strict)
            .expect_err("expected strict type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("unknown in strict mode")));
    }

    #[test]
    fn allows_calling_local_closure() {
        let source = r#"
fn f() {
    let id = |x| x;
    id(1);
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn refines_local_closure_param_from_first_call() {
        let source = r#"
fn f() {
    let id = |x| x;
    id(1);
    id("no");
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("closure argument 1 type mismatch")));
    }

    #[test]
    fn closure_infers_params_from_function_arg() {
        let source = r#"
fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x);
}

fn main() -> int {
    return apply(|x| x + 1, 5);
}
"#;
        let module = parse_module(source).expect("parse module");
        let result = typecheck_module(&module);
        assert!(
            result.is_ok(),
            "closure param should be inferred as int from function signature, errors: {:?}",
            result.err()
        );
    }

    #[test]
    fn allows_async_call_with_await() {
        let source = r#"
async fn g() -> int {
    return 1;
}

async fn f() -> int {
    return await g();
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_await_non_future() {
        let source = r#"
async fn f() -> int {
    return await 1;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("await expects future")));
    }

    #[test]
    fn allows_try_catch_with_matching_types() {
        let source = r#"
fn f() -> int {
    return try {
        ok(1)
    } catch {
        0
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_try_catch_type_mismatch() {
        let source = r#"
fn f() {
    let value = try {
        ok(1)
    } catch {
        "no"
    };
    return value;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("try/catch type mismatch")));
    }

    #[test]
    fn allows_range_with_int_bounds() {
        let source = r#"
fn f() -> int {
    let nums = 1..5;
    return len(nums);
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_range_with_non_int_bounds() {
        let source = r#"
fn f() {
    let nums = "a"..5;
    return nums;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("range start expects int")));
    }

    #[test]
    fn allows_throw_result_and_defer_expr() {
        let source = r#"
fn f() -> result<int, string> {
    defer print("cleanup");
    throw err("no");
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_throw_non_result_value() {
        let source = r#"
fn f() {
    throw 1;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("throw expects result")));
    }

    #[test]
    fn allows_set_member_set_index_and_map_spread() {
        let source = r#"
struct Box {
    value: int,
}

fn f() -> int {
    let box = Box { value: 1 };
    set box.value = 2;

    let nums = [1, 2, 3];
    set nums[1] = 9;

    let base = map { "a": 1 };
    let merged = map { ...base, "b": 2 };
    return box.value + nums[1] + merged["b"];
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_map_spread_non_map_and_set_index_type() {
        let source = r#"
fn f() {
    let nums = [1, 2];
    set nums["x"] = 3;
    let merged = map { ...nums };
    return merged;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(
            errors
                .iter()
                .any(|err| err.message.contains("set index expects int")),
            "expected set index type error"
        );
        assert!(
            errors
                .iter()
                .any(|err| err.message.contains("spread expects map")),
            "expected map spread type error"
        );
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
    fn while_body_locals_do_not_duplicate_across_fixpoint_iterations() {
        let source = r#"
fn count_primes(limit: int) -> int {
    let n: int = 2;
    let count: int = 0;
    while n < limit {
        let d: int = 2;
        let is_prime: int = 1;
        while d * d <= n {
            let divisible = if n / d * d == n { 1 } else { 0 };
            set is_prime = if divisible { 0 } else { is_prime };
            set d = if divisible { n } else { d + 1 };
        }
        set count = if is_prime { count + 1 } else { count };
        set n = n + 1;
    }
    return count;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn loop_shadowing_does_not_leak_into_outer_type_state() {
        let source = r#"
fn f() -> int {
    let x: int = 0;
    while x < 1 {
        let x = "shadow";
        break;
    }
    return x;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn allows_map_literal_index_and_casts() {
        let source = r#"
fn f() {
    let grades = map { "taylor": 3, "casey": 5 };
    let taylor = grades["taylor"];
    let count = taylor as int;
    if taylor is int {
        return count;
    }
    return 0;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn allows_union_types() {
        let source = r#"
fn f() {
    let value: int | string = 1;
    if value is int {
        return value;
    }
    return 0;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
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
    fn allows_result_union_inner_compatibility() {
        let source = r#"
fn f() -> result<int | string, string> {
    return ok(1);
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(
            typecheck_module(&module).is_ok(),
            "union-compatible result inner should pass"
        );
    }

    #[test]
    fn allows_option_union_inner_compatibility() {
        let source = r#"
fn f() -> option<int | string> {
    return some("ok");
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(
            typecheck_module(&module).is_ok(),
            "union-compatible option inner should pass"
        );
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
    fn errors_on_missing_capability_for_rng_uuid() {
        let source = r#"
fn f() -> string {
    return rng.uuid();
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("missing capability: rng")));
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
    fn allows_index_tuple_with_constant_int() {
        let source = r#"
fn f() -> string {
    let pair = (1, "ok");
    return pair[1];
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_tuple_index_out_of_bounds() {
        let source = r#"
fn f() {
    let pair = (1, "ok");
    pair[2];
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("tuple index 2 out of bounds")));
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
    fn errors_on_set_const() {
        let source = r#"
fn f() {
    const value = 1;
    set value = 2;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("cannot assign to const")));
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

    #[test]
    fn allows_yield_in_generator() {
        let source = r#"
fn f() {
    yield 1;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn allows_declared_generator_return_type() {
        let source = r#"
fn f() -> generator<int> {
    yield 1;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_yield_with_non_generator_return() {
        let source = r#"
fn f() -> int {
    yield 1;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("yield requires generator return type")));
    }

    // ---- Enum type checking ----

    #[test]
    fn allows_enum_literal_construction() {
        let source = r#"
enum Dir { Up, Down, }

fn f() -> Dir {
    return Dir::Up;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_unknown_enum_variant() {
        let source = r#"
enum Dir { Up, Down, }

fn f() {
    let d = Dir::Left;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("unknown variant Dir::Left")));
    }

    #[test]
    fn errors_on_unknown_enum_name() {
        let source = r#"
fn f() {
    let d = Bogus::X;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("unknown enum: Bogus")));
    }

    #[test]
    fn match_enum_exhaustiveness() {
        let source = r#"
enum Dir { Up, Down, Left, }

fn f(d: Dir) -> int {
    return match d {
        Dir::Up => 1,
        Dir::Down => 2,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("non-exhaustive match for enum Dir")));
    }

    #[test]
    fn allows_exhaustive_enum_match() {
        let source = r#"
enum Dir { Up, Down, }

fn f(d: Dir) -> int {
    return match d {
        Dir::Up => 1,
        Dir::Down => 2,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn allows_exhaustive_enum_match_with_wildcard() {
        let source = r#"
enum Dir { Up, Down, Left, }

fn f(d: Dir) -> int {
    return match d {
        Dir::Up => 1,
        _ => 0,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    // ---- Generic type params ----

    #[test]
    fn allows_generic_function_call() {
        let source = r#"
fn identity<T>(x: T) -> T { return x; }

fn f() -> int { return identity(42); }
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn infers_generic_return_from_args() {
        let source = r#"
fn wrap<T>(x: T) -> option<T> { return some(x); }

fn f() -> option<int> { return wrap(1); }
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_duplicate_type_params() {
        let source = r#"
fn f<T, T>(a: T) -> T { return a; }
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate function type parameter")));
    }

    // ---- Deep struct checking ----

    #[test]
    fn errors_on_unknown_struct_name() {
        let source = r#"
fn f() {
    let x = Bogus { a: 1, };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("unknown struct: Bogus")));
    }

    #[test]
    fn errors_on_unknown_struct_field() {
        let source = r#"
struct Point { x: int, y: int, }

fn f() {
    let p = Point { x: 1, y: 2, z: 3, };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("unknown field z on struct Point")));
    }

    #[test]
    fn errors_on_missing_struct_field() {
        let source = r#"
struct Point { x: int, y: int, }

fn f() {
    let p = Point { x: 1, };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("missing field y for struct Point")));
    }

    #[test]
    fn errors_on_duplicate_struct_field_in_literal() {
        let source = r#"
struct Point { x: int, y: int, }

fn f() {
    let p = Point { x: 1, y: 2, x: 3, };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate field: x")));
    }

    // ---- Binary op type errors ----

    #[test]
    fn errors_on_add_incompatible_types() {
        let source = r#"
fn f() -> int {
    return 1 + "hello";
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors.iter().any(|err| err
            .message
            .contains("operator expects int, float, or string")));
    }

    #[test]
    fn errors_on_sub_string_operands() {
        let source = r#"
fn f() -> int {
    return "a" - "b";
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("operator expects int or float")));
    }

    #[test]
    fn errors_on_comparison_non_numeric() {
        let source = r#"
fn f() -> bool {
    return "a" < "b";
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("comparison expects number")));
    }

    #[test]
    fn allows_float_promotion_in_arithmetic() {
        let source = r#"
fn f() -> float {
    return 1 + 2.0;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    // ---- Type aliases ----

    #[test]
    fn allows_type_alias_in_annotation() {
        let source = r#"
type Id = int;

fn f(x: Id) -> Id { return x; }
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_duplicate_type_alias() {
        let source = r#"
type Id = int;
type Id = string;
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate type alias: Id")));
    }

    // ---- Try operator ----

    #[test]
    fn allows_try_operator_in_result_fn() {
        let source = r#"
fn inner() -> result<int, string> {
    return ok(1);
}

fn f() -> result<int, string> {
    let x = inner()?;
    return ok(x);
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_try_in_non_result_fn() {
        let source = r#"
fn inner() -> result<int, string> {
    return ok(1);
}

fn f() -> int {
    let x = inner()?;
    return x;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("? requires function returning result")));
    }

    // ---- Missing return detection ----

    #[test]
    fn errors_on_missing_return_in_typed_fn() {
        let source = r#"
fn f() -> int {
    let x = 1;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("not all paths return")));
    }

    #[test]
    fn allows_return_in_all_branches() {
        let source = r#"
fn f(x: bool) -> int {
    if x {
        return 1;
    } else {
        return 2;
    }
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    // ---- Match exhaustiveness ----

    #[test]
    fn errors_on_non_exhaustive_bool_match() {
        let source = r#"
fn f(x: bool) -> int {
    return match x {
        true => 1,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("non-exhaustive match for bool")));
    }

    #[test]
    fn errors_on_non_exhaustive_int_match() {
        let source = r#"
fn f(x: int) -> int {
    return match x {
        1 => 10,
        2 => 20,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("non-exhaustive match for int")));
    }

    #[test]
    fn allows_exhaustive_match_with_wildcard() {
        let source = r#"
fn f(x: int) -> int {
    return match x {
        1 => 10,
        _ => 0,
    };
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    // ---- Unary operators ----

    #[test]
    fn errors_on_negate_string() {
        let source = r#"
fn f() {
    let x = -"hello";
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("- expects int or float")));
    }

    #[test]
    fn errors_on_not_string() {
        let source = r#"
fn f() {
    let x = !"hello";
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("! expects bool or int")));
    }

    // ---- Ternary expression ----

    #[test]
    fn allows_ternary_with_matching_types() {
        let source = r#"
fn f(x: bool) -> int {
    return x ? 1 : 2;
}
"#;
        let module = parse_module(source).expect("parse module");
        assert!(typecheck_module(&module).is_ok());
    }

    #[test]
    fn errors_on_ternary_type_mismatch() {
        let source = r#"
fn f(x: bool) -> int {
    return x ? 1 : "no";
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = typecheck_module(&module).expect_err("expected type errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("ternary branch mismatch")));
    }
}
