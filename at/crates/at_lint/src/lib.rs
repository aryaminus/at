use std::collections::{HashMap, HashSet};

use at_syntax::{Expr, Ident, InterpPart, Module, Span, Stmt};

/// An edit to apply to fix a lint error
#[derive(Debug, Clone)]
pub struct LintFix {
    /// Description of the fix
    pub description: String,
    /// The span to replace
    pub span: Span,
    /// The replacement text
    pub replacement: String,
}

/// A lint error with optional auto-fix information
#[derive(Debug, Clone)]
pub struct LintError {
    pub message: String,
    pub span: Option<Span>,
    /// Optional auto-fix for this error
    pub fix: Option<LintFix>,
}

impl LintError {
    /// Create a simple lint error without a fix
    pub fn new(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
            fix: None,
        }
    }

    /// Create a lint error with an auto-fix
    pub fn with_fix(
        message: impl Into<String>,
        span: Option<Span>,
        fix_description: impl Into<String>,
        replacement: impl Into<String>,
    ) -> Self {
        Self {
            message: message.into(),
            span,
            fix: Some(LintFix {
                description: fix_description.into(),
                span: span.unwrap_or(Span { start: 0, end: 0 }),
                replacement: replacement.into(),
            }),
        }
    }
}

pub fn lint_module(module: &Module) -> Result<(), Vec<LintError>> {
    let mut errors = Vec::new();
    let mut aliases: Vec<Ident> = Vec::new();
    let mut import_paths: Vec<(String, Ident)> = Vec::new();
    for stmt in &module.stmts {
        if let Stmt::Import { alias, .. } = stmt {
            aliases.push(alias.clone());
        }
        if let Stmt::Import { path, alias } = stmt {
            import_paths.push((path.clone(), alias.clone()));
        }
    }

    let mut used = HashSet::new();
    collect_alias_usage(module, &mut used);

    let mut seen_aliases: HashMap<String, Ident> = HashMap::new();
    for alias in &aliases {
        if let Some(_existing) = seen_aliases.get(&alias.name) {
            errors.push(LintError::new(
                format!("duplicate import alias: {}", alias.name),
                Some(alias.span),
            ));
        } else {
            seen_aliases.insert(alias.name.clone(), alias.clone());
        }
    }

    for alias in aliases {
        if should_ignore_name(&alias.name) {
            continue;
        }
        if !used.contains(&alias.name) {
            errors.push(LintError::new(
                format!("unused import alias: {}", alias.name),
                Some(alias.span),
            ));
        }
    }

    let mut seen_paths: HashMap<String, Ident> = HashMap::new();
    let mut import_order_check: Vec<(String, Span)> = Vec::new();
    for (path, alias) in import_paths {
        if let Some(existing) = seen_paths.get(&path) {
            if existing.name != alias.name {
                errors.push(LintError::new(
                    format!(
                        "duplicate import path with different alias: {path} (aliases: {}, {})",
                        existing.name, alias.name
                    ),
                    Some(alias.span),
                ));
            }
        } else {
            seen_paths.insert(path.clone(), alias.clone());
            import_order_check.push((path, alias.span));
        }
    }

    // Check import ordering
    if import_order_check.len() >= 2 {
        for i in 1..import_order_check.len() {
            let prev = &import_order_check[i - 1];
            let curr = &import_order_check[i];
            if prev.0 > curr.0 {
                errors.push(LintError::new(
                    format!(
                        "import not in alphabetical order: '{}' should come before '{}'",
                        curr.0, prev.0
                    ),
                    Some(curr.1),
                ));
            }
        }
    }

    let mut defined_functions: Vec<Ident> = Vec::new();
    let mut function_names: HashMap<String, Ident> = HashMap::new();
    for func in &module.functions {
        if let Some(_existing) = function_names.get(&func.name.name) {
            errors.push(LintError::new(
                format!("duplicate function: {}", func.name.name),
                Some(func.name.span),
            ));
        } else {
            function_names.insert(func.name.name.clone(), func.name.clone());
        }
        if !func.is_tool {
            defined_functions.push(func.name.clone());
        }
    }

    let mut used_functions = HashSet::new();
    collect_called_functions(module, &mut used_functions);
    for func_name in defined_functions {
        if should_ignore_name(&func_name.name) {
            continue;
        }
        if !used_functions.contains(&func_name.name) {
            errors.push(LintError::new(
                format!("unused function: {}", func_name.name),
                Some(func_name.span),
            ));
        }
    }

    lint_unused_locals(module, &mut errors);
    lint_unused_match_bindings(module, &mut errors);
    lint_unreachable_code(module, &mut errors);
    lint_unnecessary_needs(module, &mut errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn lint_unreachable_code(module: &Module, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        check_unreachable_stmts(&func.body, errors);
    }
    check_unreachable_stmts(&module.stmts, errors);
}

fn check_unreachable_stmts(stmts: &[Stmt], errors: &mut Vec<LintError>) {
    let mut unreachable_start: Option<Span> = None;

    for stmt in stmts {
        match stmt {
            Stmt::Break { break_span }
            | Stmt::Continue {
                continue_span: break_span,
            } => {
                if unreachable_start.is_none() {
                    unreachable_start = Some(*break_span);
                }
            }
            Stmt::Return(Some(_expr)) => {
                if unreachable_start.is_none() {
                    // Get span from the statement itself if possible
                    unreachable_start = Some(Span { start: 0, end: 0 });
                }
            }
            Stmt::Return(None) => {
                if unreachable_start.is_none() {
                    // Use a default span since Return without value has no span
                    unreachable_start = Some(Span { start: 0, end: 0 });
                }
            }
            _ => {
                if let Some(span) = unreachable_start {
                    errors.push(LintError::new(
                        "unreachable code after return/break/continue".to_string(),
                        Some(span),
                    ));
                    unreachable_start = None;
                }
            }
        }

        // Recursively check nested blocks
        match stmt {
            Stmt::While { body, .. } => check_unreachable_stmts(body, errors),
            Stmt::For { body, .. } => check_unreachable_stmts(body, errors),
            Stmt::Block(nested_stmts) => check_unreachable_stmts(nested_stmts, errors),
            Stmt::Test { body, .. } => check_unreachable_stmts(body, errors),
            _ => {}
        }
    }
}

fn lint_unused_match_bindings(module: &Module, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        for stmt in &func.body {
            lint_unused_match_bindings_stmt(stmt, errors);
        }
    }
    for stmt in &module.stmts {
        lint_unused_match_bindings_stmt(stmt, errors);
    }
}

fn lint_unnecessary_needs(module: &Module, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        if func.needs.is_empty() {
            continue;
        }

        // Collect all capabilities used in the function body
        let mut used_capabilities = HashSet::new();
        for stmt in &func.body {
            collect_used_capabilities_stmt(stmt, &mut used_capabilities);
        }

        // Check if any declared needs are unused
        for need in &func.needs {
            if !used_capabilities.contains(&need.name) {
                errors.push(LintError::new(
                    format!("unnecessary needs: '{}' declared but not used", need.name),
                    Some(need.span),
                ));
            }
        }
    }
}

fn collect_used_capabilities_stmt(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Using { value, .. } | Stmt::Expr(value) => {
            collect_used_capabilities_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_used_capabilities_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_used_capabilities_expr(base, used);
            collect_used_capabilities_expr(value, used);
        }
        Stmt::SetIndex { base, index, value } => {
            collect_used_capabilities_expr(base, used);
            collect_used_capabilities_expr(index, used);
            collect_used_capabilities_expr(value, used);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_used_capabilities_expr(condition, used);
            for stmt in body {
                collect_used_capabilities_stmt(stmt, used);
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_used_capabilities_expr(iter, used);
            for stmt in body {
                collect_used_capabilities_stmt(stmt, used);
            }
        }
        Stmt::Return(value) => {
            if let Some(expr) = value {
                collect_used_capabilities_expr(expr, used);
            }
        }
        Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                collect_used_capabilities_stmt(stmt, used);
            }
        }
        _ => {}
    }
}

fn collect_used_capabilities_expr(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Member { base, .. } => {
            if let Expr::Ident(ident) = base.as_ref() {
                used.insert(ident.name.clone());
            }
            collect_used_capabilities_expr(base, used);
        }
        Expr::Call { callee, args } => {
            collect_used_capabilities_expr(callee, used);
            for arg in args {
                collect_used_capabilities_expr(arg, used);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_used_capabilities_expr(left, used);
            collect_used_capabilities_expr(right, used);
        }
        Expr::Unary { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_used_capabilities_expr(condition, used);
            collect_used_capabilities_expr(then_branch, used);
            if let Some(else_expr) = else_branch {
                collect_used_capabilities_expr(else_expr, used);
            }
        }
        Expr::Match { value, arms, .. } => {
            collect_used_capabilities_expr(value, used);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_used_capabilities_expr(guard, used);
                }
                collect_used_capabilities_expr(&arm.body, used);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            for stmt in stmts {
                collect_used_capabilities_stmt(stmt, used);
            }
            if let Some(expr) = tail {
                collect_used_capabilities_expr(expr, used);
            }
        }
        Expr::Array { items, .. } => {
            for item in items {
                collect_used_capabilities_expr(item, used);
            }
        }
        Expr::Index { base, index, .. } => {
            collect_used_capabilities_expr(base, used);
            collect_used_capabilities_expr(index, used);
        }
        Expr::Try(expr) => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                collect_used_capabilities_expr(item, used);
            }
        }
        Expr::Range { start, end, .. } => {
            collect_used_capabilities_expr(start, used);
            collect_used_capabilities_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr) = part {
                    collect_used_capabilities_expr(expr, used);
                }
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                collect_used_capabilities_expr(&field.value, used);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                collect_used_capabilities_expr(expr, used);
            }
        }
        Expr::Group { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::Closure { body, .. } => {
            collect_used_capabilities_expr(body, used);
        }
        _ => {}
    }
}

fn lint_unused_match_bindings_stmt(stmt: &Stmt, errors: &mut Vec<LintError>) {
    match stmt {
        Stmt::Import { .. } | Stmt::Struct { .. } | Stmt::TypeAlias { .. } | Stmt::Enum { .. } => {}
        Stmt::Let { value, .. } | Stmt::Using { value, .. } | Stmt::Expr(value) => {
            lint_unused_match_bindings_expr(value, errors);
        }
        Stmt::Set { value, .. } => {
            lint_unused_match_bindings_expr(value, errors);
        }
        Stmt::SetMember { base, value, .. } => {
            lint_unused_match_bindings_expr(base, errors);
            lint_unused_match_bindings_expr(value, errors);
        }
        Stmt::SetIndex { base, index, value } => {
            lint_unused_match_bindings_expr(base, errors);
            lint_unused_match_bindings_expr(index, errors);
            lint_unused_match_bindings_expr(value, errors);
        }
        Stmt::While {
            condition, body, ..
        } => {
            lint_unused_match_bindings_expr(condition, errors);
            for stmt in body {
                lint_unused_match_bindings_stmt(stmt, errors);
            }
        }
        Stmt::For { iter, body, .. } => {
            lint_unused_match_bindings_expr(iter, errors);
            for stmt in body {
                lint_unused_match_bindings_stmt(stmt, errors);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return(value) => {
            if let Some(expr) = value {
                lint_unused_match_bindings_expr(expr, errors);
            }
        }
        Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                lint_unused_match_bindings_stmt(stmt, errors);
            }
        }
    }
}

fn lint_unused_match_bindings_expr(expr: &Expr, errors: &mut Vec<LintError>) {
    match expr {
        Expr::Match { value, arms, .. } => {
            lint_unused_match_bindings_expr(value, errors);
            for arm in arms {
                let mut used = HashSet::new();
                if let Some(guard) = &arm.guard {
                    collect_local_uses_expr(guard, &mut used);
                }
                collect_local_uses_expr(&arm.body, &mut used);
                for ident in match_pattern_idents(&arm.pattern) {
                    if should_ignore_name(&ident.name) {
                        continue;
                    }
                    if !used.contains(&ident.name) {
                        errors.push(LintError::new(
                            format!("unused match binding: {}", ident.name),
                            Some(ident.span),
                        ));
                    }
                }
                if let Some(guard) = &arm.guard {
                    lint_unused_match_bindings_expr(guard, errors);
                }
                lint_unused_match_bindings_expr(&arm.body, errors);
            }
        }
        Expr::Binary { left, right, .. } => {
            lint_unused_match_bindings_expr(left, errors);
            lint_unused_match_bindings_expr(right, errors);
        }
        Expr::Unary { expr, .. } => {
            lint_unused_match_bindings_expr(expr, errors);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_unused_match_bindings_expr(condition, errors);
            lint_unused_match_bindings_expr(then_branch, errors);
            if let Some(else_expr) = else_branch {
                lint_unused_match_bindings_expr(else_expr, errors);
            }
        }
        Expr::Call { callee, args } => {
            lint_unused_match_bindings_expr(callee, errors);
            for arg in args {
                lint_unused_match_bindings_expr(arg, errors);
            }
        }
        Expr::Member { base, .. } => {
            lint_unused_match_bindings_expr(base, errors);
        }
        Expr::Try(expr) => {
            lint_unused_match_bindings_expr(expr, errors);
        }
        Expr::Block { stmts, tail, .. } => {
            for stmt in stmts {
                lint_unused_match_bindings_stmt(stmt, errors);
            }
            if let Some(expr) = tail {
                lint_unused_match_bindings_expr(expr, errors);
            }
        }
        Expr::Array { items, .. } => {
            for item in items {
                lint_unused_match_bindings_expr(item, errors);
            }
        }
        Expr::Index { base, index, .. } => {
            lint_unused_match_bindings_expr(base, errors);
            lint_unused_match_bindings_expr(index, errors);
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                lint_unused_match_bindings_expr(item, errors);
            }
        }
        Expr::Range { start, end, .. } => {
            lint_unused_match_bindings_expr(start, errors);
            lint_unused_match_bindings_expr(end, errors);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr) = part {
                    lint_unused_match_bindings_expr(expr, errors);
                }
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                lint_unused_match_bindings_expr(&field.value, errors);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                lint_unused_match_bindings_expr(expr, errors);
            }
        }
        Expr::Group { expr, .. } => {
            lint_unused_match_bindings_expr(expr, errors);
        }
        Expr::Closure { body, .. } => {
            lint_unused_match_bindings_expr(body, errors);
        }
        _ => {}
    }
}

fn match_pattern_idents(pattern: &at_syntax::MatchPattern) -> Vec<Ident> {
    match pattern {
        at_syntax::MatchPattern::Int(_) => Vec::new(),
        at_syntax::MatchPattern::ResultOk(ident)
        | at_syntax::MatchPattern::ResultErr(ident)
        | at_syntax::MatchPattern::OptionSome(ident) => vec![ident.clone()],
        at_syntax::MatchPattern::OptionNone => Vec::new(),
        at_syntax::MatchPattern::Struct { fields, .. } => fields
            .iter()
            .filter_map(|field| field.binding.clone().or_else(|| Some(field.name.clone())))
            .collect(),
        at_syntax::MatchPattern::Enum { binding, .. } => {
            binding.clone().map_or_else(Vec::new, |ident| vec![ident])
        }
        at_syntax::MatchPattern::Wildcard => Vec::new(),
    }
}

fn lint_unused_locals(module: &Module, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        let mut locals = Vec::new();
        let mut used = HashSet::new();
        let mut seen = HashSet::new();
        for param in &func.params {
            if should_ignore_name(&param.name.name) {
                continue;
            }
            if !seen.insert(param.name.name.clone()) {
                errors.push(LintError::new(
                    format!("duplicate local: {}", param.name.name),
                    Some(param.name.span),
                ));
            }
            locals.push(param.name.clone());
        }
        for stmt in &func.body {
            collect_local_defs_stmt(stmt, &mut locals, &mut seen, errors);
            collect_local_uses_stmt(stmt, &mut used);
        }
        for local in locals {
            if should_ignore_name(&local.name) {
                continue;
            }
            if !used.contains(&local.name) {
                errors.push(LintError::new(
                    format!("unused local: {}", local.name),
                    Some(local.span),
                ));
            }
        }
    }

    let mut locals = Vec::new();
    let mut used = HashSet::new();
    let mut seen = HashSet::new();
    for stmt in &module.stmts {
        collect_local_defs_stmt(stmt, &mut locals, &mut seen, errors);
        collect_local_uses_stmt(stmt, &mut used);
    }
    for local in locals {
        if should_ignore_name(&local.name) {
            continue;
        }
        if !used.contains(&local.name) {
            errors.push(LintError::new(
                format!("unused local: {}", local.name),
                Some(local.span),
            ));
        }
    }
}

fn should_ignore_name(name: &str) -> bool {
    name.starts_with('_')
}

fn collect_local_defs_stmt(
    stmt: &Stmt,
    locals: &mut Vec<Ident>,
    seen: &mut HashSet<String>,
    errors: &mut Vec<LintError>,
) {
    match stmt {
        Stmt::Let { name, .. } | Stmt::Using { name, .. } => {
            if should_ignore_name(&name.name) {
                locals.push(name.clone());
                return;
            }
            if !seen.insert(name.name.clone()) {
                errors.push(LintError::new(
                    format!("duplicate local: {}", name.name),
                    Some(name.span),
                ));
            }
            locals.push(name.clone());
        }
        Stmt::Set { .. } | Stmt::SetMember { .. } | Stmt::SetIndex { .. } => {}
        Stmt::While { body, .. } => {
            let mut inner_seen = HashSet::new();
            for stmt in body {
                collect_local_defs_stmt(stmt, locals, &mut inner_seen, errors);
            }
        }
        Stmt::For { item, body, .. } => {
            let mut inner_seen = HashSet::new();
            if !should_ignore_name(&item.name) {
                inner_seen.insert(item.name.clone());
                locals.push(item.clone());
            }
            for stmt in body {
                collect_local_defs_stmt(stmt, locals, &mut inner_seen, errors);
            }
        }
        Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
            let mut inner_seen = HashSet::new();
            for stmt in stmts {
                collect_local_defs_stmt(stmt, locals, &mut inner_seen, errors);
            }
        }
        _ => {}
    }
}

fn collect_local_uses_stmt(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Import { .. } | Stmt::Struct { .. } | Stmt::TypeAlias { .. } | Stmt::Enum { .. } => {}
        Stmt::Let { value, .. } | Stmt::Using { value, .. } | Stmt::Expr(value) => {
            collect_local_uses_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_local_uses_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_local_uses_expr(base, used);
            collect_local_uses_expr(value, used);
        }
        Stmt::SetIndex { base, index, value } => {
            collect_local_uses_expr(base, used);
            collect_local_uses_expr(index, used);
            collect_local_uses_expr(value, used);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_local_uses_expr(condition, used);
            for stmt in body {
                collect_local_uses_stmt(stmt, used);
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_local_uses_expr(iter, used);
            for stmt in body {
                collect_local_uses_stmt(stmt, used);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return(value) => {
            if let Some(expr) = value {
                collect_local_uses_expr(expr, used);
            }
        }
        Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                collect_local_uses_stmt(stmt, used);
            }
        }
    }
}

fn collect_local_uses_expr(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Ident(ident) => {
            used.insert(ident.name.clone());
        }
        Expr::Unary { expr, .. } => {
            collect_local_uses_expr(expr, used);
        }
        Expr::Binary { left, right, .. } => {
            collect_local_uses_expr(left, used);
            collect_local_uses_expr(right, used);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_local_uses_expr(condition, used);
            collect_local_uses_expr(then_branch, used);
            if let Some(else_expr) = else_branch {
                collect_local_uses_expr(else_expr, used);
            }
        }
        Expr::Member { base, .. } => {
            if let Expr::Ident(ident) = base.as_ref() {
                used.insert(ident.name.clone());
            } else {
                collect_local_uses_expr(base, used);
            }
        }
        Expr::Call { callee, args } => {
            collect_local_uses_expr(callee, used);
            for arg in args {
                collect_local_uses_expr(arg, used);
            }
        }
        Expr::Try(expr) => {
            collect_local_uses_expr(expr, used);
        }
        Expr::Match { value, arms, .. } => {
            collect_local_uses_expr(value, used);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_local_uses_expr(guard, used);
                }
                collect_local_uses_expr(&arm.body, used);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            for stmt in stmts {
                collect_local_uses_stmt(stmt, used);
            }
            if let Some(expr) = tail {
                collect_local_uses_expr(expr, used);
            }
        }
        Expr::Array { items, .. } => {
            for item in items {
                collect_local_uses_expr(item, used);
            }
        }
        Expr::Index { base, index, .. } => {
            collect_local_uses_expr(base, used);
            collect_local_uses_expr(index, used);
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                collect_local_uses_expr(item, used);
            }
        }
        Expr::Range { start, end, .. } => {
            collect_local_uses_expr(start, used);
            collect_local_uses_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr) = part {
                    collect_local_uses_expr(expr, used);
                }
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                collect_local_uses_expr(&field.value, used);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                collect_local_uses_expr(expr, used);
            }
        }
        Expr::Group { expr, .. } => {
            collect_local_uses_expr(expr, used);
        }
        Expr::Closure { body, .. } => {
            collect_local_uses_expr(body, used);
        }
        _ => {}
    }
}

fn collect_alias_usage(module: &Module, used: &mut HashSet<String>) {
    for func in &module.functions {
        for stmt in &func.body {
            collect_alias_usage_stmt(stmt, used);
        }
    }
    for stmt in &module.stmts {
        collect_alias_usage_stmt(stmt, used);
    }
}

fn collect_alias_usage_stmt(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Import { .. } | Stmt::Struct { .. } | Stmt::TypeAlias { .. } | Stmt::Enum { .. } => {}
        Stmt::Let { value, .. } | Stmt::Using { value, .. } | Stmt::Expr(value) => {
            collect_alias_usage_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_alias_usage_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_alias_usage_expr(base, used);
            collect_alias_usage_expr(value, used);
        }
        Stmt::SetIndex { base, index, value } => {
            collect_alias_usage_expr(base, used);
            collect_alias_usage_expr(index, used);
            collect_alias_usage_expr(value, used);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_alias_usage_expr(condition, used);
            for stmt in body {
                collect_alias_usage_stmt(stmt, used);
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_alias_usage_expr(iter, used);
            for stmt in body {
                collect_alias_usage_stmt(stmt, used);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return(value) => {
            if let Some(expr) = value {
                collect_alias_usage_expr(expr, used);
            }
        }
        Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                collect_alias_usage_stmt(stmt, used);
            }
        }
    }
}

fn collect_alias_usage_expr(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Ident(ident) => {
            used.insert(ident.name.clone());
        }
        Expr::Unary { expr, .. } => {
            collect_alias_usage_expr(expr, used);
        }
        Expr::Binary { left, right, .. } => {
            collect_alias_usage_expr(left, used);
            collect_alias_usage_expr(right, used);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_alias_usage_expr(condition, used);
            collect_alias_usage_expr(then_branch, used);
            if let Some(else_expr) = else_branch {
                collect_alias_usage_expr(else_expr, used);
            }
        }
        Expr::Member { base, .. } => {
            if let Expr::Ident(ident) = base.as_ref() {
                used.insert(ident.name.clone());
            } else {
                collect_alias_usage_expr(base, used);
            }
        }
        Expr::Call { callee, args } => {
            collect_alias_usage_expr(callee, used);
            for arg in args {
                collect_alias_usage_expr(arg, used);
            }
        }
        Expr::Try(expr) => {
            collect_alias_usage_expr(expr, used);
        }
        Expr::Match { value, arms, .. } => {
            collect_alias_usage_expr(value, used);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_alias_usage_expr(guard, used);
                }
                collect_alias_usage_expr(&arm.body, used);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            for stmt in stmts {
                collect_alias_usage_stmt(stmt, used);
            }
            if let Some(expr) = tail {
                collect_alias_usage_expr(expr, used);
            }
        }
        Expr::Array { items, .. } => {
            for item in items {
                collect_alias_usage_expr(item, used);
            }
        }
        Expr::Index { base, index, .. } => {
            collect_alias_usage_expr(base, used);
            collect_alias_usage_expr(index, used);
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                collect_alias_usage_expr(item, used);
            }
        }
        Expr::Range { start, end, .. } => {
            collect_alias_usage_expr(start, used);
            collect_alias_usage_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr) = part {
                    collect_alias_usage_expr(expr, used);
                }
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                collect_alias_usage_expr(&field.value, used);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                collect_alias_usage_expr(expr, used);
            }
        }
        Expr::Group { expr, .. } => {
            collect_alias_usage_expr(expr, used);
        }
        Expr::Closure { body, .. } => {
            collect_alias_usage_expr(body, used);
        }
        _ => {}
    }
}

fn collect_called_functions(module: &Module, used: &mut HashSet<String>) {
    for func in &module.functions {
        for stmt in &func.body {
            collect_called_functions_stmt(stmt, used);
        }
    }
    for stmt in &module.stmts {
        collect_called_functions_stmt(stmt, used);
    }
}

fn collect_called_functions_stmt(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Import { .. } | Stmt::Struct { .. } | Stmt::TypeAlias { .. } | Stmt::Enum { .. } => {}
        Stmt::Let { value, .. } | Stmt::Using { value, .. } | Stmt::Expr(value) => {
            collect_called_functions_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_called_functions_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_called_functions_expr(base, used);
            collect_called_functions_expr(value, used);
        }
        Stmt::SetIndex { base, index, value } => {
            collect_called_functions_expr(base, used);
            collect_called_functions_expr(index, used);
            collect_called_functions_expr(value, used);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_called_functions_expr(condition, used);
            for stmt in body {
                collect_called_functions_stmt(stmt, used);
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_called_functions_expr(iter, used);
            for stmt in body {
                collect_called_functions_stmt(stmt, used);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return(value) => {
            if let Some(expr) = value {
                collect_called_functions_expr(expr, used);
            }
        }
        Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                collect_called_functions_stmt(stmt, used);
            }
        }
    }
}

fn collect_called_functions_expr(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Call { callee, args } => {
            if let Expr::Ident(ident) = callee.as_ref() {
                used.insert(ident.name.clone());
            }
            collect_called_functions_expr(callee, used);
            for arg in args {
                collect_called_functions_expr(arg, used);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_called_functions_expr(left, used);
            collect_called_functions_expr(right, used);
        }
        Expr::Unary { expr, .. } => {
            collect_called_functions_expr(expr, used);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_called_functions_expr(condition, used);
            collect_called_functions_expr(then_branch, used);
            if let Some(else_expr) = else_branch {
                collect_called_functions_expr(else_expr, used);
            }
        }
        Expr::Member { base, .. } => {
            collect_called_functions_expr(base, used);
        }
        Expr::Try(expr) => {
            collect_called_functions_expr(expr, used);
        }
        Expr::Match { value, arms, .. } => {
            collect_called_functions_expr(value, used);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_called_functions_expr(guard, used);
                }
                collect_called_functions_expr(&arm.body, used);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            for stmt in stmts {
                collect_called_functions_stmt(stmt, used);
            }
            if let Some(expr) = tail {
                collect_called_functions_expr(expr, used);
            }
        }
        Expr::Array { items, .. } => {
            for item in items {
                collect_called_functions_expr(item, used);
            }
        }
        Expr::Index { base, index, .. } => {
            collect_called_functions_expr(base, used);
            collect_called_functions_expr(index, used);
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                collect_called_functions_expr(item, used);
            }
        }
        Expr::Range { start, end, .. } => {
            collect_called_functions_expr(start, used);
            collect_called_functions_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr) = part {
                    collect_called_functions_expr(expr, used);
                }
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                collect_called_functions_expr(&field.value, used);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                collect_called_functions_expr(expr, used);
            }
        }
        Expr::Group { expr, .. } => {
            collect_called_functions_expr(expr, used);
        }
        Expr::Closure { body, .. } => {
            collect_called_functions_expr(body, used);
        }
        _ => {}
    }
}

/// Apply auto-fixes to source code
///
/// Takes source code and a list of lint errors with fixes, applies them in reverse order
/// (from end to start) to avoid offset shifting issues.
///
/// Returns the fixed source code.
pub fn apply_fixes(source: &str, errors: &[LintError]) -> String {
    // Collect all fixes with their spans
    let mut fixes: Vec<(Span, String)> = errors
        .iter()
        .filter_map(|err| {
            err.fix
                .as_ref()
                .map(|fix| (fix.span, fix.replacement.clone()))
        })
        .collect();

    // Sort by span end (descending) to apply from end to start
    // This prevents offset shifts when applying multiple fixes
    fixes.sort_by(|a, b| b.0.end.cmp(&a.0.end));

    let mut result = source.to_string();

    for (span, replacement) in fixes {
        if span.end <= result.len() && span.start <= span.end {
            result.replace_range(span.start..span.end, &replacement);
        }
    }

    result
}

/// Get the number of errors that have auto-fixes available
pub fn count_fixable(errors: &[LintError]) -> usize {
    errors.iter().filter(|err| err.fix.is_some()).count()
}

#[cfg(test)]
mod tests {
    use super::lint_module;
    use at_parser::parse_module;

    #[test]
    fn detects_unused_function() {
        let source = r#"
fn unused_helper() -> int {
    return 42;
}

fn main() {
    return 0;
}

main();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("unused function")
                && err.message.contains("unused_helper")));
    }

    #[test]
    fn detects_duplicate_import_alias() {
        let source = r#"
import "./a.at" as foo;
import "./b.at" as foo;
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate import alias")));
    }

    #[test]
    fn detects_duplicate_function() {
        let source = r#"
fn f() {}
fn f() {}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate function")));
    }

    #[test]
    fn detects_duplicate_local_in_scope() {
        let source = r#"
fn f() {
    let x = 1;
    let x = 2;
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        assert!(errors
            .iter()
            .any(|err| err.message.contains("duplicate local")));
    }

    #[test]
    fn allows_shadowing_in_block() {
        let source = r#"
fn f() {
    let _x = 1;
    {
        let _x = 2;
    }
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        assert!(lint_module(&module).is_ok());
    }

    #[test]
    fn allows_duplicate_ignored_locals() {
        let source = r#"
fn f(_x: int, _x: int) {
    let _y = 1;
    let _y = 2;
}
f(1, 2);
"#;
        let module = parse_module(source).expect("parse module");
        assert!(lint_module(&module).is_ok());
    }
}
