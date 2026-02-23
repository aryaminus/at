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

/// Lint severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintSeverity {
    Error,
    Warn,
    Info,
}

/// Lint rule identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LintRule {
    DuplicateImportAlias,
    UnusedImportAlias,
    DuplicateImportPath,
    ImportOrder,
    DuplicateFunction,
    UnusedFunction,
    UnusedLocal,
    UnusedMatchBinding,
    UnreachableCode,
    UnnecessaryNeeds,
}

impl LintRule {
    fn from_str(value: &str) -> Option<Self> {
        match value {
            "duplicate_import_alias" => Some(LintRule::DuplicateImportAlias),
            "unused_import_alias" => Some(LintRule::UnusedImportAlias),
            "duplicate_import_path" => Some(LintRule::DuplicateImportPath),
            "import_order" => Some(LintRule::ImportOrder),
            "duplicate_function" => Some(LintRule::DuplicateFunction),
            "unused_function" => Some(LintRule::UnusedFunction),
            "unused_local" => Some(LintRule::UnusedLocal),
            "unused_match_binding" => Some(LintRule::UnusedMatchBinding),
            "unreachable_code" => Some(LintRule::UnreachableCode),
            "unnecessary_needs" => Some(LintRule::UnnecessaryNeeds),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct LintConfig {
    enabled: HashSet<LintRule>,
    severity: HashMap<LintRule, LintSeverity>,
}

impl LintConfig {
    fn default() -> Self {
        let rules = [
            LintRule::DuplicateImportAlias,
            LintRule::UnusedImportAlias,
            LintRule::DuplicateImportPath,
            LintRule::ImportOrder,
            LintRule::DuplicateFunction,
            LintRule::UnusedFunction,
            LintRule::UnusedLocal,
            LintRule::UnusedMatchBinding,
            LintRule::UnreachableCode,
            LintRule::UnnecessaryNeeds,
        ];
        let mut enabled = HashSet::new();
        let mut severity = HashMap::new();
        for rule in rules {
            enabled.insert(rule);
            let level = match rule {
                LintRule::UnusedImportAlias
                | LintRule::UnusedFunction
                | LintRule::UnusedLocal
                | LintRule::UnusedMatchBinding
                | LintRule::UnnecessaryNeeds => LintSeverity::Warn,
                LintRule::ImportOrder => LintSeverity::Info,
                _ => LintSeverity::Error,
            };
            severity.insert(rule, level);
        }
        Self { enabled, severity }
    }

    fn load(source: &str) -> Self {
        let mut config = Self::default();
        let parsed = source.parse::<toml::Value>();
        let Ok(value) = parsed else {
            return config;
        };
        let Some(table) = value.as_table() else {
            return config;
        };
        if let Some(rules) = table.get("rules").and_then(|value| value.as_table()) {
            for (key, value) in rules {
                let Some(rule) = LintRule::from_str(key) else {
                    continue;
                };
                match value.as_str().unwrap_or_default() {
                    "off" => {
                        config.enabled.remove(&rule);
                    }
                    "error" => {
                        config.enabled.insert(rule);
                        config.severity.insert(rule, LintSeverity::Error);
                    }
                    "warn" => {
                        config.enabled.insert(rule);
                        config.severity.insert(rule, LintSeverity::Warn);
                    }
                    "info" => {
                        config.enabled.insert(rule);
                        config.severity.insert(rule, LintSeverity::Info);
                    }
                    _ => {}
                }
            }
        }
        if let Some(disabled) = table.get("disable").and_then(|value| value.as_array()) {
            for entry in disabled {
                if let Some(name) = entry.as_str() {
                    if let Some(rule) = LintRule::from_str(name) {
                        config.enabled.remove(&rule);
                    }
                }
            }
        }
        config
    }

    fn with_suppressions(mut self, suppressed: &HashSet<LintRule>) -> Self {
        for rule in suppressed {
            self.enabled.remove(rule);
        }
        self
    }
}

/// A lint error with optional auto-fix information
#[derive(Debug, Clone)]
pub struct LintError {
    pub message: String,
    pub span: Option<Span>,
    pub severity: LintSeverity,
    pub rule: Option<LintRule>,
    /// Optional auto-fix for this error
    pub fix: Option<LintFix>,
}

impl LintError {
    /// Create a simple lint error without a fix
    pub fn new(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
            severity: LintSeverity::Error,
            rule: None,
            fix: None,
        }
    }

    pub fn with_rule(
        rule: LintRule,
        message: impl Into<String>,
        span: Option<Span>,
        severity: LintSeverity,
    ) -> Self {
        Self {
            message: message.into(),
            span,
            severity,
            rule: Some(rule),
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
            severity: LintSeverity::Error,
            rule: None,
            fix: Some(LintFix {
                description: fix_description.into(),
                span: span.unwrap_or(Span::new(0, 0)),
                replacement: replacement.into(),
            }),
        }
    }

    pub fn with_rule_fix(
        rule: LintRule,
        message: impl Into<String>,
        span: Option<Span>,
        severity: LintSeverity,
        fix_description: impl Into<String>,
        replacement: impl Into<String>,
    ) -> Self {
        Self {
            message: message.into(),
            span,
            severity,
            rule: Some(rule),
            fix: Some(LintFix {
                description: fix_description.into(),
                span: span.unwrap_or(Span::new(0, 0)),
                replacement: replacement.into(),
            }),
        }
    }
}

pub struct LintResult {
    pub errors: Vec<LintError>,
    pub warnings: Vec<LintError>,
    pub infos: Vec<LintError>,
}

impl LintResult {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
            infos: Vec::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.errors.is_empty() && self.warnings.is_empty() && self.infos.is_empty()
    }

    fn push(&mut self, error: LintError) {
        match error.severity {
            LintSeverity::Error => self.errors.push(error),
            LintSeverity::Warn => self.warnings.push(error),
            LintSeverity::Info => self.infos.push(error),
        }
    }

    fn into_vec(self) -> Vec<LintError> {
        let mut all = Vec::new();
        all.extend(self.errors);
        all.extend(self.warnings);
        all.extend(self.infos);
        all
    }
}

pub fn lint_module(module: &Module) -> Result<(), Vec<LintError>> {
    lint_module_with_config(module, None)
}

pub fn lint_module_with_config(
    module: &Module,
    config_source: Option<&str>,
) -> Result<(), Vec<LintError>> {
    let suppressed = collect_lint_suppressions(module);
    let config = config_source
        .map(LintConfig::load)
        .unwrap_or_else(LintConfig::default);
    let config = config.with_suppressions(&suppressed);
    let mut errors = Vec::new();
    let mut aliases: Vec<Ident> = Vec::new();
    let mut import_paths: Vec<(String, Ident)> = Vec::new();
    for stmt in &module.stmts {
        if let Stmt::Import { alias, .. } = stmt {
            aliases.push(alias.clone());
        }
        if let Stmt::Import { path, alias, .. } = stmt {
            import_paths.push((path.clone(), alias.clone()));
        }
    }

    let mut used = HashSet::new();
    collect_alias_usage(module, &mut used);

    let mut seen_aliases: HashMap<String, Ident> = HashMap::new();
    for alias in &aliases {
        if let Some(_existing) = seen_aliases.get(&alias.name) {
            push_rule(
                &config,
                &mut errors,
                LintRule::DuplicateImportAlias,
                format!("duplicate import alias: {}", alias.name),
                Some(alias.span),
            );
        } else {
            seen_aliases.insert(alias.name.clone(), alias.clone());
        }
    }

    for alias in aliases {
        if should_ignore_name(&alias.name) {
            continue;
        }
        if !used.contains(&alias.name) {
            push_rule(
                &config,
                &mut errors,
                LintRule::UnusedImportAlias,
                format!("unused import alias: {}", alias.name),
                Some(alias.span),
            );
        }
    }

    let mut seen_paths: HashMap<String, Ident> = HashMap::new();
    let mut import_order_check: Vec<(String, Span)> = Vec::new();
    for (path, alias) in import_paths {
        if let Some(existing) = seen_paths.get(&path) {
            if existing.name != alias.name {
                push_rule(
                    &config,
                    &mut errors,
                    LintRule::DuplicateImportPath,
                    format!(
                        "duplicate import path with different alias: {path} (aliases: {}, {})",
                        existing.name, alias.name
                    ),
                    Some(alias.span),
                );
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
                push_rule(
                    &config,
                    &mut errors,
                    LintRule::ImportOrder,
                    format!(
                        "import not in alphabetical order: '{}' should come before '{}'",
                        curr.0, prev.0
                    ),
                    Some(curr.1),
                );
            }
        }
    }

    let mut defined_functions: Vec<Ident> = Vec::new();
    let mut function_names: HashMap<String, Ident> = HashMap::new();
    for func in &module.functions {
        if let Some(_existing) = function_names.get(&func.name.name) {
            push_rule(
                &config,
                &mut errors,
                LintRule::DuplicateFunction,
                format!("duplicate function: {}", func.name.name),
                Some(func.name.span),
            );
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
            push_rule(
                &config,
                &mut errors,
                LintRule::UnusedFunction,
                format!("unused function: {}", func_name.name),
                Some(func_name.span),
            );
        }
    }

    lint_unused_locals(module, &config, &mut errors);
    lint_unused_match_bindings(module, &config, &mut errors);
    lint_unreachable_code(module, &config, &mut errors);
    lint_unnecessary_needs(module, &config, &mut errors);

    let mut result = LintResult::new();
    for error in errors {
        result.push(error);
    }

    if result.is_empty() {
        Ok(())
    } else {
        Err(result.into_vec())
    }
}

fn lint_unreachable_code(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        check_unreachable_stmts(&func.body, config, errors);
    }
    check_unreachable_stmts(&module.stmts, config, errors);
}

fn check_unreachable_stmts(stmts: &[Stmt], config: &LintConfig, errors: &mut Vec<LintError>) {
    let mut unreachable_start: Option<Span> = None;

    for stmt in stmts {
        match stmt {
            Stmt::Break { break_span, .. }
            | Stmt::Continue {
                continue_span: break_span,
                ..
            } => {
                if unreachable_start.is_none() {
                    unreachable_start = Some(*break_span);
                }
            }
            Stmt::Return {
                expr: Some(_expr), ..
            } => {
                if unreachable_start.is_none() {
                    // Get span from the statement itself if possible
                    unreachable_start = Some(Span::new(0, 0));
                }
            }
            Stmt::Return { expr: None, .. } => {
                if unreachable_start.is_none() {
                    // Use a default span since Return without value has no span
                    unreachable_start = Some(Span::new(0, 0));
                }
            }
            Stmt::Throw { .. } => {
                if unreachable_start.is_none() {
                    unreachable_start = Some(Span::new(0, 0));
                }
            }
            Stmt::Defer { .. } => {}
            Stmt::With { body, .. } => {
                if let Some(span) = unreachable_start {
                    push_rule(
                        config,
                        errors,
                        LintRule::UnreachableCode,
                        "unreachable code after return/break/continue".to_string(),
                        Some(span),
                    );
                    unreachable_start = None;
                }
                check_unreachable_stmts(body, config, errors);
            }
            _ => {
                if let Some(span) = unreachable_start {
                    push_rule(
                        config,
                        errors,
                        LintRule::UnreachableCode,
                        "unreachable code after return/break/continue".to_string(),
                        Some(span),
                    );
                    unreachable_start = None;
                }
            }
        }

        // Recursively check nested blocks
        match stmt {
            Stmt::While { body, .. } => check_unreachable_stmts(body, config, errors),
            Stmt::For { body, .. } => check_unreachable_stmts(body, config, errors),
            Stmt::Block {
                stmts: nested_stmts,
                ..
            } => check_unreachable_stmts(nested_stmts, config, errors),
            Stmt::Test { body, .. } => check_unreachable_stmts(body, config, errors),
            _ => {}
        }
    }
}

fn lint_unused_match_bindings(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        for stmt in &func.body {
            lint_unused_match_bindings_stmt(stmt, config, errors);
        }
    }
    for stmt in &module.stmts {
        lint_unused_match_bindings_stmt(stmt, config, errors);
    }
}

fn lint_unnecessary_needs(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
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
                push_rule(
                    config,
                    errors,
                    LintRule::UnnecessaryNeeds,
                    format!("unnecessary needs: '{}' declared but not used", need.name),
                    Some(need.span),
                );
            }
        }
    }
}

fn collect_used_capabilities_stmt(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Const { value, .. }
        | Stmt::Let { value, .. }
        | Stmt::Using { value, .. }
        | Stmt::Expr { expr: value, .. } => {
            collect_used_capabilities_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_used_capabilities_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_used_capabilities_expr(base, used);
            collect_used_capabilities_expr(value, used);
        }
        Stmt::SetIndex {
            base, index, value, ..
        } => {
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
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                collect_used_capabilities_expr(expr, used);
            }
        }
        Stmt::Throw { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Stmt::Defer { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Stmt::With { value, body, .. } => {
            collect_used_capabilities_expr(value, used);
            for stmt in body {
                collect_used_capabilities_stmt(stmt, used);
            }
        }
        Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
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
        Expr::Call { callee, args, .. } => {
            collect_used_capabilities_expr(callee, used);
            for arg in args {
                collect_used_capabilities_expr(arg, used);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_used_capabilities_expr(left, used);
            collect_used_capabilities_expr(right, used);
        }
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_used_capabilities_expr(condition, used);
            collect_used_capabilities_expr(then_branch, used);
            collect_used_capabilities_expr(else_branch, used);
        }
        Expr::ChainedComparison { items, .. } => {
            for item in items {
                collect_used_capabilities_expr(item, used);
            }
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
        Expr::ArraySpread { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::Index { base, index, .. } => {
            collect_used_capabilities_expr(base, used);
            collect_used_capabilities_expr(index, used);
        }
        Expr::Try(expr, _) => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::Await { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                collect_used_capabilities_expr(item, used);
            }
        }
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            collect_used_capabilities_expr(try_block, used);
            if let Some(catch_block) = catch_block {
                collect_used_capabilities_expr(catch_block, used);
            }
            if let Some(finally_block) = finally_block {
                collect_used_capabilities_expr(finally_block, used);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_used_capabilities_expr(key, used);
                collect_used_capabilities_expr(value, used);
            }
        }
        Expr::MapSpread { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::As { expr, .. } | Expr::Is { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::Range { start, end, .. } => {
            collect_used_capabilities_expr(start, used);
            collect_used_capabilities_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
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

fn lint_unused_match_bindings_stmt(stmt: &Stmt, config: &LintConfig, errors: &mut Vec<LintError>) {
    match stmt {
        Stmt::Import { .. } | Stmt::Struct { .. } | Stmt::TypeAlias { .. } | Stmt::Enum { .. } => {}
        Stmt::Const { value, .. }
        | Stmt::Let { value, .. }
        | Stmt::Using { value, .. }
        | Stmt::Expr { expr: value, .. } => {
            lint_unused_match_bindings_expr(value, config, errors);
        }
        Stmt::Set { value, .. } => {
            lint_unused_match_bindings_expr(value, config, errors);
        }
        Stmt::SetMember { base, value, .. } => {
            lint_unused_match_bindings_expr(base, config, errors);
            lint_unused_match_bindings_expr(value, config, errors);
        }
        Stmt::SetIndex {
            base, index, value, ..
        } => {
            lint_unused_match_bindings_expr(base, config, errors);
            lint_unused_match_bindings_expr(index, config, errors);
            lint_unused_match_bindings_expr(value, config, errors);
        }
        Stmt::While {
            condition, body, ..
        } => {
            lint_unused_match_bindings_expr(condition, config, errors);
            for stmt in body {
                lint_unused_match_bindings_stmt(stmt, config, errors);
            }
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_unused_match_bindings_expr(condition, config, errors);
            for stmt in then_branch {
                lint_unused_match_bindings_stmt(stmt, config, errors);
            }
            if let Some(else_branch) = else_branch {
                for stmt in else_branch {
                    lint_unused_match_bindings_stmt(stmt, config, errors);
                }
            }
        }
        Stmt::For { iter, body, .. } => {
            lint_unused_match_bindings_expr(iter, config, errors);
            for stmt in body {
                lint_unused_match_bindings_stmt(stmt, config, errors);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                lint_unused_match_bindings_expr(expr, config, errors);
            }
        }
        Stmt::Throw { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Stmt::Defer { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Stmt::With { value, body, .. } => {
            lint_unused_match_bindings_expr(value, config, errors);
            for stmt in body {
                lint_unused_match_bindings_stmt(stmt, config, errors);
            }
        }
        Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                lint_unused_match_bindings_stmt(stmt, config, errors);
            }
        }
    }
}

fn lint_unused_match_bindings_expr(expr: &Expr, config: &LintConfig, errors: &mut Vec<LintError>) {
    match expr {
        Expr::Match { value, arms, .. } => {
            lint_unused_match_bindings_expr(value, config, errors);
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
                        push_rule(
                            config,
                            errors,
                            LintRule::UnusedMatchBinding,
                            format!("unused match binding: {}", ident.name),
                            Some(ident.span),
                        );
                    }
                }
                if let Some(guard) = &arm.guard {
                    lint_unused_match_bindings_expr(guard, config, errors);
                }
                lint_unused_match_bindings_expr(&arm.body, config, errors);
            }
        }
        Expr::Binary { left, right, .. } => {
            lint_unused_match_bindings_expr(left, config, errors);
            lint_unused_match_bindings_expr(right, config, errors);
        }
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_unused_match_bindings_expr(condition, config, errors);
            lint_unused_match_bindings_expr(then_branch, config, errors);
            lint_unused_match_bindings_expr(else_branch, config, errors);
        }
        Expr::ChainedComparison { items, .. } => {
            for item in items {
                lint_unused_match_bindings_expr(item, config, errors);
            }
        }
        Expr::Unary { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_unused_match_bindings_expr(condition, config, errors);
            lint_unused_match_bindings_expr(then_branch, config, errors);
            if let Some(else_expr) = else_branch {
                lint_unused_match_bindings_expr(else_expr, config, errors);
            }
        }
        Expr::Call { callee, args, .. } => {
            lint_unused_match_bindings_expr(callee, config, errors);
            for arg in args {
                lint_unused_match_bindings_expr(arg, config, errors);
            }
        }
        Expr::Member { base, .. } => {
            lint_unused_match_bindings_expr(base, config, errors);
        }
        Expr::Try(expr, _) => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::Await { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::Block { stmts, tail, .. } => {
            for stmt in stmts {
                lint_unused_match_bindings_stmt(stmt, config, errors);
            }
            if let Some(expr) = tail {
                lint_unused_match_bindings_expr(expr, config, errors);
            }
        }
        Expr::Array { items, .. } => {
            for item in items {
                lint_unused_match_bindings_expr(item, config, errors);
            }
        }
        Expr::ArraySpread { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::Index { base, index, .. } => {
            lint_unused_match_bindings_expr(base, config, errors);
            lint_unused_match_bindings_expr(index, config, errors);
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                lint_unused_match_bindings_expr(item, config, errors);
            }
        }
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            lint_unused_match_bindings_expr(try_block, config, errors);
            if let Some(catch_block) = catch_block {
                lint_unused_match_bindings_expr(catch_block, config, errors);
            }
            if let Some(finally_block) = finally_block {
                lint_unused_match_bindings_expr(finally_block, config, errors);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                lint_unused_match_bindings_expr(key, config, errors);
                lint_unused_match_bindings_expr(value, config, errors);
            }
        }
        Expr::MapSpread { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::As { expr, .. } | Expr::Is { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::Range { start, end, .. } => {
            lint_unused_match_bindings_expr(start, config, errors);
            lint_unused_match_bindings_expr(end, config, errors);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
                    lint_unused_match_bindings_expr(expr, config, errors);
                }
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                lint_unused_match_bindings_expr(&field.value, config, errors);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                lint_unused_match_bindings_expr(expr, config, errors);
            }
        }
        Expr::Group { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::Closure { body, .. } => {
            lint_unused_match_bindings_expr(body, config, errors);
        }
        _ => {}
    }
}

fn match_pattern_idents(pattern: &at_syntax::MatchPattern) -> Vec<Ident> {
    match pattern {
        at_syntax::MatchPattern::Int(_, _)
        | at_syntax::MatchPattern::Bool(_, _)
        | at_syntax::MatchPattern::String(_, _) => Vec::new(),
        at_syntax::MatchPattern::ResultOk(ident, _)
        | at_syntax::MatchPattern::ResultErr(ident, _)
        | at_syntax::MatchPattern::OptionSome(ident, _) => vec![ident.clone()],
        at_syntax::MatchPattern::OptionNone(_) => Vec::new(),
        at_syntax::MatchPattern::Tuple { items, .. } => {
            items.iter().flat_map(match_pattern_idents).collect()
        }
        at_syntax::MatchPattern::Struct { fields, .. } => fields
            .iter()
            .filter_map(|field| field.binding.clone().or_else(|| Some(field.name.clone())))
            .collect(),
        at_syntax::MatchPattern::Enum { binding, .. } => {
            binding.clone().map_or_else(Vec::new, |ident| vec![ident])
        }
        at_syntax::MatchPattern::Binding { name, pattern, .. } => {
            let mut items = vec![name.clone()];
            items.extend(match_pattern_idents(pattern));
            items
        }
        at_syntax::MatchPattern::Wildcard(_) => Vec::new(),
    }
}

fn lint_unused_locals(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        let mut locals = Vec::new();
        let mut used = HashSet::new();
        let mut seen = HashSet::new();
        for param in &func.params {
            if should_ignore_name(&param.name.name) {
                continue;
            }
            if !seen.insert(param.name.name.clone()) {
                push_rule(
                    config,
                    errors,
                    LintRule::UnusedLocal,
                    format!("duplicate local: {}", param.name.name),
                    Some(param.name.span),
                );
            }
            locals.push(param.name.clone());
        }
        for stmt in &func.body {
            collect_local_defs_stmt(stmt, &mut locals, &mut seen, config, errors);
            collect_local_uses_stmt(stmt, &mut used);
        }
        for local in locals {
            if should_ignore_name(&local.name) {
                continue;
            }
            if !used.contains(&local.name) {
                push_rule(
                    config,
                    errors,
                    LintRule::UnusedLocal,
                    format!("unused local: {}", local.name),
                    Some(local.span),
                );
            }
        }
    }

    let mut locals = Vec::new();
    let mut used = HashSet::new();
    let mut seen = HashSet::new();
    for stmt in &module.stmts {
        collect_local_defs_stmt(stmt, &mut locals, &mut seen, config, errors);
        collect_local_uses_stmt(stmt, &mut used);
    }
    for local in locals {
        if should_ignore_name(&local.name) {
            continue;
        }
        if !used.contains(&local.name) {
            push_rule(
                config,
                errors,
                LintRule::UnusedLocal,
                format!("unused local: {}", local.name),
                Some(local.span),
            );
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
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    match stmt {
        Stmt::Const { name, .. } | Stmt::Let { name, .. } | Stmt::Using { name, .. } => {
            if should_ignore_name(&name.name) {
                locals.push(name.clone());
                return;
            }
            if !seen.insert(name.name.clone()) {
                push_rule(
                    config,
                    errors,
                    LintRule::UnusedLocal,
                    format!("duplicate local: {}", name.name),
                    Some(name.span),
                );
            }
            locals.push(name.clone());
        }
        Stmt::Set { .. } | Stmt::SetMember { .. } | Stmt::SetIndex { .. } => {}
        Stmt::While { body, .. } => {
            let mut inner_seen = HashSet::new();
            for stmt in body {
                collect_local_defs_stmt(stmt, locals, &mut inner_seen, config, errors);
            }
        }
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            let mut inner_seen = HashSet::new();
            for stmt in then_branch {
                collect_local_defs_stmt(stmt, locals, &mut inner_seen, config, errors);
            }
            if let Some(else_branch) = else_branch {
                let mut else_seen = HashSet::new();
                for stmt in else_branch {
                    collect_local_defs_stmt(stmt, locals, &mut else_seen, config, errors);
                }
            }
        }
        Stmt::For { item, body, .. } => {
            let mut inner_seen = HashSet::new();
            if !should_ignore_name(&item.name) {
                inner_seen.insert(item.name.clone());
                locals.push(item.clone());
            }
            for stmt in body {
                collect_local_defs_stmt(stmt, locals, &mut inner_seen, config, errors);
            }
        }
        Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
            let mut inner_seen = HashSet::new();
            for stmt in stmts {
                collect_local_defs_stmt(stmt, locals, &mut inner_seen, config, errors);
            }
        }
        _ => {}
    }
}

fn collect_local_uses_stmt(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Import { .. } | Stmt::Struct { .. } | Stmt::TypeAlias { .. } | Stmt::Enum { .. } => {}
        Stmt::Const { value, .. }
        | Stmt::Let { value, .. }
        | Stmt::Using { value, .. }
        | Stmt::Expr { expr: value, .. } => {
            collect_local_uses_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_local_uses_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_local_uses_expr(base, used);
            collect_local_uses_expr(value, used);
        }
        Stmt::SetIndex {
            base, index, value, ..
        } => {
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
        Stmt::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_local_uses_expr(condition, used);
            for stmt in then_branch {
                collect_local_uses_stmt(stmt, used);
            }
            if let Some(else_branch) = else_branch {
                for stmt in else_branch {
                    collect_local_uses_stmt(stmt, used);
                }
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_local_uses_expr(iter, used);
            for stmt in body {
                collect_local_uses_stmt(stmt, used);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                collect_local_uses_expr(expr, used);
            }
        }
        Stmt::Throw { expr, .. } => {
            collect_local_uses_expr(expr, used);
        }
        Stmt::Defer { expr, .. } => {
            collect_local_uses_expr(expr, used);
        }
        Stmt::With { value, body, .. } => {
            collect_local_uses_expr(value, used);
            for stmt in body {
                collect_local_uses_stmt(stmt, used);
            }
        }
        Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                collect_local_uses_stmt(stmt, used);
            }
        }
    }
}

fn push_rule(
    config: &LintConfig,
    errors: &mut Vec<LintError>,
    rule: LintRule,
    message: String,
    span: Option<Span>,
) {
    if !config.enabled.contains(&rule) {
        return;
    }
    let severity = *config.severity.get(&rule).unwrap_or(&LintSeverity::Error);
    errors.push(LintError::with_rule(rule, message, span, severity));
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
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_local_uses_expr(condition, used);
            collect_local_uses_expr(then_branch, used);
            collect_local_uses_expr(else_branch, used);
        }
        Expr::ChainedComparison { items, .. } => {
            for item in items {
                collect_local_uses_expr(item, used);
            }
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
        Expr::Call { callee, args, .. } => {
            collect_local_uses_expr(callee, used);
            for arg in args {
                collect_local_uses_expr(arg, used);
            }
        }
        Expr::Try(expr, _) => {
            collect_local_uses_expr(expr, used);
        }
        Expr::Await { expr, .. } => {
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
        Expr::ArraySpread { expr, .. } => {
            collect_local_uses_expr(expr, used);
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
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            collect_local_uses_expr(try_block, used);
            if let Some(catch_block) = catch_block {
                collect_local_uses_expr(catch_block, used);
            }
            if let Some(finally_block) = finally_block {
                collect_local_uses_expr(finally_block, used);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_local_uses_expr(key, used);
                collect_local_uses_expr(value, used);
            }
        }
        Expr::MapSpread { expr, .. } => {
            collect_local_uses_expr(expr, used);
        }
        Expr::As { expr, .. } | Expr::Is { expr, .. } => {
            collect_local_uses_expr(expr, used);
        }
        Expr::Range { start, end, .. } => {
            collect_local_uses_expr(start, used);
            collect_local_uses_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
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
        Stmt::Const { value, .. }
        | Stmt::Let { value, .. }
        | Stmt::Using { value, .. }
        | Stmt::Expr { expr: value, .. } => {
            collect_alias_usage_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_alias_usage_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_alias_usage_expr(base, used);
            collect_alias_usage_expr(value, used);
        }
        Stmt::SetIndex {
            base, index, value, ..
        } => {
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
        Stmt::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_alias_usage_expr(condition, used);
            for stmt in then_branch {
                collect_alias_usage_stmt(stmt, used);
            }
            if let Some(else_branch) = else_branch {
                for stmt in else_branch {
                    collect_alias_usage_stmt(stmt, used);
                }
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_alias_usage_expr(iter, used);
            for stmt in body {
                collect_alias_usage_stmt(stmt, used);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                collect_alias_usage_expr(expr, used);
            }
        }
        Stmt::Throw { expr, .. } => {
            collect_alias_usage_expr(expr, used);
        }
        Stmt::Defer { expr, .. } => {
            collect_alias_usage_expr(expr, used);
        }
        Stmt::With { value, body, .. } => {
            collect_alias_usage_expr(value, used);
            for stmt in body {
                collect_alias_usage_stmt(stmt, used);
            }
        }
        Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
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
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_alias_usage_expr(condition, used);
            collect_alias_usage_expr(then_branch, used);
            collect_alias_usage_expr(else_branch, used);
        }
        Expr::ChainedComparison { items, .. } => {
            for item in items {
                collect_alias_usage_expr(item, used);
            }
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
        Expr::Call { callee, args, .. } => {
            collect_alias_usage_expr(callee, used);
            for arg in args {
                collect_alias_usage_expr(arg, used);
            }
        }
        Expr::Try(expr, _) => {
            collect_alias_usage_expr(expr, used);
        }
        Expr::Await { expr, .. } => {
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
        Expr::ArraySpread { expr, .. } => {
            collect_alias_usage_expr(expr, used);
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
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            collect_alias_usage_expr(try_block, used);
            if let Some(catch_block) = catch_block {
                collect_alias_usage_expr(catch_block, used);
            }
            if let Some(finally_block) = finally_block {
                collect_alias_usage_expr(finally_block, used);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_alias_usage_expr(key, used);
                collect_alias_usage_expr(value, used);
            }
        }
        Expr::MapSpread { expr, .. } => {
            collect_alias_usage_expr(expr, used);
        }
        Expr::As { expr, .. } | Expr::Is { expr, .. } => {
            collect_alias_usage_expr(expr, used);
        }
        Expr::Range { start, end, .. } => {
            collect_alias_usage_expr(start, used);
            collect_alias_usage_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
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
        Stmt::Const { value, .. }
        | Stmt::Let { value, .. }
        | Stmt::Using { value, .. }
        | Stmt::Expr { expr: value, .. } => {
            collect_called_functions_expr(value, used);
        }
        Stmt::Set { value, .. } => {
            collect_called_functions_expr(value, used);
        }
        Stmt::SetMember { base, value, .. } => {
            collect_called_functions_expr(base, used);
            collect_called_functions_expr(value, used);
        }
        Stmt::SetIndex {
            base, index, value, ..
        } => {
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
        Stmt::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_called_functions_expr(condition, used);
            for stmt in then_branch {
                collect_called_functions_stmt(stmt, used);
            }
            if let Some(else_branch) = else_branch {
                for stmt in else_branch {
                    collect_called_functions_stmt(stmt, used);
                }
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_called_functions_expr(iter, used);
            for stmt in body {
                collect_called_functions_stmt(stmt, used);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                collect_called_functions_expr(expr, used);
            }
        }
        Stmt::Throw { expr, .. } => {
            collect_called_functions_expr(expr, used);
        }
        Stmt::Defer { expr, .. } => {
            collect_called_functions_expr(expr, used);
        }
        Stmt::With { value, body, .. } => {
            collect_called_functions_expr(value, used);
            for stmt in body {
                collect_called_functions_stmt(stmt, used);
            }
        }
        Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                collect_called_functions_stmt(stmt, used);
            }
        }
    }
}

fn collect_called_functions_expr(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Call { callee, args, .. } => {
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
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_called_functions_expr(condition, used);
            collect_called_functions_expr(then_branch, used);
            collect_called_functions_expr(else_branch, used);
        }
        Expr::ChainedComparison { items, .. } => {
            for item in items {
                collect_called_functions_expr(item, used);
            }
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
        Expr::Try(expr, _) => {
            collect_called_functions_expr(expr, used);
        }
        Expr::Await { expr, .. } => {
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
        Expr::ArraySpread { expr, .. } => {
            collect_called_functions_expr(expr, used);
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
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            collect_called_functions_expr(try_block, used);
            if let Some(catch_block) = catch_block {
                collect_called_functions_expr(catch_block, used);
            }
            if let Some(finally_block) = finally_block {
                collect_called_functions_expr(finally_block, used);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_called_functions_expr(key, used);
                collect_called_functions_expr(value, used);
            }
        }
        Expr::MapSpread { expr, .. } => {
            collect_called_functions_expr(expr, used);
        }
        Expr::As { expr, .. } | Expr::Is { expr, .. } => {
            collect_called_functions_expr(expr, used);
        }
        Expr::Range { start, end, .. } => {
            collect_called_functions_expr(start, used);
            collect_called_functions_expr(end, used);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
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

fn collect_lint_suppressions(module: &Module) -> HashSet<LintRule> {
    let mut rules = HashSet::new();
    for comment in &module.comments {
        if let Some(rest) = comment.text.trim().strip_prefix("@lint-ignore") {
            let list = rest.trim();
            if list.is_empty() {
                continue;
            }
            for rule in list.split(',') {
                let name = rule.trim();
                if name.is_empty() {
                    continue;
                }
                if let Some(rule) = LintRule::from_str(name) {
                    rules.insert(rule);
                }
            }
        }
    }
    rules
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
