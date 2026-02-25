use std::collections::{HashMap, HashSet};

use at_syntax::{Expr, Ident, InterpPart, MatchPattern, Module, Span, Stmt};

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
    MissingImportPath,
    DuplicateFunction,
    DuplicateLocal,
    UnusedFunction,
    UnusedLocal,
    UnusedMatchBinding,
    UnreachableCode,
    UnnecessaryNeeds,
    ShadowedBinding,
    MissingReturn,
    UnknownTypeFlow,
    DeadBranch,
    NamingConvention,
    UnusedSetTarget,
    InfiniteLoop,
    BooleanLiteralComparison,
    EmptyBody,
    FunctionLength,
    NestingDepth,
    CyclomaticComplexity,
    LegacyExceptionSurface,
    UnqualifiedImportCall,
}

impl LintRule {
    fn from_str(value: &str) -> Option<Self> {
        match value {
            "duplicate_import_alias" => Some(LintRule::DuplicateImportAlias),
            "unused_import_alias" => Some(LintRule::UnusedImportAlias),
            "duplicate_import_path" => Some(LintRule::DuplicateImportPath),
            "import_order" => Some(LintRule::ImportOrder),
            "missing_import_path" => Some(LintRule::MissingImportPath),
            "duplicate_function" => Some(LintRule::DuplicateFunction),
            "duplicate_local" => Some(LintRule::DuplicateLocal),
            "unused_function" => Some(LintRule::UnusedFunction),
            "unused_local" => Some(LintRule::UnusedLocal),
            "unused_match_binding" => Some(LintRule::UnusedMatchBinding),
            "unreachable_code" => Some(LintRule::UnreachableCode),
            "unnecessary_needs" => Some(LintRule::UnnecessaryNeeds),
            "shadowed_binding" => Some(LintRule::ShadowedBinding),
            "missing_return" => Some(LintRule::MissingReturn),
            "unknown_type_flow" => Some(LintRule::UnknownTypeFlow),
            "dead_branch" => Some(LintRule::DeadBranch),
            "naming_convention" => Some(LintRule::NamingConvention),
            "unused_set_target" => Some(LintRule::UnusedSetTarget),
            "infinite_loop" => Some(LintRule::InfiniteLoop),
            "boolean_literal_comparison" => Some(LintRule::BooleanLiteralComparison),
            "empty_body" => Some(LintRule::EmptyBody),
            "function_length" => Some(LintRule::FunctionLength),
            "nesting_depth" => Some(LintRule::NestingDepth),
            "cyclomatic_complexity" => Some(LintRule::CyclomaticComplexity),
            "legacy_exception_surface" => Some(LintRule::LegacyExceptionSurface),
            "unqualified_import_call" => Some(LintRule::UnqualifiedImportCall),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct LintConfig {
    enabled: HashSet<LintRule>,
    severity: HashMap<LintRule, LintSeverity>,
    suppressions: LintSuppressions,
}

impl Default for LintConfig {
    fn default() -> Self {
        let rules = [
            LintRule::DuplicateImportAlias,
            LintRule::UnusedImportAlias,
            LintRule::DuplicateImportPath,
            LintRule::ImportOrder,
            LintRule::MissingImportPath,
            LintRule::DuplicateFunction,
            LintRule::DuplicateLocal,
            LintRule::UnusedFunction,
            LintRule::UnusedLocal,
            LintRule::UnusedMatchBinding,
            LintRule::UnreachableCode,
            LintRule::UnnecessaryNeeds,
            LintRule::ShadowedBinding,
            LintRule::MissingReturn,
            LintRule::UnknownTypeFlow,
            LintRule::DeadBranch,
            LintRule::NamingConvention,
            LintRule::UnusedSetTarget,
            LintRule::InfiniteLoop,
            LintRule::BooleanLiteralComparison,
            LintRule::EmptyBody,
            LintRule::FunctionLength,
            LintRule::NestingDepth,
            LintRule::CyclomaticComplexity,
            LintRule::LegacyExceptionSurface,
            LintRule::UnqualifiedImportCall,
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
                | LintRule::UnnecessaryNeeds
                | LintRule::ShadowedBinding
                | LintRule::MissingReturn
                | LintRule::UnknownTypeFlow
                | LintRule::DeadBranch
                | LintRule::NamingConvention
                | LintRule::UnusedSetTarget
                | LintRule::InfiniteLoop
                | LintRule::BooleanLiteralComparison
                | LintRule::EmptyBody
                | LintRule::LegacyExceptionSurface
                | LintRule::UnqualifiedImportCall => LintSeverity::Warn,
                LintRule::ImportOrder => LintSeverity::Info,
                LintRule::FunctionLength
                | LintRule::NestingDepth
                | LintRule::CyclomaticComplexity => LintSeverity::Info,
                _ => LintSeverity::Error,
            };
            severity.insert(rule, level);
        }
        Self {
            enabled,
            severity,
            suppressions: LintSuppressions::default(),
        }
    }
}

impl LintConfig {
    fn strict() -> Self {
        let mut config = Self::default();
        for rule in [
            LintRule::UnusedImportAlias,
            LintRule::UnusedFunction,
            LintRule::UnusedLocal,
            LintRule::UnusedMatchBinding,
            LintRule::UnnecessaryNeeds,
            LintRule::ShadowedBinding,
            LintRule::MissingReturn,
            LintRule::UnknownTypeFlow,
            LintRule::DeadBranch,
            LintRule::NamingConvention,
            LintRule::UnusedSetTarget,
            LintRule::InfiniteLoop,
            LintRule::BooleanLiteralComparison,
            LintRule::EmptyBody,
            LintRule::FunctionLength,
            LintRule::NestingDepth,
            LintRule::CyclomaticComplexity,
            LintRule::LegacyExceptionSurface,
            LintRule::UnqualifiedImportCall,
        ] {
            config.severity.insert(rule, LintSeverity::Error);
        }
        config
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
        if let Some(profile) = table.get("profile").and_then(|value| value.as_str()) {
            if profile == "strict" {
                config = Self::strict();
            }
        }
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

    fn with_suppressions(mut self, suppressions: LintSuppressions) -> Self {
        self.suppressions = suppressions;
        self
    }

    fn is_suppressed(&self, rule: LintRule, span: Option<Span>) -> bool {
        self.suppressions.matches(rule, span)
    }
}

#[derive(Debug, Clone, Default)]
struct LintSuppressions {
    global_all: bool,
    global: HashSet<LintRule>,
    spans: Vec<LintSuppression>,
    lines: Vec<LintLineSuppression>,
    line_starts: Vec<usize>,
}

#[derive(Debug, Clone)]
struct LintSuppression {
    span: Span,
    rules: Option<HashSet<LintRule>>,
}

#[derive(Debug, Clone)]
struct LintLineSuppression {
    line: usize,
    rules: Option<HashSet<LintRule>>,
}

impl LintSuppressions {
    fn matches(&self, rule: LintRule, span: Option<Span>) -> bool {
        if self.global_all || self.global.contains(&rule) {
            return true;
        }
        let Some(span) = span else {
            return false;
        };
        for suppression in &self.spans {
            if spans_overlap(span, suppression.span)
                && suppression
                    .rules
                    .as_ref()
                    .is_none_or(|rules| rules.contains(&rule))
            {
                return true;
            }
        }
        if let Some(line) = self.line_for_offset(span.start) {
            for suppression in &self.lines {
                if suppression.line == line
                    && suppression
                        .rules
                        .as_ref()
                        .is_none_or(|rules| rules.contains(&rule))
                {
                    return true;
                }
            }
        }
        false
    }

    fn line_for_offset(&self, offset: usize) -> Option<usize> {
        if self.line_starts.is_empty() {
            return None;
        }
        match self.line_starts.binary_search(&offset) {
            Ok(index) => Some(index),
            Err(0) => Some(0),
            Err(index) => Some(index.saturating_sub(1)),
        }
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
        let Some(span) = span else {
            return Self {
                message: message.into(),
                span: None,
                severity: LintSeverity::Error,
                rule: None,
                fix: None,
            };
        };
        Self {
            message: message.into(),
            span: Some(span),
            severity: LintSeverity::Error,
            rule: None,
            fix: Some(LintFix {
                description: fix_description.into(),
                span,
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
        let Some(span) = span else {
            return Self {
                message: message.into(),
                span: None,
                severity,
                rule: Some(rule),
                fix: None,
            };
        };
        Self {
            message: message.into(),
            span: Some(span),
            severity,
            rule: Some(rule),
            fix: Some(LintFix {
                description: fix_description.into(),
                span,
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
    lint_module_with_source_and_config(module, None, None)
}

pub fn lint_module_with_config(
    module: &Module,
    config_source: Option<&str>,
) -> Result<(), Vec<LintError>> {
    lint_module_with_source_and_config(module, None, config_source)
}

pub fn lint_module_with_source_and_config(
    module: &Module,
    source: Option<&str>,
    config_source: Option<&str>,
) -> Result<(), Vec<LintError>> {
    let config = config_source.map(LintConfig::load).unwrap_or_default();
    let suppressions = collect_lint_suppressions(module, source);
    let config = config.with_suppressions(suppressions);
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
        if !path.starts_with("http://")
            && !path.starts_with("https://")
            && path != "std"
            && path != "std.at"
        {
            // Keep lint deterministic: avoid filesystem existence checks.
            if path.trim().is_empty() {
                push_rule(
                    &config,
                    &mut errors,
                    LintRule::MissingImportPath,
                    "import path is empty".to_string(),
                    Some(alias.span),
                );
            }
        }
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
    lint_shadowed_bindings(module, &config, &mut errors);
    lint_missing_returns(module, &config, &mut errors);
    lint_unknown_type_flow(module, &config, &mut errors);
    lint_dead_branches(module, &config, &mut errors);
    lint_naming_conventions(module, &config, &mut errors);
    lint_unused_set_targets(module, &config, &mut errors);
    lint_infinite_loops(module, &config, &mut errors);
    lint_boolean_literal_comparisons(module, &config, &mut errors);
    lint_empty_bodies(module, &config, &mut errors);
    lint_function_metrics(module, &config, &mut errors);
    lint_legacy_exception_surface(module, &config, &mut errors);
    lint_unqualified_import_calls(module, &config, &mut errors);

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

fn lint_unknown_type_flow(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    if !config.enabled.contains(&LintRule::UnknownTypeFlow) {
        return;
    }
    let result = at_check::typecheck_module_with_mode(module, at_check::TypecheckMode::Strict);
    let Err(type_errors) = result else {
        return;
    };
    for error in type_errors {
        if !error.message.contains("unknown in strict mode") {
            continue;
        }
        push_rule(
            config,
            errors,
            LintRule::UnknownTypeFlow,
            error.message,
            error.span,
        );
    }
}

fn lint_shadowed_bindings(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        let mut scopes: Vec<HashSet<String>> = Vec::new();
        scopes.push(HashSet::new());
        for param in &func.params {
            if !should_ignore_name(&param.name.name)
                && scopes.iter().any(|scope| scope.contains(&param.name.name))
            {
                push_rule(
                    config,
                    errors,
                    LintRule::ShadowedBinding,
                    format!("shadowed binding: {}", param.name.name),
                    Some(param.name.span),
                );
            }
            ensure_scope(&mut scopes).insert(param.name.name.clone());
        }
        check_shadowed_stmt(&func.body, &mut scopes, config, errors);
    }
    let mut scopes: Vec<HashSet<String>> = Vec::new();
    scopes.push(HashSet::new());
    check_shadowed_stmt(&module.stmts, &mut scopes, config, errors);
}

fn lint_naming_conventions(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        if !func.is_tool && !is_snake_case(&func.name.name) {
            push_rule(
                config,
                errors,
                LintRule::NamingConvention,
                format!("function name should be snake_case: {}", func.name.name),
                Some(func.name.span),
            );
        }
        for param in &func.params {
            if !should_ignore_name(&param.name.name) && !is_snake_case(&param.name.name) {
                push_rule(
                    config,
                    errors,
                    LintRule::NamingConvention,
                    format!("parameter name should be snake_case: {}", param.name.name),
                    Some(param.name.span),
                );
            }
        }
    }
    for stmt in &module.stmts {
        match stmt {
            Stmt::Struct { name, .. } | Stmt::Enum { name, .. } | Stmt::TypeAlias { name, .. } => {
                if !is_pascal_case(&name.name) {
                    push_rule(
                        config,
                        errors,
                        LintRule::NamingConvention,
                        format!("type name should be PascalCase: {}", name.name),
                        Some(name.span),
                    );
                }
            }
            Stmt::Let { name, .. }
            | Stmt::Const { name, .. }
            | Stmt::Using { name, .. }
            | Stmt::Set { name, .. }
            | Stmt::With { name, .. } => {
                if !should_ignore_name(&name.name) && !is_snake_case(&name.name) {
                    push_rule(
                        config,
                        errors,
                        LintRule::NamingConvention,
                        format!("name should be snake_case: {}", name.name),
                        Some(name.span),
                    );
                }
            }
            _ => {}
        }
    }
}

fn lint_unused_set_targets(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        let mut reads = HashSet::new();
        for stmt in &func.body {
            collect_local_uses_stmt(stmt, &mut reads);
        }
        check_unused_set_stmts(&func.body, &reads, config, errors);
    }
}

fn check_unused_set_stmts(
    stmts: &[Stmt],
    reads: &HashSet<String>,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::Set { name, .. } => {
                if !should_ignore_name(&name.name) && !reads.contains(&name.name) {
                    push_rule(
                        config,
                        errors,
                        LintRule::UnusedSetTarget,
                        format!("value assigned but never read: {}", name.name),
                        Some(name.span),
                    );
                }
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                check_unused_set_stmts(then_branch, reads, config, errors);
                if let Some(else_branch) = else_branch {
                    check_unused_set_stmts(else_branch, reads, config, errors);
                }
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::With { body, .. }
            | Stmt::Block { stmts: body, .. }
            | Stmt::Test { body, .. } => {
                check_unused_set_stmts(body, reads, config, errors);
            }
            _ => {}
        }
    }
}

fn lint_infinite_loops(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        check_infinite_loop_stmts(&func.body, config, errors);
    }
    check_infinite_loop_stmts(&module.stmts, config, errors);
}

fn check_infinite_loop_stmts(stmts: &[Stmt], config: &LintConfig, errors: &mut Vec<LintError>) {
    for stmt in stmts {
        match stmt {
            Stmt::While {
                condition,
                body,
                while_span,
                ..
            } => {
                if eval_bool_literal(condition) == Some(true)
                    && !contains_guaranteed_termination(body, true)
                {
                    push_rule(
                        config,
                        errors,
                        LintRule::InfiniteLoop,
                        "infinite loop (while true without break/return)".to_string(),
                        Some(*while_span),
                    );
                }
                check_infinite_loop_stmts(body, config, errors);
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                check_infinite_loop_stmts(then_branch, config, errors);
                if let Some(else_branch) = else_branch {
                    check_infinite_loop_stmts(else_branch, config, errors);
                }
            }
            Stmt::For { body, .. } | Stmt::With { body, .. } | Stmt::Block { stmts: body, .. } => {
                check_infinite_loop_stmts(body, config, errors);
            }
            _ => {}
        }
    }
}

fn lint_boolean_literal_comparisons(
    module: &Module,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    check_boolean_literal_exprs(&module.stmts, config, errors);
    for func in &module.functions {
        check_boolean_literal_exprs(&func.body, config, errors);
    }
}

fn check_boolean_literal_exprs(stmts: &[Stmt], config: &LintConfig, errors: &mut Vec<LintError>) {
    for stmt in stmts {
        match stmt {
            Stmt::Expr { expr, .. }
            | Stmt::Return {
                expr: Some(expr), ..
            }
            | Stmt::Throw { expr, .. }
            | Stmt::Defer { expr, .. }
            | Stmt::Yield { expr, .. }
            | Stmt::Set { value: expr, .. } => check_boolean_literal_expr(expr, config, errors),
            Stmt::SetMember { base, value, .. } => {
                check_boolean_literal_expr(base, config, errors);
                check_boolean_literal_expr(value, config, errors);
            }
            Stmt::SetIndex {
                base, index, value, ..
            } => {
                check_boolean_literal_expr(base, config, errors);
                check_boolean_literal_expr(index, config, errors);
                check_boolean_literal_expr(value, config, errors);
            }
            Stmt::Let { value, .. }
            | Stmt::Const { value, .. }
            | Stmt::Using { value, .. }
            | Stmt::With { value, .. } => check_boolean_literal_expr(value, config, errors),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                check_boolean_literal_expr(condition, config, errors);
                check_boolean_literal_exprs(then_branch, config, errors);
                if let Some(else_branch) = else_branch {
                    check_boolean_literal_exprs(else_branch, config, errors);
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                check_boolean_literal_expr(condition, config, errors);
                check_boolean_literal_exprs(body, config, errors);
            }
            Stmt::For { iter, body, .. } => {
                check_boolean_literal_expr(iter, config, errors);
                check_boolean_literal_exprs(body, config, errors);
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                check_boolean_literal_exprs(stmts, config, errors)
            }
            _ => {}
        }
    }
}

fn check_boolean_literal_expr(expr: &Expr, config: &LintConfig, errors: &mut Vec<LintError>) {
    match expr {
        Expr::Binary {
            left, op, right, ..
        } => {
            if matches!(op, at_syntax::BinaryOp::Eq | at_syntax::BinaryOp::Neq)
                && (matches!(**left, Expr::Bool(..)) || matches!(**right, Expr::Bool(..)))
            {
                push_rule(
                    config,
                    errors,
                    LintRule::BooleanLiteralComparison,
                    "boolean literal comparison can be simplified".to_string(),
                    expr_span(expr),
                );
            }
            check_boolean_literal_expr(left, config, errors);
            check_boolean_literal_expr(right, config, errors);
        }
        Expr::Unary { expr, .. } | Expr::Await { expr, .. } | Expr::Try(expr, ..) => {
            check_boolean_literal_expr(expr, config, errors)
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            check_boolean_literal_expr(condition, config, errors);
            check_boolean_literal_expr(then_branch, config, errors);
            if let Some(else_branch) = else_branch {
                check_boolean_literal_expr(else_branch, config, errors);
            }
        }
        Expr::Call { callee, args, .. } => {
            check_boolean_literal_expr(callee, config, errors);
            for arg in args {
                check_boolean_literal_expr(arg, config, errors);
            }
        }
        Expr::Member { base, .. } => check_boolean_literal_expr(base, config, errors),
        Expr::Index { base, index, .. } => {
            check_boolean_literal_expr(base, config, errors);
            check_boolean_literal_expr(index, config, errors);
        }
        Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
            for item in items {
                check_boolean_literal_expr(item, config, errors);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                check_boolean_literal_expr(key, config, errors);
                check_boolean_literal_expr(value, config, errors);
            }
        }
        Expr::MapSpread { expr, .. }
        | Expr::ArraySpread { expr, .. }
        | Expr::Group { expr, .. }
        | Expr::As { expr, .. }
        | Expr::Is { expr, .. } => check_boolean_literal_expr(expr, config, errors),
        Expr::Match { value, arms, .. } => {
            check_boolean_literal_expr(value, config, errors);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    check_boolean_literal_expr(guard, config, errors);
                }
                check_boolean_literal_expr(&arm.body, config, errors);
            }
        }
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            check_boolean_literal_expr(try_block, config, errors);
            if let Some(catch) = catch_block {
                check_boolean_literal_expr(catch, config, errors);
            }
            if let Some(finally) = finally_block {
                check_boolean_literal_expr(finally, config, errors);
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                check_boolean_literal_expr(&field.value, config, errors);
            }
        }
        Expr::EnumLiteral {
            payload: Some(payload),
            ..
        } => check_boolean_literal_expr(payload, config, errors),
        Expr::EnumLiteral { payload: None, .. } => {}
        Expr::Range { start, end, .. } => {
            check_boolean_literal_expr(start, config, errors);
            check_boolean_literal_expr(end, config, errors);
        }
        Expr::Block { stmts, tail, .. } => {
            check_boolean_literal_exprs(stmts, config, errors);
            if let Some(tail) = tail {
                check_boolean_literal_expr(tail, config, errors);
            }
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let at_syntax::InterpPart::Expr(expr, _) = part {
                    check_boolean_literal_expr(expr, config, errors);
                }
            }
        }
        Expr::Closure { body, .. } => check_boolean_literal_expr(body, config, errors),
        _ => {}
    }
}

fn lint_empty_bodies(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        if func.body.is_empty() {
            push_rule(
                config,
                errors,
                LintRule::EmptyBody,
                format!("empty function body: {}", func.name.name),
                Some(func.name.span),
            );
        }
        for stmt in &func.body {
            check_empty_body_stmt(stmt, config, errors);
        }
    }
    for stmt in &module.stmts {
        check_empty_body_stmt(stmt, config, errors);
    }
}

fn check_empty_body_stmt(stmt: &Stmt, config: &LintConfig, errors: &mut Vec<LintError>) {
    match stmt {
        Stmt::If {
            then_branch,
            else_branch,
            if_span,
            ..
        } => {
            if then_branch.is_empty() {
                push_rule(
                    config,
                    errors,
                    LintRule::EmptyBody,
                    "empty if body".to_string(),
                    Some(*if_span),
                );
            }
            if let Some(else_branch) = else_branch {
                if else_branch.is_empty() {
                    push_rule(
                        config,
                        errors,
                        LintRule::EmptyBody,
                        "empty else body".to_string(),
                        Some(*if_span),
                    );
                }
            }
        }
        Stmt::While {
            body, while_span, ..
        } => {
            if body.is_empty() {
                push_rule(
                    config,
                    errors,
                    LintRule::EmptyBody,
                    "empty while body".to_string(),
                    Some(*while_span),
                );
            }
        }
        Stmt::For { body, for_span, .. } => {
            if body.is_empty() {
                push_rule(
                    config,
                    errors,
                    LintRule::EmptyBody,
                    "empty for body".to_string(),
                    Some(*for_span),
                );
            }
        }
        Stmt::Block { stmts, .. } => {
            if stmts.is_empty() {
                push_rule(
                    config,
                    errors,
                    LintRule::EmptyBody,
                    "empty block".to_string(),
                    stmt_span(stmt),
                );
            }
        }
        Stmt::Test { body, .. } => {
            if body.is_empty() {
                push_rule(
                    config,
                    errors,
                    LintRule::EmptyBody,
                    "empty test body".to_string(),
                    stmt_span(stmt),
                );
            }
        }
        _ => {}
    }
}

fn lint_function_metrics(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    const MAX_STMTS: usize = 200;
    const MAX_NESTING: usize = 6;
    const MAX_COMPLEXITY: usize = 20;
    for func in &module.functions {
        let stmt_count = count_stmts(&func.body);
        if stmt_count > MAX_STMTS {
            push_rule(
                config,
                errors,
                LintRule::FunctionLength,
                format!(
                    "function too long: {} has {} statements",
                    func.name.name, stmt_count
                ),
                Some(func.name.span),
            );
        }
        let nesting = max_nesting(&func.body, 0);
        if nesting > MAX_NESTING {
            push_rule(
                config,
                errors,
                LintRule::NestingDepth,
                format!("function nesting too deep: {}", func.name.name),
                Some(func.name.span),
            );
        }
        let complexity = cyclomatic_complexity(&func.body);
        if complexity > MAX_COMPLEXITY {
            push_rule(
                config,
                errors,
                LintRule::CyclomaticComplexity,
                format!("function complexity too high: {}", func.name.name),
                Some(func.name.span),
            );
        }
    }
}

fn lint_legacy_exception_surface(
    module: &Module,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    for func in &module.functions {
        lint_legacy_exception_stmts(&func.body, config, errors);
    }
    lint_legacy_exception_stmts(&module.stmts, config, errors);
}

fn lint_legacy_exception_stmts(stmts: &[Stmt], config: &LintConfig, errors: &mut Vec<LintError>) {
    for stmt in stmts {
        match stmt {
            Stmt::Throw { expr, .. } => {
                push_rule(
                    config,
                    errors,
                    LintRule::LegacyExceptionSurface,
                    "legacy exception-style `throw` is deprecated; prefer result/option flows"
                        .to_string(),
                    expr_span(expr).or_else(|| stmt_span(stmt)),
                );
                lint_legacy_exception_expr(expr, config, errors);
            }
            Stmt::Const { value, .. }
            | Stmt::Let { value, .. }
            | Stmt::Using { value, .. }
            | Stmt::Set { value, .. }
            | Stmt::Expr { expr: value, .. }
            | Stmt::Defer { expr: value, .. }
            | Stmt::Yield { expr: value, .. } => lint_legacy_exception_expr(value, config, errors),
            Stmt::SetMember { base, value, .. } => {
                lint_legacy_exception_expr(base, config, errors);
                lint_legacy_exception_expr(value, config, errors);
            }
            Stmt::SetIndex {
                base, index, value, ..
            } => {
                lint_legacy_exception_expr(base, config, errors);
                lint_legacy_exception_expr(index, config, errors);
                lint_legacy_exception_expr(value, config, errors);
            }
            Stmt::While {
                condition, body, ..
            } => {
                lint_legacy_exception_expr(condition, config, errors);
                lint_legacy_exception_stmts(body, config, errors);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                lint_legacy_exception_expr(condition, config, errors);
                lint_legacy_exception_stmts(then_branch, config, errors);
                if let Some(else_branch) = else_branch {
                    lint_legacy_exception_stmts(else_branch, config, errors);
                }
            }
            Stmt::For { iter, body, .. } => {
                lint_legacy_exception_expr(iter, config, errors);
                lint_legacy_exception_stmts(body, config, errors);
            }
            Stmt::Return {
                expr: Some(expr), ..
            } => lint_legacy_exception_expr(expr, config, errors),
            Stmt::Return { expr: None, .. } => {}
            Stmt::With { value, body, .. } => {
                lint_legacy_exception_expr(value, config, errors);
                lint_legacy_exception_stmts(body, config, errors);
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                lint_legacy_exception_stmts(stmts, config, errors);
            }
            Stmt::Break { .. }
            | Stmt::Continue { .. }
            | Stmt::Import { .. }
            | Stmt::Struct { .. }
            | Stmt::TypeAlias { .. }
            | Stmt::Enum { .. } => {}
        }
    }
}

fn lint_legacy_exception_expr(expr: &Expr, config: &LintConfig, errors: &mut Vec<LintError>) {
    match expr {
        Expr::Unary { expr, .. }
        | Expr::Try(expr, _)
        | Expr::Await { expr, .. }
        | Expr::ArraySpread { expr, .. }
        | Expr::MapSpread { expr, .. }
        | Expr::As { expr, .. }
        | Expr::Is { expr, .. }
        | Expr::Group { expr, .. } => lint_legacy_exception_expr(expr, config, errors),
        Expr::Binary { left, right, .. } => {
            lint_legacy_exception_expr(left, config, errors);
            lint_legacy_exception_expr(right, config, errors);
        }
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_legacy_exception_expr(condition, config, errors);
            lint_legacy_exception_expr(then_branch, config, errors);
            lint_legacy_exception_expr(else_branch, config, errors);
        }
        Expr::ChainedComparison { items, .. }
        | Expr::Array { items, .. }
        | Expr::Tuple { items, .. } => {
            for item in items {
                lint_legacy_exception_expr(item, config, errors);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_legacy_exception_expr(condition, config, errors);
            lint_legacy_exception_expr(then_branch, config, errors);
            if let Some(else_branch) = else_branch {
                lint_legacy_exception_expr(else_branch, config, errors);
            }
        }
        Expr::Member { base, .. } => lint_legacy_exception_expr(base, config, errors),
        Expr::Call { callee, args, .. } => {
            lint_legacy_exception_expr(callee, config, errors);
            for arg in args {
                lint_legacy_exception_expr(arg, config, errors);
            }
        }
        Expr::Match { value, arms, .. } => {
            lint_legacy_exception_expr(value, config, errors);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    lint_legacy_exception_expr(guard, config, errors);
                }
                lint_legacy_exception_expr(&arm.body, config, errors);
            }
        }
        Expr::TryCatch {
            try_span,
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            push_rule(
                config,
                errors,
                LintRule::LegacyExceptionSurface,
                "legacy `try { ... } catch { ... }` is deprecated; prefer `?` and `match`"
                    .to_string(),
                Some(*try_span),
            );
            lint_legacy_exception_expr(try_block, config, errors);
            if let Some(catch_block) = catch_block {
                lint_legacy_exception_expr(catch_block, config, errors);
            }
            if let Some(finally_block) = finally_block {
                lint_legacy_exception_expr(finally_block, config, errors);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            lint_legacy_exception_stmts(stmts, config, errors);
            if let Some(tail) = tail {
                lint_legacy_exception_expr(tail, config, errors);
            }
        }
        Expr::Index { base, index, .. } => {
            lint_legacy_exception_expr(base, config, errors);
            lint_legacy_exception_expr(index, config, errors);
        }
        Expr::Range { start, end, .. } => {
            lint_legacy_exception_expr(start, config, errors);
            lint_legacy_exception_expr(end, config, errors);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
                    lint_legacy_exception_expr(expr, config, errors);
                }
            }
        }
        Expr::Closure { body, .. } => lint_legacy_exception_expr(body, config, errors),
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                lint_legacy_exception_expr(&field.value, config, errors);
            }
        }
        Expr::EnumLiteral {
            payload: Some(payload),
            ..
        } => lint_legacy_exception_expr(payload, config, errors),
        Expr::EnumLiteral { payload: None, .. } => {}
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                lint_legacy_exception_expr(key, config, errors);
                lint_legacy_exception_expr(value, config, errors);
            }
        }
        Expr::Int(_, _, _)
        | Expr::Float(_, _, _)
        | Expr::String(_, _, _)
        | Expr::Bool(_, _, _)
        | Expr::Ident(_) => {}
    }
}

fn lint_unqualified_import_calls(
    module: &Module,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    if !module
        .stmts
        .iter()
        .any(|stmt| matches!(stmt, Stmt::Import { .. }))
    {
        return;
    }

    let function_names: HashSet<String> = module
        .functions
        .iter()
        .map(|func| func.name.name.clone())
        .collect();

    for func in &module.functions {
        let mut local_names = HashSet::new();
        for param in &func.params {
            local_names.insert(param.name.name.clone());
        }
        collect_declared_names_stmts(&func.body, &mut local_names);
        lint_unqualified_import_calls_stmts(
            &func.body,
            &function_names,
            &local_names,
            config,
            errors,
        );
    }

    let mut module_local_names = HashSet::new();
    collect_declared_names_stmts(&module.stmts, &mut module_local_names);
    lint_unqualified_import_calls_stmts(
        &module.stmts,
        &function_names,
        &module_local_names,
        config,
        errors,
    );
}

fn lint_unqualified_import_calls_stmts(
    stmts: &[Stmt],
    function_names: &HashSet<String>,
    local_names: &HashSet<String>,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::Import { .. }
            | Stmt::Struct { .. }
            | Stmt::TypeAlias { .. }
            | Stmt::Enum { .. } => {}
            Stmt::Const { value, .. }
            | Stmt::Let { value, .. }
            | Stmt::Using { value, .. }
            | Stmt::Set { value, .. }
            | Stmt::Expr { expr: value, .. }
            | Stmt::Throw { expr: value, .. }
            | Stmt::Defer { expr: value, .. }
            | Stmt::Yield { expr: value, .. } => lint_unqualified_import_calls_expr(
                value,
                function_names,
                local_names,
                config,
                errors,
            ),
            Stmt::SetMember { base, value, .. } => {
                lint_unqualified_import_calls_expr(
                    base,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_expr(
                    value,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
            Stmt::SetIndex {
                base, index, value, ..
            } => {
                lint_unqualified_import_calls_expr(
                    base,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_expr(
                    index,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_expr(
                    value,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
            Stmt::While {
                condition, body, ..
            } => {
                lint_unqualified_import_calls_expr(
                    condition,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_stmts(
                    body,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                lint_unqualified_import_calls_expr(
                    condition,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_stmts(
                    then_branch,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                if let Some(else_branch) = else_branch {
                    lint_unqualified_import_calls_stmts(
                        else_branch,
                        function_names,
                        local_names,
                        config,
                        errors,
                    );
                }
            }
            Stmt::For { iter, body, .. } => {
                lint_unqualified_import_calls_expr(
                    iter,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_stmts(
                    body,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
            Stmt::Return {
                expr: Some(expr), ..
            } => {
                lint_unqualified_import_calls_expr(
                    expr,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
            Stmt::Return { expr: None, .. } => {}
            Stmt::With { value, body, .. } => {
                lint_unqualified_import_calls_expr(
                    value,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_stmts(
                    body,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                lint_unqualified_import_calls_stmts(
                    stmts,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
    }
}

fn lint_unqualified_import_calls_expr(
    expr: &Expr,
    function_names: &HashSet<String>,
    local_names: &HashSet<String>,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    match expr {
        Expr::Call { callee, args, .. } => {
            if let Expr::Ident(ident) = callee.as_ref() {
                if is_unqualified_import_call_candidate(&ident.name, function_names, local_names) {
                    push_rule(
                        config,
                        errors,
                        LintRule::UnqualifiedImportCall,
                        format!(
                            "unqualified call with imports: use alias-qualified access for `{}`",
                            ident.name
                        ),
                        Some(ident.span),
                    );
                }
            }
            lint_unqualified_import_calls_expr(callee, function_names, local_names, config, errors);
            for arg in args {
                lint_unqualified_import_calls_expr(
                    arg,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::Unary { expr, .. }
        | Expr::Try(expr, _)
        | Expr::Await { expr, .. }
        | Expr::ArraySpread { expr, .. }
        | Expr::MapSpread { expr, .. }
        | Expr::As { expr, .. }
        | Expr::Is { expr, .. }
        | Expr::Group { expr, .. } => {
            lint_unqualified_import_calls_expr(expr, function_names, local_names, config, errors);
        }
        Expr::Binary { left, right, .. } => {
            lint_unqualified_import_calls_expr(left, function_names, local_names, config, errors);
            lint_unqualified_import_calls_expr(right, function_names, local_names, config, errors);
        }
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_unqualified_import_calls_expr(
                condition,
                function_names,
                local_names,
                config,
                errors,
            );
            lint_unqualified_import_calls_expr(
                then_branch,
                function_names,
                local_names,
                config,
                errors,
            );
            lint_unqualified_import_calls_expr(
                else_branch,
                function_names,
                local_names,
                config,
                errors,
            );
        }
        Expr::ChainedComparison { items, .. }
        | Expr::Array { items, .. }
        | Expr::Tuple { items, .. } => {
            for item in items {
                lint_unqualified_import_calls_expr(
                    item,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_unqualified_import_calls_expr(
                condition,
                function_names,
                local_names,
                config,
                errors,
            );
            lint_unqualified_import_calls_expr(
                then_branch,
                function_names,
                local_names,
                config,
                errors,
            );
            if let Some(else_branch) = else_branch {
                lint_unqualified_import_calls_expr(
                    else_branch,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::Member { base, .. } => {
            lint_unqualified_import_calls_expr(base, function_names, local_names, config, errors);
        }
        Expr::Match { value, arms, .. } => {
            lint_unqualified_import_calls_expr(value, function_names, local_names, config, errors);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    lint_unqualified_import_calls_expr(
                        guard,
                        function_names,
                        local_names,
                        config,
                        errors,
                    );
                }
                lint_unqualified_import_calls_expr(
                    &arm.body,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::Block { stmts, tail, .. } => {
            lint_unqualified_import_calls_stmts(stmts, function_names, local_names, config, errors);
            if let Some(tail) = tail {
                lint_unqualified_import_calls_expr(
                    tail,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::Index { base, index, .. } => {
            lint_unqualified_import_calls_expr(base, function_names, local_names, config, errors);
            lint_unqualified_import_calls_expr(index, function_names, local_names, config, errors);
        }
        Expr::Range { start, end, .. } => {
            lint_unqualified_import_calls_expr(start, function_names, local_names, config, errors);
            lint_unqualified_import_calls_expr(end, function_names, local_names, config, errors);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
                    lint_unqualified_import_calls_expr(
                        expr,
                        function_names,
                        local_names,
                        config,
                        errors,
                    );
                }
            }
        }
        Expr::Closure { body, .. } => {
            lint_unqualified_import_calls_expr(body, function_names, local_names, config, errors);
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                lint_unqualified_import_calls_expr(
                    &field.value,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::EnumLiteral {
            payload: Some(payload),
            ..
        } => {
            lint_unqualified_import_calls_expr(payload, function_names, local_names, config, errors)
        }
        Expr::EnumLiteral { payload: None, .. } => {}
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            lint_unqualified_import_calls_expr(
                try_block,
                function_names,
                local_names,
                config,
                errors,
            );
            if let Some(catch_block) = catch_block {
                lint_unqualified_import_calls_expr(
                    catch_block,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
            if let Some(finally_block) = finally_block {
                lint_unqualified_import_calls_expr(
                    finally_block,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                lint_unqualified_import_calls_expr(
                    key,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
                lint_unqualified_import_calls_expr(
                    value,
                    function_names,
                    local_names,
                    config,
                    errors,
                );
            }
        }
        Expr::Int(_, _, _)
        | Expr::Float(_, _, _)
        | Expr::String(_, _, _)
        | Expr::Bool(_, _, _)
        | Expr::Ident(_) => {}
    }
}

fn collect_declared_names_stmts(stmts: &[Stmt], names: &mut HashSet<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, value, .. }
            | Stmt::Const { name, value, .. }
            | Stmt::Using { name, value, .. } => {
                names.insert(name.name.clone());
                collect_declared_names_expr(value, names);
            }
            Stmt::With {
                name, value, body, ..
            } => {
                names.insert(name.name.clone());
                collect_declared_names_expr(value, names);
                collect_declared_names_stmts(body, names);
            }
            Stmt::For {
                item, iter, body, ..
            } => {
                names.insert(item.name.clone());
                collect_declared_names_expr(iter, names);
                collect_declared_names_stmts(body, names);
            }
            Stmt::Set { value, .. }
            | Stmt::Expr { expr: value, .. }
            | Stmt::Throw { expr: value, .. }
            | Stmt::Defer { expr: value, .. }
            | Stmt::Yield { expr: value, .. } => collect_declared_names_expr(value, names),
            Stmt::SetMember { base, value, .. } => {
                collect_declared_names_expr(base, names);
                collect_declared_names_expr(value, names);
            }
            Stmt::SetIndex {
                base, index, value, ..
            } => {
                collect_declared_names_expr(base, names);
                collect_declared_names_expr(index, names);
                collect_declared_names_expr(value, names);
            }
            Stmt::While {
                condition, body, ..
            } => {
                collect_declared_names_expr(condition, names);
                collect_declared_names_stmts(body, names);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                collect_declared_names_expr(condition, names);
                collect_declared_names_stmts(then_branch, names);
                if let Some(else_branch) = else_branch {
                    collect_declared_names_stmts(else_branch, names);
                }
            }
            Stmt::Return {
                expr: Some(expr), ..
            } => collect_declared_names_expr(expr, names),
            Stmt::Return { expr: None, .. } => {}
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                collect_declared_names_stmts(stmts, names);
            }
            Stmt::Break { .. }
            | Stmt::Continue { .. }
            | Stmt::Import { .. }
            | Stmt::Struct { .. }
            | Stmt::TypeAlias { .. }
            | Stmt::Enum { .. } => {}
        }
    }
}

fn collect_declared_names_expr(expr: &Expr, names: &mut HashSet<String>) {
    match expr {
        Expr::Unary { expr, .. }
        | Expr::Try(expr, _)
        | Expr::Await { expr, .. }
        | Expr::ArraySpread { expr, .. }
        | Expr::MapSpread { expr, .. }
        | Expr::As { expr, .. }
        | Expr::Is { expr, .. }
        | Expr::Group { expr, .. } => collect_declared_names_expr(expr, names),
        Expr::Binary { left, right, .. } => {
            collect_declared_names_expr(left, names);
            collect_declared_names_expr(right, names);
        }
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_declared_names_expr(condition, names);
            collect_declared_names_expr(then_branch, names);
            collect_declared_names_expr(else_branch, names);
        }
        Expr::ChainedComparison { items, .. }
        | Expr::Array { items, .. }
        | Expr::Tuple { items, .. } => {
            for item in items {
                collect_declared_names_expr(item, names);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_declared_names_expr(condition, names);
            collect_declared_names_expr(then_branch, names);
            if let Some(else_branch) = else_branch {
                collect_declared_names_expr(else_branch, names);
            }
        }
        Expr::Member { base, .. } => collect_declared_names_expr(base, names),
        Expr::Call { callee, args, .. } => {
            collect_declared_names_expr(callee, names);
            for arg in args {
                collect_declared_names_expr(arg, names);
            }
        }
        Expr::Match { value, arms, .. } => {
            collect_declared_names_expr(value, names);
            for arm in arms {
                collect_pattern_bindings(&arm.pattern, names);
                if let Some(guard) = &arm.guard {
                    collect_declared_names_expr(guard, names);
                }
                collect_declared_names_expr(&arm.body, names);
            }
        }
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            collect_declared_names_expr(try_block, names);
            if let Some(catch_block) = catch_block {
                collect_declared_names_expr(catch_block, names);
            }
            if let Some(finally_block) = finally_block {
                collect_declared_names_expr(finally_block, names);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            collect_declared_names_stmts(stmts, names);
            if let Some(tail) = tail {
                collect_declared_names_expr(tail, names);
            }
        }
        Expr::Index { base, index, .. } => {
            collect_declared_names_expr(base, names);
            collect_declared_names_expr(index, names);
        }
        Expr::Range { start, end, .. } => {
            collect_declared_names_expr(start, names);
            collect_declared_names_expr(end, names);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
                    collect_declared_names_expr(expr, names);
                }
            }
        }
        Expr::Closure { params, body, .. } => {
            for param in params {
                names.insert(param.name.clone());
            }
            collect_declared_names_expr(body, names);
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                collect_declared_names_expr(&field.value, names);
            }
        }
        Expr::EnumLiteral {
            payload: Some(payload),
            ..
        } => collect_declared_names_expr(payload, names),
        Expr::EnumLiteral { payload: None, .. } => {}
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_declared_names_expr(key, names);
                collect_declared_names_expr(value, names);
            }
        }
        Expr::Int(_, _, _)
        | Expr::Float(_, _, _)
        | Expr::String(_, _, _)
        | Expr::Bool(_, _, _)
        | Expr::Ident(_) => {}
    }
}

fn collect_pattern_bindings(pattern: &MatchPattern, names: &mut HashSet<String>) {
    match pattern {
        MatchPattern::ResultOk(ident, _)
        | MatchPattern::ResultErr(ident, _)
        | MatchPattern::OptionSome(ident, _) => {
            names.insert(ident.name.clone());
        }
        MatchPattern::Tuple { items, .. } => {
            for item in items {
                collect_pattern_bindings(item, names);
            }
        }
        MatchPattern::Struct { fields, .. } => {
            for field in fields {
                if let Some(binding) = &field.binding {
                    names.insert(binding.name.clone());
                }
            }
        }
        MatchPattern::Enum { binding, .. } => {
            if let Some(binding) = binding {
                names.insert(binding.name.clone());
            }
        }
        MatchPattern::Binding { name, pattern, .. } => {
            names.insert(name.name.clone());
            collect_pattern_bindings(pattern, names);
        }
        MatchPattern::Int(_, _)
        | MatchPattern::Bool(_, _)
        | MatchPattern::String(_, _)
        | MatchPattern::OptionNone(_)
        | MatchPattern::Wildcard(_) => {}
    }
}

fn is_unqualified_import_call_candidate(
    name: &str,
    function_names: &HashSet<String>,
    local_names: &HashSet<String>,
) -> bool {
    if name.contains('.') {
        return false;
    }
    if function_names.contains(name) {
        return false;
    }
    if local_names.contains(name) {
        return false;
    }
    !is_builtin_like_call(name)
}

fn is_builtin_like_call(name: &str) -> bool {
    matches!(
        name,
        "assert"
            | "assert_eq"
            | "print"
            | "next"
            | "len"
            | "append"
            | "contains"
            | "slice"
            | "split"
            | "trim"
            | "substring"
            | "char_at"
            | "to_upper"
            | "to_lower"
            | "parse_int"
            | "to_string"
            | "ok"
            | "err"
            | "some"
            | "none"
            | "map"
    )
}

fn count_stmts(stmts: &[Stmt]) -> usize {
    let mut count = 0;
    for stmt in stmts {
        count += 1;
        match stmt {
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                count += count_stmts(then_branch);
                if let Some(else_branch) = else_branch {
                    count += count_stmts(else_branch);
                }
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::With { body, .. }
            | Stmt::Block { stmts: body, .. }
            | Stmt::Test { body, .. } => {
                count += count_stmts(body);
            }
            _ => {}
        }
    }
    count
}

fn max_nesting(stmts: &[Stmt], depth: usize) -> usize {
    let mut max_depth = depth;
    for stmt in stmts {
        let next = match stmt {
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                let then_depth = max_nesting(then_branch, depth + 1);
                let else_depth = else_branch
                    .as_ref()
                    .map(|branch| max_nesting(branch, depth + 1))
                    .unwrap_or(depth + 1);
                then_depth.max(else_depth)
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::With { body, .. }
            | Stmt::Block { stmts: body, .. }
            | Stmt::Test { body, .. } => max_nesting(body, depth + 1),
            _ => depth,
        };
        if next > max_depth {
            max_depth = next;
        }
    }
    max_depth
}

fn cyclomatic_complexity(stmts: &[Stmt]) -> usize {
    let mut score = 1;
    for stmt in stmts {
        match stmt {
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                score += 1;
                score += cyclomatic_complexity(then_branch) - 1;
                if let Some(else_branch) = else_branch {
                    score += cyclomatic_complexity(else_branch) - 1;
                }
            }
            Stmt::While { body, .. } | Stmt::For { body, .. } => {
                score += 1;
                score += cyclomatic_complexity(body) - 1;
            }
            Stmt::Block { stmts, .. } | Stmt::With { body: stmts, .. } => {
                score += cyclomatic_complexity(stmts) - 1;
            }
            Stmt::Test { body, .. } => {
                score += cyclomatic_complexity(body) - 1;
            }
            _ => {}
        }
    }
    score
}

fn is_snake_case(value: &str) -> bool {
    let mut prev_underscore = false;
    for (idx, ch) in value.chars().enumerate() {
        if ch == '_' {
            if idx == 0 || prev_underscore {
                return false;
            }
            prev_underscore = true;
            continue;
        }
        prev_underscore = false;
        if !ch.is_ascii_lowercase() && !ch.is_ascii_digit() {
            return false;
        }
    }
    !value.is_empty() && !value.ends_with('_')
}

fn is_pascal_case(value: &str) -> bool {
    let mut chars = value.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_uppercase() {
        return false;
    }
    for ch in chars {
        if ch == '_' || (!ch.is_ascii_alphanumeric()) {
            return false;
        }
    }
    true
}

fn contains_guaranteed_termination(stmts: &[Stmt], allow_break: bool) -> bool {
    for stmt in stmts {
        if stmt_guarantees_termination(stmt, allow_break) {
            return true;
        }
    }
    false
}

fn stmt_guarantees_termination(stmt: &Stmt, allow_break: bool) -> bool {
    match stmt {
        Stmt::Return { .. } | Stmt::Throw { .. } => true,
        Stmt::Break { .. } => allow_break,
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            let Some(else_branch) = else_branch else {
                return false;
            };
            contains_guaranteed_termination(then_branch, allow_break)
                && contains_guaranteed_termination(else_branch, allow_break)
        }
        Stmt::With { body, .. } | Stmt::Block { stmts: body, .. } | Stmt::Test { body, .. } => {
            contains_guaranteed_termination(body, allow_break)
        }
        // Termination inside nested loops does not guarantee the current loop exits.
        Stmt::While { .. } | Stmt::For { .. } => false,
        _ => false,
    }
}

fn ensure_scope(scopes: &mut Vec<HashSet<String>>) -> &mut HashSet<String> {
    if scopes.is_empty() {
        scopes.push(HashSet::new());
    }
    scopes.last_mut().expect("scope must exist")
}

fn check_shadowed_stmt(
    stmts: &[Stmt],
    scopes: &mut Vec<HashSet<String>>,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::Let { name, .. }
            | Stmt::Const { name, .. }
            | Stmt::Using { name, .. }
            | Stmt::With { name, .. } => {
                if !should_ignore_name(&name.name)
                    && scopes.iter().any(|scope| scope.contains(&name.name))
                {
                    push_rule(
                        config,
                        errors,
                        LintRule::ShadowedBinding,
                        format!("shadowed binding: {}", name.name),
                        Some(name.span),
                    );
                }
                ensure_scope(scopes).insert(name.name.clone());
            }
            Stmt::For { item, body, .. } => {
                scopes.push(HashSet::new());
                if !should_ignore_name(&item.name)
                    && scopes.iter().any(|scope| scope.contains(&item.name))
                {
                    push_rule(
                        config,
                        errors,
                        LintRule::ShadowedBinding,
                        format!("shadowed binding: {}", item.name),
                        Some(item.span),
                    );
                }
                ensure_scope(scopes).insert(item.name.clone());
                check_shadowed_stmt(body, scopes, config, errors);
                scopes.pop();
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                scopes.push(HashSet::new());
                check_shadowed_stmt(then_branch, scopes, config, errors);
                scopes.pop();
                if let Some(else_branch) = else_branch {
                    scopes.push(HashSet::new());
                    check_shadowed_stmt(else_branch, scopes, config, errors);
                    scopes.pop();
                }
            }
            Stmt::While { body, .. }
            | Stmt::Block { stmts: body, .. }
            | Stmt::Test { body, .. } => {
                scopes.push(HashSet::new());
                check_shadowed_stmt(body, scopes, config, errors);
                scopes.pop();
            }
            _ => {}
        }
    }
}

fn lint_missing_returns(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        if func.return_ty.is_none() {
            continue;
        }
        if !function_always_returns(&func.body) {
            push_rule(
                config,
                errors,
                LintRule::MissingReturn,
                format!(
                    "function '{}' may not return along all paths",
                    func.name.name
                ),
                Some(func.name.span),
            );
        }
    }
}

fn function_always_returns(stmts: &[Stmt]) -> bool {
    for stmt in stmts {
        match stmt {
            Stmt::Return { .. } => return true,
            Stmt::If {
                then_branch,
                else_branch: Some(else_branch),
                ..
            } => {
                if function_always_returns(then_branch) && function_always_returns(else_branch) {
                    return true;
                }
            }
            Stmt::If {
                else_branch: None, ..
            } => {}
            Stmt::Block { stmts, .. } => {
                if function_always_returns(stmts) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

fn lint_dead_branches(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        check_dead_branches(&func.body, config, errors);
    }
    check_dead_branches(&module.stmts, config, errors);
}

fn check_dead_branches(stmts: &[Stmt], config: &LintConfig, errors: &mut Vec<LintError>) {
    for stmt in stmts {
        match stmt {
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                if let Some(value) = eval_bool_literal(condition) {
                    let span = expr_span(condition).or_else(|| stmt_span(stmt));
                    let message = if value {
                        "else branch is unreachable"
                    } else {
                        "then branch is unreachable"
                    };
                    push_rule(
                        config,
                        errors,
                        LintRule::DeadBranch,
                        message.to_string(),
                        span,
                    );
                }
                check_dead_branches(then_branch, config, errors);
                if let Some(else_branch) = else_branch {
                    check_dead_branches(else_branch, config, errors);
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                if let Some(false) = eval_bool_literal(condition) {
                    let span = expr_span(condition).or_else(|| stmt_span(stmt));
                    push_rule(
                        config,
                        errors,
                        LintRule::DeadBranch,
                        "while body is unreachable".to_string(),
                        span,
                    );
                }
                check_dead_branches(body, config, errors);
            }
            Stmt::For { body, .. } => {
                check_dead_branches(body, config, errors);
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                check_dead_branches(stmts, config, errors);
            }
            _ => {}
        }
    }
}

fn eval_bool_literal(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Bool(value, ..) => Some(*value),
        Expr::Unary {
            op: at_syntax::UnaryOp::Not,
            expr,
            ..
        } => eval_bool_literal(expr).map(|value| !value),
        Expr::Unary { .. } => None,
        _ => None,
    }
}

fn expr_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Int(_, span, _)
        | Expr::Float(_, span, _)
        | Expr::String(_, span, _)
        | Expr::Bool(_, span, _)
        | Expr::InterpolatedString { span, .. } => Some(*span),
        Expr::Ident(ident) => Some(ident.span),
        Expr::Unary { op_span, .. }
        | Expr::Binary { op_span, .. }
        | Expr::ArraySpread {
            spread_span: op_span,
            ..
        } => Some(*op_span),
        Expr::Ternary { span, .. }
        | Expr::ChainedComparison { span, .. }
        | Expr::If { if_span: span, .. }
        | Expr::Block {
            block_span: span, ..
        }
        | Expr::Array {
            array_span: span, ..
        }
        | Expr::Tuple {
            tuple_span: span, ..
        }
        | Expr::Range {
            range_span: span, ..
        }
        | Expr::MapLiteral { span, .. }
        | Expr::StructLiteral { span, .. }
        | Expr::EnumLiteral { span, .. }
        | Expr::Group { span, .. }
        | Expr::As { span, .. }
        | Expr::Is { span, .. }
        | Expr::TryCatch { try_span: span, .. }
        | Expr::Match {
            match_span: span, ..
        }
        | Expr::Closure { span, .. } => Some(*span),
        Expr::Member { name, .. } => Some(name.span),
        Expr::Call { callee, .. } => expr_span(callee),
        Expr::Try(expr, _) => expr_span(expr),
        Expr::Await { await_span, .. } => Some(*await_span),
        Expr::Index { index_span, .. } => Some(*index_span),
        Expr::MapSpread { spread_span, .. } => Some(*spread_span),
    }
}

fn lint_unreachable_code(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        check_unreachable_stmts(&func.body, config, errors);
    }
    check_unreachable_stmts(&module.stmts, config, errors);
}

fn stmt_span(stmt: &Stmt) -> Option<Span> {
    match stmt {
        Stmt::Break { break_span, .. } => Some(*break_span),
        Stmt::Continue { continue_span, .. } => Some(*continue_span),
        Stmt::Let { name, .. }
        | Stmt::Const { name, .. }
        | Stmt::Using { name, .. }
        | Stmt::Set { name, .. }
        | Stmt::With { name, .. } => Some(name.span),
        Stmt::SetMember { field, .. } => Some(field.span),
        Stmt::SetIndex { index, .. } => expr_span(index),
        Stmt::Return { expr, .. } => expr.as_ref().and_then(expr_span),
        Stmt::Throw { expr, .. }
        | Stmt::Yield { expr, .. }
        | Stmt::Expr { expr, .. }
        | Stmt::Defer { expr, .. } => expr_span(expr),
        Stmt::If { condition, .. } | Stmt::While { condition, .. } => expr_span(condition),
        Stmt::For { iter, .. } => expr_span(iter),
        Stmt::Block { stmts, .. } => stmts.first().and_then(stmt_span),
        Stmt::Test { .. } => None,
        Stmt::Struct { name, .. }
        | Stmt::Enum { name, .. }
        | Stmt::TypeAlias { name, .. }
        | Stmt::Import { alias: name, .. } => Some(name.span),
    }
}

fn check_unreachable_stmts(stmts: &[Stmt], config: &LintConfig, errors: &mut Vec<LintError>) {
    let mut unreachable_start: Option<Span> = None;
    let mut unreachable_pending = false;

    for stmt in stmts {
        match stmt {
            Stmt::Break { break_span, .. }
            | Stmt::Continue {
                continue_span: break_span,
                ..
            } => {
                if unreachable_start.is_none() {
                    unreachable_start = Some(*break_span);
                    unreachable_pending = true;
                }
            }
            Stmt::Return { expr, .. } => {
                if unreachable_start.is_none() {
                    unreachable_start = expr
                        .as_ref()
                        .and_then(expr_span)
                        .or_else(|| stmt_span(stmt));
                    unreachable_pending = true;
                }
            }
            Stmt::Throw { expr, .. } => {
                if unreachable_start.is_none() {
                    unreachable_start = expr_span(expr).or_else(|| stmt_span(stmt));
                    unreachable_pending = true;
                }
            }
            Stmt::Defer { .. } => {}
            Stmt::With { body, .. } => {
                if unreachable_pending {
                    if let Some(span) = unreachable_start.or_else(|| stmt_span(stmt)) {
                        push_rule(
                            config,
                            errors,
                            LintRule::UnreachableCode,
                            "unreachable code after return/break/continue".to_string(),
                            Some(span),
                        );
                    }
                    unreachable_start = None;
                    unreachable_pending = false;
                }
                check_unreachable_stmts(body, config, errors);
            }
            Stmt::Yield { .. } => {
                if unreachable_start.is_none() {
                    unreachable_start = stmt_span(stmt);
                    unreachable_pending = true;
                }
            }
            _ => {
                if unreachable_pending {
                    if let Some(span) = unreachable_start.or_else(|| stmt_span(stmt)) {
                        push_rule(
                            config,
                            errors,
                            LintRule::UnreachableCode,
                            "unreachable code after return/break/continue".to_string(),
                            Some(span),
                        );
                    }
                    unreachable_start = None;
                    unreachable_pending = false;
                }
            }
        }

        // Recursively check nested blocks
        match stmt {
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                check_unreachable_stmts(then_branch, config, errors);
                if let Some(else_branch) = else_branch {
                    check_unreachable_stmts(else_branch, config, errors);
                }
            }
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
        Stmt::Return {
            expr: Some(expr), ..
        } => collect_used_capabilities_expr(expr, used),
        Stmt::Return { expr: None, .. } => {}
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
        Stmt::Yield { expr, .. } => {
            collect_used_capabilities_expr(expr, used);
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
        Expr::EnumLiteral {
            payload: Some(expr),
            ..
        } => {
            collect_used_capabilities_expr(expr, used);
        }
        Expr::EnumLiteral { payload: None, .. } => {}
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
        Stmt::Return {
            expr: Some(expr), ..
        } => lint_unused_match_bindings_expr(expr, config, errors),
        Stmt::Return { expr: None, .. } => {}
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
        Stmt::Yield { expr, .. } => {
            lint_unused_match_bindings_expr(expr, config, errors);
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
        Expr::EnumLiteral {
            payload: Some(expr),
            ..
        } => {
            lint_unused_match_bindings_expr(expr, config, errors);
        }
        Expr::EnumLiteral { payload: None, .. } => {}
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
                    LintRule::DuplicateLocal,
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
                    LintRule::DuplicateLocal,
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
        Stmt::Return {
            expr: Some(expr), ..
        } => collect_local_uses_expr(expr, used),
        Stmt::Return { expr: None, .. } => {}
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
        Stmt::Yield { expr, .. } => {
            collect_local_uses_expr(expr, used);
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
    if config.is_suppressed(rule, span) {
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
        Expr::EnumLiteral {
            payload: Some(expr),
            ..
        } => {
            collect_local_uses_expr(expr, used);
        }
        Expr::EnumLiteral { payload: None, .. } => {}
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
        Stmt::Return {
            expr: Some(expr), ..
        } => collect_alias_usage_expr(expr, used),
        Stmt::Return { expr: None, .. } => {}
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
        Stmt::Yield { expr, .. } => {
            collect_alias_usage_expr(expr, used);
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
        Expr::EnumLiteral {
            payload: Some(expr),
            ..
        } => {
            collect_alias_usage_expr(expr, used);
        }
        Expr::EnumLiteral { payload: None, .. } => {}
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
        Stmt::Return {
            expr: Some(expr), ..
        } => collect_called_functions_expr(expr, used),
        Stmt::Return { expr: None, .. } => {}
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
        Stmt::Yield { expr, .. } => {
            collect_called_functions_expr(expr, used);
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
        // Track function references in any position (call target, HOF argument, assignment, etc.)
        Expr::Ident(ident) => {
            used.insert(ident.name.clone());
        }
        Expr::Call { callee, args, .. } => {
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
        Expr::EnumLiteral {
            payload: Some(expr),
            ..
        } => {
            collect_called_functions_expr(expr, used);
        }
        Expr::EnumLiteral { payload: None, .. } => {}
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

fn collect_lint_suppressions(module: &Module, source: Option<&str>) -> LintSuppressions {
    let mut suppressions = LintSuppressions::default();
    if let Some(source) = source {
        suppressions.line_starts = collect_line_starts(source);
    }
    let mut next_spans = Vec::new();
    collect_next_stmt_spans(module, &mut next_spans);
    for comment in &module.comments {
        let text = normalize_comment_directive(&comment.text);
        if let Some(rest) = text.strip_prefix("@lint-ignore-next") {
            let rules = parse_suppression_rules(rest.trim());
            if let Some(span) = next_spans
                .iter()
                .find(|span| span.start >= comment.span.end)
                .copied()
            {
                suppressions.spans.push(LintSuppression { span, rules });
            }
            continue;
        }
        if let Some(rest) = text.strip_prefix("@lint-ignore-line") {
            let rules = parse_suppression_rules(rest.trim());
            if let Some(line) = suppressions.line_for_offset(comment.span.start) {
                suppressions.lines.push(LintLineSuppression { line, rules });
            } else {
                // Fallback when source text is unavailable.
                let span = Span::new(comment.span.start, comment.span.end);
                suppressions.spans.push(LintSuppression { span, rules });
            }
            continue;
        }
        if let Some(rest) = text.strip_prefix("@lint-ignore") {
            let rules = parse_suppression_rules(rest.trim());
            match rules {
                None => {
                    suppressions.global_all = true;
                }
                Some(rules) => {
                    suppressions.global.extend(rules);
                }
            }
        }
    }
    suppressions
}

fn collect_line_starts(source: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (index, ch) in source.char_indices() {
        if ch == '\n' {
            starts.push(index + 1);
        }
    }
    starts
}

fn normalize_comment_directive(text: &str) -> &str {
    let trimmed = text.trim();
    if let Some(rest) = trimmed.strip_prefix("//") {
        return rest.trim_start();
    }
    if let Some(rest) = trimmed.strip_prefix("/*") {
        if let Some(rest) = rest.strip_suffix("*/") {
            return rest.trim();
        }
    }
    trimmed
}

fn parse_suppression_rules(list: &str) -> Option<HashSet<LintRule>> {
    if list.is_empty() {
        return None;
    }
    let mut rules = HashSet::new();
    for entry in list.split(',') {
        let name = entry.trim();
        if name.is_empty() {
            continue;
        }
        if let Some(rule) = LintRule::from_str(name) {
            rules.insert(rule);
        }
    }
    if rules.is_empty() {
        None
    } else {
        Some(rules)
    }
}

fn collect_next_stmt_spans(module: &Module, spans: &mut Vec<Span>) {
    for func in &module.functions {
        collect_next_stmt_spans_from_stmts(&func.body, spans);
    }
    collect_next_stmt_spans_from_stmts(&module.stmts, spans);
    spans.sort_by_key(|span| span.start);
}

fn collect_next_stmt_spans_from_stmts(stmts: &[Stmt], spans: &mut Vec<Span>) {
    for stmt in stmts {
        if let Some(span) = stmt_span(stmt) {
            spans.push(span);
        }
        match stmt {
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                collect_next_stmt_spans_from_stmts(then_branch, spans);
                if let Some(branch) = else_branch {
                    collect_next_stmt_spans_from_stmts(branch, spans);
                }
            }
            Stmt::While { body, .. } => {
                collect_next_stmt_spans_from_stmts(body, spans);
            }
            Stmt::For { body, .. } => {
                collect_next_stmt_spans_from_stmts(body, spans);
            }
            Stmt::With { body, .. } => {
                collect_next_stmt_spans_from_stmts(body, spans);
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                collect_next_stmt_spans_from_stmts(stmts, spans);
            }
            _ => {}
        }
    }
}

fn spans_overlap(a: Span, b: Span) -> bool {
    a.start < b.end && b.start < a.end
}

/// Get the number of errors that have auto-fixes available
pub fn count_fixable(errors: &[LintError]) -> usize {
    errors.iter().filter(|err| err.fix.is_some()).count()
}

#[cfg(test)]
mod tests {
    use super::{
        lint_module, lint_module_with_config, lint_module_with_source_and_config, LintConfig,
        LintRule, LintSeverity,
    };
    use at_parser::parse_module;
    use at_parser::parse_module_with_errors;

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
    fn strict_profile_promotes_severity() {
        let config = LintConfig::load("profile = \"strict\"");
        assert_eq!(
            config
                .severity
                .get(&LintRule::UnusedLocal)
                .copied()
                .unwrap_or(LintSeverity::Warn),
            LintSeverity::Error
        );
        assert_eq!(
            config
                .severity
                .get(&LintRule::UnknownTypeFlow)
                .copied()
                .unwrap_or(LintSeverity::Warn),
            LintSeverity::Error
        );
    }

    #[test]
    fn unknown_type_flow_warns_by_default() {
        let source = r#"
fn f() {
    let id = |x| x;
    id(1);
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        let unknown = errors
            .iter()
            .find(|err| err.rule == Some(LintRule::UnknownTypeFlow))
            .expect("expected unknown type flow lint");
        assert_eq!(unknown.severity, LintSeverity::Warn);
    }

    #[test]
    fn unknown_type_flow_is_error_in_strict_profile() {
        let source = r#"
fn f() {
    let id = |x| x;
    id(1);
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module_with_config(&module, Some("profile = \"strict\""))
            .expect_err("expected lint errors");
        let unknown = errors
            .iter()
            .find(|err| err.rule == Some(LintRule::UnknownTypeFlow))
            .expect("expected unknown type flow lint");
        assert_eq!(unknown.severity, LintSeverity::Error);
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
            .any(|err| err.rule == Some(LintRule::DuplicateLocal)));
    }

    #[test]
    fn allows_shadowing_in_block() {
        let source = r#"
fn f() -> unit {
    let _x = 1;
    {
        let _x = 2;
    }
    return;
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        assert!(lint_module(&module).is_ok());
    }

    #[test]
    fn allows_duplicate_ignored_locals() {
        let source = r#"
fn f(_x: int, _x: int) -> unit {
    let _y = 1;
    let _y = 2;
    return;
}
f(1, 2);
"#;
        let module = parse_module(source).expect("parse module");
        assert!(lint_module(&module).is_ok());
    }

    #[test]
    fn dead_branch_span_is_real() {
        let source = r#"
fn f() {
    if true {
        let x = 1;
    } else {
        let y = 2;
    }
}
"#;
        let (module, errors) = parse_module_with_errors(source);
        assert!(errors.is_empty());
        let errors = lint_module(&module).expect_err("expected lint errors");
        let dead_branch = errors
            .iter()
            .find(|err| err.message.contains("branch is unreachable"))
            .expect("dead branch lint");
        let span = dead_branch.span.expect("span");
        assert_ne!(span.start, 0);
        assert_ne!(span.end, 0);
    }

    #[test]
    fn unreachable_after_yield_has_span() {
        let source = r#"
fn f() {
    yield 1;
    let x = 2;
}
"#;
        let (module, errors) = parse_module_with_errors(source);
        assert!(errors.is_empty());
        let errors = lint_module(&module).expect_err("expected lint errors");
        let unreachable = errors
            .iter()
            .find(|err| err.message.contains("unreachable code"))
            .expect("unreachable lint");
        let span = unreachable.span.expect("span");
        assert_ne!(span.start, 0);
        assert_ne!(span.end, 0);
    }

    #[test]
    fn legacy_exception_surface_warns_by_default() {
        let source = r#"
fn f() {
    throw err("boom");
}

fn g() {
    return try { ok(1) } catch { err("no") };
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        let legacy = errors
            .iter()
            .find(|err| err.rule == Some(LintRule::LegacyExceptionSurface))
            .expect("expected legacy exception lint");
        assert_eq!(legacy.severity, LintSeverity::Warn);
    }

    #[test]
    fn unqualified_import_call_warns_by_default() {
        let source = r#"
import "./lib.at" as lib;

fn f() {
    return version();
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        let unqualified = errors
            .iter()
            .find(|err| err.rule == Some(LintRule::UnqualifiedImportCall))
            .expect("expected unqualified import call lint");
        assert_eq!(unqualified.severity, LintSeverity::Warn);
    }

    #[test]
    fn staged_deprecation_rules_are_errors_in_strict_profile() {
        let source = r#"
import "./lib.at" as lib;

fn f() {
    throw err("boom");
    return version();
}
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module_with_config(&module, Some("profile = \"strict\""))
            .expect_err("expected lint errors");
        let legacy = errors
            .iter()
            .find(|err| err.rule == Some(LintRule::LegacyExceptionSurface))
            .expect("expected legacy exception lint");
        let unqualified = errors
            .iter()
            .find(|err| err.rule == Some(LintRule::UnqualifiedImportCall))
            .expect("expected unqualified import call lint");
        assert_eq!(legacy.severity, LintSeverity::Error);
        assert_eq!(unqualified.severity, LintSeverity::Error);
    }

    #[test]
    fn lint_ignore_line_suppresses_diagnostic_on_same_line() {
        let source = r#"
fn f() {
    let x = 1; // @lint-ignore-line unused-local
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        let result = lint_module_with_source_and_config(&module, Some(source), None);
        if let Err(errors) = result {
            assert!(
                !errors
                    .iter()
                    .any(|err| err.rule == Some(LintRule::UnusedLocal)),
                "unused-local should be suppressed on the directive line: {errors:?}"
            );
        }
    }

    #[test]
    fn unreachable_inside_if_branch_is_reported() {
        let source = r#"
fn f() {
    if true {
        return;
        let x = 1;
    }
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        assert!(
            errors
                .iter()
                .any(|err| err.rule == Some(LintRule::UnreachableCode)),
            "expected unreachable code lint in if-branch: {errors:?}"
        );
    }

    #[test]
    fn infinite_loop_requires_guaranteed_termination() {
        let source = r#"
fn f(flag: bool) {
    while true {
        if flag {
            break;
        }
    }
}
f(true);
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module).expect_err("expected lint errors");
        assert!(
            errors
                .iter()
                .any(|err| err.rule == Some(LintRule::InfiniteLoop)),
            "expected infinite loop lint when break is not guaranteed: {errors:?}"
        );
    }

    #[test]
    fn infinite_loop_allows_unconditional_break() {
        let source = r#"
fn f() {
    while true {
        break;
    }
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module(&module);
        if let Err(errors) = errors {
            assert!(
                !errors
                    .iter()
                    .any(|err| err.rule == Some(LintRule::InfiniteLoop)),
                "did not expect infinite-loop lint with unconditional break: {errors:?}"
            );
        }
    }

    #[test]
    fn function_reference_counts_as_usage() {
        let source = r#"
fn helper() -> int {
    return 1;
}

fn f() {
    let g = helper;
    g();
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors =
            lint_module_with_config(&module, Some("[rules]\nunknown_type_flow = \"off\"\n"));
        if let Err(errors) = errors {
            assert!(
                !errors.iter().any(|err| {
                    err.rule == Some(LintRule::UnusedFunction) && err.message.contains("helper")
                }),
                "helper should count as used when referenced: {errors:?}"
            );
        }
    }
}
