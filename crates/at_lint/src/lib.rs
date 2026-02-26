use std::collections::{HashMap, HashSet};

use at_syntax::{Expr, Ident, InterpPart, MapEntry, MatchPattern, Module, Span, Stmt};

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
    ImportAliasMatchesFilename,
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
            "import_alias_matches_filename" => Some(LintRule::ImportAliasMatchesFilename),
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
            LintRule::ImportAliasMatchesFilename,
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
                | LintRule::UnqualifiedImportCall
                | LintRule::ImportAliasMatchesFilename => LintSeverity::Warn,
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
            LintRule::ImportAliasMatchesFilename,
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
}

struct LintResult {
    errors: Vec<LintError>,
    warnings: Vec<LintError>,
    infos: Vec<LintError>,
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

// ---------------------------------------------------------------------------
// LintVisitor trait + generic walkers
// ---------------------------------------------------------------------------

trait LintVisitor {
    /// Called for each statement. Return `true` to recurse into children, `false` to skip.
    fn visit_stmt(&mut self, _stmt: &Stmt) -> bool {
        true
    }
    /// Called for each expression. Return `true` to recurse into children, `false` to skip.
    fn visit_expr(&mut self, _expr: &Expr) -> bool {
        true
    }
    /// Called before iterating a statement list (scope enter).
    fn enter_stmts(&mut self, _stmts: &[Stmt]) {}
    /// Called after iterating a statement list (scope exit).
    fn exit_stmts(&mut self, _stmts: &[Stmt]) {}
    /// Walk a statement list. Default calls enter_stmts, walks each stmt, calls exit_stmts.
    fn visit_stmts(&mut self, stmts: &[Stmt])
    where
        Self: Sized,
    {
        self.enter_stmts(stmts);
        for stmt in stmts {
            walk_stmt(self, stmt);
        }
        self.exit_stmts(stmts);
    }
}

fn walk_stmt(visitor: &mut (impl LintVisitor + Sized), stmt: &Stmt) {
    if !visitor.visit_stmt(stmt) {
        return;
    }
    match stmt {
        Stmt::Import { .. }
        | Stmt::Struct { .. }
        | Stmt::TypeAlias { .. }
        | Stmt::Enum { .. }
        | Stmt::Break { .. }
        | Stmt::Continue { .. } => {}
        Stmt::Const { value, .. }
        | Stmt::Let { value, .. }
        | Stmt::Using { value, .. }
        | Stmt::Set { value, .. }
        | Stmt::Expr { expr: value, .. }
        | Stmt::Throw { expr: value, .. }
        | Stmt::Defer { expr: value, .. }
        | Stmt::Yield { expr: value, .. } => walk_expr(visitor, value),
        Stmt::SetMember { base, value, .. } => {
            walk_expr(visitor, base);
            walk_expr(visitor, value);
        }
        Stmt::SetIndex {
            base, index, value, ..
        } => {
            walk_expr(visitor, base);
            walk_expr(visitor, index);
            walk_expr(visitor, value);
        }
        Stmt::Return {
            expr: Some(expr), ..
        } => walk_expr(visitor, expr),
        Stmt::Return { expr: None, .. } => {}
        Stmt::While {
            condition, body, ..
        } => {
            walk_expr(visitor, condition);
            visitor.visit_stmts(body);
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            walk_expr(visitor, condition);
            visitor.visit_stmts(then_branch);
            if let Some(else_branch) = else_branch {
                visitor.visit_stmts(else_branch);
            }
        }
        Stmt::For { iter, body, .. } => {
            walk_expr(visitor, iter);
            visitor.visit_stmts(body);
        }
        Stmt::With { value, body, .. } => {
            walk_expr(visitor, value);
            visitor.visit_stmts(body);
        }
        Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
            visitor.visit_stmts(stmts);
        }
    }
}

fn walk_expr(visitor: &mut (impl LintVisitor + Sized), expr: &Expr) {
    if !visitor.visit_expr(expr) {
        return;
    }
    match expr {
        Expr::Int(..) | Expr::Float(..) | Expr::String(..) | Expr::Bool(..) | Expr::Ident(_) => {}
        Expr::Unary { expr, .. }
        | Expr::Try(expr, _)
        | Expr::Await { expr, .. }
        | Expr::ArraySpread { expr, .. }
        | Expr::As { expr, .. }
        | Expr::Is { expr, .. }
        | Expr::Group { expr, .. } => walk_expr(visitor, expr),
        Expr::Binary { left, right, .. } => {
            walk_expr(visitor, left);
            walk_expr(visitor, right);
        }
        Expr::Ternary {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            walk_expr(visitor, condition);
            walk_expr(visitor, then_branch);
            walk_expr(visitor, else_branch);
        }
        Expr::ChainedComparison { items, .. }
        | Expr::Array { items, .. }
        | Expr::Tuple { items, .. } => {
            for item in items {
                walk_expr(visitor, item);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            walk_expr(visitor, condition);
            walk_expr(visitor, then_branch);
            if let Some(else_branch) = else_branch {
                walk_expr(visitor, else_branch);
            }
        }
        Expr::Member { base, .. } => walk_expr(visitor, base),
        Expr::Call { callee, args, .. } => {
            walk_expr(visitor, callee);
            for arg in args {
                walk_expr(visitor, arg);
            }
        }
        Expr::Match { value, arms, .. } => {
            walk_expr(visitor, value);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expr(visitor, guard);
                }
                walk_expr(visitor, &arm.body);
            }
        }
        Expr::TryCatch {
            try_block,
            catch_block,
            finally_block,
            ..
        } => {
            walk_expr(visitor, try_block);
            if let Some(catch_block) = catch_block {
                walk_expr(visitor, catch_block);
            }
            if let Some(finally_block) = finally_block {
                walk_expr(visitor, finally_block);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            visitor.visit_stmts(stmts);
            if let Some(tail) = tail {
                walk_expr(visitor, tail);
            }
        }
        Expr::Index { base, index, .. } => {
            walk_expr(visitor, base);
            walk_expr(visitor, index);
        }
        Expr::Range { start, end, .. } => {
            walk_expr(visitor, start);
            walk_expr(visitor, end);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let InterpPart::Expr(expr, _) = part {
                    walk_expr(visitor, expr);
                }
            }
        }
        Expr::Closure { body, .. } => walk_expr(visitor, body),
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                walk_expr(visitor, &field.value);
            }
        }
        Expr::EnumLiteral {
            payload: Some(exprs),
            ..
        } => {
            for expr in exprs {
                walk_expr(visitor, expr);
            }
        }
        Expr::EnumLiteral { payload: None, .. } => {}
        Expr::MapLiteral { entries, .. } => {
            for entry in entries {
                match entry {
                    MapEntry::KeyValue { key, value } => {
                        walk_expr(visitor, key);
                        walk_expr(visitor, value);
                    }
                    MapEntry::Spread(expr) => walk_expr(visitor, expr),
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Visitor structs — collectors (stmt+expr pairs)
// ---------------------------------------------------------------------------

/// Collects alias usages. Special: on `Member { base: Ident(..) }`, inserts the
/// ident name and does NOT recurse into base. On bare `Ident`, inserts name.
struct AliasUsageCollector<'a> {
    used: &'a mut HashSet<String>,
}

impl LintVisitor for AliasUsageCollector<'_> {
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        match expr {
            Expr::Member { base, .. } => {
                if let Expr::Ident(ident) = base.as_ref() {
                    self.used.insert(ident.name.clone());
                    // Do NOT recurse into base — we extracted the ident already.
                    // But we still need to NOT recurse into children via the default walker,
                    // because the default walker would call walk_expr(base) which would
                    // add the ident again (harmless) but skip the short-circuit.
                    // Return false and handle non-base children manually? No — Member only
                    // has `base` as child, so returning false is fine.
                    return false;
                }
                // Non-ident base: let default walker recurse.
                true
            }
            Expr::Ident(ident) => {
                self.used.insert(ident.name.clone());
                true
            }
            _ => true,
        }
    }
}

/// Collects called/referenced function names. On `Ident`, inserts name.
/// On `Member`, recurses into base normally (unlike AliasUsageCollector).
struct CalledFunctionsCollector<'a> {
    used: &'a mut HashSet<String>,
}

impl LintVisitor for CalledFunctionsCollector<'_> {
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        if let Expr::Ident(ident) = expr {
            self.used.insert(ident.name.clone());
        }
        true
    }
}

/// Collects local variable uses. Same special Member logic as AliasUsageCollector.
struct LocalUsesCollector<'a> {
    used: &'a mut HashSet<String>,
}

impl LintVisitor for LocalUsesCollector<'_> {
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        match expr {
            Expr::Member { base, .. } => {
                if let Expr::Ident(ident) = base.as_ref() {
                    self.used.insert(ident.name.clone());
                    return false;
                }
                true
            }
            Expr::Ident(ident) => {
                self.used.insert(ident.name.clone());
                true
            }
            _ => true,
        }
    }
}

/// Collects used capabilities. On `Member { base: Ident(..) }`, inserts the ident
/// AND continues recursion (unlike AliasUsageCollector which stops).
struct UsedCapabilitiesCollector<'a> {
    used: &'a mut HashSet<String>,
}

impl LintVisitor for UsedCapabilitiesCollector<'_> {
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        if let Expr::Member { base, .. } = expr {
            if let Expr::Ident(ident) = base.as_ref() {
                self.used.insert(ident.name.clone());
            }
        }
        true
    }
}

/// Collects names declared in stmts (let/const/using/with/for bindings, closure params,
/// match pattern bindings).
struct DeclaredNamesCollector<'a> {
    names: &'a mut HashSet<String>,
}

impl LintVisitor for DeclaredNamesCollector<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Let { name, .. }
            | Stmt::Const { name, .. }
            | Stmt::Using { name, .. }
            | Stmt::With { name, .. } => {
                self.names.insert(name.name.clone());
            }
            Stmt::For { item, .. } => {
                self.names.insert(item.name.clone());
            }
            _ => {}
        }
        true
    }
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        match expr {
            Expr::Match { arms, .. } => {
                for arm in arms {
                    collect_pattern_bindings(&arm.pattern, self.names);
                }
            }
            Expr::Closure { params, .. } => {
                for param in params {
                    self.names.insert(param.name.clone());
                }
            }
            _ => {}
        }
        true
    }
}

// ---------------------------------------------------------------------------
// Visitor structs — lint checkers (stmt+expr pairs)
// ---------------------------------------------------------------------------

struct UnusedMatchBindingsLint<'a> {
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for UnusedMatchBindingsLint<'_> {
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        if let Expr::Match { value, arms, .. } = expr {
            // Walk the value with self (default recursion will handle it, but we need
            // to process arms specially). Return false to handle children manually.
            walk_expr(self, value);
            for arm in arms {
                let mut used = HashSet::new();
                if let Some(guard) = &arm.guard {
                    let mut collector = LocalUsesCollector { used: &mut used };
                    walk_expr(&mut collector, guard);
                }
                {
                    let mut collector = LocalUsesCollector { used: &mut used };
                    walk_expr(&mut collector, &arm.body);
                }
                for ident in match_pattern_idents(&arm.pattern) {
                    if should_ignore_name(&ident.name) {
                        continue;
                    }
                    if !used.contains(&ident.name) {
                        push_rule(
                            self.config,
                            self.errors,
                            LintRule::UnusedMatchBinding,
                            format!("unused match binding: {}", ident.name),
                            Some(ident.span),
                        );
                    }
                }
                if let Some(guard) = &arm.guard {
                    walk_expr(self, guard);
                }
                walk_expr(self, &arm.body);
            }
            return false;
        }
        true
    }
}

struct BooleanLiteralLint<'a> {
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for BooleanLiteralLint<'_> {
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        if let Expr::Binary {
            left, op, right, ..
        } = expr
        {
            if matches!(op, at_syntax::BinaryOp::Eq | at_syntax::BinaryOp::Neq)
                && (matches!(**left, Expr::Bool(..)) || matches!(**right, Expr::Bool(..)))
            {
                push_rule(
                    self.config,
                    self.errors,
                    LintRule::BooleanLiteralComparison,
                    "boolean literal comparison can be simplified".to_string(),
                    expr_span(expr),
                );
            }
        }
        true
    }
}

struct LegacyExceptionLint<'a> {
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for LegacyExceptionLint<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> bool {
        if let Stmt::Throw { expr, .. } = stmt {
            push_rule(
                self.config,
                self.errors,
                LintRule::LegacyExceptionSurface,
                "legacy exception-style `throw` is deprecated; prefer result/option flows"
                    .to_string(),
                expr_span(expr).or_else(|| stmt_span(stmt)),
            );
        }
        true
    }
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        if let Expr::TryCatch { try_span, .. } = expr {
            push_rule(
                self.config,
                self.errors,
                LintRule::LegacyExceptionSurface,
                "legacy `try { ... } catch { ... }` is deprecated; prefer `?` and `match`"
                    .to_string(),
                Some(*try_span),
            );
        }
        true
    }
}

struct UnqualifiedImportCallsLint<'a> {
    function_names: &'a HashSet<String>,
    local_names: &'a HashSet<String>,
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for UnqualifiedImportCallsLint<'_> {
    fn visit_expr(&mut self, expr: &Expr) -> bool {
        if let Expr::Call { callee, .. } = expr {
            if let Expr::Ident(ident) = callee.as_ref() {
                if is_unqualified_import_call_candidate(
                    &ident.name,
                    self.function_names,
                    self.local_names,
                ) {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::UnqualifiedImportCall,
                        format!(
                            "unqualified call with imports: use alias-qualified access for `{}`",
                            ident.name
                        ),
                        Some(ident.span),
                    );
                }
            }
        }
        true
    }
}

// ---------------------------------------------------------------------------
// Visitor structs — stmt-only walkers
// ---------------------------------------------------------------------------

struct ShadowedCheckVisitor<'a> {
    scopes: &'a mut Vec<HashSet<String>>,
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for ShadowedCheckVisitor<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Let { name, .. }
            | Stmt::Const { name, .. }
            | Stmt::Using { name, .. }
            | Stmt::With { name, .. } => {
                if !should_ignore_name(&name.name)
                    && self.scopes.iter().any(|scope| scope.contains(&name.name))
                {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::ShadowedBinding,
                        format!("shadowed binding: {}", name.name),
                        Some(name.span),
                    );
                }
                ensure_scope(self.scopes).insert(name.name.clone());
                // For With, we still need to recurse into value + body.
                // For Let/Const/Using, walk_stmt recurses into value.
                // Default recursion is fine for all of these.
                true
            }
            Stmt::For { item, body, .. } => {
                // For creates a new scope for its body.
                self.scopes.push(HashSet::new());
                if !should_ignore_name(&item.name)
                    && self.scopes.iter().any(|scope| scope.contains(&item.name))
                {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::ShadowedBinding,
                        format!("shadowed binding: {}", item.name),
                        Some(item.span),
                    );
                }
                ensure_scope(self.scopes).insert(item.name.clone());
                // Manually walk body since we manage the scope ourselves.
                self.visit_stmts(body);
                self.scopes.pop();
                // Return false — we handled children.
                false
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                // Each branch gets its own scope. Don't recurse via default;
                // handle manually.
                self.scopes.push(HashSet::new());
                self.visit_stmts(then_branch);
                self.scopes.pop();
                if let Some(else_branch) = else_branch {
                    self.scopes.push(HashSet::new());
                    self.visit_stmts(else_branch);
                    self.scopes.pop();
                }
                false
            }
            Stmt::While { body, .. }
            | Stmt::Block { stmts: body, .. }
            | Stmt::Test { body, .. } => {
                self.scopes.push(HashSet::new());
                self.visit_stmts(body);
                self.scopes.pop();
                false
            }
            _ => {
                // No scope changes; skip expr recursion since shadowed bindings
                // are a stmt-level check only.
                false
            }
        }
    }
    // We don't need visit_expr — shadowed bindings is stmt-only.
    fn visit_expr(&mut self, _expr: &Expr) -> bool {
        false
    }
}

struct DeadBranchLint<'a> {
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for DeadBranchLint<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> bool {
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
                        self.config,
                        self.errors,
                        LintRule::DeadBranch,
                        message.to_string(),
                        span,
                    );
                }
                self.visit_stmts(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_stmts(else_branch);
                }
                false
            }
            Stmt::While {
                condition, body, ..
            } => {
                if let Some(false) = eval_bool_literal(condition) {
                    let span = expr_span(condition).or_else(|| stmt_span(stmt));
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::DeadBranch,
                        "while body is unreachable".to_string(),
                        span,
                    );
                }
                self.visit_stmts(body);
                false
            }
            Stmt::For { body, .. } => {
                self.visit_stmts(body);
                false
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                self.visit_stmts(stmts);
                false
            }
            _ => false,
        }
    }
    fn visit_expr(&mut self, _expr: &Expr) -> bool {
        false
    }
}

struct UnusedSetLint<'a> {
    reads: &'a HashSet<String>,
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for UnusedSetLint<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Set { name, .. } => {
                if !should_ignore_name(&name.name) && !self.reads.contains(&name.name) {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::UnusedSetTarget,
                        format!("value assigned but never read: {}", name.name),
                        Some(name.span),
                    );
                }
                false
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_stmts(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_stmts(else_branch);
                }
                false
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::With { body, .. }
            | Stmt::Block { stmts: body, .. }
            | Stmt::Test { body, .. } => {
                self.visit_stmts(body);
                false
            }
            _ => false,
        }
    }
    fn visit_expr(&mut self, _expr: &Expr) -> bool {
        false
    }
}

struct InfiniteLoopLint<'a> {
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for InfiniteLoopLint<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> bool {
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
                        self.config,
                        self.errors,
                        LintRule::InfiniteLoop,
                        "infinite loop (while true without break/return)".to_string(),
                        Some(*while_span),
                    );
                }
                self.visit_stmts(body);
                false
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_stmts(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_stmts(else_branch);
                }
                false
            }
            Stmt::For { body, .. } | Stmt::With { body, .. } | Stmt::Block { stmts: body, .. } => {
                self.visit_stmts(body);
                false
            }
            _ => false,
        }
    }
    fn visit_expr(&mut self, _expr: &Expr) -> bool {
        false
    }
}

/// EmptyBodyLint: checks for empty bodies. Does NOT recurse into children.
struct EmptyBodyLint<'a> {
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for EmptyBodyLint<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::If {
                then_branch,
                else_branch,
                if_span,
                ..
            } => {
                if then_branch.is_empty() {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::EmptyBody,
                        "empty if body".to_string(),
                        Some(*if_span),
                    );
                }
                if let Some(else_branch) = else_branch {
                    if else_branch.is_empty() {
                        push_rule(
                            self.config,
                            self.errors,
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
                        self.config,
                        self.errors,
                        LintRule::EmptyBody,
                        "empty while body".to_string(),
                        Some(*while_span),
                    );
                }
            }
            Stmt::For { body, for_span, .. } => {
                if body.is_empty() {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::EmptyBody,
                        "empty for body".to_string(),
                        Some(*for_span),
                    );
                }
            }
            Stmt::Block { stmts, .. } => {
                if stmts.is_empty() {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::EmptyBody,
                        "empty block".to_string(),
                        stmt_span(stmt),
                    );
                }
            }
            Stmt::Test { body, .. } => {
                if body.is_empty() {
                    push_rule(
                        self.config,
                        self.errors,
                        LintRule::EmptyBody,
                        "empty test body".to_string(),
                        stmt_span(stmt),
                    );
                }
            }
            _ => {}
        }
        // No recursion needed for empty body checks.
        false
    }
    fn visit_expr(&mut self, _expr: &Expr) -> bool {
        false
    }
}

/// UnreachableLint: tracks cross-sibling flow state.
/// Overrides `visit_stmts` to detect unreachable code after return/break/continue/yield.
struct UnreachableLint<'a> {
    config: &'a LintConfig,
    errors: &'a mut Vec<LintError>,
}

impl LintVisitor for UnreachableLint<'_> {
    fn visit_stmts(&mut self, stmts: &[Stmt]) {
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
                                self.config,
                                self.errors,
                                LintRule::UnreachableCode,
                                "unreachable code after return/break/continue".to_string(),
                                Some(span),
                            );
                        }
                        unreachable_start = None;
                        unreachable_pending = false;
                    }
                    self.visit_stmts(body);
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
                                self.config,
                                self.errors,
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
                    self.visit_stmts(then_branch);
                    if let Some(else_branch) = else_branch {
                        self.visit_stmts(else_branch);
                    }
                }
                Stmt::While { body, .. } => self.visit_stmts(body),
                Stmt::For { body, .. } => self.visit_stmts(body),
                Stmt::Block {
                    stmts: nested_stmts,
                    ..
                } => self.visit_stmts(nested_stmts),
                Stmt::Test { body, .. } => self.visit_stmts(body),
                _ => {}
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

#[cfg(test)]
fn lint_module(module: &Module) -> Result<(), Vec<LintError>> {
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
            && path.trim().is_empty()
        {
            push_rule(
                &config,
                &mut errors,
                LintRule::MissingImportPath,
                "import path is empty".to_string(),
                Some(alias.span),
            );
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
            import_order_check.push((path.clone(), alias.span));
        }

        // Check that the alias matches the filename stem
        if path != "std"
            && path != "std.at"
            && !path.starts_with("http://")
            && !path.starts_with("https://")
            && !path.trim().is_empty()
        {
            let file_part = path.rsplit('/').next().unwrap_or(&path);
            let stem = file_part.strip_suffix(".at").unwrap_or(file_part);
            if !stem.is_empty() && stem != alias.name {
                push_rule(
                    &config,
                    &mut errors,
                    LintRule::ImportAliasMatchesFilename,
                    format!(
                        "import alias `{}` does not match filename `{stem}`; consider `as {stem}`",
                        alias.name
                    ),
                    Some(alias.span),
                );
            }
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

// ---------------------------------------------------------------------------
// Lint entry points (using visitors)
// ---------------------------------------------------------------------------

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
        let mut visitor = ShadowedCheckVisitor {
            scopes: &mut scopes,
            config,
            errors,
        };
        visitor.visit_stmts(&func.body);
    }
    let mut scopes: Vec<HashSet<String>> = Vec::new();
    scopes.push(HashSet::new());
    let mut visitor = ShadowedCheckVisitor {
        scopes: &mut scopes,
        config,
        errors,
    };
    visitor.visit_stmts(&module.stmts);
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
        let mut collector = LocalUsesCollector { used: &mut reads };
        collector.visit_stmts(&func.body);
        let mut lint = UnusedSetLint {
            reads: &reads,
            config,
            errors,
        };
        lint.visit_stmts(&func.body);
    }
}

fn lint_infinite_loops(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    let mut lint = InfiniteLoopLint { config, errors };
    for func in &module.functions {
        lint.visit_stmts(&func.body);
    }
    lint.visit_stmts(&module.stmts);
}

fn lint_boolean_literal_comparisons(
    module: &Module,
    config: &LintConfig,
    errors: &mut Vec<LintError>,
) {
    let mut lint = BooleanLiteralLint { config, errors };
    lint.visit_stmts(&module.stmts);
    for func in &module.functions {
        lint.visit_stmts(&func.body);
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
        let mut lint = EmptyBodyLint { config, errors };
        for stmt in &func.body {
            walk_stmt(&mut lint, stmt);
        }
    }
    let mut lint = EmptyBodyLint { config, errors };
    for stmt in &module.stmts {
        walk_stmt(&mut lint, stmt);
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
    let mut lint = LegacyExceptionLint { config, errors };
    for func in &module.functions {
        lint.visit_stmts(&func.body);
    }
    lint.visit_stmts(&module.stmts);
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
        let mut name_collector = DeclaredNamesCollector {
            names: &mut local_names,
        };
        name_collector.visit_stmts(&func.body);
        let mut lint = UnqualifiedImportCallsLint {
            function_names: &function_names,
            local_names: &local_names,
            config,
            errors,
        };
        lint.visit_stmts(&func.body);
    }

    let mut module_local_names = HashSet::new();
    let mut name_collector = DeclaredNamesCollector {
        names: &mut module_local_names,
    };
    name_collector.visit_stmts(&module.stmts);
    let mut lint = UnqualifiedImportCallsLint {
        function_names: &function_names,
        local_names: &module_local_names,
        config,
        errors,
    };
    lint.visit_stmts(&module.stmts);
}

fn lint_unreachable_code(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    let mut lint = UnreachableLint { config, errors };
    for func in &module.functions {
        lint.visit_stmts(&func.body);
    }
    lint.visit_stmts(&module.stmts);
}

fn lint_unused_match_bindings(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    let mut lint = UnusedMatchBindingsLint { config, errors };
    for func in &module.functions {
        lint.visit_stmts(&func.body);
    }
    lint.visit_stmts(&module.stmts);
}

fn lint_unnecessary_needs(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    for func in &module.functions {
        if func.needs.is_empty() {
            continue;
        }
        let mut used_capabilities = HashSet::new();
        let mut collector = UsedCapabilitiesCollector {
            used: &mut used_capabilities,
        };
        collector.visit_stmts(&func.body);
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

fn lint_dead_branches(module: &Module, config: &LintConfig, errors: &mut Vec<LintError>) {
    let mut lint = DeadBranchLint { config, errors };
    for func in &module.functions {
        lint.visit_stmts(&func.body);
    }
    lint.visit_stmts(&module.stmts);
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
        }
        let mut collector = LocalUsesCollector { used: &mut used };
        collector.visit_stmts(&func.body);
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
    }
    let mut collector = LocalUsesCollector { used: &mut used };
    collector.visit_stmts(&module.stmts);
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

// ---------------------------------------------------------------------------
// Collector convenience wrappers
// ---------------------------------------------------------------------------

fn collect_alias_usage(module: &Module, used: &mut HashSet<String>) {
    let mut collector = AliasUsageCollector { used };
    for func in &module.functions {
        collector.visit_stmts(&func.body);
    }
    collector.visit_stmts(&module.stmts);
}

fn collect_called_functions(module: &Module, used: &mut HashSet<String>) {
    let mut collector = CalledFunctionsCollector { used };
    for func in &module.functions {
        collector.visit_stmts(&func.body);
    }
    collector.visit_stmts(&module.stmts);
}

// ---------------------------------------------------------------------------
// Functions kept as-is (not visitorizable)
// ---------------------------------------------------------------------------

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
        MatchPattern::Enum { bindings, .. } => {
            for binding in bindings {
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
        Stmt::While { .. } | Stmt::For { .. } => false,
        _ => false,
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

fn ensure_scope(scopes: &mut Vec<HashSet<String>>) -> &mut HashSet<String> {
    if scopes.is_empty() {
        scopes.push(HashSet::new());
    }
    scopes.last_mut().expect("scope must exist")
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
        at_syntax::MatchPattern::Enum { bindings, .. } => bindings.clone(),
        at_syntax::MatchPattern::Binding { name, pattern, .. } => {
            let mut items = vec![name.clone()];
            items.extend(match_pattern_idents(pattern));
            items
        }
        at_syntax::MatchPattern::Wildcard(_) => Vec::new(),
    }
}

fn should_ignore_name(name: &str) -> bool {
    name.starts_with('_')
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

// ---------------------------------------------------------------------------
// Span helpers
// ---------------------------------------------------------------------------

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
    }
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

// ---------------------------------------------------------------------------
// Auto-fix support
// ---------------------------------------------------------------------------

/// Apply auto-fixes to source code
///
/// Takes source code and a list of lint errors with fixes, applies them in reverse order
/// (from end to start) to avoid offset shifting issues.
///
/// Returns the fixed source code.
pub fn apply_fixes(source: &str, errors: &[LintError]) -> String {
    let mut fixes: Vec<(Span, String)> = errors
        .iter()
        .filter_map(|err| {
            err.fix
                .as_ref()
                .map(|fix| (fix.span, fix.replacement.clone()))
        })
        .collect();

    fixes.sort_by(|a, b| b.0.end.cmp(&a.0.end));

    let mut result = source.to_string();

    for (span, replacement) in fixes {
        if span.end <= result.len() && span.start <= span.end {
            result.replace_range(span.start..span.end, &replacement);
        }
    }

    result
}

// ---------------------------------------------------------------------------
// Suppression infrastructure
// ---------------------------------------------------------------------------

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

    #[test]
    fn import_alias_matches_filename_warns_on_mismatch() {
        let source = r#"
import "./math.at" as m;
fn f() { m.add(); }
f();
"#;
        let module = parse_module(source).expect("parse module");
        let errors = lint_module_with_config(
            &module,
            Some("[rules]\nunused_function = \"off\"\nunknown_type_flow = \"off\"\n"),
        )
        .expect_err("expected lint errors");
        assert!(
            errors
                .iter()
                .any(|err| err.rule == Some(LintRule::ImportAliasMatchesFilename)
                    && err.message.contains("does not match filename")),
            "expected import_alias_matches_filename lint: {errors:?}"
        );
    }

    #[test]
    fn import_alias_matches_filename_passes_when_matching() {
        let source = r#"
import "./math.at" as math;
fn f() { math.add(); }
f();
"#;
        let module = parse_module(source).expect("parse module");
        let result = lint_module_with_config(
            &module,
            Some("[rules]\nunused_function = \"off\"\nunknown_type_flow = \"off\"\n"),
        );
        if let Err(errors) = result {
            assert!(
                !errors
                    .iter()
                    .any(|err| err.rule == Some(LintRule::ImportAliasMatchesFilename)),
                "should not warn when alias matches filename: {errors:?}"
            );
        }
    }

    #[test]
    fn import_alias_matches_filename_skips_std() {
        let source = r#"
import "std" as std;
fn f() { std.print(); }
f();
"#;
        let module = parse_module(source).expect("parse module");
        let result = lint_module_with_config(
            &module,
            Some("[rules]\nunused_function = \"off\"\nunknown_type_flow = \"off\"\n"),
        );
        if let Err(errors) = result {
            assert!(
                !errors
                    .iter()
                    .any(|err| err.rule == Some(LintRule::ImportAliasMatchesFilename)),
                "should not warn for std import: {errors:?}"
            );
        }
    }
}
