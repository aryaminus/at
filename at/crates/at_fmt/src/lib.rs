use std::collections::HashSet;

use at_syntax::{Comment, Expr, Function, MatchPattern, Module, Stmt};

struct CommentState {
    comments: Vec<Comment>,
    index: usize,
}

impl CommentState {
    fn new(mut comments: Vec<Comment>) -> Self {
        comments.sort_by_key(|comment| comment.span.start);
        Self { comments, index: 0 }
    }

    fn emit_until(&mut self, out: &mut String, limit: usize) {
        while self.index < self.comments.len() {
            let comment = &self.comments[self.index];
            if comment.span.start >= limit {
                break;
            }
            out.push_str(comment.text.trim_end());
            if !comment.text.ends_with('\n') {
                out.push('\n');
            }
            self.index += 1;
        }
    }

    fn emit_inline_between(&mut self, out: &mut String, start: usize, end: usize) {
        if start >= end {
            return;
        }
        let mut emitted_any = false;
        while self.index < self.comments.len() {
            let comment = &self.comments[self.index];
            if comment.span.start < start {
                self.index += 1;
                continue;
            }
            if comment.span.start >= end {
                break;
            }
            if out.ends_with('\n') {
                out.pop();
            }
            if !out.ends_with(' ') {
                out.push(' ');
            }
            out.push_str(comment.text.trim_end());
            emitted_any = true;
            self.index += 1;
        }
        if emitted_any {
            out.push('\n');
        }
    }
}

pub fn format_module(module: &Module) -> String {
    let mut out = String::new();
    let import_aliases = collect_import_aliases(module);
    let mut comment_state = CommentState::new(module.comments.clone());
    for func in &module.functions {
        comment_state.emit_until(&mut out, func.name.span.start);
        format_function(func, &mut out, 0, &import_aliases, &mut comment_state);
        out.push('\n');
    }
    for stmt in &module.stmts {
        comment_state.emit_until(&mut out, stmt_span(stmt));
        format_stmt(stmt, &mut out, 0, &mut comment_state);
    }
    comment_state.emit_until(&mut out, usize::MAX);
    out
}

fn format_function(
    func: &Function,
    out: &mut String,
    indent: usize,
    import_aliases: &HashSet<String>,
    comment_state: &mut CommentState,
) {
    indent_to(out, indent);
    if func.is_tool {
        out.push_str("tool ");
    }
    out.push_str("fn ");
    out.push_str(&func.name.name);
    out.push('(');
    format_params(&func.params, out);
    out.push(')');
    if let Some(return_ty) = &func.return_ty {
        out.push_str(" -> ");
        format_type_ref(return_ty, out);
    }
    let inferred = infer_needs(func, import_aliases);
    let needs = if func.needs.is_empty() {
        inferred
    } else {
        func.needs.iter().map(|ident| ident.name.clone()).collect()
    };
    if !needs.is_empty() {
        out.push(' ');
        out.push_str("needs {");
        out.push(' ');
        format_name_list(&needs, out);
        out.push(' ');
        out.push('}');
    }
    out.push(' ');
    out.push_str("{");
    out.push('\n');
    for stmt in &func.body {
        comment_state.emit_until(out, stmt_span(stmt));
        format_stmt(stmt, out, indent + 4, comment_state);
    }
    indent_to(out, indent);
    out.push_str("}\n");
}

fn stmt_span(stmt: &Stmt) -> usize {
    match stmt {
        Stmt::Import { alias, .. } => alias.span.start,
        Stmt::TypeAlias { name, .. } => name.span.start,
        Stmt::Enum { name, .. } => name.span.start,
        Stmt::Struct { name, .. } => name.span.start,
        Stmt::Let { name, .. } => name.span.start,
        Stmt::Using { name, .. } => name.span.start,
        Stmt::Set { name, .. } => name.span.start,
        Stmt::SetMember { field, .. } => field.span.start,
        Stmt::SetIndex { base, .. } => expr_span(base).unwrap_or(0),
        Stmt::While { while_span, .. } => while_span.start,
        Stmt::For { for_span, .. } => for_span.start,
        Stmt::Break { break_span } => break_span.start,
        Stmt::Continue { continue_span } => continue_span.start,
        Stmt::Expr(expr) => expr_span(expr).unwrap_or(0),
        Stmt::Return(expr) => expr.as_ref().and_then(expr_span).unwrap_or(0),
        Stmt::Block(stmts) => stmts
            .first()
            .and_then(|stmt| Some(stmt_span(stmt)))
            .unwrap_or(0),
        Stmt::Test { .. } => 0,
    }
}

fn expr_span(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Int(_, span) | Expr::Float(_, span) | Expr::String(_, span) | Expr::Bool(_, span) => {
            Some(span.start)
        }
        Expr::Ident(ident) => Some(ident.span.start),
        Expr::Unary { expr, .. } => expr_span(expr),
        Expr::Binary { left, .. } => expr_span(left),
        Expr::If { condition, .. } => expr_span(condition),
        Expr::Member { base, .. } => expr_span(base),
        Expr::Call { callee, .. } => expr_span(callee),
        Expr::Try(expr) => expr_span(expr),
        Expr::Match { match_span, .. } => Some(match_span.start),
        Expr::Block { block_span, .. } => Some(block_span.start),
        Expr::Array { array_span, .. } => Some(array_span.start),
        Expr::Index { index_span, .. } => Some(index_span.start),
        Expr::Tuple { tuple_span, .. } => Some(tuple_span.start),
        Expr::Range { range_span, .. } => Some(range_span.start),
        Expr::InterpolatedString { span, .. } => Some(span.start),
        Expr::Closure { span, .. } => Some(span.start),
        Expr::StructLiteral { span, .. } => Some(span.start),
        Expr::EnumLiteral { span, .. } => Some(span.start),
    }
}

fn expr_end(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Int(_, span) | Expr::Float(_, span) | Expr::String(_, span) | Expr::Bool(_, span) => {
            Some(span.end)
        }
        Expr::Ident(ident) => Some(ident.span.end),
        Expr::Unary { expr, .. } => expr_end(expr),
        Expr::Binary { right, .. } => expr_end(right),
        Expr::If {
            else_branch,
            then_branch,
            ..
        } => {
            if let Some(else_branch) = else_branch {
                expr_end(else_branch)
            } else {
                expr_end(then_branch)
            }
        }
        Expr::Member { name, .. } => Some(name.span.end),
        Expr::Call { callee, args } => {
            if let Some(last) = args.last() {
                expr_end(last)
            } else {
                expr_end(callee)
            }
        }
        Expr::Try(expr) => expr_end(expr),
        Expr::Match { match_span, .. } => Some(match_span.end),
        Expr::Block { block_span, .. } => Some(block_span.end),
        Expr::Array { array_span, .. } => Some(array_span.end),
        Expr::Index { index_span, .. } => Some(index_span.end),
        Expr::Tuple { tuple_span, .. } => Some(tuple_span.end),
        Expr::Range { range_span, .. } => Some(range_span.end),
        Expr::InterpolatedString { span, .. } => Some(span.end),
        Expr::Closure { span, .. } => Some(span.end),
        Expr::StructLiteral { span, .. } => Some(span.end),
        Expr::EnumLiteral { span, .. } => Some(span.end),
    }
}

fn format_stmt(stmt: &Stmt, out: &mut String, indent: usize, comment_state: &mut CommentState) {
    match stmt {
        Stmt::Import { path, alias } => {
            indent_to(out, indent);
            out.push_str("import \"");
            out.push_str(path);
            out.push_str("\" as ");
            out.push_str(&alias.name);
            out.push_str(";\n");
        }
        Stmt::TypeAlias { name, ty } => {
            indent_to(out, indent);
            out.push_str("type ");
            out.push_str(&name.name);
            out.push_str(" = ");
            format_type_ref(ty, out);
            out.push_str(";\n");
        }
        Stmt::Enum { name, variants } => {
            indent_to(out, indent);
            out.push_str("enum ");
            out.push_str(&name.name);
            out.push_str(" {\n");
            for variant in variants {
                indent_to(out, indent + 4);
                out.push_str(&variant.name.name);
                if let Some(payload) = &variant.payload {
                    out.push('(');
                    format_type_ref(payload, out);
                    out.push(')');
                }
                out.push_str(",\n");
            }
            indent_to(out, indent);
            out.push_str("}\n");
        }
        Stmt::Struct { name, fields } => {
            indent_to(out, indent);
            out.push_str("struct ");
            out.push_str(&name.name);
            out.push_str(" {\n");
            for field in fields {
                indent_to(out, indent + 4);
                out.push_str(&field.name.name);
                out.push_str(": ");
                format_type_ref(&field.ty, out);
                out.push_str(",\n");
            }
            indent_to(out, indent);
            out.push_str("}\n");
        }
        Stmt::Let { name, ty, value } => {
            indent_to(out, indent);
            out.push_str("let ");
            out.push_str(&name.name);
            if let Some(ty) = ty {
                out.push_str(": ");
                format_type_ref(ty, out);
            }
            out.push_str(" = ");
            format_expr_with_indent(value, out, indent, comment_state);
            if let Some(value_span) = expr_span(value) {
                comment_state.emit_inline_between(out, value_span, value_span + 1);
            }
            out.push_str(";\n");
        }
        Stmt::Using { name, ty, value } => {
            indent_to(out, indent);
            out.push_str("using ");
            out.push_str(&name.name);
            if let Some(ty) = ty {
                out.push_str(": ");
                format_type_ref(ty, out);
            }
            out.push_str(" = ");
            format_expr_with_indent(value, out, indent, comment_state);
            if let Some(value_span) = expr_span(value) {
                comment_state.emit_inline_between(out, value_span, value_span + 1);
            }
            out.push_str(";\n");
        }
        Stmt::Set { name, value } => {
            indent_to(out, indent);
            out.push_str("set ");
            out.push_str(&name.name);
            out.push_str(" = ");
            format_expr_with_indent(value, out, indent, comment_state);
            if let Some(value_span) = expr_span(value) {
                comment_state.emit_inline_between(out, value_span, value_span + 1);
            }
            out.push_str(";\n");
        }
        Stmt::SetMember { base, field, value } => {
            indent_to(out, indent);
            out.push_str("set ");
            format_expr_with_indent(base, out, indent, comment_state);
            out.push('.');
            out.push_str(&field.name);
            out.push_str(" = ");
            format_expr_with_indent(value, out, indent, comment_state);
            if let Some(value_span) = expr_span(value) {
                comment_state.emit_inline_between(out, value_span, value_span + 1);
            }
            out.push_str(";\n");
        }
        Stmt::SetIndex { base, index, value } => {
            indent_to(out, indent);
            out.push_str("set ");
            format_expr_with_indent(base, out, indent, comment_state);
            out.push('[');
            format_expr_with_indent(index, out, indent, comment_state);
            out.push(']');
            out.push_str(" = ");
            format_expr_with_indent(value, out, indent, comment_state);
            if let Some(value_span) = expr_span(value) {
                comment_state.emit_inline_between(out, value_span, value_span + 1);
            }
            out.push_str(";\n");
        }
        Stmt::While {
            condition,
            body,
            while_span,
        } => {
            indent_to(out, indent);
            out.push_str("while ");
            format_expr_with_indent(condition, out, indent, comment_state);
            if let Some(condition_span) = expr_span(condition) {
                comment_state.emit_inline_between(out, condition_span, condition_span + 1);
            }
            out.push_str(" {\n");
            for stmt in body {
                comment_state.emit_until(out, stmt_span(stmt));
                format_stmt(stmt, out, indent + 4, comment_state);
            }
            indent_to(out, indent);
            out.push_str("}");
            comment_state.emit_inline_between(out, while_span.start, while_span.end);
            if !out.ends_with('\n') {
                out.push('\n');
            }
        }
        Stmt::For {
            item,
            iter,
            body,
            for_span,
        } => {
            indent_to(out, indent);
            out.push_str("for ");
            out.push_str(&item.name);
            out.push_str(" in ");
            format_expr_with_indent(iter, out, indent, comment_state);
            if let Some(iter_span) = expr_span(iter) {
                comment_state.emit_inline_between(out, iter_span, iter_span + 1);
            }
            out.push_str(" {\n");
            for stmt in body {
                comment_state.emit_until(out, stmt_span(stmt));
                format_stmt(stmt, out, indent + 4, comment_state);
            }
            indent_to(out, indent);
            out.push_str("}");
            comment_state.emit_inline_between(out, for_span.start, for_span.end);
            if !out.ends_with('\n') {
                out.push('\n');
            }
        }
        Stmt::Break { .. } => {
            indent_to(out, indent);
            out.push_str("break;\n");
        }
        Stmt::Continue { .. } => {
            indent_to(out, indent);
            out.push_str("continue;\n");
        }
        Stmt::Expr(expr) => {
            indent_to(out, indent);
            format_expr_with_indent(expr, out, indent, comment_state);
            if let Some(expr_span) = expr_span(expr) {
                comment_state.emit_inline_between(out, expr_span, expr_span + 1);
            }
            out.push_str(";\n");
        }
        Stmt::Return(expr) => {
            indent_to(out, indent);
            out.push_str("return");
            if let Some(expr) = expr {
                out.push(' ');
                format_expr_with_indent(expr, out, indent, comment_state);
                if let Some(expr_span) = expr_span(expr) {
                    comment_state.emit_inline_between(out, expr_span, expr_span + 1);
                }
            }
            out.push_str(";\n");
        }
        Stmt::Block(stmts) => {
            indent_to(out, indent);
            out.push_str("{\n");
            for stmt in stmts {
                comment_state.emit_until(out, stmt_span(stmt));
                format_stmt(stmt, out, indent + 4, comment_state);
            }
            indent_to(out, indent);
            out.push('}');
            if !stmts.is_empty() {
                let last = stmt_span(stmts.last().unwrap());
                comment_state.emit_inline_between(out, last, last + 1);
            }
            if !out.ends_with('\n') {
                out.push('\n');
            }
        }
        Stmt::Test { name, body } => {
            indent_to(out, indent);
            out.push_str("test \"");
            out.push_str(name);
            out.push_str("\" {\n");
            for stmt in body {
                comment_state.emit_until(out, stmt_span(stmt));
                format_stmt(stmt, out, indent + 4, comment_state);
            }
            indent_to(out, indent);
            out.push('}');
            if !body.is_empty() {
                let last = stmt_span(body.last().unwrap());
                comment_state.emit_inline_between(out, last, last + 1);
            }
            if !out.ends_with('\n') {
                out.push('\n');
            }
        }
    }
}

fn format_expr_with_indent(
    expr: &Expr,
    out: &mut String,
    indent: usize,
    comment_state: &mut CommentState,
) {
    format_expr_prec_indent(expr, out, 0, indent, comment_state);
}

fn format_expr_prec_indent(
    expr: &Expr,
    out: &mut String,
    parent_prec: u8,
    indent: usize,
    comment_state: &mut CommentState,
) {
    match expr {
        Expr::Int(value, _) => {
            out.push_str(&value.to_string());
        }
        Expr::Float(value, _) => {
            out.push_str(&value.to_string());
        }
        Expr::String(value, _) => {
            out.push('"');
            for ch in value.chars() {
                match ch {
                    '\n' => out.push_str("\\n"),
                    '\t' => out.push_str("\\t"),
                    '\r' => out.push_str("\\r"),
                    '\0' => out.push_str("\\0"),
                    '"' => out.push_str("\\\""),
                    '\\' => out.push_str("\\\\"),
                    other => out.push(other),
                }
            }
            out.push('"');
        }
        Expr::Bool(value, _) => {
            if *value {
                out.push_str("true");
            } else {
                out.push_str("false");
            }
        }
        Expr::Ident(ident) => {
            out.push_str(&ident.name);
        }
        Expr::Unary { op, expr, .. } => {
            let prec = unary_prec();
            let wrap = prec < parent_prec;
            if wrap {
                out.push('(');
            }
            out.push_str(unary_op_str(*op));
            let needs = needs_paren(expr, prec, true);
            if needs {
                out.push('(');
            }
            format_expr_prec_indent(expr, out, prec, indent, comment_state);
            if needs {
                out.push(')');
            }
            if wrap {
                out.push(')');
            }
        }
        Expr::Binary {
            left, op, right, ..
        } => {
            let prec = binary_prec(*op);
            let wrap = prec < parent_prec;
            if wrap {
                out.push('(');
            }
            let left_needs_paren = needs_paren(left, prec, false);
            if left_needs_paren {
                out.push('(');
            }
            format_expr_prec_indent(left, out, prec, indent, comment_state);
            if left_needs_paren {
                out.push(')');
            }
            out.push(' ');
            out.push_str(binary_op_str(*op));
            out.push(' ');
            let right_needs_paren = needs_paren(right, prec, true);
            if right_needs_paren {
                out.push('(');
            }
            format_expr_prec_indent(right, out, prec, indent, comment_state);
            if right_needs_paren {
                out.push(')');
            }
            if wrap {
                out.push(')');
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let wrap = parent_prec > 0;
            if wrap {
                out.push('(');
            }
            out.push_str("if ");
            format_expr_prec_indent(condition, out, 0, indent, comment_state);
            if let Some(condition_span) = expr_span(condition) {
                comment_state.emit_inline_between(out, condition_span, condition_span + 1);
            }
            out.push(' ');
            format_expr_prec_indent(then_branch, out, 0, indent, comment_state);
            if let Some(else_expr) = else_branch {
                out.push_str(" else ");
                format_expr_prec_indent(else_expr, out, 0, indent, comment_state);
            }
            if wrap {
                out.push(')');
            }
        }
        Expr::Member { base, name } => {
            format_expr_prec_indent(base, out, 0, indent, comment_state);
            out.push('.');
            out.push_str(&name.name);
        }
        Expr::Call { callee, args } => {
            format_expr_prec_indent(callee, out, 0, indent, comment_state);
            out.push('(');
            for (idx, arg) in args.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                format_expr_prec_indent(arg, out, 0, indent, comment_state);
                if let Some(arg_span) = expr_end(arg) {
                    comment_state.emit_inline_between(out, arg_span, arg_span + 1);
                }
            }
            out.push(')');
            if let Some(call_end) = expr_end(expr) {
                comment_state.emit_inline_between(out, call_end, call_end + 1);
            }
        }
        Expr::Try(expr) => {
            format_expr_prec_indent(expr, out, 0, indent, comment_state);
            out.push('?');
        }
        Expr::Match { value, arms, .. } => {
            let wrap = parent_prec > 0;
            if wrap {
                out.push('(');
            }
            out.push_str("match ");
            format_expr_prec_indent(value, out, 0, indent, comment_state);
            if let Some(value_span) = expr_span(value) {
                comment_state.emit_inline_between(out, value_span, value_span + 1);
            }
            out.push_str(" {\n");
            for arm in arms {
                indent_to(out, indent + 4);
                format_match_pattern(&arm.pattern, out);
                if let Some(guard) = &arm.guard {
                    out.push_str(" if ");
                    format_expr_prec_indent(guard, out, 0, indent + 4, comment_state);
                    if let Some(guard_span) = expr_span(guard) {
                        comment_state.emit_inline_between(out, guard_span, guard_span + 1);
                    }
                }
                out.push_str(" => ");
                format_expr_prec_indent(&arm.body, out, 0, indent + 4, comment_state);
                out.push_str(",\n");
            }
            indent_to(out, indent);
            out.push('}');
            if wrap {
                out.push(')');
            }
        }
        Expr::Block { stmts, tail, .. } => {
            out.push_str("{\n");
            for stmt in stmts {
                comment_state.emit_until(out, stmt_span(stmt));
                format_stmt(stmt, out, indent + 4, comment_state);
            }
            if let Some(expr) = tail {
                indent_to(out, indent + 4);
                format_expr_prec_indent(expr, out, 0, indent + 4, comment_state);
                out.push('\n');
            }
            indent_to(out, indent);
            out.push('}');
        }
        Expr::Array { items, .. } => {
            out.push('[');
            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                format_expr_prec_indent(item, out, 0, indent, comment_state);
                if let Some(item_span) = expr_span(item) {
                    comment_state.emit_inline_between(out, item_span, item_span + 1);
                }
            }
            out.push(']');
        }
        Expr::Index { base, index, .. } => {
            format_expr_prec_indent(base, out, 0, indent, comment_state);
            out.push('[');
            format_expr_prec_indent(index, out, 0, indent, comment_state);
            out.push(']');
            if let Some(index_end) = expr_end(expr) {
                comment_state.emit_inline_between(out, index_end, index_end + 1);
            }
        }
        Expr::Tuple { items, .. } => {
            out.push('(');
            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                format_expr_prec_indent(item, out, 0, indent, comment_state);
                if let Some(item_span) = expr_span(item) {
                    comment_state.emit_inline_between(out, item_span, item_span + 1);
                }
            }
            out.push(')');
        }
        Expr::Range {
            start,
            end,
            inclusive,
            ..
        } => {
            format_expr_prec_indent(start, out, 0, indent, comment_state);
            if *inclusive {
                out.push_str("..=");
            } else {
                out.push_str("..");
            }
            format_expr_prec_indent(end, out, 0, indent, comment_state);
        }
        Expr::InterpolatedString { parts, .. } => {
            out.push('"');
            for part in parts {
                match part {
                    at_syntax::InterpPart::String(s) => out.push_str(s),
                    at_syntax::InterpPart::Expr(expr) => {
                        out.push('{');
                        format_expr_prec_indent(expr, out, 0, indent, comment_state);
                        out.push('}');
                    }
                }
            }
            out.push('"');
        }
        Expr::StructLiteral { name, fields, .. } => {
            out.push_str(&name.name);
            out.push_str(" { ");
            for (idx, field) in fields.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                out.push_str(&field.name.name);
                out.push_str(": ");
                format_expr_prec_indent(&field.value, out, 0, indent, comment_state);
                if let Some(value_span) = expr_span(&field.value) {
                    comment_state.emit_inline_between(out, value_span, value_span + 1);
                }
            }
            out.push_str(" }");
        }
        Expr::EnumLiteral {
            name,
            variant,
            payload,
            ..
        } => {
            out.push_str(&name.name);
            out.push_str("::");
            out.push_str(&variant.name);
            if let Some(expr) = payload {
                out.push('(');
                format_expr_prec_indent(expr, out, 0, indent, comment_state);
                if let Some(expr_span) = expr_span(expr) {
                    comment_state.emit_inline_between(out, expr_span, expr_span + 1);
                }
                out.push(')');
            }
        }
        Expr::Closure { params, body, .. } => {
            out.push('|');
            for (idx, param) in params.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                out.push_str(&param.name);
            }
            out.push_str("| ");
            format_expr_prec_indent(body, out, 0, indent, comment_state);
            if let Some(body_span) = expr_span(body) {
                comment_state.emit_inline_between(out, body_span, body_span + 1);
            }
        }
    }
}

fn binary_prec(op: at_syntax::BinaryOp) -> u8 {
    match op {
        at_syntax::BinaryOp::Or => 1,
        at_syntax::BinaryOp::And => 2,
        at_syntax::BinaryOp::BitOr => 3,
        at_syntax::BinaryOp::BitXor => 4,
        at_syntax::BinaryOp::BitAnd => 5,
        at_syntax::BinaryOp::Shl | at_syntax::BinaryOp::Shr => 6,
        at_syntax::BinaryOp::Eq | at_syntax::BinaryOp::Neq => 7,
        at_syntax::BinaryOp::Lt
        | at_syntax::BinaryOp::Lte
        | at_syntax::BinaryOp::Gt
        | at_syntax::BinaryOp::Gte => 8,
        at_syntax::BinaryOp::Add | at_syntax::BinaryOp::Sub => 9,
        at_syntax::BinaryOp::Mul | at_syntax::BinaryOp::Div | at_syntax::BinaryOp::Mod => 10,
    }
}

fn unary_prec() -> u8 {
    11
}

fn binary_op_str(op: at_syntax::BinaryOp) -> &'static str {
    match op {
        at_syntax::BinaryOp::Or => "||",
        at_syntax::BinaryOp::And => "&&",
        at_syntax::BinaryOp::BitOr => "|",
        at_syntax::BinaryOp::BitXor => "^",
        at_syntax::BinaryOp::BitAnd => "&",
        at_syntax::BinaryOp::Shl => "<<",
        at_syntax::BinaryOp::Shr => ">>",
        at_syntax::BinaryOp::Add => "+",
        at_syntax::BinaryOp::Sub => "-",
        at_syntax::BinaryOp::Mul => "*",
        at_syntax::BinaryOp::Div => "/",
        at_syntax::BinaryOp::Mod => "%",
        at_syntax::BinaryOp::Eq => "==",
        at_syntax::BinaryOp::Neq => "!=",
        at_syntax::BinaryOp::Lt => "<",
        at_syntax::BinaryOp::Lte => "<=",
        at_syntax::BinaryOp::Gt => ">",
        at_syntax::BinaryOp::Gte => ">=",
    }
}

fn unary_op_str(op: at_syntax::UnaryOp) -> &'static str {
    match op {
        at_syntax::UnaryOp::Neg => "-",
        at_syntax::UnaryOp::Not => "!",
    }
}

fn needs_paren(expr: &Expr, parent_prec: u8, is_right: bool) -> bool {
    match expr {
        Expr::Unary { .. } => parent_prec > unary_prec(),
        Expr::Binary { op, .. } => {
            let child_prec = binary_prec(*op);
            if is_right {
                child_prec <= parent_prec
            } else {
                child_prec < parent_prec
            }
        }
        Expr::If { .. } | Expr::Match { .. } | Expr::Block { .. } => parent_prec > 0,
        _ => false,
    }
}

fn format_params(params: &[at_syntax::Param], out: &mut String) {
    for (idx, param) in params.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&param.name.name);
        if let Some(ty) = &param.ty {
            out.push_str(": ");
            format_type_ref(ty, out);
        }
    }
}

fn format_type_ref(ty: &at_syntax::TypeRef, out: &mut String) {
    match ty {
        at_syntax::TypeRef::Named { name, args } => {
            out.push_str(&name.name);
            if !args.is_empty() {
                out.push('<');
                for (idx, arg) in args.iter().enumerate() {
                    if idx > 0 {
                        out.push_str(", ");
                    }
                    format_type_ref(arg, out);
                }
                out.push('>');
            }
        }
        at_syntax::TypeRef::Function {
            params, return_ty, ..
        } => {
            out.push_str("fn(");
            for (idx, param) in params.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                format_type_ref(param, out);
            }
            out.push_str(") -> ");
            format_type_ref(return_ty, out);
        }
    }
}

fn format_name_list(names: &[String], out: &mut String) {
    for (idx, name) in names.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(name);
    }
}

fn infer_needs(func: &Function, import_aliases: &HashSet<String>) -> Vec<String> {
    let mut needs = Vec::new();
    for stmt in &func.body {
        collect_needs_stmt(stmt, &mut needs, import_aliases);
    }
    needs.sort();
    needs.dedup();
    needs
}

fn collect_needs_stmt(stmt: &Stmt, needs: &mut Vec<String>, import_aliases: &HashSet<String>) {
    match stmt {
        Stmt::Import { .. } | Stmt::Struct { .. } | Stmt::TypeAlias { .. } | Stmt::Enum { .. } => {}
        Stmt::Let { value, .. } | Stmt::Using { value, .. } | Stmt::Expr(value) => {
            collect_needs_expr(value, needs, import_aliases);
        }
        Stmt::Set { value, .. } | Stmt::SetMember { value, .. } | Stmt::SetIndex { value, .. } => {
            collect_needs_expr(value, needs, import_aliases);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_needs_expr(condition, needs, import_aliases);
            for stmt in body {
                collect_needs_stmt(stmt, needs, import_aliases);
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_needs_expr(iter, needs, import_aliases);
            for stmt in body {
                collect_needs_stmt(stmt, needs, import_aliases);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return(value) => {
            if let Some(expr) = value {
                collect_needs_expr(expr, needs, import_aliases);
            }
        }
        Stmt::Block(stmts) | Stmt::Test { body: stmts, .. } => {
            for stmt in stmts {
                collect_needs_stmt(stmt, needs, import_aliases);
            }
        }
    }
}

fn collect_needs_expr(expr: &Expr, needs: &mut Vec<String>, import_aliases: &HashSet<String>) {
    match expr {
        Expr::Ident(ident) => maybe_add_need(&ident.name, needs, import_aliases),
        Expr::Unary { expr, .. } => {
            collect_needs_expr(expr, needs, import_aliases);
        }
        Expr::Binary { left, right, .. } => {
            collect_needs_expr(left, needs, import_aliases);
            collect_needs_expr(right, needs, import_aliases);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_needs_expr(condition, needs, import_aliases);
            collect_needs_expr(then_branch, needs, import_aliases);
            if let Some(else_expr) = else_branch {
                collect_needs_expr(else_expr, needs, import_aliases);
            }
        }
        Expr::Member { base, .. } => {
            if let Expr::Ident(ident) = base.as_ref() {
                maybe_add_need(&ident.name, needs, import_aliases);
            } else {
                collect_needs_expr(base, needs, import_aliases);
            }
        }
        Expr::Call { callee, args } => {
            collect_needs_expr(callee, needs, import_aliases);
            for arg in args {
                collect_needs_expr(arg, needs, import_aliases);
            }
        }
        Expr::Tuple { items, .. } => {
            for item in items {
                collect_needs_expr(item, needs, import_aliases);
            }
        }
        Expr::Range { start, end, .. } => {
            collect_needs_expr(start, needs, import_aliases);
            collect_needs_expr(end, needs, import_aliases);
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let at_syntax::InterpPart::Expr(expr) = part {
                    collect_needs_expr(expr, needs, import_aliases);
                }
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                collect_needs_expr(&field.value, needs, import_aliases);
            }
        }
        Expr::EnumLiteral { payload, .. } => {
            if let Some(expr) = payload {
                collect_needs_expr(expr, needs, import_aliases);
            }
        }
        Expr::Closure { body, .. } => {
            collect_needs_expr(body, needs, import_aliases);
        }
        Expr::Try(expr) => {
            collect_needs_expr(expr, needs, import_aliases);
        }
        Expr::Match { value, arms, .. } => {
            collect_needs_expr(value, needs, import_aliases);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_needs_expr(guard, needs, import_aliases);
                }
                collect_needs_expr(&arm.body, needs, import_aliases);
            }
        }
        _ => {}
    }
}

fn format_match_pattern(pattern: &MatchPattern, out: &mut String) {
    match pattern {
        MatchPattern::Int(value) => {
            out.push_str(&value.to_string());
        }
        MatchPattern::ResultOk(ident) => {
            out.push_str("ok(");
            out.push_str(&ident.name);
            out.push(')');
        }
        MatchPattern::ResultErr(ident) => {
            out.push_str("err(");
            out.push_str(&ident.name);
            out.push(')');
        }
        MatchPattern::OptionSome(ident) => {
            out.push_str("some(");
            out.push_str(&ident.name);
            out.push(')');
        }
        MatchPattern::OptionNone => {
            out.push_str("none");
        }
        MatchPattern::Struct { name, fields } => {
            out.push_str(&name.name);
            out.push_str(" { ");
            for (idx, field) in fields.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                out.push_str(&field.name.name);
                if let Some(binding) = &field.binding {
                    out.push_str(": ");
                    out.push_str(&binding.name);
                }
            }
            out.push_str(" }");
        }
        MatchPattern::Enum {
            name,
            variant,
            binding,
        } => {
            out.push_str(&name.name);
            out.push_str("::");
            out.push_str(&variant.name);
            if let Some(binding) = binding {
                out.push('(');
                out.push_str(&binding.name);
                out.push(')');
            }
        }
        MatchPattern::Wildcard => {
            out.push('_');
        }
    }
}

fn maybe_add_need(name: &str, needs: &mut Vec<String>, import_aliases: &HashSet<String>) {
    match name {
        "time" | "rng" => needs.push(name.to_string()),
        _ => {
            if import_aliases.contains(name) {
                needs.push(name.to_string());
            }
        }
    }
}

fn collect_import_aliases(module: &Module) -> HashSet<String> {
    let mut aliases = HashSet::new();
    for stmt in &module.stmts {
        if let Stmt::Import { alias, .. } = stmt {
            aliases.insert(alias.name.clone());
        }
    }
    aliases
}

fn indent_to(out: &mut String, indent: usize) {
    for _ in 0..indent {
        out.push(' ');
    }
}

#[cfg(test)]
mod tests {
    use super::format_module;
    use at_parser::parse_module;

    #[test]
    fn formats_arrays_and_helpers() {
        let source = r#"
fn f(){
let values=[1,2,3];
let first=values[0];
let next=append(values,4);
let has=contains(values,2);
let mid=slice(next,1,3);
let value=true&&false||true;
for item in values{print(item);}
return first;
}
"#;
        let module = parse_module(source).expect("parse module");
        let formatted = format_module(&module);
        let expected = r#"fn f() {
    let values = [1, 2, 3];
    let first = values[0];
    let next = append(values, 4);
    let has = contains(values, 2);
    let mid = slice(next, 1, 3);
    let value = true && false || true;
    for item in values {
        print(item);
    }
    return first;
}

"#;
        assert_eq!(formatted, expected);
    }

    #[test]
    fn formats_match_indent() {
        let source = r#"
fn f(){
let x=none();
return match x{some(v)=>v,none=>0,};
}
"#;
        let module = parse_module(source).expect("parse module");
        let formatted = format_module(&module);
        let expected = r#"fn f() {
    let x = none();
    return match x {
        some(v) => v,
        none => 0,
    };
}

"#;
        assert_eq!(formatted, expected);
    }
}
