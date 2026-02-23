use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use at_syntax::{Expr, Function, Ident, Module, Span, Stmt};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(Rc<String>),
    Bool(bool),
    Array(Rc<Vec<Value>>),
    Tuple(Rc<Vec<Value>>),
    Map(Rc<Vec<(Value, Value)>>),
    Struct(Rc<BTreeMap<String, Value>>),
    Enum {
        name: String,
        variant: String,
        payload: Option<Rc<Value>>,
    },
    Option(Option<Rc<Value>>),
    Result(Result<Rc<Value>, Rc<Value>>),
    Closure(Rc<ClosureValue>),
    Future(Rc<FutureValue>),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClosureValue {
    func_id: usize,
    captures: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FutureValue {
    func_id: usize,
    captures: Vec<Value>,
    args: Vec<Value>,
    capabilities: HashSet<String>,
}

pub fn format_value(value: &Value) -> String {
    match value {
        Value::Int(value) => value.to_string(),
        Value::Float(value) => value.to_string(),
        Value::String(value) => format!("\"{}\"", value),
        Value::Bool(value) => value.to_string(),
        Value::Unit => "unit".to_string(),
        Value::Closure(_) => "<closure>".to_string(),
        Value::Future(_) => "<future>".to_string(),
        Value::Array(items) => format!(
            "[{}]",
            items
                .iter()
                .map(format_value)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Map(entries) => {
            let items = entries
                .iter()
                .map(|(key, value)| format!("{}: {}", format_value(key), format_value(value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("map {{{items}}}")
        }
        Value::Struct(fields) => {
            let mut items = Vec::new();
            for (key, value) in fields.iter() {
                items.push(format!("{key}: {}", format_value(value)));
            }
            format!("{{{}}}", items.join(", "))
        }
        Value::Enum {
            name,
            variant,
            payload,
        } => {
            if let Some(payload) = payload {
                format!("{name}::{variant}({})", format_value(payload))
            } else {
                format!("{name}::{variant}")
            }
        }
        Value::Tuple(items) => format!(
            "({})",
            items
                .iter()
                .map(format_value)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Option(Some(inner)) => format!("some({})", format_value(inner)),
        Value::Option(None) => "none".to_string(),
        Value::Result(Ok(inner)) => format!("ok({})", format_value(inner)),
        Value::Result(Err(inner)) => format!("err({})", format_value(inner)),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Op {
    Const(usize),
    LoadLocal(usize),
    StoreLocal(usize),
    GrantCapability(String),
    PushCapabilityScope,
    PopCapabilityScope,
    Call(usize, usize),
    CallAsync(usize, usize),
    Builtin(Builtin),
    Assert,
    Try,
    Throw,
    Defer,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Array(usize),
    ArraySpread(usize),
    Tuple(usize),
    Index,
    StoreIndex,
    StoreMember(String),
    Struct(Vec<String>),
    GetMember(String),
    Enum {
        name: String,
        variant: String,
        has_payload: bool,
    },
    Closure(usize, usize),
    CallValue(usize),
    Await,
    Range(bool),
    Map(usize),
    MapSpread(usize),
    IsType(TypeCheck),
    Cast(TypeCheck),
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Neg,
    Not,
    MatchResultOk(usize),
    MatchResultErr(usize),
    MatchOptionSome(usize),
    MatchOptionNone(usize),
    MatchStruct(Vec<String>, usize),
    MatchEnum {
        name: String,
        variant: String,
        has_payload: bool,
        target: usize,
    },
    MatchInt(i64, usize),
    MatchBool(bool, usize),
    MatchString(String, usize),
    MatchTuple(usize, usize),
    MatchFail,
    JumpIfFalse(usize),
    Jump(usize),
    Return,
    Pop,
    Halt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Builtin {
    TimeNow,
    TimeFixed,
    RngDeterministic,
    RngUuid,
    AssertEq,
    OptionSome,
    OptionNone,
    ResultOk,
    ResultErr,
    IsSome,
    IsNone,
    IsOk,
    IsErr,
    Print,
    Len,
    Append,
    Contains,
    Slice,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeCheck {
    Int,
    Float,
    String,
    Bool,
    Unit,
    Array,
    Tuple,
    Map,
    Struct,
    Option,
    Result,
    Enum(String),
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
    pub spans: Vec<Option<Span>>,
}

impl Chunk {
    fn push(&mut self, op: Op, span: Option<Span>) {
        self.code.push(op);
        self.spans.push(span);
    }

    fn add_const(&mut self, value: Value) -> usize {
        if let Some(index) = self
            .constants
            .iter()
            .position(|existing| existing == &value)
        {
            return index;
        }
        let index = self.constants.len();
        self.constants.push(value);
        index
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionChunk {
    pub name: String,
    pub params: usize,
    pub captures: usize,
    pub locals: usize,
    pub needs: Vec<String>,
    pub is_async: bool,
    pub chunk: Chunk,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Program {
    pub functions: Vec<FunctionChunk>,
    pub main: Chunk,
    pub main_locals: usize,
}

impl Program {
    pub fn to_bytes(&self) -> Result<Vec<u8>, bincode::Error> {
        bincode::serialize(self)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, bincode::Error> {
        bincode::deserialize(bytes)
    }
}

#[derive(Debug, Default)]
struct LoopContext {
    breaks: Vec<usize>,
    continues: Vec<usize>,
}

#[derive(Clone)]
enum SetSegment {
    Member(Ident),
    Index { expr: Expr, span: Option<Span> },
}

impl LoopContext {
    fn new() -> Self {
        Self {
            breaks: Vec::new(),
            continues: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum VmError {
    StackUnderflow,
    Compile {
        message: String,
        span: Option<Span>,
    },
    Runtime {
        message: String,
        span: Option<Span>,
        stack: Option<Vec<StackFrame>>,
    },
    ExecutionLimit {
        message: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StackFrame {
    pub name: String,
    pub span: Option<Span>,
}

pub struct Compiler {
    scopes: Vec<HashMap<String, usize>>,
    const_scopes: Vec<HashSet<String>>,
    next_local: usize,
    functions: HashMap<String, usize>,
    function_arity: HashMap<String, usize>,
    function_needs: HashMap<String, Vec<String>>,
    function_async: HashMap<String, bool>,
    structs: HashSet<String>,
    enums: HashSet<String>,
    current_function: Option<String>,
    loop_stack: Vec<LoopContext>,
    synthetic_id: usize,
    base_function_count: usize,
    closure_chunks: Vec<FunctionChunk>,
}

impl Compiler {
    fn compile_set_target(
        &mut self,
        base: &Expr,
        segments: Vec<SetSegment>,
        value: &Expr,
        chunk: &mut Chunk,
    ) -> Result<(), VmError> {
        let mut path = Vec::new();
        let mut current = base;
        loop {
            match current {
                Expr::Member { base, name, .. } => {
                    path.push(SetSegment::Member(name.clone()));
                    current = base;
                }
                Expr::Index {
                    base,
                    index,
                    index_span,
                    ..
                } => {
                    path.push(SetSegment::Index {
                        expr: (**index).clone(),
                        span: Some(*index_span),
                    });
                    current = base;
                }
                _ => break,
            }
        }

        let root_ident = match current {
            Expr::Ident(ident) => ident,
            _ => {
                return Err(compile_error(
                    "set target must start with identifier".to_string(),
                    expr_span(current),
                ))
            }
        };

        let slot = self.resolve_local(&root_ident.name).ok_or_else(|| {
            compile_error(
                format!("unknown identifier: {}", root_ident.name),
                Some(root_ident.span),
            )
        })?;

        path.reverse();
        let mut all_segments = path;
        all_segments.extend(segments);

        let mut temp_slots = Vec::with_capacity(all_segments.len());
        for _ in 0..all_segments.len() {
            let temp_name = self.next_synthetic_name("set_base");
            let temp_slot = self.bind_local_checked(&temp_name, root_ident.span)?;
            temp_slots.push(temp_slot);
        }
        let mut index_slots: Vec<Option<usize>> = vec![None; all_segments.len()];
        let updated_slot = {
            let temp_name = self.next_synthetic_name("set_value");
            self.bind_local_checked(&temp_name, root_ident.span)?
        };

        chunk.push(Op::LoadLocal(slot), Some(root_ident.span));
        chunk.push(Op::StoreLocal(temp_slots[0]), Some(root_ident.span));

        for (idx, segment) in all_segments
            .iter()
            .enumerate()
            .take(all_segments.len().saturating_sub(1))
        {
            match segment {
                SetSegment::Member(name) => {
                    chunk.push(Op::LoadLocal(temp_slots[idx]), Some(root_ident.span));
                    chunk.push(Op::GetMember(name.name.clone()), Some(name.span));
                }
                SetSegment::Index { expr, span } => {
                    let index_slot = match index_slots[idx] {
                        Some(slot) => slot,
                        None => {
                            let temp_name = self.next_synthetic_name("set_index");
                            let temp_slot = self.bind_local_checked(&temp_name, root_ident.span)?;
                            self.compile_expr(expr, chunk)?;
                            chunk.push(Op::StoreLocal(temp_slot), *span);
                            index_slots[idx] = Some(temp_slot);
                            temp_slot
                        }
                    };
                    chunk.push(Op::LoadLocal(temp_slots[idx]), Some(root_ident.span));
                    chunk.push(Op::LoadLocal(index_slot), *span);
                    chunk.push(Op::Index, *span);
                }
            }
            chunk.push(Op::StoreLocal(temp_slots[idx + 1]), Some(root_ident.span));
        }

        let last_segment = all_segments.last().ok_or_else(|| {
            compile_error(
                "set target is missing member/index".to_string(),
                Some(root_ident.span),
            )
        })?;

        let last_idx = all_segments.len() - 1;
        match last_segment {
            SetSegment::Member(name) => {
                chunk.push(Op::LoadLocal(temp_slots[last_idx]), Some(root_ident.span));
                self.compile_expr(value, chunk)?;
                chunk.push(Op::StoreMember(name.name.clone()), Some(name.span));
            }
            SetSegment::Index { expr, span } => {
                let index_slot = match index_slots[last_idx] {
                    Some(slot) => slot,
                    None => {
                        let temp_name = self.next_synthetic_name("set_index");
                        let temp_slot = self.bind_local_checked(&temp_name, root_ident.span)?;
                        self.compile_expr(expr, chunk)?;
                        chunk.push(Op::StoreLocal(temp_slot), *span);
                        index_slots[last_idx] = Some(temp_slot);
                        temp_slot
                    }
                };
                chunk.push(Op::LoadLocal(temp_slots[last_idx]), Some(root_ident.span));
                chunk.push(Op::LoadLocal(index_slot), *span);
                self.compile_expr(value, chunk)?;
                chunk.push(Op::StoreIndex, *span);
            }
        }

        chunk.push(Op::StoreLocal(updated_slot), Some(root_ident.span));

        for (idx, segment) in all_segments.iter().enumerate().take(last_idx).rev() {
            chunk.push(Op::LoadLocal(temp_slots[idx]), Some(root_ident.span));
            match segment {
                SetSegment::Member(name) => {
                    chunk.push(Op::LoadLocal(updated_slot), Some(root_ident.span));
                    chunk.push(Op::StoreMember(name.name.clone()), Some(name.span));
                }
                SetSegment::Index { span, .. } => {
                    let index_slot = match index_slots[idx] {
                        Some(slot) => slot,
                        None => {
                            return Err(compile_error("set index slot missing".to_string(), *span))
                        }
                    };
                    chunk.push(Op::LoadLocal(index_slot), *span);
                    chunk.push(Op::LoadLocal(updated_slot), Some(root_ident.span));
                    chunk.push(Op::StoreIndex, *span);
                }
            }
            chunk.push(Op::StoreLocal(updated_slot), Some(root_ident.span));
        }

        chunk.push(Op::LoadLocal(updated_slot), Some(root_ident.span));
        chunk.push(Op::StoreLocal(slot), Some(root_ident.span));
        Ok(())
    }

    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            const_scopes: Vec::new(),
            next_local: 0,
            functions: HashMap::new(),
            function_arity: HashMap::new(),
            function_needs: HashMap::new(),
            function_async: HashMap::new(),
            structs: HashSet::new(),
            enums: HashSet::new(),
            current_function: None,
            loop_stack: Vec::new(),
            synthetic_id: 0,
            base_function_count: 0,
            closure_chunks: Vec::new(),
        }
    }

    pub fn compile_module(&mut self, module: &Module) -> Result<Program, VmError> {
        let mut program = Program::default();

        self.functions.clear();
        self.function_arity.clear();
        self.function_needs.clear();
        self.function_async.clear();
        self.structs.clear();
        self.enums.clear();
        self.const_scopes.clear();
        self.current_function = None;
        self.closure_chunks.clear();
        self.base_function_count = module.functions.len();

        for stmt in &module.stmts {
            match stmt {
                Stmt::Struct { name, .. } => {
                    self.structs.insert(name.name.clone());
                }
                Stmt::Enum { name, .. } => {
                    self.enums.insert(name.name.clone());
                }
                _ => {}
            }
        }

        for func in &module.functions {
            let id = program.functions.len();
            if self.functions.contains_key(&func.name.name) {
                return Err(compile_error(
                    format!("duplicate function: {}", func.name.name),
                    Some(func.name.span),
                ));
            }
            self.functions.insert(func.name.name.clone(), id);
            self.function_arity
                .insert(func.name.name.clone(), func.params.len());
            self.function_async
                .insert(func.name.name.clone(), func.is_async);
            let needs: Vec<String> = func.needs.iter().map(|ident| ident.name.clone()).collect();
            self.function_needs
                .insert(func.name.name.clone(), needs.clone());
            program.functions.push(FunctionChunk {
                name: func.name.name.clone(),
                params: func.params.len(),
                captures: 0,
                locals: 0,
                needs,
                is_async: func.is_async,
                chunk: Chunk::default(),
            });
        }

        for (id, func) in module.functions.iter().enumerate() {
            self.current_function = Some(func.name.name.clone());
            let (chunk, locals) = self.compile_function(func)?;
            self.current_function = None;
            program.functions[id].chunk = chunk;
            program.functions[id].locals = locals;
        }

        self.scopes.clear();
        self.next_local = 0;
        self.push_scope();
        for stmt in &module.stmts {
            self.compile_stmt(stmt, &mut program.main)?;
        }
        program.main.push(Op::Halt, None);
        program.main_locals = self.next_local;

        for chunk in self.closure_chunks.drain(..) {
            program.functions.push(chunk);
        }

        self.propagate_transitive_needs(&mut program);

        Ok(program)
    }

    fn propagate_transitive_needs(&mut self, program: &mut Program) {
        loop {
            let mut changed = false;
            let functions_needs: Vec<(usize, Vec<String>)> = program
                .functions
                .iter()
                .enumerate()
                .map(|(id, func)| {
                    let mut needs = func.needs.clone();
                    for op in &func.chunk.code {
                        if let Op::Call(func_id, _) = op {
                            if *func_id < program.functions.len() {
                                let callee_needs = &program.functions[*func_id].needs;
                                for need in callee_needs {
                                    if !needs.contains(need) {
                                        needs.push(need.clone());
                                    }
                                }
                            }
                        }
                    }
                    (id, needs)
                })
                .collect();
            for (id, needs) in functions_needs {
                if needs != program.functions[id].needs {
                    program.functions[id].needs = needs;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }

    pub fn compile_test_body(&mut self, stmts: &[Stmt]) -> Result<Chunk, VmError> {
        self.scopes.clear();
        self.next_local = 0;
        self.push_scope();
        let mut chunk = Chunk::default();
        for stmt in stmts {
            self.compile_stmt(stmt, &mut chunk)?;
        }
        chunk.push(Op::Halt, None);
        Ok(chunk)
    }

    fn compile_function(&mut self, func: &Function) -> Result<(Chunk, usize), VmError> {
        self.scopes.clear();
        self.next_local = 0;
        self.push_scope();
        for param in &func.params {
            self.bind_local_checked(&param.name.name, param.name.span)?;
        }

        let mut chunk = Chunk::default();
        for stmt in &func.body {
            self.compile_stmt(stmt, &mut chunk)?;
        }
        let unit_index = chunk.add_const(Value::Unit);
        chunk.push(Op::Const(unit_index), None);
        chunk.push(Op::Return, None);
        Ok((chunk, self.next_local))
    }

    fn add_transitive_needs(&mut self, callee_name: &str) {
        if let Some(current) = &self.current_function {
            if let Some(callee_needs) = self.function_needs.get(callee_name).cloned() {
                let current_needs = self
                    .function_needs
                    .entry(current.clone())
                    .or_insert_with(Vec::new);
                for need in callee_needs {
                    if !current_needs.contains(&need) {
                        current_needs.push(need);
                    }
                }
            }
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt, chunk: &mut Chunk) -> Result<(), VmError> {
        match stmt {
            Stmt::Import { .. } => {}
            Stmt::TypeAlias { .. } => {}
            Stmt::Enum { .. } => {}
            Stmt::Struct { .. } => {}
            Stmt::Const { name, value, .. } => {
                self.compile_expr(value, chunk)?;
                let slot = self.bind_local_checked(&name.name, name.span)?;
                self.bind_const(&name.name);
                chunk.push(Op::StoreLocal(slot), Some(name.span));
            }
            Stmt::Let { name, value, .. } => {
                self.compile_expr(value, chunk)?;
                let slot = self.bind_local_checked(&name.name, name.span)?;
                chunk.push(Op::StoreLocal(slot), Some(name.span));
            }
            Stmt::Using { name, value, .. } => {
                self.compile_expr(value, chunk)?;
                let slot = self.bind_local_checked(&name.name, name.span)?;
                chunk.push(Op::StoreLocal(slot), Some(name.span));
                chunk.push(Op::GrantCapability(name.name.clone()), Some(name.span));
            }
            Stmt::With {
                name, value, body, ..
            } => {
                chunk.push(Op::PushCapabilityScope, Some(name.span));
                self.compile_expr(value, chunk)?;
                let slot = self.bind_local_checked(&name.name, name.span)?;
                chunk.push(Op::StoreLocal(slot), Some(name.span));
                chunk.push(Op::GrantCapability(name.name.clone()), Some(name.span));
                self.push_scope();
                for stmt in body {
                    self.compile_stmt(stmt, chunk)?;
                }
                self.pop_scope();
                chunk.push(Op::PopCapabilityScope, Some(name.span));
            }
            Stmt::Yield { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                return Err(compile_error(
                    "yield is not supported yet".to_string(),
                    expr_span(expr),
                ));
            }
            Stmt::Set { name, value, .. } => {
                if self.is_const(&name.name) {
                    return Err(compile_error(
                        format!("cannot assign to const {}", name.name),
                        Some(name.span),
                    ));
                }
                self.compile_expr(value, chunk)?;
                let slot = self.resolve_local(&name.name).ok_or_else(|| {
                    compile_error(
                        format!("unknown identifier: {}", name.name),
                        Some(name.span),
                    )
                })?;
                chunk.push(Op::StoreLocal(slot), Some(name.span));
            }
            Stmt::SetMember {
                base, field, value, ..
            } => {
                let segments = vec![SetSegment::Member(field.clone())];
                self.compile_set_target(base, segments, value, chunk)?;
            }
            Stmt::SetIndex {
                base, index, value, ..
            } => {
                let segments = vec![SetSegment::Index {
                    expr: index.clone(),
                    span: expr_span(index),
                }];
                self.compile_set_target(base, segments, value, chunk)?;
            }
            Stmt::While {
                while_span,
                condition,
                body,
                ..
            } => {
                let loop_start = chunk.code.len();
                self.loop_stack.push(LoopContext::new());
                self.compile_expr(condition, chunk)?;
                let jump_if_false = chunk.code.len();
                chunk.push(Op::JumpIfFalse(usize::MAX), Some(*while_span));
                self.push_scope();
                for stmt in body {
                    self.compile_stmt(stmt, chunk)?;
                }
                self.pop_scope();
                chunk.push(Op::Jump(loop_start), Some(*while_span));
                let end = chunk.code.len();
                patch_jump(&mut chunk.code, jump_if_false, end);
                if let Some(loop_ctx) = self.loop_stack.pop() {
                    for jump in loop_ctx.breaks {
                        patch_jump(&mut chunk.code, jump, end);
                    }
                    for jump in loop_ctx.continues {
                        patch_jump(&mut chunk.code, jump, loop_start);
                    }
                }
            }
            Stmt::If {
                if_span,
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.compile_expr(condition, chunk)?;
                let jump_if_false = chunk.code.len();
                chunk.push(Op::JumpIfFalse(usize::MAX), Some(*if_span));
                self.push_scope();
                for stmt in then_branch {
                    self.compile_stmt(stmt, chunk)?;
                }
                self.pop_scope();
                let jump_to_end = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), Some(*if_span));
                let else_start = chunk.code.len();
                patch_jump(&mut chunk.code, jump_if_false, else_start);
                if let Some(else_branch) = else_branch {
                    self.push_scope();
                    for stmt in else_branch {
                        self.compile_stmt(stmt, chunk)?;
                    }
                    self.pop_scope();
                }
                let end = chunk.code.len();
                patch_jump(&mut chunk.code, jump_to_end, end);
            }
            Stmt::For {
                for_span,
                item,
                iter,
                body,
                ..
            } => {
                self.compile_expr(iter, chunk)?;
                let arr_name = self.next_synthetic_name("for_arr");
                let arr_slot = self.bind_local_checked(&arr_name, item.span)?;
                chunk.push(Op::StoreLocal(arr_slot), Some(item.span));

                let idx_name = self.next_synthetic_name("for_idx");
                let idx_slot = self.bind_local_checked(&idx_name, item.span)?;
                let zero_index = chunk.add_const(Value::Int(0));
                chunk.push(Op::Const(zero_index), Some(*for_span));
                chunk.push(Op::StoreLocal(idx_slot), Some(*for_span));

                let loop_start = chunk.code.len();
                self.loop_stack.push(LoopContext::new());
                chunk.push(Op::LoadLocal(idx_slot), Some(*for_span));
                chunk.push(Op::LoadLocal(arr_slot), Some(*for_span));
                chunk.push(Op::Builtin(Builtin::Len), Some(*for_span));
                chunk.push(Op::Lt, Some(*for_span));
                let jump_if_false = chunk.code.len();
                chunk.push(Op::JumpIfFalse(usize::MAX), Some(*for_span));

                self.push_scope();
                let item_slot = self.bind_local_checked(&item.name, item.span)?;
                chunk.push(Op::LoadLocal(arr_slot), Some(item.span));
                chunk.push(Op::LoadLocal(idx_slot), Some(item.span));
                chunk.push(Op::Index, Some(item.span));
                chunk.push(Op::StoreLocal(item_slot), Some(item.span));
                for stmt in body {
                    self.compile_stmt(stmt, chunk)?;
                }
                self.pop_scope();

                let continue_target = chunk.code.len();
                let one_index = chunk.add_const(Value::Int(1));
                chunk.push(Op::LoadLocal(idx_slot), Some(*for_span));
                chunk.push(Op::Const(one_index), Some(*for_span));
                chunk.push(Op::Add, Some(*for_span));
                chunk.push(Op::StoreLocal(idx_slot), Some(*for_span));
                chunk.push(Op::Jump(loop_start), Some(*for_span));

                let end = chunk.code.len();
                patch_jump(&mut chunk.code, jump_if_false, end);
                if let Some(loop_ctx) = self.loop_stack.pop() {
                    for jump in loop_ctx.breaks {
                        patch_jump(&mut chunk.code, jump, end);
                    }
                    for jump in loop_ctx.continues {
                        patch_jump(&mut chunk.code, jump, continue_target);
                    }
                }
            }
            Stmt::Break { break_span, .. } => {
                let jump = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), Some(*break_span));
                if let Some(loop_ctx) = self.loop_stack.last_mut() {
                    loop_ctx.breaks.push(jump);
                } else {
                    return Err(compile_error(
                        "break used outside of loop".to_string(),
                        Some(*break_span),
                    ));
                }
            }
            Stmt::Continue { continue_span, .. } => {
                let jump = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), Some(*continue_span));
                if let Some(loop_ctx) = self.loop_stack.last_mut() {
                    loop_ctx.continues.push(jump);
                } else {
                    return Err(compile_error(
                        "continue used outside of loop".to_string(),
                        Some(*continue_span),
                    ));
                }
            }
            Stmt::Expr { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::Pop, expr_span(expr));
            }
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    self.compile_expr(expr, chunk)?;
                } else {
                    let unit_index = chunk.add_const(Value::Unit);
                    chunk.push(Op::Const(unit_index), None);
                }
                chunk.push(Op::Return, expr.as_ref().and_then(expr_span));
            }
            Stmt::Throw { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::Throw, expr_span(expr));
            }
            Stmt::Defer { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::Defer, expr_span(expr));
            }
            Stmt::Block { stmts, .. } => {
                self.push_scope();
                for stmt in stmts {
                    self.compile_stmt(stmt, chunk)?;
                }
                self.pop_scope();
            }
            Stmt::Test { .. } => {}
        }
        Ok(())
    }

    fn compile_to_bool(&mut self, expr: &Expr, chunk: &mut Chunk) -> Result<(), VmError> {
        self.compile_expr(expr, chunk)?;
        chunk.push(Op::Not, expr_span(expr));
        chunk.push(Op::Not, expr_span(expr));
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr, chunk: &mut Chunk) -> Result<(), VmError> {
        match expr {
            Expr::Int(value, _, _) => {
                let index = chunk.add_const(Value::Int(*value));
                chunk.push(Op::Const(index), expr_span(expr));
            }
            Expr::Float(value, _, _) => {
                let index = chunk.add_const(Value::Float(*value));
                chunk.push(Op::Const(index), expr_span(expr));
            }
            Expr::String(value, _, _) => {
                let index = chunk.add_const(Value::String(Rc::new(value.clone())));
                chunk.push(Op::Const(index), expr_span(expr));
            }
            Expr::Bool(value, _, _) => {
                let index = chunk.add_const(Value::Bool(*value));
                chunk.push(Op::Const(index), expr_span(expr));
            }
            Expr::Ident(ident) => {
                let slot = self.resolve_local(&ident.name).ok_or_else(|| {
                    compile_error(
                        format!("unknown identifier: {}", ident.name),
                        Some(ident.span),
                    )
                })?;
                chunk.push(Op::LoadLocal(slot), Some(ident.span));
            }
            Expr::Binary {
                left,
                op,
                op_span,
                right,
                ..
            } => match op {
                at_syntax::BinaryOp::And => {
                    self.compile_to_bool(left, chunk)?;
                    let jump_if_false = chunk.code.len();
                    chunk.push(Op::JumpIfFalse(usize::MAX), Some(*op_span));
                    self.compile_to_bool(right, chunk)?;
                    let jump_to_end = chunk.code.len();
                    chunk.push(Op::Jump(usize::MAX), Some(*op_span));
                    let false_index = chunk.add_const(Value::Bool(false));
                    let false_pos = chunk.code.len();
                    chunk.push(Op::Const(false_index), Some(*op_span));
                    patch_jump(&mut chunk.code, jump_if_false, false_pos);
                    let end = chunk.code.len();
                    patch_jump(&mut chunk.code, jump_to_end, end);
                }
                at_syntax::BinaryOp::Or => {
                    self.compile_to_bool(left, chunk)?;
                    let jump_if_false = chunk.code.len();
                    chunk.push(Op::JumpIfFalse(usize::MAX), Some(*op_span));
                    let true_index = chunk.add_const(Value::Bool(true));
                    chunk.push(Op::Const(true_index), Some(*op_span));
                    let jump_to_end = chunk.code.len();
                    chunk.push(Op::Jump(usize::MAX), Some(*op_span));
                    let right_start = chunk.code.len();
                    patch_jump(&mut chunk.code, jump_if_false, right_start);
                    self.compile_to_bool(right, chunk)?;
                    let end = chunk.code.len();
                    patch_jump(&mut chunk.code, jump_to_end, end);
                }
                _ => {
                    self.compile_expr(left, chunk)?;
                    self.compile_expr(right, chunk)?;
                    let op = match op {
                        at_syntax::BinaryOp::Add => Op::Add,
                        at_syntax::BinaryOp::Sub => Op::Sub,
                        at_syntax::BinaryOp::Mul => Op::Mul,
                        at_syntax::BinaryOp::Div => Op::Div,
                        at_syntax::BinaryOp::Mod => Op::Mod,
                        at_syntax::BinaryOp::BitAnd => Op::BitAnd,
                        at_syntax::BinaryOp::BitOr => Op::BitOr,
                        at_syntax::BinaryOp::BitXor => Op::BitXor,
                        at_syntax::BinaryOp::Shl => Op::Shl,
                        at_syntax::BinaryOp::Shr => Op::Shr,
                        at_syntax::BinaryOp::Eq => Op::Eq,
                        at_syntax::BinaryOp::Neq => Op::Neq,
                        at_syntax::BinaryOp::Lt => Op::Lt,
                        at_syntax::BinaryOp::Lte => Op::Lte,
                        at_syntax::BinaryOp::Gt => Op::Gt,
                        at_syntax::BinaryOp::Gte => Op::Gte,
                        at_syntax::BinaryOp::And | at_syntax::BinaryOp::Or => {
                            return Err(compile_error(
                                "unexpected logical op".to_string(),
                                Some(*op_span),
                            ));
                        }
                    };
                    chunk.push(op, Some(*op_span));
                }
            },
            Expr::Unary {
                op, op_span, expr, ..
            } => {
                self.compile_expr(expr, chunk)?;
                let op = match op {
                    at_syntax::UnaryOp::Neg => Op::Neg,
                    at_syntax::UnaryOp::Not => Op::Not,
                };
                chunk.push(op, Some(*op_span));
            }
            Expr::Ternary {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.compile_expr(condition, chunk)?;
                let jump_if_false = chunk.code.len();
                chunk.push(Op::JumpIfFalse(usize::MAX), expr_span(condition));
                self.compile_expr(then_branch, chunk)?;
                let jump_to_end = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), expr_span(then_branch));
                let else_start = chunk.code.len();
                patch_jump(&mut chunk.code, jump_if_false, else_start);
                self.compile_expr(else_branch, chunk)?;
                let end = chunk.code.len();
                patch_jump(&mut chunk.code, jump_to_end, end);
            }
            Expr::ChainedComparison { items, ops, .. } => {
                let mut false_jumps = Vec::with_capacity(ops.len());
                for (idx, (op, op_span)) in ops.iter().enumerate() {
                    let left = if idx == 0 { &items[0] } else { &items[idx] };
                    let right = &items[idx + 1];
                    self.compile_expr(left, chunk)?;
                    self.compile_expr(right, chunk)?;
                    let op = match op {
                        at_syntax::BinaryOp::Lt => Op::Lt,
                        at_syntax::BinaryOp::Lte => Op::Lte,
                        at_syntax::BinaryOp::Gt => Op::Gt,
                        at_syntax::BinaryOp::Gte => Op::Gte,
                        _ => {
                            return Err(compile_error(
                                "invalid chained comparison".to_string(),
                                Some(*op_span),
                            ))
                        }
                    };
                    chunk.push(op, Some(*op_span));
                    let jump_if_false = chunk.code.len();
                    chunk.push(Op::JumpIfFalse(usize::MAX), Some(*op_span));
                    false_jumps.push(jump_if_false);
                }
                let true_index = chunk.add_const(Value::Bool(true));
                chunk.push(Op::Const(true_index), expr_span(expr));
                let jump_to_end = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), expr_span(expr));
                let false_pos = chunk.code.len();
                let false_index = chunk.add_const(Value::Bool(false));
                chunk.push(Op::Const(false_index), expr_span(expr));
                for jump in false_jumps {
                    patch_jump(&mut chunk.code, jump, false_pos);
                }
                let end = chunk.code.len();
                patch_jump(&mut chunk.code, jump_to_end, end);
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.compile_expr(condition, chunk)?;
                let jump_if_false = chunk.code.len();
                chunk.push(Op::JumpIfFalse(usize::MAX), expr_span(condition));
                self.compile_expr(then_branch, chunk)?;
                let jump_to_end = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), expr_span(then_branch));
                let else_start = chunk.code.len();
                patch_jump(&mut chunk.code, jump_if_false, else_start);
                if let Some(else_branch) = else_branch {
                    self.compile_expr(else_branch, chunk)?;
                } else {
                    let unit_index = chunk.add_const(Value::Unit);
                    chunk.push(Op::Const(unit_index), expr_span(expr));
                }
                let end = chunk.code.len();
                patch_jump(&mut chunk.code, jump_to_end, end);
            }
            Expr::Block { stmts, tail, .. } => {
                self.push_scope();
                for stmt in stmts {
                    self.compile_stmt(stmt, chunk)?;
                }
                if let Some(expr) = tail {
                    self.compile_expr(expr, chunk)?;
                } else {
                    let unit_index = chunk.add_const(Value::Unit);
                    chunk.push(Op::Const(unit_index), None);
                }
                self.pop_scope();
            }
            Expr::Array { items, .. } => {
                let mut array_count = 0;
                let mut chunk_count = 0;
                let mut has_spread = false;
                for item in items {
                    self.compile_expr(item, chunk)?;
                    match item {
                        Expr::ArraySpread { .. } => {
                            if array_count > 0 {
                                chunk.push(Op::Array(array_count), expr_span(expr));
                                array_count = 0;
                                chunk_count += 1;
                            }
                            has_spread = true;
                            chunk_count += 1;
                        }
                        _ => {
                            array_count += 1;
                        }
                    }
                }
                if array_count > 0 {
                    chunk.push(Op::Array(array_count), expr_span(expr));
                    chunk_count += 1;
                }
                if has_spread {
                    chunk.push(Op::ArraySpread(chunk_count), expr_span(expr));
                }
            }
            Expr::ArraySpread { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::ArraySpread(1), expr_span(expr));
            }
            Expr::Index { base, index, .. } => {
                self.compile_expr(base, chunk)?;
                self.compile_expr(index, chunk)?;
                chunk.push(Op::Index, expr_span(expr));
            }
            Expr::Tuple { items, .. } => {
                for item in items {
                    self.compile_expr(item, chunk)?;
                }
                chunk.push(Op::Tuple(items.len()), expr_span(expr));
            }
            Expr::MapLiteral { entries, .. } => {
                let mut map_count = 0;
                let mut chunk_count = 0;
                let mut has_spread = false;
                for (key, value) in entries {
                    self.compile_expr(key, chunk)?;
                    if let Expr::MapSpread { .. } = key {
                        if map_count > 0 {
                            chunk.push(Op::Map(map_count), expr_span(expr));
                            map_count = 0;
                            chunk_count += 1;
                        }
                        has_spread = true;
                        chunk_count += 1;
                    } else {
                        self.compile_expr(value, chunk)?;
                        map_count += 1;
                    }
                }
                if map_count > 0 {
                    chunk.push(Op::Map(map_count), expr_span(expr));
                    chunk_count += 1;
                }
                if has_spread {
                    chunk.push(Op::MapSpread(chunk_count), expr_span(expr));
                }
            }
            Expr::MapSpread { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::MapSpread(1), expr_span(expr));
            }
            Expr::As { expr, ty, .. } => {
                self.compile_expr(expr, chunk)?;
                let check = self.type_check_from_ref(ty)?;
                chunk.push(Op::Cast(check), expr_span(expr));
            }
            Expr::Is { expr, ty, .. } => {
                self.compile_expr(expr, chunk)?;
                let check = self.type_check_from_ref(ty)?;
                chunk.push(Op::IsType(check), expr_span(expr));
            }
            Expr::Range {
                start,
                end,
                inclusive,
                ..
            } => {
                self.compile_expr(start, chunk)?;
                self.compile_expr(end, chunk)?;
                chunk.push(Op::Range(*inclusive), expr_span(expr));
            }
            Expr::InterpolatedString { parts, .. } => {
                let empty_idx = chunk.add_const(Value::String(Rc::new(String::new())));
                chunk.push(Op::Const(empty_idx), expr_span(expr));
                for part in parts {
                    match part {
                        at_syntax::InterpPart::String(s, _) => {
                            let idx = chunk.add_const(Value::String(Rc::new(s.clone())));
                            chunk.push(Op::Const(idx), expr_span(expr));
                        }
                        at_syntax::InterpPart::Expr(e, _) => {
                            self.compile_expr(e, chunk)?;
                        }
                    }
                    chunk.push(Op::Add, expr_span(expr));
                }
            }
            Expr::StructLiteral { name, fields, .. } => {
                for field in fields {
                    self.compile_expr(&field.value, chunk)?;
                }
                let field_names = fields
                    .iter()
                    .map(|field| field.name.name.clone())
                    .collect::<Vec<_>>();
                if field_names.is_empty() {
                    let empty = BTreeMap::new();
                    let index = chunk.add_const(Value::Struct(Rc::new(empty)));
                    chunk.push(Op::Const(index), Some(name.span));
                } else {
                    chunk.push(Op::Struct(field_names), Some(name.span));
                }
            }
            Expr::EnumLiteral {
                name,
                variant,
                payload,
                ..
            } => {
                if let Some(expr) = payload {
                    self.compile_expr(expr, chunk)?;
                }
                chunk.push(
                    Op::Enum {
                        name: name.name.clone(),
                        variant: variant.name.clone(),
                        has_payload: payload.is_some(),
                    },
                    Some(name.span),
                );
            }
            Expr::Group { expr, .. } => {
                self.compile_expr(expr, chunk)?;
            }
            Expr::Closure {
                span, params, body, ..
            } => {
                self.compile_closure_expr(*span, params, body, chunk)?;
            }
            Expr::Call { callee, args, .. } => {
                if let Expr::Member { base, name, .. } = callee.as_ref() {
                    if let Expr::Ident(base_ident) = base.as_ref() {
                        if let Some(builtin) = map_builtin(&base_ident.name, &name.name, args.len())
                        {
                            for arg in args {
                                self.compile_expr(arg, chunk)?;
                            }
                            chunk.push(Op::Builtin(builtin), Some(name.span));
                            return Ok(());
                        }

                        let func_name = format!("{}.{}", base_ident.name, name.name);
                        if let Some(expected) = self.function_arity.get(&func_name) {
                            if *expected != args.len() {
                                return Err(compile_error(
                                    format!(
                                        "wrong arity for {}: expected {}, got {}",
                                        func_name,
                                        expected,
                                        args.len()
                                    ),
                                    Some(name.span),
                                ));
                            }
                        }
                        let func_id = *self.functions.get(&func_name).ok_or_else(|| {
                            compile_error(format!("unknown function: {func_name}"), Some(name.span))
                        })?;
                        for arg in args {
                            self.compile_expr(arg, chunk)?;
                        }
                        let op = if self
                            .function_async
                            .get(&func_name)
                            .copied()
                            .unwrap_or(false)
                        {
                            Op::CallAsync(func_id, args.len())
                        } else {
                            Op::Call(func_id, args.len())
                        };
                        chunk.push(op, Some(name.span));
                        self.add_transitive_needs(&func_name);
                        return Ok(());
                    }
                    return Err(compile_error(
                        "invalid call target".to_string(),
                        expr_span(callee),
                    ));
                }
                if let Expr::Ident(ident) = callee.as_ref() {
                    if ident.name == "map" {
                        if args.len() != 2 {
                            return Err(compile_error(
                                "wrong arity for map".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_map_expr(&args[0], &args[1], ident.span, chunk)?;
                        return Ok(());
                    }
                    if ident.name == "filter" {
                        if args.len() != 2 {
                            return Err(compile_error(
                                "wrong arity for filter".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_filter_expr(&args[0], &args[1], ident.span, chunk)?;
                        return Ok(());
                    }
                    if ident.name == "reduce" {
                        if args.len() != 3 {
                            return Err(compile_error(
                                "wrong arity for reduce".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_reduce_expr(&args[0], &args[1], &args[2], ident.span, chunk)?;
                        return Ok(());
                    }
                    if ident.name == "assert" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for assert".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Assert, Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "assert_eq" {
                        if args.len() != 2 {
                            return Err(compile_error(
                                "wrong arity for assert_eq".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        self.compile_expr(&args[1], chunk)?;
                        chunk.push(Op::Builtin(Builtin::AssertEq), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "print" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for print".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::Print), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "len" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for len".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::Len), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "append" {
                        if args.len() != 2 {
                            return Err(compile_error(
                                "wrong arity for append".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        self.compile_expr(&args[1], chunk)?;
                        chunk.push(Op::Builtin(Builtin::Append), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "contains" {
                        if args.len() != 2 {
                            return Err(compile_error(
                                "wrong arity for contains".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        self.compile_expr(&args[1], chunk)?;
                        chunk.push(Op::Builtin(Builtin::Contains), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "slice" {
                        if args.len() != 3 {
                            return Err(compile_error(
                                "wrong arity for slice".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        self.compile_expr(&args[1], chunk)?;
                        self.compile_expr(&args[2], chunk)?;
                        chunk.push(Op::Builtin(Builtin::Slice), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "some" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for some".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::OptionSome), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "none" {
                        if !args.is_empty() {
                            return Err(compile_error(
                                "wrong arity for none".to_string(),
                                Some(ident.span),
                            ));
                        }
                        chunk.push(Op::Builtin(Builtin::OptionNone), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "ok" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for ok".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::ResultOk), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "err" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for err".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::ResultErr), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "is_some" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for is_some".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::IsSome), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "is_none" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for is_none".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::IsNone), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "is_ok" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for is_ok".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::IsOk), Some(ident.span));
                        return Ok(());
                    }
                    if ident.name == "is_err" {
                        if args.len() != 1 {
                            return Err(compile_error(
                                "wrong arity for is_err".to_string(),
                                Some(ident.span),
                            ));
                        }
                        self.compile_expr(&args[0], chunk)?;
                        chunk.push(Op::Builtin(Builtin::IsErr), Some(ident.span));
                        return Ok(());
                    }
                    let func_id = *self.functions.get(&ident.name).ok_or_else(|| {
                        compile_error(
                            format!("unknown function: {}", ident.name),
                            Some(ident.span),
                        )
                    })?;
                    if let Some(expected) = self.function_arity.get(&ident.name) {
                        if *expected != args.len() {
                            return Err(compile_error(
                                format!(
                                    "wrong arity for {}: expected {}, got {}",
                                    ident.name,
                                    expected,
                                    args.len()
                                ),
                                Some(ident.span),
                            ));
                        }
                    }
                    for arg in args {
                        self.compile_expr(arg, chunk)?;
                    }
                    let op = if self
                        .function_async
                        .get(&ident.name)
                        .copied()
                        .unwrap_or(false)
                    {
                        Op::CallAsync(func_id, args.len())
                    } else {
                        Op::Call(func_id, args.len())
                    };
                    chunk.push(op, Some(ident.span));
                    self.add_transitive_needs(&ident.name);
                    return Ok(());
                }
                self.compile_expr(callee, chunk)?;
                for arg in args {
                    self.compile_expr(arg, chunk)?;
                }
                chunk.push(Op::CallValue(args.len()), expr_span(callee));
                return Ok(());
            }
            Expr::Try(expr, _) => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::Try, expr_span(expr));
            }
            Expr::Await { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::Await, expr_span(expr));
            }
            Expr::TryCatch {
                try_block,
                catch_block,
                finally_block,
                ..
            } => {
                let catch_block = match catch_block {
                    Some(block) => block,
                    None => {
                        return Err(compile_error(
                            "try requires catch".to_string(),
                            expr_span(expr),
                        ))
                    }
                };

                self.compile_expr(try_block, chunk)?;
                let match_index = chunk.code.len();
                chunk.push(Op::MatchResultOk(usize::MAX), expr_span(try_block));

                let jump_index = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), None);

                let catch_start = chunk.code.len();
                chunk.push(Op::Pop, expr_span(catch_block));
                self.compile_expr(catch_block, chunk)?;

                let after_catch = chunk.code.len();
                patch_jump(&mut chunk.code, match_index, catch_start);
                patch_jump(&mut chunk.code, jump_index, after_catch);

                if let Some(finally_block) = finally_block {
                    let _ = self.compile_expr(finally_block, chunk)?;
                    chunk.push(Op::Pop, expr_span(finally_block));
                }
            }
            Expr::Match {
                match_span,
                value,
                arms,
                ..
            } => {
                self.compile_match_expr(*match_span, value, arms, chunk)?;
            }
            Expr::Member { base, name, .. } => {
                self.compile_expr(base, chunk)?;
                chunk.push(Op::GetMember(name.name.clone()), Some(name.span));
            }
        }
        Ok(())
    }

    fn compile_match_expr(
        &mut self,
        match_span: Span,
        value: &Expr,
        arms: &[at_syntax::MatchArm],
        chunk: &mut Chunk,
    ) -> Result<(), VmError> {
        if arms.is_empty() {
            let unit_index = chunk.add_const(Value::Unit);
            chunk.push(Op::Const(unit_index), Some(match_span));
            return Ok(());
        }

        self.compile_expr(value, chunk)?;
        let mut end_jumps = Vec::new();

        let mut match_op_indices: Vec<(usize, usize)> = Vec::new();
        let mut arm_start_indices = Vec::new();
        let mut guard_fail_jumps: Vec<(usize, usize)> = Vec::new();
        let mut wildcard_body_index: Option<usize> = None;
        for (arm_index, arm) in arms.iter().enumerate() {
            let is_wildcard = matches!(arm.pattern, at_syntax::MatchPattern::Wildcard(_));
            let arm_start = if is_wildcard {
                let idx = chunk.code.len();
                if arm.guard.is_none() {
                    wildcard_body_index = Some(idx);
                    chunk.push(Op::Pop, Some(match_span));
                }
                idx
            } else {
                let match_op_index = chunk.code.len();
                chunk.push(
                    match &arm.pattern {
                        at_syntax::MatchPattern::Int(value, _) => Op::MatchInt(*value, usize::MAX),
                        at_syntax::MatchPattern::Bool(value, _) => {
                            Op::MatchBool(*value, usize::MAX)
                        }
                        at_syntax::MatchPattern::String(value, _) => {
                            Op::MatchString(value.clone(), usize::MAX)
                        }
                        at_syntax::MatchPattern::ResultOk(_, _) => Op::MatchResultOk(usize::MAX),
                        at_syntax::MatchPattern::ResultErr(_, _) => Op::MatchResultErr(usize::MAX),
                        at_syntax::MatchPattern::OptionSome(_, _) => {
                            Op::MatchOptionSome(usize::MAX)
                        }
                        at_syntax::MatchPattern::OptionNone(_) => Op::MatchOptionNone(usize::MAX),
                        at_syntax::MatchPattern::Tuple { items, .. } => {
                            Op::MatchTuple(items.len(), usize::MAX)
                        }
                        at_syntax::MatchPattern::Struct { fields, .. } => {
                            let names = fields
                                .iter()
                                .map(|field| field.name.name.clone())
                                .collect::<Vec<_>>();
                            Op::MatchStruct(names, usize::MAX)
                        }
                        at_syntax::MatchPattern::Enum {
                            name,
                            variant,
                            binding,
                            ..
                        } => Op::MatchEnum {
                            name: name.name.clone(),
                            variant: variant.name.clone(),
                            has_payload: binding.is_some(),
                            target: usize::MAX,
                        },
                        at_syntax::MatchPattern::Binding { pattern, .. } => {
                            match pattern.as_ref() {
                                at_syntax::MatchPattern::Int(value, _) => {
                                    Op::MatchInt(*value, usize::MAX)
                                }
                                at_syntax::MatchPattern::Bool(value, _) => {
                                    Op::MatchBool(*value, usize::MAX)
                                }
                                at_syntax::MatchPattern::String(value, _) => {
                                    Op::MatchString(value.clone(), usize::MAX)
                                }
                                at_syntax::MatchPattern::ResultOk(_, _)
                                | at_syntax::MatchPattern::ResultErr(_, _)
                                | at_syntax::MatchPattern::OptionSome(_, _)
                                | at_syntax::MatchPattern::OptionNone(_)
                                | at_syntax::MatchPattern::Struct { .. }
                                | at_syntax::MatchPattern::Enum { .. }
                                | at_syntax::MatchPattern::Wildcard(_)
                                | at_syntax::MatchPattern::Binding { .. } => Op::MatchFail,
                                at_syntax::MatchPattern::Tuple { items, .. } => {
                                    Op::MatchTuple(items.len(), usize::MAX)
                                }
                            }
                        }
                        at_syntax::MatchPattern::Wildcard(_) => unreachable!(),
                    },
                    Some(match_span),
                );
                match_op_indices.push((arm_index, match_op_index));
                match_op_index
            };
            arm_start_indices.push(arm_start);

            self.push_scope();
            let mut binding_slot: Option<usize> = None;
            let mut binding_span: Option<Span> = None;
            let mut struct_guard_slot: Option<usize> = None;
            match &arm.pattern {
                at_syntax::MatchPattern::Int(_, _)
                | at_syntax::MatchPattern::Bool(_, _)
                | at_syntax::MatchPattern::String(_, _) => {}
                at_syntax::MatchPattern::ResultOk(ident, _)
                | at_syntax::MatchPattern::ResultErr(ident, _)
                | at_syntax::MatchPattern::OptionSome(ident, _) => {
                    let slot = self.bind_local_checked(&ident.name, ident.span)?;
                    chunk.push(Op::StoreLocal(slot), Some(ident.span));
                    binding_slot = Some(slot);
                    binding_span = Some(ident.span);
                }
                at_syntax::MatchPattern::OptionNone(_) => {}
                at_syntax::MatchPattern::Enum { binding, .. } => {
                    if let Some(binding) = binding {
                        let slot = self.bind_local_checked(&binding.name, binding.span)?;
                        chunk.push(Op::StoreLocal(slot), Some(binding.span));
                        binding_slot = Some(slot);
                        binding_span = Some(binding.span);
                    } else {
                        chunk.push(Op::Pop, Some(match_span));
                    }
                }
                at_syntax::MatchPattern::Struct { fields, .. } => {
                    for field in fields.iter().rev() {
                        let binding = field.binding.as_ref().unwrap_or(&field.name);
                        if binding.name == "_" {
                            chunk.push(Op::Pop, Some(binding.span));
                            continue;
                        }
                        let slot = self.bind_local_checked(&binding.name, binding.span)?;
                        chunk.push(Op::StoreLocal(slot), Some(binding.span));
                    }

                    if arm.guard.is_some() {
                        let temp_name = self.next_synthetic_name("match_struct");
                        let temp_slot = self.bind_local_checked(&temp_name, match_span)?;
                        chunk.push(Op::StoreLocal(temp_slot), Some(match_span));
                        struct_guard_slot = Some(temp_slot);
                    } else {
                        chunk.push(Op::Pop, Some(match_span));
                    }
                }
                at_syntax::MatchPattern::Tuple { items, .. } => {
                    let temp_name = self.next_synthetic_name("match_tuple");
                    let temp_slot = self.bind_local_checked(&temp_name, match_span)?;
                    chunk.push(Op::StoreLocal(temp_slot), Some(match_span));
                    struct_guard_slot = Some(temp_slot);
                    for _ in 0..items.len() {
                        chunk.push(Op::Pop, Some(match_span));
                    }
                }
                at_syntax::MatchPattern::Binding { name, pattern, .. } => {
                    let slot = self.bind_local_checked(&name.name, name.span)?;
                    chunk.push(Op::StoreLocal(slot), Some(name.span));
                    binding_slot = Some(slot);
                    binding_span = Some(name.span);
                    match pattern.as_ref() {
                        at_syntax::MatchPattern::Int(_, _)
                        | at_syntax::MatchPattern::Bool(_, _)
                        | at_syntax::MatchPattern::String(_, _)
                        | at_syntax::MatchPattern::Tuple { .. } => {}
                        _ => {
                            chunk.push(Op::Pop, Some(match_span));
                        }
                    }
                }
                at_syntax::MatchPattern::Wildcard(_) => {}
            }

            let mut guard_jump_index: Option<usize> = None;
            if let Some(guard) = &arm.guard {
                self.compile_expr(guard, chunk)?;
                let jump_index = chunk.code.len();
                chunk.push(Op::JumpIfFalse(usize::MAX), expr_span(guard));
                guard_jump_index = Some(jump_index);
            }

            if is_wildcard && arm.guard.is_some() {
                chunk.push(Op::Pop, Some(match_span));
            }

            self.compile_expr(&arm.body, chunk)?;
            let jump_index = chunk.code.len();
            chunk.push(Op::Jump(usize::MAX), None);
            end_jumps.push(jump_index);

            if let Some(guard_jump_index) = guard_jump_index {
                let guard_fail_index = chunk.code.len();
                patch_jump(&mut chunk.code, guard_jump_index, guard_fail_index);

                match &arm.pattern {
                    at_syntax::MatchPattern::Int(value, _) => {
                        let idx = chunk.add_const(Value::Int(*value));
                        chunk.push(Op::Const(idx), Some(match_span));
                    }
                    at_syntax::MatchPattern::Bool(value, _) => {
                        let idx = chunk.add_const(Value::Bool(*value));
                        chunk.push(Op::Const(idx), Some(match_span));
                    }
                    at_syntax::MatchPattern::String(value, _) => {
                        let idx = chunk.add_const(Value::String(Rc::new(value.clone())));
                        chunk.push(Op::Const(idx), Some(match_span));
                    }
                    at_syntax::MatchPattern::Struct { .. } => {
                        if let Some(slot) = struct_guard_slot {
                            chunk.push(Op::LoadLocal(slot), Some(match_span));
                        }
                    }
                    at_syntax::MatchPattern::Tuple { .. } => {
                        if let Some(slot) = struct_guard_slot {
                            chunk.push(Op::LoadLocal(slot), Some(match_span));
                        }
                    }
                    at_syntax::MatchPattern::Enum { binding, .. } => {
                        if binding.is_some() {
                            let slot = binding_slot.ok_or_else(|| {
                                compile_error("missing match binding".to_string(), Some(match_span))
                            })?;
                            let span = binding_span.or(Some(match_span));
                            chunk.push(Op::LoadLocal(slot), span);
                        }
                    }
                    at_syntax::MatchPattern::ResultOk(_, _) => {
                        let slot = binding_slot.ok_or_else(|| {
                            compile_error("missing match binding".to_string(), Some(match_span))
                        })?;
                        let span = binding_span.or(Some(match_span));
                        chunk.push(Op::LoadLocal(slot), span);
                        chunk.push(Op::Builtin(Builtin::ResultOk), span);
                    }
                    at_syntax::MatchPattern::ResultErr(_, _) => {
                        let slot = binding_slot.ok_or_else(|| {
                            compile_error("missing match binding".to_string(), Some(match_span))
                        })?;
                        let span = binding_span.or(Some(match_span));
                        chunk.push(Op::LoadLocal(slot), span);
                        chunk.push(Op::Builtin(Builtin::ResultErr), span);
                    }
                    at_syntax::MatchPattern::OptionSome(_, _) => {
                        let slot = binding_slot.ok_or_else(|| {
                            compile_error("missing match binding".to_string(), Some(match_span))
                        })?;
                        let span = binding_span.or(Some(match_span));
                        chunk.push(Op::LoadLocal(slot), span);
                        chunk.push(Op::Builtin(Builtin::OptionSome), span);
                    }
                    at_syntax::MatchPattern::OptionNone(_) => {
                        chunk.push(Op::Builtin(Builtin::OptionNone), Some(match_span));
                    }
                    at_syntax::MatchPattern::Binding { name, pattern, .. } => {
                        let slot = binding_slot.ok_or_else(|| {
                            compile_error("missing match binding".to_string(), Some(match_span))
                        })?;
                        let span = binding_span.or(Some(match_span)).unwrap_or(name.span);
                        chunk.push(Op::LoadLocal(slot), Some(span));
                        if let at_syntax::MatchPattern::Tuple { items, .. } = pattern.as_ref() {
                            if let Some(slot) = struct_guard_slot {
                                chunk.push(Op::LoadLocal(slot), Some(match_span));
                            }
                            for item in items.iter().rev() {
                                if let at_syntax::MatchPattern::Binding { name, .. } = item {
                                    let slot = self.bind_local_checked(&name.name, name.span)?;
                                    chunk.push(Op::StoreLocal(slot), Some(name.span));
                                }
                            }
                        }
                    }
                    at_syntax::MatchPattern::Wildcard(_) => {}
                }

                let guard_fail_jump = chunk.code.len();
                chunk.push(Op::Jump(usize::MAX), None);
                guard_fail_jumps.push((arm_index, guard_fail_jump));
            }

            self.pop_scope();
        }

        let fail_index = if let Some(wildcard_idx) = wildcard_body_index {
            wildcard_idx
        } else {
            let idx = chunk.code.len();
            chunk.push(Op::MatchFail, Some(match_span));
            idx
        };

        for (arm_index, match_index) in &match_op_indices {
            let target = if *arm_index + 1 < arm_start_indices.len() {
                arm_start_indices[*arm_index + 1]
            } else {
                fail_index
            };
            patch_jump(&mut chunk.code, *match_index, target);
        }

        for (arm_index, jump_index) in guard_fail_jumps {
            let target = if arm_index + 1 < arm_start_indices.len() {
                arm_start_indices[arm_index + 1]
            } else {
                fail_index
            };
            patch_jump(&mut chunk.code, jump_index, target);
        }

        let end = chunk.code.len();
        for jump_index in end_jumps {
            patch_jump(&mut chunk.code, jump_index, end);
        }
        Ok(())
    }

    fn compile_map_expr(
        &mut self,
        array: &Expr,
        func: &Expr,
        span: Span,
        chunk: &mut Chunk,
    ) -> Result<(), VmError> {
        self.push_scope();

        self.compile_expr(array, chunk)?;
        let arr_name = self.next_synthetic_name("map_arr");
        let arr_slot = self.bind_local_checked(&arr_name, span)?;
        chunk.push(Op::StoreLocal(arr_slot), Some(span));

        self.compile_expr(func, chunk)?;
        let func_name = self.next_synthetic_name("map_fn");
        let func_slot = self.bind_local_checked(&func_name, span)?;
        chunk.push(Op::StoreLocal(func_slot), Some(span));

        let result_name = self.next_synthetic_name("map_res");
        let result_slot = self.bind_local_checked(&result_name, span)?;
        chunk.push(Op::Array(0), Some(span));
        chunk.push(Op::StoreLocal(result_slot), Some(span));

        let idx_name = self.next_synthetic_name("map_idx");
        let idx_slot = self.bind_local_checked(&idx_name, span)?;
        let zero_index = chunk.add_const(Value::Int(0));
        chunk.push(Op::Const(zero_index), Some(span));
        chunk.push(Op::StoreLocal(idx_slot), Some(span));

        let loop_start = chunk.code.len();
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::LoadLocal(arr_slot), Some(span));
        chunk.push(Op::Builtin(Builtin::Len), Some(span));
        chunk.push(Op::Lt, Some(span));
        let jump_if_false = chunk.code.len();
        chunk.push(Op::JumpIfFalse(usize::MAX), Some(span));

        let item_name = self.next_synthetic_name("map_item");
        let item_slot = self.bind_local_checked(&item_name, span)?;
        chunk.push(Op::LoadLocal(arr_slot), Some(span));
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::Index, Some(span));
        chunk.push(Op::StoreLocal(item_slot), Some(span));

        let value_name = self.next_synthetic_name("map_val");
        let value_slot = self.bind_local_checked(&value_name, span)?;
        chunk.push(Op::LoadLocal(func_slot), Some(span));
        chunk.push(Op::LoadLocal(item_slot), Some(span));
        chunk.push(Op::CallValue(1), Some(span));
        chunk.push(Op::StoreLocal(value_slot), Some(span));

        chunk.push(Op::LoadLocal(result_slot), Some(span));
        chunk.push(Op::LoadLocal(value_slot), Some(span));
        chunk.push(Op::Builtin(Builtin::Append), Some(span));
        chunk.push(Op::StoreLocal(result_slot), Some(span));

        let one_index = chunk.add_const(Value::Int(1));
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::Const(one_index), Some(span));
        chunk.push(Op::Add, Some(span));
        chunk.push(Op::StoreLocal(idx_slot), Some(span));
        chunk.push(Op::Jump(loop_start), Some(span));

        let end = chunk.code.len();
        patch_jump(&mut chunk.code, jump_if_false, end);

        chunk.push(Op::LoadLocal(result_slot), Some(span));
        self.pop_scope();
        Ok(())
    }

    fn compile_filter_expr(
        &mut self,
        array: &Expr,
        func: &Expr,
        span: Span,
        chunk: &mut Chunk,
    ) -> Result<(), VmError> {
        self.push_scope();

        self.compile_expr(array, chunk)?;
        let arr_name = self.next_synthetic_name("filter_arr");
        let arr_slot = self.bind_local_checked(&arr_name, span)?;
        chunk.push(Op::StoreLocal(arr_slot), Some(span));

        self.compile_expr(func, chunk)?;
        let func_name = self.next_synthetic_name("filter_fn");
        let func_slot = self.bind_local_checked(&func_name, span)?;
        chunk.push(Op::StoreLocal(func_slot), Some(span));

        let result_name = self.next_synthetic_name("filter_res");
        let result_slot = self.bind_local_checked(&result_name, span)?;
        chunk.push(Op::Array(0), Some(span));
        chunk.push(Op::StoreLocal(result_slot), Some(span));

        let idx_name = self.next_synthetic_name("filter_idx");
        let idx_slot = self.bind_local_checked(&idx_name, span)?;
        let zero_index = chunk.add_const(Value::Int(0));
        chunk.push(Op::Const(zero_index), Some(span));
        chunk.push(Op::StoreLocal(idx_slot), Some(span));

        let loop_start = chunk.code.len();
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::LoadLocal(arr_slot), Some(span));
        chunk.push(Op::Builtin(Builtin::Len), Some(span));
        chunk.push(Op::Lt, Some(span));
        let jump_if_false = chunk.code.len();
        chunk.push(Op::JumpIfFalse(usize::MAX), Some(span));

        let item_name = self.next_synthetic_name("filter_item");
        let item_slot = self.bind_local_checked(&item_name, span)?;
        chunk.push(Op::LoadLocal(arr_slot), Some(span));
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::Index, Some(span));
        chunk.push(Op::StoreLocal(item_slot), Some(span));

        chunk.push(Op::LoadLocal(func_slot), Some(span));
        chunk.push(Op::LoadLocal(item_slot), Some(span));
        chunk.push(Op::CallValue(1), Some(span));
        let skip_append = chunk.code.len();
        chunk.push(Op::JumpIfFalse(usize::MAX), Some(span));

        chunk.push(Op::LoadLocal(result_slot), Some(span));
        chunk.push(Op::LoadLocal(item_slot), Some(span));
        chunk.push(Op::Builtin(Builtin::Append), Some(span));
        chunk.push(Op::StoreLocal(result_slot), Some(span));

        let after_append = chunk.code.len();
        patch_jump(&mut chunk.code, skip_append, after_append);

        let one_index = chunk.add_const(Value::Int(1));
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::Const(one_index), Some(span));
        chunk.push(Op::Add, Some(span));
        chunk.push(Op::StoreLocal(idx_slot), Some(span));
        chunk.push(Op::Jump(loop_start), Some(span));

        let end = chunk.code.len();
        patch_jump(&mut chunk.code, jump_if_false, end);

        chunk.push(Op::LoadLocal(result_slot), Some(span));
        self.pop_scope();
        Ok(())
    }

    fn compile_reduce_expr(
        &mut self,
        array: &Expr,
        initial: &Expr,
        func: &Expr,
        span: Span,
        chunk: &mut Chunk,
    ) -> Result<(), VmError> {
        self.push_scope();

        self.compile_expr(array, chunk)?;
        let arr_name = self.next_synthetic_name("reduce_arr");
        let arr_slot = self.bind_local_checked(&arr_name, span)?;
        chunk.push(Op::StoreLocal(arr_slot), Some(span));

        self.compile_expr(initial, chunk)?;
        let acc_name = self.next_synthetic_name("reduce_acc");
        let acc_slot = self.bind_local_checked(&acc_name, span)?;
        chunk.push(Op::StoreLocal(acc_slot), Some(span));

        self.compile_expr(func, chunk)?;
        let func_name = self.next_synthetic_name("reduce_fn");
        let func_slot = self.bind_local_checked(&func_name, span)?;
        chunk.push(Op::StoreLocal(func_slot), Some(span));

        let idx_name = self.next_synthetic_name("reduce_idx");
        let idx_slot = self.bind_local_checked(&idx_name, span)?;
        let zero_index = chunk.add_const(Value::Int(0));
        chunk.push(Op::Const(zero_index), Some(span));
        chunk.push(Op::StoreLocal(idx_slot), Some(span));

        let loop_start = chunk.code.len();
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::LoadLocal(arr_slot), Some(span));
        chunk.push(Op::Builtin(Builtin::Len), Some(span));
        chunk.push(Op::Lt, Some(span));
        let jump_if_false = chunk.code.len();
        chunk.push(Op::JumpIfFalse(usize::MAX), Some(span));

        let item_name = self.next_synthetic_name("reduce_item");
        let item_slot = self.bind_local_checked(&item_name, span)?;
        chunk.push(Op::LoadLocal(arr_slot), Some(span));
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::Index, Some(span));
        chunk.push(Op::StoreLocal(item_slot), Some(span));

        chunk.push(Op::LoadLocal(func_slot), Some(span));
        chunk.push(Op::LoadLocal(acc_slot), Some(span));
        chunk.push(Op::LoadLocal(item_slot), Some(span));
        chunk.push(Op::CallValue(2), Some(span));
        chunk.push(Op::StoreLocal(acc_slot), Some(span));

        let one_index = chunk.add_const(Value::Int(1));
        chunk.push(Op::LoadLocal(idx_slot), Some(span));
        chunk.push(Op::Const(one_index), Some(span));
        chunk.push(Op::Add, Some(span));
        chunk.push(Op::StoreLocal(idx_slot), Some(span));
        chunk.push(Op::Jump(loop_start), Some(span));

        let end = chunk.code.len();
        patch_jump(&mut chunk.code, jump_if_false, end);

        chunk.push(Op::LoadLocal(acc_slot), Some(span));
        self.pop_scope();
        Ok(())
    }

    fn compile_closure_expr(
        &mut self,
        span: Span,
        params: &[at_syntax::Ident],
        body: &Expr,
        chunk: &mut Chunk,
    ) -> Result<(), VmError> {
        let captures = self.collect_closure_captures(params, body);
        for capture in &captures {
            let slot = self.resolve_local(capture).ok_or_else(|| {
                compile_error(format!("unknown identifier: {}", capture), Some(span))
            })?;
            chunk.push(Op::LoadLocal(slot), Some(span));
        }

        let func_id = self.compile_closure_chunk(span, params, body, &captures)?;
        chunk.push(Op::Closure(func_id, captures.len()), Some(span));
        Ok(())
    }

    fn compile_closure_chunk(
        &mut self,
        span: Span,
        params: &[at_syntax::Ident],
        body: &Expr,
        captures: &[String],
    ) -> Result<usize, VmError> {
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_next_local = self.next_local;
        let saved_loop_stack = std::mem::take(&mut self.loop_stack);
        let saved_current_function = self.current_function.take();

        self.scopes = Vec::new();
        self.next_local = 0;
        self.loop_stack = Vec::new();
        self.push_scope();

        for name in captures {
            self.bind_local_checked(name, span)?;
        }
        for param in params {
            self.bind_local_checked(&param.name, param.span)?;
        }

        let mut closure_chunk = Chunk::default();
        self.compile_expr(body, &mut closure_chunk)?;
        closure_chunk.push(Op::Return, expr_span(body));
        let locals = self.next_local;

        self.scopes = saved_scopes;
        self.next_local = saved_next_local;
        self.loop_stack = saved_loop_stack;
        self.current_function = saved_current_function;

        let closure_name = format!("__closure_{}", self.synthetic_id);
        self.synthetic_id += 1;
        let func_id = self.base_function_count + self.closure_chunks.len();
        self.closure_chunks.push(FunctionChunk {
            name: closure_name,
            params: params.len(),
            captures: captures.len(),
            locals,
            needs: Vec::new(),
            is_async: false,
            chunk: closure_chunk,
        });

        Ok(func_id)
    }

    fn collect_closure_captures(&self, params: &[at_syntax::Ident], body: &Expr) -> Vec<String> {
        let mut bound = Vec::new();
        let mut initial = HashSet::new();
        for param in params {
            initial.insert(param.name.clone());
        }
        bound.push(initial);
        let mut captures = Vec::new();
        let mut seen = HashSet::new();
        self.collect_free_vars_expr(body, &mut bound, &mut captures, &mut seen);
        captures
    }

    fn collect_free_vars_expr(
        &self,
        expr: &Expr,
        bound: &mut Vec<HashSet<String>>,
        captures: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match expr {
            Expr::Int(_, _, _)
            | Expr::Float(_, _, _)
            | Expr::String(_, _, _)
            | Expr::Bool(_, _, _) => {}
            Expr::Range { start, end, .. } => {
                self.collect_free_vars_expr(start, bound, captures, seen);
                self.collect_free_vars_expr(end, bound, captures, seen);
            }
            Expr::Ident(ident) => {
                if self.is_bound(bound, &ident.name) {
                    return;
                }
                if self.resolve_local(&ident.name).is_some() && seen.insert(ident.name.clone()) {
                    captures.push(ident.name.clone());
                }
            }
            Expr::Unary { expr, .. } | Expr::Group { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Expr::Binary { left, right, .. } => {
                self.collect_free_vars_expr(left, bound, captures, seen);
                self.collect_free_vars_expr(right, bound, captures, seen);
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.collect_free_vars_expr(condition, bound, captures, seen);
                self.collect_free_vars_expr(then_branch, bound, captures, seen);
                if let Some(else_expr) = else_branch {
                    self.collect_free_vars_expr(else_expr, bound, captures, seen);
                }
            }
            Expr::Member { base, .. } => {
                self.collect_free_vars_expr(base, bound, captures, seen);
            }
            Expr::Call { callee, args, .. } => {
                self.collect_free_vars_expr(callee, bound, captures, seen);
                for arg in args {
                    self.collect_free_vars_expr(arg, bound, captures, seen);
                }
            }
            Expr::Try(expr, _) => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Expr::Await { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Expr::TryCatch {
                try_block,
                catch_block,
                finally_block,
                ..
            } => {
                self.collect_free_vars_expr(try_block, bound, captures, seen);
                if let Some(catch_block) = catch_block {
                    self.collect_free_vars_expr(catch_block, bound, captures, seen);
                }
                if let Some(finally_block) = finally_block {
                    self.collect_free_vars_expr(finally_block, bound, captures, seen);
                }
            }
            Expr::Match { value, arms, .. } => {
                self.collect_free_vars_expr(value, bound, captures, seen);
                for arm in arms {
                    self.push_bound_scope(bound);
                    self.bind_pattern_names(&arm.pattern, bound);
                    if let Some(guard) = &arm.guard {
                        self.collect_free_vars_expr(guard, bound, captures, seen);
                    }
                    self.collect_free_vars_expr(&arm.body, bound, captures, seen);
                    self.pop_bound_scope(bound);
                }
            }
            Expr::Block { stmts, tail, .. } => {
                self.push_bound_scope(bound);
                for stmt in stmts {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                if let Some(expr) = tail {
                    self.collect_free_vars_expr(expr, bound, captures, seen);
                }
                self.pop_bound_scope(bound);
            }
            Expr::Array { items, .. } | Expr::Tuple { items, .. } => {
                for item in items {
                    self.collect_free_vars_expr(item, bound, captures, seen);
                }
            }
            Expr::ArraySpread { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Expr::MapLiteral { entries, .. } => {
                for (key, value) in entries {
                    self.collect_free_vars_expr(key, bound, captures, seen);
                    self.collect_free_vars_expr(value, bound, captures, seen);
                }
            }
            Expr::MapSpread { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Expr::As { expr, .. } | Expr::Is { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Expr::Index { base, index, .. } => {
                self.collect_free_vars_expr(base, bound, captures, seen);
                self.collect_free_vars_expr(index, bound, captures, seen);
            }
            Expr::InterpolatedString { parts, .. } => {
                for part in parts {
                    if let at_syntax::InterpPart::Expr(expr, _) = part {
                        self.collect_free_vars_expr(expr, bound, captures, seen);
                    }
                }
            }
            Expr::Ternary {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.collect_free_vars_expr(condition, bound, captures, seen);
                self.collect_free_vars_expr(then_branch, bound, captures, seen);
                self.collect_free_vars_expr(else_branch, bound, captures, seen);
            }
            Expr::ChainedComparison { items, .. } => {
                for item in items {
                    self.collect_free_vars_expr(item, bound, captures, seen);
                }
            }
            Expr::EnumLiteral { payload, .. } => {
                if let Some(expr) = payload {
                    self.collect_free_vars_expr(expr, bound, captures, seen);
                }
            }
            Expr::StructLiteral { fields, .. } => {
                for field in fields {
                    self.collect_free_vars_expr(&field.value, bound, captures, seen);
                }
            }
            Expr::Closure { .. } => {}
        }
    }

    fn collect_free_vars_stmt(
        &self,
        stmt: &Stmt,
        bound: &mut Vec<HashSet<String>>,
        captures: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        match stmt {
            Stmt::Import { .. } => {}
            Stmt::TypeAlias { .. } => {}
            Stmt::Enum { .. } => {}
            Stmt::Struct { .. } => {}
            Stmt::Let { name, value, .. } | Stmt::Using { name, value, .. } => {
                self.collect_free_vars_expr(value, bound, captures, seen);
                if let Some(scope) = bound.last_mut() {
                    scope.insert(name.name.clone());
                }
            }
            Stmt::Const { name, value, .. } => {
                self.collect_free_vars_expr(value, bound, captures, seen);
                if let Some(scope) = bound.last_mut() {
                    scope.insert(name.name.clone());
                }
            }
            Stmt::Set { value, .. } => {
                self.collect_free_vars_expr(value, bound, captures, seen);
            }
            Stmt::SetMember { base, value, .. } => {
                self.collect_free_vars_expr(base, bound, captures, seen);
                self.collect_free_vars_expr(value, bound, captures, seen);
            }
            Stmt::SetIndex {
                base, index, value, ..
            } => {
                self.collect_free_vars_expr(base, bound, captures, seen);
                self.collect_free_vars_expr(index, bound, captures, seen);
                self.collect_free_vars_expr(value, bound, captures, seen);
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.collect_free_vars_expr(condition, bound, captures, seen);
                self.push_bound_scope(bound);
                for stmt in body {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                self.pop_bound_scope(bound);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.collect_free_vars_expr(condition, bound, captures, seen);
                self.push_bound_scope(bound);
                for stmt in then_branch {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                self.pop_bound_scope(bound);
                if let Some(else_branch) = else_branch {
                    self.push_bound_scope(bound);
                    for stmt in else_branch {
                        self.collect_free_vars_stmt(stmt, bound, captures, seen);
                    }
                    self.pop_bound_scope(bound);
                }
            }
            Stmt::For {
                item, iter, body, ..
            } => {
                self.collect_free_vars_expr(iter, bound, captures, seen);
                self.push_bound_scope(bound);
                if let Some(scope) = bound.last_mut() {
                    scope.insert(item.name.clone());
                }
                for stmt in body {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                self.pop_bound_scope(bound);
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
            Stmt::Expr { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Stmt::Return { expr, .. } => {
                if let Some(expr) = expr {
                    self.collect_free_vars_expr(expr, bound, captures, seen);
                }
            }
            Stmt::Throw { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Stmt::Defer { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Stmt::With {
                name, value, body, ..
            } => {
                self.collect_free_vars_expr(value, bound, captures, seen);
                self.push_bound_scope(bound);
                if let Some(scope) = bound.last_mut() {
                    scope.insert(name.name.clone());
                }
                for stmt in body {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                self.pop_bound_scope(bound);
            }
            Stmt::Yield { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                self.push_bound_scope(bound);
                for stmt in stmts {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                self.pop_bound_scope(bound);
            }
        }
    }

    fn bind_pattern_names(
        &self,
        pattern: &at_syntax::MatchPattern,
        bound: &mut Vec<HashSet<String>>,
    ) {
        let name = match pattern {
            at_syntax::MatchPattern::ResultOk(ident, _)
            | at_syntax::MatchPattern::ResultErr(ident, _)
            | at_syntax::MatchPattern::OptionSome(ident, _) => Some(&ident.name),
            at_syntax::MatchPattern::Binding { name, .. } => Some(&name.name),
            at_syntax::MatchPattern::Int(_, _)
            | at_syntax::MatchPattern::Bool(_, _)
            | at_syntax::MatchPattern::String(_, _)
            | at_syntax::MatchPattern::OptionNone(_)
            | at_syntax::MatchPattern::Tuple { .. }
            | at_syntax::MatchPattern::Wildcard(_) => None,
            at_syntax::MatchPattern::Struct { .. } => None,
            at_syntax::MatchPattern::Enum { binding, .. } => binding.as_ref().map(|b| &b.name),
        };
        if let Some(name) = name {
            if let Some(scope) = bound.last_mut() {
                scope.insert(name.clone());
            }
        }

        match pattern {
            at_syntax::MatchPattern::Struct { fields, .. } => {
                if let Some(scope) = bound.last_mut() {
                    for field in fields {
                        let binding = field.binding.as_ref().unwrap_or(&field.name);
                        if binding.name != "_" {
                            scope.insert(binding.name.clone());
                        }
                    }
                }
            }
            at_syntax::MatchPattern::Enum { binding, .. } => {
                if let (Some(scope), Some(binding)) = (bound.last_mut(), binding.as_ref()) {
                    if binding.name != "_" {
                        scope.insert(binding.name.clone());
                    }
                }
            }
            at_syntax::MatchPattern::Tuple { items, .. } => {
                for item in items {
                    self.bind_pattern_names(item, bound);
                }
            }
            at_syntax::MatchPattern::Binding { name, pattern, .. } => {
                if let Some(scope) = bound.last_mut() {
                    scope.insert(name.name.clone());
                }
                self.bind_pattern_names(pattern, bound);
            }
            _ => {}
        }
    }

    fn push_bound_scope(&self, bound: &mut Vec<HashSet<String>>) {
        bound.push(HashSet::new());
    }

    fn pop_bound_scope(&self, bound: &mut Vec<HashSet<String>>) {
        bound.pop();
    }

    fn is_bound(&self, bound: &[HashSet<String>], name: &str) -> bool {
        bound.iter().rev().any(|scope| scope.contains(name))
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.const_scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.const_scopes.pop();
    }

    fn bind_local_checked(&mut self, name: &str, span: Span) -> Result<usize, VmError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                return Err(compile_error(
                    format!("duplicate local: {name}"),
                    Some(span),
                ));
            }
            let slot = self.next_local;
            self.next_local += 1;
            scope.insert(name.to_string(), slot);
            return Ok(slot);
        }
        let slot = self.next_local;
        self.next_local += 1;
        Ok(slot)
    }

    fn bind_const(&mut self, name: &str) {
        if let Some(scope) = self.const_scopes.last_mut() {
            scope.insert(name.to_string());
        }
    }

    fn is_const(&self, name: &str) -> bool {
        self.const_scopes
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    fn next_synthetic_name(&mut self, prefix: &str) -> String {
        loop {
            let name = format!("__at_{}_{}", prefix, self.synthetic_id);
            self.synthetic_id += 1;
            if self.resolve_local(&name).is_none() {
                return name;
            }
        }
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(slot) = scope.get(name) {
                return Some(*slot);
            }
        }
        None
    }

    fn type_check_from_ref(&self, ty: &at_syntax::TypeRef) -> Result<TypeCheck, VmError> {
        match ty {
            at_syntax::TypeRef::Qualified { ty, .. } => self.type_check_from_ref(ty),
            at_syntax::TypeRef::Named { name, .. } => match name.name.as_str() {
                "int" => Ok(TypeCheck::Int),
                "float" => Ok(TypeCheck::Float),
                "string" => Ok(TypeCheck::String),
                "bool" => Ok(TypeCheck::Bool),
                "unit" => Ok(TypeCheck::Unit),
                "array" => Ok(TypeCheck::Array),
                "map" => Ok(TypeCheck::Map),
                "tuple" => Ok(TypeCheck::Tuple),
                "option" => Ok(TypeCheck::Option),
                "result" => Ok(TypeCheck::Result),
                other => {
                    if self.enums.contains(other) {
                        Ok(TypeCheck::Enum(other.to_string()))
                    } else if self.structs.contains(other) {
                        Ok(TypeCheck::Struct)
                    } else {
                        Err(compile_error(
                            format!("unknown type: {other}"),
                            Some(name.span),
                        ))
                    }
                }
            },
            at_syntax::TypeRef::Tuple { .. } => Ok(TypeCheck::Tuple),
            at_syntax::TypeRef::Union { .. } | at_syntax::TypeRef::Intersection { .. } => {
                Err(compile_error(
                    "cannot cast or check union/intersection type".to_string(),
                    None,
                ))
            }
            at_syntax::TypeRef::Function { .. } => Err(compile_error(
                "cannot cast or check function type".to_string(),
                None,
            )),
        }
    }
}

struct Frame {
    chunk_id: usize,
    ip: usize,
    locals: Vec<Value>,
    capabilities: HashSet<String>,
    defers: Vec<usize>,
    capability_scopes: Vec<Vec<String>>,
}

pub struct Vm {
    stack: Vec<Value>,
    frames: Vec<Frame>,
    locals_pool: Vec<Vec<Value>>,
    initial_capabilities: HashSet<String>,
    instruction_count: usize,
    max_instructions: Option<usize>,
    max_frames: Option<usize>,
    output_buffer: Option<Rc<RefCell<Vec<String>>>>,
}

impl Vm {
    fn run_future(&mut self, program: &Program, future: &FutureValue) -> Result<Value, VmError> {
        let func = program.functions.get(future.func_id).ok_or_else(|| {
            self.runtime_error(
                format!("invalid function id: {}", future.func_id),
                None,
                program,
            )
        })?;
        if !func.is_async {
            return Err(self.runtime_error(
                "await expects async function".to_string(),
                None,
                program,
            ));
        }
        if let Some(max) = self.max_frames {
            if self.frames.len() >= max {
                return Err(self.runtime_error(
                    format!("stack overflow: maximum call depth {} exceeded", max),
                    None,
                    program,
                ));
            }
        }
        let required_locals = func.params + func.captures;
        let mut locals = self.take_locals(func.locals.max(required_locals));
        for (idx, value) in future.captures.iter().cloned().enumerate() {
            locals[idx] = value;
        }
        for (idx, value) in future.args.iter().cloned().enumerate() {
            locals[func.captures + idx] = value;
        }
        let capabilities = future.capabilities.clone();
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_frames = std::mem::take(&mut self.frames);
        let saved_instruction_count = self.instruction_count;

        self.stack = Vec::new();
        self.frames = Vec::new();
        self.instruction_count = 0;
        self.frames.push(Frame {
            chunk_id: future.func_id,
            ip: 0,
            locals,
            capabilities,
            defers: Vec::new(),
            capability_scopes: Vec::new(),
        });
        let result = self
            .run_with_existing_frames(program)?
            .unwrap_or(Value::Unit);

        self.stack = saved_stack;
        self.frames = saved_frames;
        self.instruction_count = saved_instruction_count;
        Ok(result)
    }
    fn runtime_error(&self, message: String, span: Option<Span>, program: &Program) -> VmError {
        VmError::Runtime {
            message,
            span,
            stack: Some(self.build_stack_trace(program)),
        }
    }

    fn build_stack_trace(&self, program: &Program) -> Vec<StackFrame> {
        self.frames
            .iter()
            .rev()
            .map(|frame| {
                let (name, span) = if frame.chunk_id == usize::MAX {
                    ("<main>".to_string(), span_at(&program.main, frame.ip))
                } else if let Some(func) = program.functions.get(frame.chunk_id) {
                    (func.name.clone(), span_at(&func.chunk, frame.ip))
                } else {
                    ("<unknown>".to_string(), None)
                };
                StackFrame { name, span }
            })
            .collect()
    }

    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            locals_pool: Vec::new(),
            initial_capabilities: HashSet::new(),
            instruction_count: 0,
            max_instructions: None,
            max_frames: None,
            output_buffer: None,
        }
    }

    pub fn with_capabilities(capabilities: HashSet<String>) -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            locals_pool: Vec::new(),
            initial_capabilities: capabilities,
            instruction_count: 0,
            max_instructions: None,
            max_frames: None,
            output_buffer: None,
        }
    }

    pub fn with_execution_limit(max_instructions: usize) -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            locals_pool: Vec::new(),
            initial_capabilities: HashSet::new(),
            instruction_count: 0,
            max_instructions: Some(max_instructions),
            max_frames: None,
            output_buffer: None,
        }
    }

    pub fn with_output_capture() -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            locals_pool: Vec::new(),
            initial_capabilities: HashSet::new(),
            instruction_count: 0,
            max_instructions: None,
            max_frames: None,
            output_buffer: Some(Rc::new(RefCell::new(Vec::new()))),
        }
    }

    pub fn get_output(&self) -> Option<Vec<String>> {
        self.output_buffer.as_ref().map(|buf| buf.borrow().clone())
    }

    pub fn with_execution_limit_and_output(max_instructions: usize) -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            locals_pool: Vec::new(),
            initial_capabilities: HashSet::new(),
            instruction_count: 0,
            max_instructions: Some(max_instructions),
            max_frames: None,
            output_buffer: Some(Rc::new(RefCell::new(Vec::new()))),
        }
    }

    pub fn with_limits(max_instructions: usize, max_frames: usize) -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            locals_pool: Vec::new(),
            initial_capabilities: HashSet::new(),
            instruction_count: 0,
            max_instructions: Some(max_instructions),
            max_frames: Some(max_frames),
            output_buffer: Some(Rc::new(RefCell::new(Vec::new()))),
        }
    }

    pub fn run(&mut self, program: &Program) -> Result<Option<Value>, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.instruction_count = 0;
        let locals = self.take_locals(program.main_locals);
        let capabilities = self.initial_capabilities.clone();
        self.frames.push(Frame {
            chunk_id: usize::MAX,
            ip: 0,
            locals,
            capabilities,
            defers: Vec::new(),
            capability_scopes: Vec::new(),
        });
        self.run_with_existing_frames(program)
    }

    fn run_with_existing_frames(&mut self, program: &Program) -> Result<Option<Value>, VmError> {
        loop {
            // Check execution limit
            if let Some(max) = self.max_instructions {
                if self.instruction_count >= max {
                    return Err(VmError::ExecutionLimit {
                        message: format!("execution limit exceeded: {} instructions", max),
                    });
                }
            }
            self.instruction_count += 1;

            let frame_index = match self.frames.len() {
                0 => return Ok(self.stack.pop()),
                len => len - 1,
            };
            let frame_chunk_id = self.frames[frame_index].chunk_id;
            let frame_ip = self.frames[frame_index].ip;

            let chunk = if frame_chunk_id == usize::MAX {
                &program.main
            } else {
                match program
                    .functions
                    .get(frame_chunk_id)
                    .map(|func| &func.chunk)
                {
                    Some(chunk) => chunk,
                    None => {
                        return Err(self.runtime_error(
                            format!("invalid function id: {}", frame_chunk_id),
                            None,
                            program,
                        ))
                    }
                }
            };

            if frame_ip >= chunk.code.len() {
                let result = self.stack.pop();
                if frame_chunk_id != usize::MAX {
                    let defers = std::mem::take(&mut self.frames[frame_index].defers);
                    let parent_capabilities = self.frames[frame_index].capabilities.clone();
                    self.frames.pop();
                    for func_id in defers.into_iter().rev() {
                        let locals = self.take_locals(0);
                        self.frames.push(Frame {
                            chunk_id: func_id,
                            ip: 0,
                            locals,
                            capabilities: parent_capabilities.clone(),
                            defers: Vec::new(),
                            capability_scopes: Vec::new(),
                        });
                        let _ = self.run_with_existing_frames(program)?;
                    }
                }
                return Ok(result);
            }

            set_stack_trace(self.build_stack_trace(program));
            set_current_span(span_at(chunk, frame_ip));

            let mut advance = true;
            match &chunk.code[frame_ip] {
                Op::Const(index) => {
                    let value = chunk
                        .constants
                        .get(*index)
                        .cloned()
                        .ok_or(VmError::StackUnderflow)?;
                    self.stack.push(value);
                }
                Op::LoadLocal(slot) => {
                    let value = self.frames[frame_index]
                        .locals
                        .get(*slot)
                        .cloned()
                        .ok_or_else(|| {
                            runtime_error_at(
                                format!("invalid local: {}", slot),
                                span_at(chunk, frame_ip),
                            )
                        })?;
                    self.stack.push(value);
                }
                Op::StoreLocal(slot) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    if *slot >= self.frames[frame_index].locals.len() {
                        return Err(runtime_error_at(
                            format!("invalid local: {}", slot),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    self.frames[frame_index].locals[*slot] = value;
                }
                Op::GrantCapability(name) => {
                    self.frames[frame_index].capabilities.insert(name.clone());
                    if let Some(scope) = self.frames[frame_index].capability_scopes.last_mut() {
                        scope.push(name.clone());
                    }
                }
                Op::PushCapabilityScope => {
                    self.frames[frame_index].capability_scopes.push(Vec::new());
                }
                Op::PopCapabilityScope => {
                    if let Some(scope) = self.frames[frame_index].capability_scopes.pop() {
                        for name in scope {
                            self.frames[frame_index].capabilities.remove(&name);
                        }
                    }
                }
                Op::Closure(func_id, capture_count) => {
                    let mut captures = Vec::with_capacity(*capture_count);
                    for _ in 0..*capture_count {
                        captures.push(self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?);
                    }
                    captures.reverse();
                    self.stack.push(Value::Closure(Rc::new(ClosureValue {
                        func_id: *func_id,
                        captures,
                    })));
                }
                Op::Call(func_id, arg_count) => {
                    let func = match program.functions.get(*func_id) {
                        Some(func) => func,
                        None => {
                            return Err(runtime_error_at(
                                format!("invalid function id: {}", func_id),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                    if *arg_count != func.params {
                        return Err(runtime_error_at(
                            format!(
                                "wrong arity: expected {} args, got {}",
                                func.params, arg_count
                            ),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    for need in &func.needs {
                        if !self.frames[frame_index].capabilities.contains(need) {
                            return Err(runtime_error_at(
                                format!("missing capability: {}", need),
                                span_at(chunk, frame_ip),
                            ));
                        }
                    }
                    if let Some(max) = self.max_frames {
                        if self.frames.len() >= max {
                            return Err(runtime_error_at(
                                format!("stack overflow: maximum call depth {} exceeded", max),
                                span_at(chunk, frame_ip),
                            ));
                        }
                    }
                    let mut locals = self.take_locals(func.locals.max(*arg_count + func.captures));
                    let mut index = *arg_count;
                    while index > 0 {
                        index -= 1;
                        locals[index] = self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?;
                    }
                    let capabilities = self.frames[frame_index].capabilities.clone();
                    self.frames[frame_index].ip = frame_ip + 1;
                    advance = false;
                    self.frames.push(Frame {
                        chunk_id: *func_id,
                        ip: 0,
                        locals,
                        capabilities,
                        defers: Vec::new(),
                        capability_scopes: Vec::new(),
                    });
                }
                Op::CallValue(arg_count) => {
                    let mut args = Vec::with_capacity(*arg_count);
                    let mut index = *arg_count;
                    while index > 0 {
                        index -= 1;
                        args.push(self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?);
                    }
                    args.reverse();

                    let callee = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let closure = match callee {
                        Value::Closure(value) => value,
                        Value::Future(future) => {
                            if *arg_count != 0 {
                                return Err(runtime_error_at(
                                    "wrong arity: expected 0 args for future".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let result = self.run_future(program, &future)?;
                            self.stack.push(result);
                            continue;
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "call expects closure".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                    let func = match program.functions.get(closure.func_id) {
                        Some(func) => func,
                        None => {
                            return Err(runtime_error_at(
                                format!("invalid function id: {}", closure.func_id),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                    if *arg_count != func.params {
                        return Err(runtime_error_at(
                            format!(
                                "wrong arity: expected {} args, got {}",
                                func.params, arg_count
                            ),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    if let Some(max) = self.max_frames {
                        if self.frames.len() >= max {
                            return Err(runtime_error_at(
                                format!("stack overflow: maximum call depth {} exceeded", max),
                                span_at(chunk, frame_ip),
                            ));
                        }
                    }
                    let required_locals = func.params + func.captures;
                    let mut locals = self.take_locals(func.locals.max(required_locals));
                    for (idx, value) in closure.captures.iter().cloned().enumerate() {
                        locals[idx] = value;
                    }
                    for (idx, value) in args.into_iter().enumerate() {
                        locals[func.captures + idx] = value;
                    }
                    let capabilities = self.frames[frame_index].capabilities.clone();
                    self.frames[frame_index].ip = frame_ip + 1;
                    advance = false;
                    self.frames.push(Frame {
                        chunk_id: closure.func_id,
                        ip: 0,
                        locals,
                        capabilities,
                        defers: Vec::new(),
                        capability_scopes: Vec::new(),
                    });
                }
                Op::CallAsync(func_id, arg_count) => {
                    let mut args = Vec::with_capacity(*arg_count);
                    let mut index = *arg_count;
                    while index > 0 {
                        index -= 1;
                        args.push(self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?);
                    }
                    args.reverse();
                    let func = program.functions.get(*func_id).ok_or_else(|| {
                        runtime_error_at(
                            format!("invalid function id: {func_id}"),
                            span_at(chunk, frame_ip),
                        )
                    })?;
                    if *arg_count != func.params {
                        return Err(runtime_error_at(
                            format!(
                                "wrong arity: expected {} args, got {}",
                                func.params, arg_count
                            ),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    let future = FutureValue {
                        func_id: *func_id,
                        captures: Vec::new(),
                        args,
                        capabilities: self.frames[frame_index].capabilities.clone(),
                    };
                    self.stack.push(Value::Future(Rc::new(future)));
                }
                Op::Builtin(builtin) => {
                    let result = match builtin {
                        Builtin::TimeNow => {
                            if !self.frames[frame_index].capabilities.contains("time") {
                                return Err(runtime_error_at(
                                    "missing capability: time".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let now = SystemTime::now()
                                .duration_since(UNIX_EPOCH)
                                .unwrap()
                                .as_secs() as i64;
                            Value::Int(now)
                        }
                        Builtin::TimeFixed => {
                            let _ = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            Value::String(Rc::new("time.fixed".to_string()))
                        }
                        Builtin::RngDeterministic => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::Int(seed) => Value::Int(seed),
                                _ => {
                                    return Err(runtime_error_at(
                                        "invalid builtin usage".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::RngUuid => {
                            if !self.frames[frame_index].capabilities.contains("rng") {
                                return Err(runtime_error_at(
                                    "missing capability: rng".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            Value::String(Rc::new(Uuid::new_v4().to_string()))
                        }
                        Builtin::AssertEq => {
                            let right = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let left = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            if left == right {
                                Value::Unit
                            } else {
                                return Err(runtime_error_at(
                                    "assert_eq failed".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        Builtin::OptionSome => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            Value::Option(Some(Rc::new(value)))
                        }
                        Builtin::OptionNone => Value::Option(None),
                        Builtin::ResultOk => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            Value::Result(Ok(Rc::new(value)))
                        }
                        Builtin::ResultErr => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            Value::Result(Err(Rc::new(value)))
                        }
                        Builtin::IsSome => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::Option(Some(_)) => Value::Bool(true),
                                Value::Option(None) => Value::Bool(false),
                                _ => {
                                    return Err(runtime_error_at(
                                        "invalid builtin usage".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::IsNone => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::Option(Some(_)) => Value::Bool(false),
                                Value::Option(None) => Value::Bool(true),
                                _ => {
                                    return Err(runtime_error_at(
                                        "invalid builtin usage".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::IsOk => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::Result(Ok(_)) => Value::Bool(true),
                                Value::Result(Err(_)) => Value::Bool(false),
                                _ => {
                                    return Err(runtime_error_at(
                                        "invalid builtin usage".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::IsErr => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::Result(Ok(_)) => Value::Bool(false),
                                Value::Result(Err(_)) => Value::Bool(true),
                                _ => {
                                    return Err(runtime_error_at(
                                        "invalid builtin usage".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Print => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let output = match &value {
                                Value::String(text) => text.to_string(),
                                _ => format_value(&value),
                            };
                            // Write to output buffer if configured, otherwise print to stdout
                            if let Some(ref buf) = self.output_buffer {
                                buf.borrow_mut().push(output);
                            } else {
                                println!("{}", output);
                            }
                            Value::Unit
                        }
                        Builtin::Len => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::Array(items) => Value::Int(items.len() as i64),
                                Value::String(value) => Value::Int(value.len() as i64),
                                _ => {
                                    return Err(runtime_error_at(
                                        "len expects array or string".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Append => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let array = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match array {
                                Value::Array(items) => {
                                    let mut items = items.clone();
                                    Rc::make_mut(&mut items).push(value);
                                    Value::Array(items)
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "append expects array".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Contains => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let array = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match array {
                                Value::Array(items) => {
                                    Value::Bool(items.iter().any(|item| item == &value))
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "contains expects array".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Slice => {
                            let end = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let start = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let array = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let (start, end) = match (start, end) {
                                (Value::Int(start), Value::Int(end)) => (start, end),
                                _ => {
                                    return Err(runtime_error_at(
                                        "slice expects int bounds".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            };
                            if start < 0 || end < start {
                                return Err(runtime_error_at(
                                    "slice out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            match array {
                                Value::Array(items) => {
                                    let start = start as usize;
                                    let end = end as usize;
                                    if end > items.len() {
                                        return Err(runtime_error_at(
                                            "slice out of bounds".to_string(),
                                            span_at(chunk, frame_ip),
                                        ));
                                    }
                                    Value::Array(Rc::new(items[start..end].to_vec()))
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "slice expects array".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                    };
                    self.stack.push(result);
                }
                Op::Assert => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Int(0) => {
                            return Err(self.runtime_error(
                                "assert failed".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                        Value::Int(_) => {}
                        Value::Bool(false) => {
                            return Err(self.runtime_error(
                                "assert failed".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                        Value::Bool(true) => {}
                        _ => {
                            return Err(self.runtime_error(
                                "assert expects bool or int".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                    self.stack.push(Value::Unit);
                }
                Op::Try => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Result(Ok(inner)) => {
                            self.stack.push((*inner).clone());
                        }
                        Value::Result(Err(inner)) => {
                            let span = self.current_span(program);
                            let message = match inner.as_ref() {
                                Value::String(text) => text.as_ref().clone(),
                                other => format_value(other),
                            };
                            return Err(self.runtime_error(message, span, program));
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "? expects result".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::Throw => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Result(Err(inner)) => {
                            let span = self.current_span(program);
                            let message = match inner.as_ref() {
                                Value::String(text) => text.as_ref().clone(),
                                other => format_value(other),
                            };
                            return Err(self.runtime_error(message, span, program));
                        }
                        Value::Result(Ok(inner)) => {
                            self.stack.push(Value::Result(Ok(inner)));
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "throw expects result".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::Defer => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Closure(closure) => {
                            self.frames[frame_index].defers.push(closure.func_id);
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "defer expects closure".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::Await => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Future(future) => {
                            let result = self.run_future(program, &future)?;
                            self.stack.push(result);
                        }
                        other => {
                            return Err(self.runtime_error(
                                format!("await expects future, got {}", format_value(&other)),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::Add => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            if let Some(value) = left.checked_add(right) {
                                self.stack.push(Value::Int(value));
                            } else {
                                return Err(runtime_error_at(
                                    "integer overflow".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.stack.push(Value::Float(left + right));
                        }
                        (Value::String(left), Value::String(right)) => {
                            let mut combined = String::with_capacity(left.len() + right.len());
                            combined.push_str(&left);
                            combined.push_str(&right);
                            self.stack.push(Value::String(Rc::new(combined)));
                        }
                        (Value::String(left), right) => {
                            let right_str = format_value(&right).trim_matches('"').to_string();
                            let mut combined = String::with_capacity(left.len() + right_str.len());
                            combined.push_str(&left);
                            combined.push_str(&right_str);
                            self.stack.push(Value::String(Rc::new(combined)));
                        }
                        (left, Value::String(right)) => {
                            let left_str = format_value(&left).trim_matches('"').to_string();
                            let mut combined = String::with_capacity(left_str.len() + right.len());
                            combined.push_str(&left_str);
                            combined.push_str(&right);
                            self.stack.push(Value::String(Rc::new(combined)));
                        }
                        (left, right) => {
                            let left_str = format_value(&left);
                            let right_str = format_value(&right);
                            let mut combined =
                                String::with_capacity(left_str.len() + right_str.len());
                            combined.push_str(&left_str);
                            combined.push_str(&right_str);
                            self.stack.push(Value::String(Rc::new(combined)));
                        }
                    };
                }
                Op::Sub => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            if let Some(value) = left.checked_sub(right) {
                                self.stack.push(Value::Int(value));
                            } else {
                                return Err(runtime_error_at(
                                    "integer overflow".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.stack.push(Value::Float(left - right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid binary operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                }
                Op::Mul => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            if let Some(value) = left.checked_mul(right) {
                                self.stack.push(Value::Int(value));
                            } else {
                                return Err(runtime_error_at(
                                    "integer overflow".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.stack.push(Value::Float(left * right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid binary operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                }
                Op::Div => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            if right == 0 {
                                return Err(runtime_error_at(
                                    "division by zero".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            if let Some(value) = left.checked_div(right) {
                                self.stack.push(Value::Int(value));
                            } else {
                                return Err(runtime_error_at(
                                    "integer overflow".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            if right == 0.0 {
                                return Err(runtime_error_at(
                                    "division by zero".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            self.stack.push(Value::Float(left / right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid binary operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                }
                Op::Mod => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            if right == 0 {
                                return Err(runtime_error_at(
                                    "division by zero".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            if let Some(value) = left.checked_rem(right) {
                                self.stack.push(Value::Int(value));
                            } else {
                                return Err(runtime_error_at(
                                    "integer overflow".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            if right == 0.0 {
                                return Err(runtime_error_at(
                                    "division by zero".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            self.stack.push(Value::Float(left % right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid binary operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                }
                Op::Array(count) => {
                    if self.stack.len() < *count {
                        return Err(runtime_error_at(
                            "stack underflow".to_string(),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    let mut items = Vec::with_capacity(*count);
                    for _ in 0..*count {
                        items.push(self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?);
                    }
                    items.reverse();
                    self.stack.push(Value::Array(Rc::new(items)));
                }
                Op::Tuple(count) => {
                    if self.stack.len() < *count {
                        return Err(runtime_error_at(
                            "stack underflow".to_string(),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    let mut items = Vec::with_capacity(*count);
                    for _ in 0..*count {
                        items.push(self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?);
                    }
                    items.reverse();
                    self.stack.push(Value::Tuple(Rc::new(items)));
                }
                Op::Range(inclusive) => {
                    let end_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let start_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let (start, end) = match (start_value, end_value) {
                        (Value::Int(start), Value::Int(end)) => (start, end),
                        _ => {
                            return Err(runtime_error_at(
                                "range expects int bounds".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                    let end_val = if *inclusive { end } else { end - 1 };
                    let mut items = Vec::new();
                    let mut current = start;
                    while current <= end_val {
                        items.push(Value::Int(current));
                        current += 1;
                    }
                    self.stack.push(Value::Array(Rc::new(items)));
                }
                Op::Map(count) => {
                    if self.stack.len() < *count * 2 {
                        return Err(runtime_error_at(
                            "stack underflow".to_string(),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    let mut entries = Vec::with_capacity(*count);
                    for _ in 0..*count {
                        let value = self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?;
                        let key = self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?;
                        entries.push((key, value));
                    }
                    entries.reverse();
                    self.stack.push(Value::Map(Rc::new(entries)));
                }
                Op::ArraySpread(count) => {
                    if self.stack.len() < *count {
                        return Err(runtime_error_at(
                            "stack underflow".to_string(),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    let mut chunks = Vec::with_capacity(*count);
                    for _ in 0..*count {
                        let value = self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?;
                        chunks.push(value);
                    }
                    chunks.reverse();
                    let mut items = Vec::new();
                    for item in chunks {
                        match item {
                            Value::Array(values) | Value::Tuple(values) => {
                                items.extend(values.iter().cloned());
                            }
                            _ => {
                                return Err(runtime_error_at(
                                    "spread expects array or tuple".to_string(),
                                    span_at(chunk, frame_ip),
                                ))
                            }
                        }
                    }
                    self.stack.push(Value::Array(Rc::new(items)));
                }
                Op::MapSpread(count) => {
                    if self.stack.len() < *count {
                        return Err(runtime_error_at(
                            "stack underflow".to_string(),
                            span_at(chunk, frame_ip),
                        ));
                    }
                    let mut chunks = Vec::with_capacity(*count);
                    for _ in 0..*count {
                        let value = self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?;
                        chunks.push(value);
                    }
                    chunks.reverse();
                    let mut entries = Vec::new();
                    for item in chunks {
                        match item {
                            Value::Map(values) => {
                                entries.extend(values.iter().cloned());
                            }
                            _ => {
                                return Err(runtime_error_at(
                                    "spread expects map".to_string(),
                                    span_at(chunk, frame_ip),
                                ))
                            }
                        }
                    }
                    self.stack.push(Value::Map(Rc::new(entries)));
                }
                Op::Index => {
                    let index_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let base_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match base_value {
                        Value::Array(items) | Value::Tuple(items) => {
                            let index = match index_value {
                                Value::Int(value) => value,
                                _ => {
                                    return Err(runtime_error_at(
                                        "index expects int".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            };
                            if index < 0 {
                                return Err(runtime_error_at(
                                    "index out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let idx = index as usize;
                            let value = items.get(idx).cloned().ok_or_else(|| {
                                runtime_error_at(
                                    "index out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            self.stack.push(value);
                        }
                        Value::Map(entries) => {
                            let mut found = None;
                            for (key, value) in entries.iter() {
                                if *key == index_value {
                                    found = Some(value.clone());
                                    break;
                                }
                            }
                            if let Some(value) = found {
                                self.stack.push(value);
                            } else {
                                return Err(runtime_error_at(
                                    "map key not found".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "index expects array, tuple, or map".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::StoreIndex => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let index_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let base_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match base_value {
                        Value::Array(items) => {
                            let index = match index_value {
                                Value::Int(value) => value,
                                _ => {
                                    return Err(runtime_error_at(
                                        "set index expects int".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            };
                            if index < 0 {
                                return Err(runtime_error_at(
                                    "index out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let idx = index as usize;
                            if idx >= items.len() {
                                return Err(runtime_error_at(
                                    "index out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let mut new_items = (*items).clone();
                            new_items[idx] = value;
                            self.stack.push(Value::Array(Rc::new(new_items)));
                        }
                        Value::Tuple(items) => {
                            let index = match index_value {
                                Value::Int(value) => value,
                                _ => {
                                    return Err(runtime_error_at(
                                        "set index expects int".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            };
                            if index < 0 {
                                return Err(runtime_error_at(
                                    "index out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let idx = index as usize;
                            if idx >= items.len() {
                                return Err(runtime_error_at(
                                    "index out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let mut new_items = (*items).clone();
                            new_items[idx] = value;
                            self.stack.push(Value::Tuple(Rc::new(new_items)));
                        }
                        Value::Map(entries) => {
                            let mut new_entries = (*entries).clone();
                            let mut updated = false;
                            for (key, existing) in new_entries.iter_mut() {
                                if *key == index_value {
                                    *existing = value.clone();
                                    updated = true;
                                    break;
                                }
                            }
                            if !updated {
                                new_entries.push((index_value, value));
                            }
                            self.stack.push(Value::Map(Rc::new(new_entries)));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "set index expects array, tuple, or map".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::Struct(fields) => {
                    let mut map = BTreeMap::new();
                    for name in fields.iter().rev() {
                        let value = self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?;
                        map.insert(name.clone(), value);
                    }
                    self.stack.push(Value::Struct(Rc::new(map)));
                }
                Op::Enum {
                    name,
                    variant,
                    has_payload,
                } => {
                    let payload = if *has_payload {
                        Some(Rc::new(self.stack.pop().ok_or_else(|| {
                            runtime_error_at(
                                "stack underflow".to_string(),
                                span_at(chunk, frame_ip),
                            )
                        })?))
                    } else {
                        None
                    };
                    self.stack.push(Value::Enum {
                        name: name.clone(),
                        variant: variant.clone(),
                        payload,
                    });
                }
                Op::GetMember(field) => {
                    let base_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match base_value {
                        Value::Struct(fields) => {
                            if let Some(value) = fields.get(field) {
                                self.stack.push(value.clone());
                            } else {
                                return Err(runtime_error_at(
                                    format!("unknown field: {}", field),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "member access expects struct".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::StoreMember(field) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let base_value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match base_value {
                        Value::Array(items) => {
                            let mut new_items = (*items).clone();
                            let mut pending = Some(value);
                            for item in &mut new_items {
                                match item {
                                    Value::Tuple(tuple_items) => {
                                        if tuple_items.len() != 2 {
                                            return Err(runtime_error_at(
                                                "set member expects array of (string, value) tuples"
                                                    .to_string(),
                                                span_at(chunk, frame_ip),
                                            ));
                                        }
                                        if let Value::String(key) = &tuple_items[0] {
                                            if key.as_str() == field {
                                                let value = pending.take().ok_or_else(|| {
                                                    runtime_error_at(
                                                        "set member internal error".to_string(),
                                                        span_at(chunk, frame_ip),
                                                    )
                                                })?;
                                                *item = Value::Tuple(Rc::new(vec![
                                                    Value::String(Rc::new(field.clone())),
                                                    value,
                                                ]));
                                                break;
                                            }
                                        } else {
                                            return Err(runtime_error_at(
                                                "set member expects array of (string, value) tuples"
                                                    .to_string(),
                                                span_at(chunk, frame_ip),
                                            ));
                                        }
                                    }
                                    _ => {
                                        return Err(runtime_error_at(
                                            "set member expects array of (string, value) tuples"
                                                .to_string(),
                                            span_at(chunk, frame_ip),
                                        ))
                                    }
                                }
                            }
                            if let Some(value) = pending {
                                new_items.push(Value::Tuple(Rc::new(vec![
                                    Value::String(Rc::new(field.clone())),
                                    value,
                                ])));
                            }
                            self.stack.push(Value::Array(Rc::new(new_items)));
                        }
                        Value::Struct(fields) => {
                            let mut new_fields = (*fields).clone();
                            new_fields.insert(field.clone(), value);
                            self.stack.push(Value::Struct(Rc::new(new_fields)));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "set member expects struct or array of (string, value) tuples"
                                    .to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::Eq => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    self.stack.push(Value::Bool(left == right));
                }
                Op::Neq => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    self.stack.push(Value::Bool(left != right));
                }
                Op::Neg => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Int(value) => self.stack.push(Value::Int(-value)),
                        Value::Float(value) => self.stack.push(Value::Float(-value)),
                        _ => {
                            return Err(runtime_error_at(
                                "invalid unary operand".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::Not => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let value = match value {
                        Value::Bool(value) => Value::Bool(!value),
                        Value::Int(value) => Value::Bool(value == 0),
                        Value::Float(value) => Value::Bool(value == 0.0),
                        _ => {
                            return Err(runtime_error_at(
                                "invalid unary operand".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                    self.stack.push(value);
                }
                Op::Lt => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.stack.push(Value::Bool(left < right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.stack.push(Value::Bool(left < right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid comparison operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::Lte => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.stack.push(Value::Bool(left <= right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.stack.push(Value::Bool(left <= right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid comparison operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::Gt => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.stack.push(Value::Bool(left > right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.stack.push(Value::Bool(left > right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid comparison operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::Gte => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            self.stack.push(Value::Bool(left >= right));
                        }
                        (Value::Float(left), Value::Float(right)) => {
                            self.stack.push(Value::Bool(left >= right));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "invalid comparison operands".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                op @ Op::BitAnd
                | op @ Op::BitOr
                | op @ Op::BitXor
                | op @ Op::Shl
                | op @ Op::Shr => {
                    let right = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let left = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            let result = match op {
                                Op::BitAnd => left & right,
                                Op::BitOr => left | right,
                                Op::BitXor => left ^ right,
                                Op::Shl => left << right,
                                Op::Shr => left >> right,
                                _ => unreachable!(),
                            };
                            self.stack.push(Value::Int(result));
                        }
                        _ => {
                            return Err(runtime_error_at(
                                "bitwise expects int".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    }
                }
                Op::MatchResultOk(target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Result(Ok(inner)) => {
                            self.stack.push((*inner).clone());
                        }
                        Value::Result(Err(inner)) => {
                            self.stack.push(Value::Result(Err(inner)));
                            self.frames[frame_index].ip = *target;
                            advance = false;
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects result".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchResultErr(target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Result(Err(inner)) => {
                            self.stack.push((*inner).clone());
                        }
                        Value::Result(Ok(inner)) => {
                            self.stack.push(Value::Result(Ok(inner)));
                            self.frames[frame_index].ip = *target;
                            advance = false;
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects result".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchOptionSome(target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Option(Some(inner)) => {
                            self.stack.push((*inner).clone());
                        }
                        Value::Option(None) => {
                            self.stack.push(Value::Option(None));
                            self.frames[frame_index].ip = *target;
                            advance = false;
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects option".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchOptionNone(target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Option(None) => {}
                        Value::Option(Some(inner)) => {
                            self.stack.push(Value::Option(Some(inner)));
                            self.frames[frame_index].ip = *target;
                            advance = false;
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects option".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchStruct(fields, target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Struct(values) => {
                            let mut extracted = Vec::with_capacity(fields.len());
                            let mut ok = true;
                            for field in fields {
                                if let Some(val) = values.get(field) {
                                    extracted.push(val.clone());
                                } else {
                                    ok = false;
                                    break;
                                }
                            }
                            if ok {
                                self.stack.push(Value::Struct(values));
                                for value in extracted {
                                    self.stack.push(value);
                                }
                            } else {
                                self.stack.push(Value::Struct(values));
                                self.frames[frame_index].ip = *target;
                                advance = false;
                            }
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects struct".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchEnum {
                    name,
                    variant,
                    has_payload,
                    target,
                } => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Enum {
                            name: enum_name,
                            variant: enum_variant,
                            payload,
                        } => {
                            if &enum_name == name && &enum_variant == variant {
                                if *has_payload {
                                    if let Some(payload) = payload {
                                        self.stack.push((*payload).clone());
                                    } else {
                                        return Err(self.runtime_error(
                                            format!("enum {}::{} expects value", name, variant),
                                            self.current_span(program),
                                            program,
                                        ));
                                    }
                                }
                            } else {
                                self.stack.push(Value::Enum {
                                    name: enum_name,
                                    variant: enum_variant,
                                    payload,
                                });
                                self.frames[frame_index].ip = *target;
                                advance = false;
                            }
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects enum".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchInt(expected, target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Int(actual) => {
                            if actual != *expected {
                                self.stack.push(Value::Int(actual));
                                self.frames[frame_index].ip = *target;
                                advance = false;
                            }
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects int".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchFail => {
                    return Err(self.runtime_error(
                        "non-exhaustive match".to_string(),
                        self.current_span(program),
                        program,
                    ));
                }
                Op::IsType(check) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let result = value_matches_check(&value, check);
                    self.stack.push(Value::Bool(result));
                }
                Op::Cast(check) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    if value_matches_check(&value, check) {
                        self.stack.push(value);
                    } else {
                        return Err(self.runtime_error(
                            format!("cast failed: expected {}", check.describe()),
                            self.current_span(program),
                            program,
                        ));
                    }
                }
                Op::JumpIfFalse(target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let is_false = match value {
                        Value::Bool(false) => true,
                        Value::Bool(true) => false,
                        Value::Int(0) => true,
                        Value::Int(_) => false,
                        _ => {
                            return Err(runtime_error_at(
                                "if expects bool or int".to_string(),
                                span_at(chunk, frame_ip),
                            ))
                        }
                    };
                    if is_false {
                        self.frames[frame_index].ip = *target;
                        advance = false;
                    }
                }
                Op::Jump(target) => {
                    self.frames[frame_index].ip = *target;
                    advance = false;
                }
                Op::Return => {
                    let value = self.stack.pop().unwrap_or(Value::Unit);
                    self.pop_frame();
                    if self.frames.is_empty() {
                        return Ok(Some(value));
                    }
                    self.stack.push(value);
                    advance = false;
                }
                Op::Pop => {
                    self.stack.pop().ok_or(VmError::StackUnderflow)?;
                }
                Op::Halt => {
                    return Ok(self.stack.pop());
                }
                Op::MatchTuple(expected, target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Tuple(items) => {
                            if items.len() == *expected {
                                self.stack.push(Value::Tuple(items));
                            } else {
                                self.stack.push(Value::Tuple(items));
                                self.frames[frame_index].ip = *target;
                                advance = false;
                            }
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects tuple".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchBool(expected, target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::Bool(actual) => {
                            if actual != *expected {
                                self.stack.push(Value::Bool(actual));
                                self.frames[frame_index].ip = *target;
                                advance = false;
                            }
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects bool".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::MatchString(expected, target) => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    match value {
                        Value::String(actual) => {
                            if &*actual != expected {
                                self.stack.push(Value::String(actual));
                                self.frames[frame_index].ip = *target;
                                advance = false;
                            }
                        }
                        _ => {
                            return Err(self.runtime_error(
                                "match expects string".to_string(),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
            }

            if advance {
                if let Some(frame) = self.frames.last_mut() {
                    frame.ip += 1;
                }
            }
        }
    }
}

impl Vm {
    fn take_locals(&mut self, size: usize) -> Vec<Value> {
        let mut locals = self.locals_pool.pop().unwrap_or_default();
        locals.clear();
        if locals.capacity() < size {
            locals.reserve(size - locals.capacity());
        }
        locals.resize(size, Value::Unit);
        locals
    }

    fn pop_frame(&mut self) {
        if let Some(frame) = self.frames.pop() {
            self.recycle_locals(frame.locals);
        }
    }

    fn recycle_locals(&mut self, mut locals: Vec<Value>) {
        locals.clear();
        self.locals_pool.push(locals);
    }
}

impl Vm {
    fn current_span(&self, program: &Program) -> Option<Span> {
        let frame = self.frames.last()?;
        let chunk = if frame.chunk_id == usize::MAX {
            &program.main
        } else {
            &program.functions.get(frame.chunk_id)?.chunk
        };
        let ip = frame.ip;
        if ip >= chunk.spans.len() {
            return None;
        }
        chunk.spans.get(ip).cloned().flatten()
    }
}

thread_local! {
    static STACK_TRACE: RefCell<Option<Vec<StackFrame>>> = RefCell::new(None);
    static CURRENT_SPAN: RefCell<Option<Span>> = RefCell::new(None);
}

fn set_stack_trace(trace: Vec<StackFrame>) {
    STACK_TRACE.with(|cell| {
        *cell.borrow_mut() = Some(trace);
    });
}

fn set_current_span(span: Option<Span>) {
    CURRENT_SPAN.with(|cell| {
        *cell.borrow_mut() = span;
    });
}

fn current_stack_trace() -> Option<Vec<StackFrame>> {
    STACK_TRACE.with(|cell| cell.borrow().clone())
}

fn current_span() -> Option<Span> {
    CURRENT_SPAN.with(|cell| *cell.borrow())
}

fn span_at(chunk: &Chunk, ip: usize) -> Option<Span> {
    chunk.spans.get(ip).cloned().flatten()
}

fn runtime_error_at(message: String, span: Option<Span>) -> VmError {
    let span = span.or_else(current_span);
    VmError::Runtime {
        message,
        span,
        stack: current_stack_trace(),
    }
}

fn expr_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Int(_, span, _) => Some(*span),
        Expr::Float(_, span, _) => Some(*span),
        Expr::String(_, span, _) => Some(*span),
        Expr::Bool(_, span, _) => Some(*span),
        Expr::Ident(ident) => Some(ident.span),
        Expr::Unary { op_span, .. } => Some(*op_span),
        Expr::Binary { op_span, .. } => Some(*op_span),
        Expr::If { if_span, .. } => Some(*if_span),
        Expr::Member { name, .. } => Some(name.span),
        Expr::Call { callee, .. } => expr_span(callee),
        Expr::Try(expr, _) => expr_span(expr),
        Expr::Await { await_span, .. } => Some(*await_span),
        Expr::TryCatch { try_span, .. } => Some(*try_span),
        Expr::Group { span, .. } => Some(*span),
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
        Expr::MapSpread { spread_span, .. } => Some(*spread_span),
        Expr::As { span, .. } => Some(*span),
        Expr::Is { span, .. } => Some(*span),
        Expr::Ternary { span, .. } => Some(*span),
        Expr::ChainedComparison { span, .. } => Some(*span),
    }
}

fn value_matches_check(value: &Value, check: &TypeCheck) -> bool {
    match (value, check) {
        (Value::Int(_), TypeCheck::Int) => true,
        (Value::Float(_), TypeCheck::Float) => true,
        (Value::String(_), TypeCheck::String) => true,
        (Value::Bool(_), TypeCheck::Bool) => true,
        (Value::Unit, TypeCheck::Unit) => true,
        (Value::Array(_), TypeCheck::Array) => true,
        (Value::Tuple(_), TypeCheck::Tuple) => true,
        (Value::Map(_), TypeCheck::Map) => true,
        (Value::Struct(_), TypeCheck::Struct) => true,
        (Value::Option(_), TypeCheck::Option) => true,
        (Value::Result(_), TypeCheck::Result) => true,
        (Value::Enum { name, .. }, TypeCheck::Enum(expected)) => name == expected,
        _ => false,
    }
}

impl TypeCheck {
    fn describe(&self) -> String {
        match self {
            TypeCheck::Int => "int".to_string(),
            TypeCheck::Float => "float".to_string(),
            TypeCheck::String => "string".to_string(),
            TypeCheck::Bool => "bool".to_string(),
            TypeCheck::Unit => "unit".to_string(),
            TypeCheck::Array => "array".to_string(),
            TypeCheck::Tuple => "tuple".to_string(),
            TypeCheck::Map => "map".to_string(),
            TypeCheck::Struct => "struct".to_string(),
            TypeCheck::Option => "option".to_string(),
            TypeCheck::Result => "result".to_string(),
            TypeCheck::Enum(name) => format!("{name}"),
        }
    }
}

fn compile_error(message: String, span: Option<Span>) -> VmError {
    VmError::Compile { message, span }
}

fn patch_jump(code: &mut [Op], index: usize, target: usize) {
    match &mut code[index] {
        Op::MatchResultOk(ref mut slot)
        | Op::MatchResultErr(ref mut slot)
        | Op::MatchOptionSome(ref mut slot)
        | Op::MatchOptionNone(ref mut slot)
        | Op::MatchStruct(_, ref mut slot)
        | Op::MatchInt(_, ref mut slot)
        | Op::MatchBool(_, ref mut slot)
        | Op::MatchString(_, ref mut slot)
        | Op::MatchTuple(_, ref mut slot)
        | Op::MatchEnum {
            target: ref mut slot,
            ..
        }
        | Op::JumpIfFalse(ref mut slot)
        | Op::Jump(ref mut slot) => {
            *slot = target;
        }
        _ => {}
    }
}

fn map_builtin(base: &str, name: &str, args: usize) -> Option<Builtin> {
    match (base, name, args) {
        ("time", "now", 0) => Some(Builtin::TimeNow),
        ("time", "fixed", 1) => Some(Builtin::TimeFixed),
        ("rng", "deterministic", 1) => Some(Builtin::RngDeterministic),
        ("rng", "uuid", 0) => Some(Builtin::RngUuid),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{Compiler, Vm, VmError};
    use at_parser::parse_module;

    #[test]
    fn match_fail_has_span() {
        let source = r#"
fn f() {
    let x = none();
    return match x {
        some(v) => v,
    };
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let err = vm.run(&program).expect_err("expected runtime error");
        match err {
            VmError::Runtime { message, span, .. } => {
                assert!(span.is_some(), "missing span for runtime error: {message}");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn compile_error_on_break_outside_loop() {
        let source = r#"
fn f() {
    break;
}
"#;
        let module = parse_module(source).expect("parse module");
        let err = Compiler::new()
            .compile_module(&module)
            .expect_err("expected compile error");
        match err {
            VmError::Compile { message, .. } => {
                assert!(message.contains("break used outside of loop"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn compile_error_on_continue_outside_loop() {
        let source = r#"
fn f() {
    continue;
}
"#;
        let module = parse_module(source).expect("parse module");
        let err = Compiler::new()
            .compile_module(&module)
            .expect_err("expected compile error");
        match err {
            VmError::Compile { message, .. } => {
                assert!(message.contains("continue used outside of loop"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn compiles_let_as_const_then_store() {
        let source = r#"
fn f() -> int {
    let i: int = 0;
    return i;
}
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let chunk = &program.functions[0].chunk;
        assert!(matches!(chunk.code.get(0), Some(super::Op::Const(_))));
        assert!(matches!(chunk.code.get(1), Some(super::Op::StoreLocal(_))));
    }

    #[test]
    fn runs_let_and_return() {
        let source = r#"
fn f() -> int {
    let i: int = 0;
    return i;
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let result = vm.run(&program).expect("run program");
        assert_eq!(result, None);
    }

    #[test]
    fn runtime_errors_on_missing_capability() {
        let source = r#"
fn f() -> int {
    return time.now();
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let err = vm.run(&program).expect_err("expected runtime error");
        match err {
            VmError::Runtime { message, .. } => {
                assert!(message.contains("missing capability: time"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn runtime_allows_using_capability() {
        let source = r#"
fn f() -> int {
    return time.now();
}

using time = time.fixed("2026-01-01T00:00:00Z");
f();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let result = vm.run(&program).expect("run program");
        assert_eq!(result, None);
    }

    #[test]
    fn runtime_errors_on_missing_capability_for_needs() {
        let source = r#"
fn f() -> int needs { time } {
    return time.now();
}

f();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let err = vm.run(&program).expect_err("expected runtime error");
        match err {
            VmError::Runtime { message, .. } => {
                assert!(message.contains("missing capability: time"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn runs_for_loop_sum() {
        let source = r#"
fn sum(values: array<int>) -> int {
    let total = 0;
    for value in values {
        set total = total + value;
    }
    return total;
}

sum([1, 2, 3, 4]);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let result = vm.run(&program).expect("run program");
        assert_eq!(result, None);
    }

    #[test]
    fn for_loop_respects_break() {
        let source = r#"
fn sum(values: array<int>) -> int {
    let total = 0;
    for value in values {
        if value == 3 {
            break;
        } else { }
        set total = total + value;
    }
    return total;
}

sum([1, 2, 3, 4]);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let result = vm.run(&program).expect("run program");
        assert_eq!(result, None);
    }
}
