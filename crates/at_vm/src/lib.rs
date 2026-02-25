use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::Read;
use std::rc::Rc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use at_parser::parse_module;
use at_syntax::{Expr, Function, Ident, MapEntry, Module, Span, Stmt};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fmt;
use uuid::Uuid;

const EMBEDDED_STDLIB_SOURCE: &str = include_str!("../../../stdlib/std.at");
const REMOTE_FETCH_TIMEOUT_SECS: u64 = 10;
const REMOTE_FETCH_RETRIES: usize = 3;
const REMOTE_FETCH_MAX_BYTES: u64 = 2 * 1024 * 1024;

fn days_from_civil(year: i64, month: i64, day: i64) -> i64 {
    let adjusted_year = year - i64::from(month <= 2);
    let era = if adjusted_year >= 0 {
        adjusted_year / 400
    } else {
        (adjusted_year - 399) / 400
    };
    let yoe = adjusted_year - era * 400;
    let month_index = month + if month > 2 { -3 } else { 9 };
    let doy = (153 * month_index + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

fn parse_fixed_timestamp(input: &str) -> Option<i64> {
    let text = input.strip_suffix('Z')?;
    let (date, time) = text.split_once('T')?;
    let mut date_parts = date.split('-');
    let year: i64 = date_parts.next()?.parse().ok()?;
    let month: i64 = date_parts.next()?.parse().ok()?;
    let day: i64 = date_parts.next()?.parse().ok()?;
    if date_parts.next().is_some() {
        return None;
    }
    let mut time_parts = time.split(':');
    let hour: i64 = time_parts.next()?.parse().ok()?;
    let minute: i64 = time_parts.next()?.parse().ok()?;
    let second: i64 = time_parts.next()?.parse().ok()?;
    if time_parts.next().is_some() {
        return None;
    }

    if !(1..=12).contains(&month) || !(0..=23).contains(&hour) || !(0..=59).contains(&minute) {
        return None;
    }
    if !(0..=60).contains(&second) {
        return None;
    }
    let is_leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;
    let max_day = match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap => 29,
        2 => 28,
        _ => return None,
    };
    if !(1..=max_day).contains(&day) {
        return None;
    }

    let days = days_from_civil(year, month, day);
    Some(days * 86_400 + hour * 3_600 + minute * 60 + second)
}

fn deterministic_random_from_seed(seed: i64) -> i64 {
    let mut value = (seed as u64) ^ 0x9E37_79B9_7F4A_7C15;
    value ^= value >> 12;
    value ^= value << 25;
    value ^= value >> 27;
    let mixed = value.wrapping_mul(0x2545_F491_4F6C_DD1D);
    (mixed >> 1) as i64
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(Rc<String>),
    Bool(bool),
    Array(Rc<Vec<Value>>),
    Tuple(Rc<Vec<Value>>),
    Map(Rc<IndexMap<MapKey, Value>>),
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
    Generator(Rc<RefCell<GeneratorValue>>),
    TaskHandle(TaskId),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MapKey {
    Int(i64),
    Float(u64),
    Bool(bool),
    String(String),
    Tuple(Vec<MapKey>),
}

impl MapKey {
    pub fn try_from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Int(v) => Ok(MapKey::Int(*v)),
            Value::Float(v) => Ok(MapKey::Float(v.to_bits())),
            Value::Bool(v) => Ok(MapKey::Bool(*v)),
            Value::String(v) => Ok(MapKey::String(v.as_ref().clone())),
            Value::Tuple(items) => {
                let mut converted = Vec::with_capacity(items.len());
                for item in items.iter() {
                    converted.push(MapKey::try_from_value(item)?);
                }
                Ok(MapKey::Tuple(converted))
            }
            _ => Err(format!(
                "unsupported map key type: {}",
                type_name_of_value(value)
            )),
        }
    }

    pub fn to_value(&self) -> Value {
        match self {
            MapKey::Int(v) => Value::Int(*v),
            MapKey::Float(bits) => Value::Float(f64::from_bits(*bits)),
            MapKey::Bool(v) => Value::Bool(*v),
            MapKey::String(v) => Value::String(Rc::new(v.clone())),
            MapKey::Tuple(items) => {
                let values = items.iter().map(MapKey::to_value).collect::<Vec<_>>();
                Value::Tuple(Rc::new(values))
            }
        }
    }
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
    capabilities: HashSet<usize>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GeneratorValue {
    func_id: usize,
    captures: Vec<Value>,
    args: Vec<Value>,
    capabilities: HashSet<usize>,
    frames: Vec<Frame>,
    stack: Vec<Value>,
    done: bool,
}

/// Unique identifier for a cooperative async task.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TaskId(pub u64);

pub fn format_value(value: &Value) -> String {
    match value {
        Value::Int(value) => value.to_string(),
        Value::Float(value) => value.to_string(),
        Value::String(value) => format!("\"{}\"", value),
        Value::Bool(value) => value.to_string(),
        Value::Unit => "unit".to_string(),
        Value::Closure(_) => "<closure>".to_string(),
        Value::Future(_) => "<future>".to_string(),
        Value::Generator(_) => "<generator>".to_string(),
        Value::TaskHandle(id) => format!("<task:{}>", id.0),
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
                .map(|(key, value)| {
                    format!("{}: {}", format_value(&key.to_value()), format_value(value))
                })
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

fn type_name_of_value(value: &Value) -> &'static str {
    match value {
        Value::Int(_) => "int",
        Value::Float(_) => "float",
        Value::String(_) => "string",
        Value::Bool(_) => "bool",
        Value::Array(_) => "array",
        Value::Tuple(_) => "tuple",
        Value::Map(_) => "map",
        Value::Struct(_) => "struct",
        Value::Enum { .. } => "enum",
        Value::Option(_) => "option",
        Value::Result(_) => "result",
        Value::Closure(_) => "closure",
        Value::Future(_) => "future",
        Value::Generator(_) => "generator",
        Value::TaskHandle(_) => "task",
        Value::Unit => "unit",
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Op {
    Const(usize),
    LoadLocal(usize),
    StoreLocal(usize),
    GrantCapability(usize),
    PushCapabilityScope,
    PopCapabilityScope,
    Call(usize, usize),
    CallAsync(usize, usize),
    CallGenerator(usize, usize),
    Builtin(Builtin),
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
    Yield,
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
    ToBool,
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
    Assert,
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
    Next,
    Split,
    Trim,
    Substring,
    CharAt,
    ToUpper,
    ToLower,
    ParseInt,
    ToString,
    Keys,
    Values,
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

    fn rewrite_jumps(&mut self, mapping: &[usize]) {
        for op in &mut self.code {
            match op {
                Op::Jump(target) | Op::JumpIfFalse(target) => {
                    let mapped = mapping.get(*target).copied().unwrap_or(*target);
                    *target = mapped;
                }
                Op::MatchResultOk(target)
                | Op::MatchResultErr(target)
                | Op::MatchOptionSome(target)
                | Op::MatchOptionNone(target) => {
                    let mapped = mapping.get(*target).copied().unwrap_or(*target);
                    *target = mapped;
                }
                Op::MatchStruct(_, target)
                | Op::MatchInt(_, target)
                | Op::MatchBool(_, target)
                | Op::MatchString(_, target)
                | Op::MatchTuple(_, target) => {
                    let mapped = mapping.get(*target).copied().unwrap_or(*target);
                    *target = mapped;
                }
                Op::MatchEnum { target, .. } => {
                    let mapped = mapping.get(*target).copied().unwrap_or(*target);
                    *target = mapped;
                }
                _ => {}
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionChunk {
    pub name: String,
    pub params: usize,
    pub captures: usize,
    pub locals: usize,
    pub source_path: Option<String>,
    pub needs: Vec<usize>,
    pub is_async: bool,
    pub is_generator: bool,
    pub stmts: Vec<Stmt>,
    pub chunk: Chunk,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Program {
    pub functions: Vec<FunctionChunk>,
    pub main: Chunk,
    pub main_locals: usize,
    pub sources: HashMap<String, String>,
    pub entry: Option<String>,
    pub capability_names: Vec<String>,
}

impl Program {
    pub fn to_bytes(&self) -> Result<Vec<u8>, bincode::Error> {
        bincode::serialize(self)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, bincode::Error> {
        bincode::deserialize(bytes)
    }
}

fn optimize_program(program: &mut Program) {
    optimize_chunk(&mut program.main);
    for func in &mut program.functions {
        optimize_chunk(&mut func.chunk);
    }
    ensure_capability_order(program);
}

fn optimize_chunk(chunk: &mut Chunk) {
    simplify_jump_chains(chunk);
    remove_noop_jumps(chunk);
    remove_dead_code(chunk);
    remove_trailing_unreachable(chunk);
}

fn remove_trailing_unreachable(chunk: &mut Chunk) {
    let mut new_len = chunk.code.len();
    for (idx, op) in chunk.code.iter().enumerate() {
        if matches!(op, Op::Return | Op::Halt) {
            new_len = idx + 1;
            break;
        }
    }
    chunk.code.truncate(new_len);
    chunk.spans.truncate(new_len);
}

fn remove_dead_code(chunk: &mut Chunk) {
    let len = chunk.code.len();
    if len == 0 {
        return;
    }
    let mut reachable = vec![false; len];
    let mut work = vec![0usize];
    while let Some(ip) = work.pop() {
        if ip >= len || reachable[ip] {
            continue;
        }
        reachable[ip] = true;
        match &chunk.code[ip] {
            Op::Jump(target)
            | Op::JumpIfFalse(target)
            | Op::MatchResultOk(target)
            | Op::MatchResultErr(target)
            | Op::MatchOptionSome(target)
            | Op::MatchOptionNone(target)
            | Op::MatchStruct(_, target)
            | Op::MatchInt(_, target)
            | Op::MatchBool(_, target)
            | Op::MatchString(_, target)
            | Op::MatchTuple(_, target) => {
                work.push(*target);
                work.push(ip + 1);
            }
            Op::MatchEnum { target, .. } => {
                work.push(*target);
                work.push(ip + 1);
            }
            Op::Return | Op::Halt => {}
            _ => {
                work.push(ip + 1);
            }
        }
    }

    if reachable.iter().all(|flag| *flag) {
        return;
    }

    let mut mapping = vec![0usize; len];
    let mut new_code = Vec::with_capacity(len);
    let mut new_spans = Vec::with_capacity(len);
    let mut next_index = 0usize;
    for (idx, is_live) in reachable.iter().enumerate() {
        if *is_live {
            mapping[idx] = next_index;
            next_index += 1;
            new_code.push(chunk.code[idx].clone());
            new_spans.push(chunk.spans[idx]);
        } else {
            mapping[idx] = next_index.saturating_sub(1);
        }
    }
    chunk.code = new_code;
    chunk.spans = new_spans;
    chunk.rewrite_jumps(&mapping);
}

fn simplify_jump_chains(chunk: &mut Chunk) {
    let len = chunk.code.len();
    if len == 0 {
        return;
    }
    let mut resolved = Vec::with_capacity(len);
    for op in &chunk.code {
        if let Op::Jump(target) = op {
            resolved.push(resolve_jump_target(&chunk.code, *target));
        } else {
            resolved.push(usize::MAX);
        }
    }
    for (op, target) in chunk.code.iter_mut().zip(resolved.into_iter()) {
        if let Op::Jump(current) = op {
            if target != usize::MAX {
                *current = target;
            }
        }
    }
}

fn resolve_jump_target(code: &[Op], mut target: usize) -> usize {
    let len = code.len();
    let mut hops = 0usize;
    while target < len {
        match code.get(target) {
            Some(Op::Jump(next)) => {
                if *next == target {
                    break;
                }
                target = *next;
                hops += 1;
                if hops > len {
                    break;
                }
            }
            _ => break,
        }
    }
    target
}

fn remove_noop_jumps(chunk: &mut Chunk) {
    let len = chunk.code.len();
    if len == 0 {
        return;
    }
    let mut mapping = vec![0usize; len + 1];
    let mut new_code = Vec::with_capacity(len);
    let mut new_spans = Vec::with_capacity(len);
    let mut next_index = 0usize;
    for (idx, op) in chunk.code.iter().enumerate() {
        let remove = matches!(op, Op::Jump(target) if *target == idx + 1);
        if remove {
            mapping[idx] = next_index;
            continue;
        }
        mapping[idx] = next_index;
        next_index += 1;
        new_code.push(op.clone());
        new_spans.push(chunk.spans[idx]);
    }
    mapping[len] = next_index;
    if new_code.len() == len {
        return;
    }
    chunk.code = new_code;
    chunk.spans = new_spans;
    chunk.rewrite_jumps(&mapping);
}

fn ensure_capability_order(program: &mut Program) {
    let preferred = ["time", "rng"];
    if preferred.len() <= 1 || program.capability_names.is_empty() {
        return;
    }
    let mut mapping = Vec::with_capacity(program.capability_names.len());
    let mut new_names = Vec::with_capacity(program.capability_names.len());
    let mut assigned = HashMap::new();
    let mut next_index = 0usize;
    for name in preferred {
        if let Some(index) = program.capability_names.iter().position(|n| n == name) {
            assigned.insert(index, next_index);
            new_names.push(name.to_string());
            next_index += 1;
        }
    }
    for (index, name) in program.capability_names.iter().enumerate() {
        if assigned.contains_key(&index) {
            continue;
        }
        assigned.insert(index, next_index);
        new_names.push(name.clone());
        next_index += 1;
    }
    mapping.resize(program.capability_names.len(), 0);
    for (old, new) in assigned {
        if old < mapping.len() {
            mapping[old] = new;
        }
    }
    program.capability_names = new_names;
    for func in &mut program.functions {
        for need in &mut func.needs {
            if let Some(mapped) = mapping.get(*need) {
                *need = *mapped;
            }
        }
    }
    for chunk in program
        .functions
        .iter_mut()
        .map(|func| &mut func.chunk)
        .chain(std::iter::once(&mut program.main))
    {
        for op in &mut chunk.code {
            if let Op::GrantCapability(id) = op {
                if let Some(mapped) = mapping.get(*id) {
                    *id = *mapped;
                }
            }
        }
    }
}

#[derive(Debug, Default)]
struct LoopContext {
    breaks: Vec<usize>,
    continues: Vec<usize>,
}

#[derive(Debug, Clone)]
struct LoadedModule {
    module: Module,
    imports: Vec<String>,
    function_sources: HashMap<String, String>,
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

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VmError::StackUnderflow => write!(f, "stack underflow"),
            VmError::Compile { message, span } => {
                if let Some(span) = span {
                    write!(f, "compile error: {message} at {}:{}", span.start, span.end)
                } else {
                    write!(f, "compile error: {message}")
                }
            }
            VmError::Runtime { message, span, .. } => {
                if let Some(span) = span {
                    write!(f, "runtime error: {message} at {}:{}", span.start, span.end)
                } else {
                    write!(f, "runtime error: {message}")
                }
            }
            VmError::ExecutionLimit { message } => write!(f, "{message}"),
        }
    }
}

impl std::error::Error for VmError {}

#[derive(Debug, Clone, PartialEq)]
pub struct StackFrame {
    pub name: String,
    pub source_path: Option<String>,
    pub span: Option<Span>,
}

pub trait DebuggerHook {
    fn before_op(&mut self, program: &Program, chunk_id: usize, ip: usize, op: Option<&Op>);
}

pub struct Compiler {
    scopes: Vec<HashMap<String, usize>>,
    const_scopes: Vec<HashSet<String>>,
    next_local: usize,
    /// Slots freed by `pop_scope`, available for reuse.
    free_slots: Vec<usize>,
    /// Per-scope record of which slots were allocated in that scope.
    scope_allocated: Vec<Vec<usize>>,
    functions: HashMap<String, usize>,
    function_arity: HashMap<String, usize>,
    function_needs: HashMap<String, Vec<usize>>,
    function_async: HashMap<String, bool>,
    function_generator: HashMap<String, bool>,
    structs: HashSet<String>,
    enums: HashSet<String>,
    current_function: Option<String>,
    loop_stack: Vec<LoopContext>,
    synthetic_id: usize,
    base_function_count: usize,
    closure_chunks: Vec<FunctionChunk>,
    capability_ids: HashMap<String, usize>,
    capability_names: Vec<String>,
    function_sources: HashMap<String, String>,
    current_function_source: Option<String>,
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

        // Wrap temp slots in a dedicated scope so they are reclaimed after use.
        self.push_scope();

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

        // Reclaim temp slots for reuse.
        self.pop_scope();
        Ok(())
    }

    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            const_scopes: Vec::new(),
            next_local: 0,
            free_slots: Vec::new(),
            scope_allocated: Vec::new(),
            functions: HashMap::new(),
            function_arity: HashMap::new(),
            function_needs: HashMap::new(),
            function_async: HashMap::new(),
            function_generator: HashMap::new(),
            structs: HashSet::new(),
            enums: HashSet::new(),
            current_function: None,
            loop_stack: Vec::new(),
            synthetic_id: 0,
            base_function_count: 0,
            closure_chunks: Vec::new(),
            capability_ids: HashMap::new(),
            capability_names: Vec::new(),
            function_sources: HashMap::new(),
            current_function_source: None,
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

fn stmts_contain_yield(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_contains_yield)
}

fn stmt_contains_yield(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Yield { .. } => true,
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            stmts_contain_yield(then_branch)
                || else_branch.as_ref().is_some_and(|b| stmts_contain_yield(b))
        }
        Stmt::While { body, .. }
        | Stmt::For { body, .. }
        | Stmt::With { body, .. }
        | Stmt::Block { stmts: body, .. }
        | Stmt::Test { body, .. } => stmts_contain_yield(body),
        _ => false,
    }
}

impl Compiler {
    pub fn compile_module(&mut self, module: &Module) -> Result<Program, VmError> {
        let mut program = Program::default();
        program.sources.clear();
        program.entry = module.source_path.clone();

        self.functions.clear();
        self.function_arity.clear();
        self.function_needs.clear();
        self.function_async.clear();
        self.function_generator.clear();
        self.structs.clear();
        self.enums.clear();
        self.const_scopes.clear();
        self.current_function = None;
        self.current_function_source = None;
        self.closure_chunks.clear();
        self.base_function_count = module.functions.len();
        self.capability_ids.clear();
        self.capability_names.clear();

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

        if let Some(path) = module.source_path.as_ref() {
            if let Ok(source) = std::fs::read_to_string(path) {
                program.sources.insert(path.clone(), source);
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
            let is_generator = stmts_contain_yield(&func.body);
            self.function_generator
                .insert(func.name.name.clone(), is_generator);
            let needs: Vec<usize> = func
                .needs
                .iter()
                .map(|ident| self.capability_id(&ident.name))
                .collect();
            self.function_needs
                .insert(func.name.name.clone(), needs.clone());
            let source_path = self
                .function_sources
                .get(&func.name.name)
                .cloned()
                .or_else(|| module.source_path.clone());
            program.functions.push(FunctionChunk {
                name: func.name.name.clone(),
                params: func.params.len(),
                captures: 0,
                locals: 0,
                source_path,
                needs,
                is_async: func.is_async,
                is_generator,
                stmts: func.body.clone(),
                chunk: Chunk::default(),
            });
        }

        for (id, func) in module.functions.iter().enumerate() {
            self.current_function = Some(func.name.name.clone());
            self.current_function_source = program.functions[id].source_path.clone();
            let (chunk, locals) = self.compile_function(func)?;
            self.current_function = None;
            self.current_function_source = None;
            program.functions[id].chunk = chunk;
            program.functions[id].locals = locals;
        }

        self.scopes.clear();
        self.next_local = 0;
        self.free_slots.clear();
        self.scope_allocated.clear();
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
        optimize_program(&mut program);
        program.capability_names = self.capability_names.clone();

        Ok(program)
    }

    fn capability_id(&mut self, name: &str) -> usize {
        if let Some(existing) = self.capability_ids.get(name) {
            return *existing;
        }
        let id = self.capability_names.len();
        self.capability_names.push(name.to_string());
        self.capability_ids.insert(name.to_string(), id);
        id
    }

    fn propagate_transitive_needs(&mut self, program: &mut Program) {
        let mut needs_sets: Vec<std::collections::HashSet<usize>> = program
            .functions
            .iter()
            .map(|func| func.needs.iter().cloned().collect())
            .collect();
        loop {
            let mut changed = false;
            for (id, func) in program.functions.iter().enumerate() {
                let mut updated = needs_sets[id].clone();
                for op in &func.chunk.code {
                    if let Op::Call(func_id, _) = op {
                        if *func_id < needs_sets.len() {
                            updated.extend(needs_sets[*func_id].iter().cloned());
                        }
                    }
                }
                if updated.len() != needs_sets[id].len() {
                    needs_sets[id] = updated;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        for (id, set) in needs_sets.into_iter().enumerate() {
            let mut needs: Vec<usize> = set.into_iter().collect();
            needs.sort();
            program.functions[id].needs = needs;
        }
    }

    pub fn compile_test_body(&mut self, stmts: &[Stmt]) -> Result<(Chunk, usize), VmError> {
        self.scopes.clear();
        self.next_local = 0;
        self.free_slots.clear();
        self.scope_allocated.clear();
        self.push_scope();
        let mut chunk = Chunk::default();
        for stmt in stmts {
            self.compile_stmt(stmt, &mut chunk)?;
        }
        chunk.push(Op::Halt, None);
        Ok((chunk, self.next_local))
    }

    fn compile_function(&mut self, func: &Function) -> Result<(Chunk, usize), VmError> {
        self.scopes.clear();
        self.next_local = 0;
        self.free_slots.clear();
        self.scope_allocated.clear();
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
                let current_needs = self.function_needs.entry(current.clone()).or_default();
                for need in callee_needs {
                    if !current_needs.contains(&need) {
                        current_needs.push(need);
                    }
                }
            }
        }
    }

    fn function_is_generator(&self, name: &str) -> bool {
        self.function_generator.get(name).copied().unwrap_or(false)
    }

    fn compile_stmt(&mut self, stmt: &Stmt, chunk: &mut Chunk) -> Result<(), VmError> {
        match stmt {
            // Imports are resolved by module loading before code generation.
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
                let cap_id = self.capability_id(&name.name);
                chunk.push(Op::GrantCapability(cap_id), Some(name.span));
            }
            Stmt::With {
                name, value, body, ..
            } => {
                chunk.push(Op::PushCapabilityScope, Some(name.span));
                self.compile_expr(value, chunk)?;
                let slot = self.bind_local_checked(&name.name, name.span)?;
                chunk.push(Op::StoreLocal(slot), Some(name.span));
                let cap_id = self.capability_id(&name.name);
                chunk.push(Op::GrantCapability(cap_id), Some(name.span));
                self.push_scope();
                for stmt in body {
                    self.compile_stmt(stmt, chunk)?;
                }
                self.pop_scope();
                chunk.push(Op::PopCapabilityScope, Some(name.span));
            }
            Stmt::Yield { expr, .. } => {
                self.compile_expr(expr, chunk)?;
                chunk.push(Op::Yield, expr_span(expr));
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
        chunk.push(Op::ToBool, expr_span(expr));
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
                    match item {
                        Expr::ArraySpread { .. } => {
                            // Flush accumulated non-spread items BEFORE compiling the spread
                            if array_count > 0 {
                                chunk.push(Op::Array(array_count), expr_span(expr));
                                array_count = 0;
                                chunk_count += 1;
                            }
                            self.compile_expr(item, chunk)?;
                            has_spread = true;
                            chunk_count += 1;
                        }
                        _ => {
                            self.compile_expr(item, chunk)?;
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
                for entry in entries {
                    match entry {
                        MapEntry::Spread(spread_expr) => {
                            self.compile_expr(spread_expr, chunk)?;
                            chunk.push(Op::MapSpread(1), expr_span(spread_expr));
                            if map_count > 0 {
                                chunk.push(Op::Map(map_count), expr_span(expr));
                                map_count = 0;
                                chunk_count += 1;
                            }
                            has_spread = true;
                            chunk_count += 1;
                        }
                        MapEntry::KeyValue { key, value } => {
                            self.compile_expr(key, chunk)?;
                            self.compile_expr(value, chunk)?;
                            map_count += 1;
                        }
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
                    if let Some(builtin) = map_builtin("", &ident.name, args.len()) {
                        for arg in args {
                            self.compile_expr(arg, chunk)?;
                        }
                        chunk.push(Op::Builtin(builtin), Some(ident.span));
                        return Ok(());
                    }
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
                    } else if self.function_is_generator(&ident.name) {
                        Op::CallGenerator(func_id, args.len())
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
                    self.compile_expr(finally_block, chunk)?;
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
        let saved_free_slots = std::mem::take(&mut self.free_slots);
        let saved_scope_allocated = std::mem::take(&mut self.scope_allocated);
        let saved_loop_stack = std::mem::take(&mut self.loop_stack);
        let saved_current_function = self.current_function.take();

        self.scopes = Vec::new();
        self.next_local = 0;
        self.free_slots = Vec::new();
        self.scope_allocated = Vec::new();
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
        self.free_slots = saved_free_slots;
        self.scope_allocated = saved_scope_allocated;
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
            source_path: self.current_function_source.clone(),
            needs: Vec::new(),
            is_async: false,
            is_generator: false,
            stmts: Vec::new(),
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
                    Self::push_bound_scope(bound);
                    Self::bind_pattern_names(&arm.pattern, bound);
                    if let Some(guard) = &arm.guard {
                        self.collect_free_vars_expr(guard, bound, captures, seen);
                    }
                    self.collect_free_vars_expr(&arm.body, bound, captures, seen);
                    Self::pop_bound_scope(bound);
                }
            }
            Expr::Block { stmts, tail, .. } => {
                Self::push_bound_scope(bound);
                for stmt in stmts {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                if let Some(expr) = tail {
                    self.collect_free_vars_expr(expr, bound, captures, seen);
                }
                Self::pop_bound_scope(bound);
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
                for entry in entries {
                    match entry {
                        MapEntry::KeyValue { key, value } => {
                            self.collect_free_vars_expr(key, bound, captures, seen);
                            self.collect_free_vars_expr(value, bound, captures, seen);
                        }
                        MapEntry::Spread(expr) => {
                            self.collect_free_vars_expr(expr, bound, captures, seen);
                        }
                    }
                }
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
            Expr::Closure { params, body, .. } => {
                // Nested closures may reference symbols from ancestor scopes.
                // Walk the nested body so those free vars are captured transitively.
                Self::push_bound_scope(bound);
                if let Some(scope) = bound.last_mut() {
                    for param in params {
                        scope.insert(param.name.clone());
                    }
                }
                self.collect_free_vars_expr(body, bound, captures, seen);
                Self::pop_bound_scope(bound);
            }
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
                Self::push_bound_scope(bound);
                for stmt in body {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                Self::pop_bound_scope(bound);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.collect_free_vars_expr(condition, bound, captures, seen);
                Self::push_bound_scope(bound);
                for stmt in then_branch {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                Self::pop_bound_scope(bound);
                if let Some(else_branch) = else_branch {
                    Self::push_bound_scope(bound);
                    for stmt in else_branch {
                        self.collect_free_vars_stmt(stmt, bound, captures, seen);
                    }
                    Self::pop_bound_scope(bound);
                }
            }
            Stmt::For {
                item, iter, body, ..
            } => {
                self.collect_free_vars_expr(iter, bound, captures, seen);
                Self::push_bound_scope(bound);
                if let Some(scope) = bound.last_mut() {
                    scope.insert(item.name.clone());
                }
                for stmt in body {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                Self::pop_bound_scope(bound);
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
                Self::push_bound_scope(bound);
                if let Some(scope) = bound.last_mut() {
                    scope.insert(name.name.clone());
                }
                for stmt in body {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                Self::pop_bound_scope(bound);
            }
            Stmt::Yield { expr, .. } => {
                self.collect_free_vars_expr(expr, bound, captures, seen);
            }
            Stmt::Block { stmts, .. } | Stmt::Test { body: stmts, .. } => {
                Self::push_bound_scope(bound);
                for stmt in stmts {
                    self.collect_free_vars_stmt(stmt, bound, captures, seen);
                }
                Self::pop_bound_scope(bound);
            }
        }
    }

    fn bind_pattern_names(pattern: &at_syntax::MatchPattern, bound: &mut Vec<HashSet<String>>) {
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
                    Self::bind_pattern_names(item, bound);
                }
            }
            at_syntax::MatchPattern::Binding { name, pattern, .. } => {
                if let Some(scope) = bound.last_mut() {
                    scope.insert(name.name.clone());
                }
                Self::bind_pattern_names(pattern, bound);
            }
            _ => {}
        }
    }

    fn push_bound_scope(bound: &mut Vec<HashSet<String>>) {
        bound.push(HashSet::new());
    }

    fn pop_bound_scope(bound: &mut Vec<HashSet<String>>) {
        bound.pop();
    }

    fn is_bound(&self, bound: &[HashSet<String>], name: &str) -> bool {
        bound.iter().rev().any(|scope| scope.contains(name))
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.const_scopes.push(HashSet::new());
        self.scope_allocated.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.const_scopes.pop();
        if let Some(slots) = self.scope_allocated.pop() {
            self.free_slots.extend(slots);
        }
    }

    fn bind_local_checked(&mut self, name: &str, span: Span) -> Result<usize, VmError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                return Err(compile_error(
                    format!("duplicate local: {name}"),
                    Some(span),
                ));
            }
            let slot = if let Some(reused) = self.free_slots.pop() {
                reused
            } else {
                let s = self.next_local;
                self.next_local += 1;
                s
            };
            scope.insert(name.to_string(), slot);
            if let Some(alloc) = self.scope_allocated.last_mut() {
                alloc.push(slot);
            }
            return Ok(slot);
        }
        let slot = if let Some(reused) = self.free_slots.pop() {
            reused
        } else {
            let s = self.next_local;
            self.next_local += 1;
            s
        };
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

    pub fn compile_module_with_sources(
        &mut self,
        module: &Module,
        sources: HashMap<String, String>,
        entry: Option<String>,
        function_sources: HashMap<String, String>,
    ) -> Result<Program, VmError> {
        self.function_sources = function_sources;
        let mut program = self.compile_module(module)?;
        program.sources = sources;
        program.entry = entry;
        Ok(program)
    }
}

pub fn compile_entry_with_imports(path: &str) -> Result<Program, VmError> {
    let (loaded, sources) = load_module_with_sources(path)?;
    let entry = loaded.module.source_path.clone();
    let mut compiler = Compiler::new();
    compiler.compile_module_with_sources(&loaded.module, sources, entry, loaded.function_sources)
}

fn load_module_with_sources(
    path: &str,
) -> Result<(LoadedModule, HashMap<String, String>), VmError> {
    let mut visited = HashSet::new();
    let mut sources = HashMap::new();
    let module = load_module_inner(path, &mut visited, &mut sources)?;
    Ok((module, sources))
}

fn load_module_inner(
    path: &str,
    visited: &mut HashSet<String>,
    sources: &mut HashMap<String, String>,
) -> Result<LoadedModule, VmError> {
    let normalized = std::path::Path::new(path)
        .canonicalize()
        .map_err(|err| compile_error(format!("error reading {path}: {err}"), None))?;
    let normalized_str = normalized.to_string_lossy().to_string();
    if !visited.insert(normalized_str.clone()) {
        return Ok(LoadedModule {
            module: Module {
                id: at_syntax::NodeId(0),
                functions: Vec::new(),
                stmts: Vec::new(),
                comments: Vec::new(),
                source_path: Some(normalized_str),
            },
            imports: Vec::new(),
            function_sources: HashMap::new(),
        });
    }

    let source = std::fs::read_to_string(&normalized)
        .map_err(|err| compile_error(format!("error reading {path}: {err}"), None))?;
    sources.insert(normalized_str.clone(), source.clone());
    let mut module = parse_module(&source)
        .map_err(|err| compile_error(format!("parse error: {err:?}"), None))?;
    module.source_path = Some(normalized_str.clone());

    let mut merged_functions = module.functions.clone();
    let mut merged_stmts = Vec::new();
    let mut merged_imports = Vec::new();
    let mut merged_function_sources = HashMap::new();
    for func in &merged_functions {
        merged_function_sources.insert(func.name.name.clone(), normalized_str.clone());
    }
    let mut seen_aliases = HashSet::new();
    let base_dir = normalized
        .parent()
        .ok_or_else(|| compile_error(format!("invalid path: {}", normalized.display()), None))?;

    for stmt in module.stmts.drain(..) {
        match stmt {
            Stmt::Import { path, alias, .. } => {
                if !seen_aliases.insert(alias.name.clone()) {
                    return Err(compile_error(
                        format!("duplicate import alias: {}", alias.name),
                        Some(alias.span),
                    ));
                }
                let import_path = if path == "std" || path == "std.at" {
                    ensure_stdlib_path()
                        .map_err(|message| compile_error(message, Some(alias.span)))?
                } else if path.starts_with("http://") || path.starts_with("https://") {
                    fetch_remote(&path, base_dir)
                        .map_err(|message| compile_error(message, Some(alias.span)))?
                } else {
                    base_dir.join(path)
                };
                let mut imported =
                    load_module_inner(&import_path.to_string_lossy(), visited, sources)?;
                let public_function_names: Vec<String> = imported
                    .module
                    .functions
                    .iter()
                    .filter(|func| func.is_pub)
                    .map(|func| func.name.name.clone())
                    .collect();
                prefix_module(&mut imported.module, &alias.name);
                for name in public_function_names {
                    if let Some(source_path) = imported.function_sources.remove(&name) {
                        imported
                            .function_sources
                            .insert(format!("{}.{}", alias.name, name), source_path);
                    }
                }
                merged_imports.push(import_path.to_string_lossy().to_string());
                merged_imports.extend(imported.imports);
                merged_function_sources.extend(imported.function_sources);
                merged_functions.extend(imported.module.functions);
                merged_stmts.extend(imported.module.stmts);
                continue;
            }
            other => merged_stmts.push(other),
        }
    }

    Ok(LoadedModule {
        module: Module {
            id: module.id,
            functions: merged_functions,
            stmts: merged_stmts,
            comments: Vec::new(),
            source_path: Some(normalized_str),
        },
        imports: merged_imports,
        function_sources: merged_function_sources,
    })
}

fn ensure_stdlib_path() -> Result<std::path::PathBuf, String> {
    let dir = std::env::temp_dir().join("at").join("stdlib");
    std::fs::create_dir_all(&dir).map_err(|err| {
        format!(
            "error creating embedded stdlib directory {}: {err}",
            dir.display()
        )
    })?;
    let path = dir.join("std.at");
    let write_file = match std::fs::read_to_string(&path) {
        Ok(existing) => existing != EMBEDDED_STDLIB_SOURCE,
        Err(_) => true,
    };
    if write_file {
        std::fs::write(&path, EMBEDDED_STDLIB_SOURCE).map_err(|err| {
            format!(
                "error writing embedded stdlib file {}: {err}",
                path.display()
            )
        })?;
    }
    Ok(path)
}

fn prefix_module(module: &mut Module, alias: &str) {
    for func in &mut module.functions {
        if func.is_pub {
            func.name.name = format!("{}.{}", alias, func.name.name);
        }
    }
    for stmt in &mut module.stmts {
        match stmt {
            Stmt::Struct { name, is_pub, .. }
            | Stmt::Enum { name, is_pub, .. }
            | Stmt::TypeAlias { name, is_pub, .. } => {
                if *is_pub {
                    name.name = format!("{}.{}", alias, name.name);
                }
            }
            _ => {}
        }
    }
}

fn fetch_remote(url: &str, base_dir: &std::path::Path) -> Result<std::path::PathBuf, String> {
    match resolve_cached_path(base_dir, url) {
        Ok(path) if path.exists() => return Ok(path),
        Ok(_) => {}
        Err(_) => {}
    }
    for attempt in 1..=REMOTE_FETCH_RETRIES {
        let request = ureq::get(url).timeout(Duration::from_secs(REMOTE_FETCH_TIMEOUT_SECS));
        match request.call() {
            Ok(response) => {
                let contents = read_remote_response(url, response)?;
                return store_remote_contents(url, &contents, base_dir);
            }
            Err(ureq::Error::Status(status, _response)) => {
                if status >= 500 && attempt < REMOTE_FETCH_RETRIES {
                    std::thread::sleep(Duration::from_millis(150 * attempt as u64));
                    continue;
                }
                return Err(format!(
                    "error fetching {url}: remote returned status {status}"
                ));
            }
            Err(err) => {
                if attempt < REMOTE_FETCH_RETRIES {
                    std::thread::sleep(Duration::from_millis(150 * attempt as u64));
                    continue;
                }
                return Err(format!("error fetching {url}: {err}"));
            }
        }
    }
    Err(format!("error fetching {url}: exhausted retries"))
}

fn read_remote_response(url: &str, response: ureq::Response) -> Result<String, String> {
    let mut reader = response.into_reader().take(REMOTE_FETCH_MAX_BYTES + 1);
    let mut bytes = Vec::new();
    reader
        .read_to_end(&mut bytes)
        .map_err(|err| format!("error reading response from {url}: {err}"))?;
    if bytes.len() as u64 > REMOTE_FETCH_MAX_BYTES {
        return Err(format!(
            "error fetching {url}: response exceeds {} bytes",
            REMOTE_FETCH_MAX_BYTES
        ));
    }
    String::from_utf8(bytes)
        .map_err(|_| format!("error reading response from {url}: body is not valid utf-8"))
}

fn store_remote_contents(
    url: &str,
    contents: &str,
    base_dir: &std::path::Path,
) -> Result<std::path::PathBuf, String> {
    let cache_dir = cache_dir(base_dir);
    std::fs::create_dir_all(&cache_dir)
        .map_err(|err| format!("error creating cache directory: {err}"))?;
    let hash = hash_contents(contents);
    let filename = format!("{hash}.at");
    let cache_path = cache_dir.join(filename);
    std::fs::write(&cache_path, contents)
        .map_err(|err| format!("error writing {}: {err}", cache_path.display()))?;
    let mut lockfile = load_lockfile(base_dir)?;
    lockfile.entries.insert(url.to_string(), hash);
    save_lockfile(base_dir, &lockfile)?;
    Ok(cache_path)
}

fn resolve_cached_path(
    base_dir: &std::path::Path,
    url: &str,
) -> Result<std::path::PathBuf, String> {
    let lockfile = load_lockfile(base_dir)?;
    if let Some(hash) = lockfile.entries.get(url) {
        let path = cache_dir(base_dir).join(format!("{hash}.at"));
        if path.exists() {
            let contents = std::fs::read_to_string(&path)
                .map_err(|err| format!("error reading {}: {err}", path.display()))?;
            let actual = hash_contents(&contents);
            if actual != *hash {
                return Err(format!(
                    "cached remote integrity check failed for {}: expected {}, found {}",
                    path.display(),
                    hash,
                    actual
                ));
            }
        }
        return Ok(path);
    }
    Ok(cache_dir(base_dir).join(cache_file_name(url)))
}

fn cache_dir(base_dir: &std::path::Path) -> std::path::PathBuf {
    base_dir.join(".at").join("cache")
}

fn cache_file_name(url: &str) -> String {
    let encoded = hex::encode(Sha256::digest(url.as_bytes()));
    format!("{encoded}.at")
}

fn hash_contents(contents: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(contents.as_bytes());
    hex::encode(hasher.finalize())
}

#[derive(Debug, Default, Clone)]
struct Lockfile {
    version: u32,
    entries: HashMap<String, String>,
}

const LOCKFILE_VERSION: u32 = 1;

fn lockfile_path(base_dir: &std::path::Path) -> std::path::PathBuf {
    base_dir.join(".at").join("lock")
}

fn load_lockfile(base_dir: &std::path::Path) -> Result<Lockfile, String> {
    let path = lockfile_path(base_dir);
    if !path.exists() {
        return Ok(Lockfile {
            version: LOCKFILE_VERSION,
            entries: HashMap::new(),
        });
    }
    let contents = std::fs::read_to_string(&path)
        .map_err(|err| format!("error reading {}: {err}", path.display()))?;
    let mut entries = HashMap::new();
    let mut version = 0;
    for line in contents.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let mut parts = line.split_whitespace();
        let token = match parts.next() {
            Some(value) => value.to_string(),
            None => continue,
        };
        if token == "version" {
            let value = match parts.next() {
                Some(value) => value,
                None => continue,
            };
            version = value
                .parse::<u32>()
                .map_err(|_| format!("invalid lockfile version: {value}"))?;
            continue;
        }
        if let Some(hash) = parts.next() {
            entries.insert(token, hash.to_string());
        }
    }
    if version > LOCKFILE_VERSION {
        return Err(format!("unsupported lockfile version: {version}"));
    }
    let version = if version == 0 {
        LOCKFILE_VERSION
    } else {
        version
    };
    Ok(Lockfile { version, entries })
}

fn save_lockfile(base_dir: &std::path::Path, lockfile: &Lockfile) -> Result<(), String> {
    let path = lockfile_path(base_dir);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|err| format!("error creating lockfile dir: {err}"))?;
    }
    let mut lines: Vec<String> = lockfile
        .entries
        .iter()
        .map(|(url, hash)| format!("{url} {hash}"))
        .collect();
    lines.sort();
    lines.insert(0, format!("version {}", lockfile.version));
    std::fs::write(path, lines.join("\n"))
        .map_err(|err| format!("error writing lockfile: {err}"))?;
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Frame {
    chunk_id: usize,
    ip: usize,
    locals: Vec<Value>,
    capabilities: HashSet<usize>,
    defers: Vec<Rc<ClosureValue>>,
    capability_scopes: Vec<Vec<usize>>,
}

/// A cooperative task: a suspended async computation with its own
/// stack and call frames.  Tasks are scheduled round-robin by the
/// [`Scheduler`] and make progress in instruction-quantum slices.
#[derive(Debug, Clone)]
struct Task {
    id: TaskId,
    frames: Vec<Frame>,
    stack: Vec<Value>,
    /// Whether the task has finished execution.
    done: bool,
}

/// Single-threaded, deterministic cooperative scheduler.
///
/// Maintains a FIFO ready queue of [`Task`]s and runs each for up to
/// `quantum` instructions before rotating to the next.
#[derive(Debug)]
struct Scheduler {
    ready: std::collections::VecDeque<Task>,
    next_task_id: u64,
    /// Maximum instructions per task time-slice.
    quantum: usize,
    /// Results of completed tasks, keyed by [`TaskId`].
    completed: HashMap<TaskId, Value>,
}

impl Scheduler {
    fn new() -> Self {
        Self {
            ready: std::collections::VecDeque::new(),
            next_task_id: 0,
            quantum: 1024,
            completed: HashMap::new(),
        }
    }

    fn spawn(&mut self, frames: Vec<Frame>, stack: Vec<Value>) -> TaskId {
        let id = TaskId(self.next_task_id);
        self.next_task_id += 1;
        self.ready.push_back(Task {
            id,
            frames,
            stack,
            done: false,
        });
        id
    }

    fn reset(&mut self) {
        self.ready.clear();
        self.completed.clear();
    }
}

pub struct Vm {
    stack: Vec<Value>,
    frames: Vec<Frame>,
    locals_pool: Vec<Vec<Value>>,
    initial_capabilities: HashSet<usize>,
    instruction_count: usize,
    max_instructions: Option<usize>,
    max_frames: Option<usize>,
    output_buffer: Option<Rc<RefCell<Vec<String>>>>,
    debugger: Option<Rc<RefCell<dyn DebuggerHook>>>,
    profiler: Option<Profiler>,
    scheduler: Scheduler,
}

#[derive(Debug, Clone, Default)]
pub struct Profiler {
    pub total_instructions: usize,
    pub op_counts: HashMap<String, usize>,
    pub function_counts: HashMap<String, usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionMode {
    ToCompletion,
    UntilYield,
    /// Run until an `await` on an unresolved future is encountered,
    /// then return `Ok(None)` so the scheduler can context-switch.
    UntilSuspend,
}

impl Vm {
    fn capability_id_for_name(name: &str) -> Option<usize> {
        match name {
            "time" => Some(0),
            "rng" => Some(1),
            _ => None,
        }
    }

    /// Prepare a [`Frame`] for the given future's async function.
    fn prepare_future_frame(
        &mut self,
        program: &Program,
        future: &FutureValue,
    ) -> Result<Frame, VmError> {
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
        Ok(Frame {
            chunk_id: future.func_id,
            ip: 0,
            locals,
            capabilities: future.capabilities.clone(),
            defers: Vec::new(),
            capability_scopes: Vec::new(),
        })
    }

    /// Spawn a future as a scheduler [`Task`] and run it.  Returns the
    /// result immediately if the task completes within one quantum, or
    /// `None` if it suspended (hit a nested `await`).
    fn run_spawned_task(
        &mut self,
        program: &Program,
        task_id: TaskId,
    ) -> Result<Option<Value>, VmError> {
        // Already completed (e.g. by a prior scheduler round)?
        if let Some(val) = self.scheduler.completed.remove(&task_id) {
            return Ok(Some(val));
        }
        let task_pos = self.scheduler.ready.iter().position(|t| t.id == task_id);
        let mut task = match task_pos {
            Some(pos) => self
                .scheduler
                .ready
                .remove(pos)
                .expect("position was just found"),
            None => {
                return Err(self.runtime_error(
                    format!("task {} not found", task_id.0),
                    None,
                    program,
                ));
            }
        };

        let saved_stack = std::mem::take(&mut self.stack);
        let saved_frames = std::mem::take(&mut self.frames);

        self.stack = std::mem::take(&mut task.stack);
        self.frames = std::mem::take(&mut task.frames);

        let result = match self.run_with_existing_frames(program, ExecutionMode::UntilSuspend) {
            Ok(val) => val,
            Err(err) => {
                self.stack = saved_stack;
                self.frames = saved_frames;
                return Err(err);
            }
        };

        if self.frames.is_empty() {
            // Task completed.
            let value = result.unwrap_or(Value::Unit);
            self.scheduler.completed.insert(task_id, value.clone());
            self.stack = saved_stack;
            self.frames = saved_frames;
            Ok(Some(value))
        } else {
            // Task suspended — save and re-enqueue.
            task.stack = std::mem::take(&mut self.stack);
            task.frames = std::mem::take(&mut self.frames);
            self.scheduler.ready.push_back(task);
            self.stack = saved_stack;
            self.frames = saved_frames;
            Ok(None)
        }
    }

    /// Run the scheduler round-robin until the target task completes.
    fn scheduler_drain_until(
        &mut self,
        program: &Program,
        target_id: TaskId,
    ) -> Result<Value, VmError> {
        loop {
            if let Some(val) = self.scheduler.completed.remove(&target_id) {
                return Ok(val);
            }
            if self.scheduler.ready.is_empty() {
                return Err(self.runtime_error(
                    format!("deadlock: task {} never completed", target_id.0),
                    None,
                    program,
                ));
            }
            // Round-robin: run the next ready task for one quantum.
            let mut task = self
                .scheduler
                .ready
                .pop_front()
                .expect("ready queue non-empty");
            if task.done {
                continue;
            }
            let task_id = task.id;

            let saved_stack = std::mem::take(&mut self.stack);
            let saved_frames = std::mem::take(&mut self.frames);

            self.stack = std::mem::take(&mut task.stack);
            self.frames = std::mem::take(&mut task.frames);

            let result = match self.run_with_existing_frames(program, ExecutionMode::UntilSuspend) {
                Ok(val) => val,
                Err(err) => {
                    self.stack = saved_stack;
                    self.frames = saved_frames;
                    return Err(err);
                }
            };

            if self.frames.is_empty() {
                let value = result.unwrap_or(Value::Unit);
                self.scheduler.completed.insert(task_id, value);
                task.done = true;
            } else {
                task.stack = std::mem::take(&mut self.stack);
                task.frames = std::mem::take(&mut self.frames);
                self.scheduler.ready.push_back(task);
            }

            self.stack = saved_stack;
            self.frames = saved_frames;
        }
    }

    /// Run a future to completion using the cooperative scheduler.
    ///
    /// The future is spawned as a [`Task`].  If it encounters nested
    /// `await` expressions, they are recursively spawned as sub-tasks
    /// and the scheduler round-robins between them.
    fn run_future(&mut self, program: &Program, future: &FutureValue) -> Result<Value, VmError> {
        let frame = self.prepare_future_frame(program, future)?;
        let task_id = self.scheduler.spawn(vec![frame], Vec::new());

        match self.run_spawned_task(program, task_id)? {
            Some(value) => Ok(value),
            None => self.scheduler_drain_until(program, task_id),
        }
    }

    fn run_generator(
        &mut self,
        program: &Program,
        generator: &mut GeneratorValue,
    ) -> Result<Option<Value>, VmError> {
        let func = program.functions.get(generator.func_id).ok_or_else(|| {
            self.runtime_error(
                format!("invalid function id: {}", generator.func_id),
                None,
                program,
            )
        })?;
        if func.is_async {
            return Err(self.runtime_error(
                "generator function cannot be async".to_string(),
                None,
                program,
            ));
        }
        if generator.done {
            return Ok(None);
        }
        if let Some(max) = self.max_frames {
            if self.frames.len().saturating_add(generator.frames.len()) >= max {
                return Err(self.runtime_error(
                    format!("stack overflow: maximum call depth {} exceeded", max),
                    None,
                    program,
                ));
            }
        }
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_frames = std::mem::take(&mut self.frames);
        let saved_instruction_count = self.instruction_count;

        self.stack = std::mem::take(&mut generator.stack);
        self.frames = std::mem::take(&mut generator.frames);
        for frame in &mut self.frames {
            frame.capabilities = generator.capabilities.clone();
        }
        let result = self.run_with_existing_frames(program, ExecutionMode::UntilYield)?;
        generator.stack = std::mem::take(&mut self.stack);
        generator.frames = std::mem::take(&mut self.frames);
        for frame in &mut generator.frames {
            frame.capabilities = generator.capabilities.clone();
        }

        self.stack = saved_stack;
        self.frames = saved_frames;
        self.instruction_count = saved_instruction_count;

        if generator.frames.is_empty() {
            generator.done = true;
            return Ok(None);
        }

        if result.is_none() {
            generator.done = true;
        }

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
                let (name, source_path, span) = if frame.chunk_id == usize::MAX {
                    (
                        "<main>".to_string(),
                        program.entry.clone(),
                        span_at(&program.main, frame.ip),
                    )
                } else if let Some(func) = program.functions.get(frame.chunk_id) {
                    (
                        func.name.clone(),
                        func.source_path.clone(),
                        span_at(&func.chunk, frame.ip),
                    )
                } else {
                    ("<unknown>".to_string(), None, None)
                };
                StackFrame {
                    name,
                    source_path,
                    span,
                }
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
            debugger: None,
            profiler: None,
            scheduler: Scheduler::new(),
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn with_capabilities(capabilities: HashSet<String>) -> Self {
        let mut ids = HashSet::new();
        for capability in capabilities {
            if let Some(id) = Self::capability_id_for_name(&capability) {
                ids.insert(id);
            }
        }
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            locals_pool: Vec::new(),
            initial_capabilities: ids,
            instruction_count: 0,
            max_instructions: None,
            max_frames: None,
            output_buffer: None,
            debugger: None,
            profiler: None,
            scheduler: Scheduler::new(),
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
            debugger: None,
            profiler: None,
            scheduler: Scheduler::new(),
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
            debugger: None,
            profiler: None,
            scheduler: Scheduler::new(),
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
            debugger: None,
            profiler: None,
            scheduler: Scheduler::new(),
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
            debugger: None,
            profiler: None,
            scheduler: Scheduler::new(),
        }
    }

    pub fn with_debugger(debugger: Rc<RefCell<dyn DebuggerHook>>) -> Self {
        let mut vm = Vm::new();
        vm.debugger = Some(debugger);
        vm
    }

    pub fn with_profiler() -> Self {
        let mut vm = Vm::new();
        vm.profiler = Some(Profiler::default());
        vm
    }

    pub fn enable_profiler(&mut self) {
        if self.profiler.is_none() {
            self.profiler = Some(Profiler::default());
        }
    }

    pub fn take_profiler(&mut self) -> Option<Profiler> {
        self.profiler.take()
    }

    pub fn run(&mut self, program: &Program) -> Result<Option<Value>, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.instruction_count = 0;
        self.scheduler.reset();
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
        self.run_with_existing_frames(program, ExecutionMode::ToCompletion)
    }

    fn run_with_existing_frames(
        &mut self,
        program: &Program,
        mode: ExecutionMode,
    ) -> Result<Option<Value>, VmError> {
        let result = self.run_dispatch_loop(program, mode);
        // Enrich runtime errors with a stack trace built from the live VM state.
        // This replaces the previous per-instruction thread-local approach.
        match result {
            Err(VmError::Runtime {
                message,
                span,
                stack: None,
            }) => Err(VmError::Runtime {
                message,
                span,
                stack: Some(self.build_stack_trace(program)),
            }),
            other => other,
        }
    }

    fn run_dispatch_loop(
        &mut self,
        program: &Program,
        mode: ExecutionMode,
    ) -> Result<Option<Value>, VmError> {
        let quantum = self.scheduler.quantum;
        let mut quantum_counter: usize = 0;
        loop {
            // Check execution limit
            if let Some(max) = self.max_instructions {
                if self.instruction_count >= max {
                    return Err(VmError::ExecutionLimit {
                        message: format!("execution limit exceeded: {} instructions", max),
                    });
                }
            }
            // Quantum preemption for cooperative scheduling.
            if mode == ExecutionMode::UntilSuspend {
                quantum_counter += 1;
                if quantum_counter > quantum {
                    return Ok(None);
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

            if let Some(profiler) = self.profiler.as_mut() {
                profiler.total_instructions += 1;
                let func_name = if frame_chunk_id == usize::MAX {
                    "<main>"
                } else if let Some(func) = program.functions.get(frame_chunk_id) {
                    func.name.as_str()
                } else {
                    "<unknown>"
                };
                *profiler
                    .function_counts
                    .entry(func_name.to_string())
                    .or_insert(0) += 1;
                if let Some(op) = chunk.code.get(frame_ip) {
                    let name = Self::opcode_name(op);
                    *profiler.op_counts.entry(name.to_string()).or_insert(0) += 1;
                }
            }

            if let Some(debugger) = self.debugger.as_ref() {
                debugger.borrow_mut().before_op(
                    program,
                    frame_chunk_id,
                    frame_ip,
                    chunk.code.get(frame_ip),
                );
            }

            if frame_ip >= chunk.code.len() {
                let result = self.stack.pop();
                if frame_chunk_id != usize::MAX {
                    let defers = std::mem::take(&mut self.frames[frame_index].defers);
                    let parent_capabilities = self.frames[frame_index].capabilities.clone();
                    self.frames.pop();
                    for closure in defers.into_iter().rev() {
                        let func = program.functions.get(closure.func_id).ok_or_else(|| {
                            runtime_error_at(
                                format!("invalid defer function id: {}", closure.func_id),
                                None,
                            )
                        })?;
                        let required_locals = func.captures + func.params;
                        let mut locals = self.take_locals(func.locals.max(required_locals));
                        for (idx, value) in closure.captures.iter().cloned().enumerate() {
                            locals[idx] = value;
                        }
                        self.frames.push(Frame {
                            chunk_id: closure.func_id,
                            ip: 0,
                            locals,
                            capabilities: parent_capabilities.clone(),
                            defers: Vec::new(),
                            capability_scopes: Vec::new(),
                        });
                        let _ =
                            self.run_with_existing_frames(program, ExecutionMode::ToCompletion)?;
                    }
                }
                return Ok(result);
            }

            // Stack trace and span are built on-demand only when an error
            // occurs, avoiding per-instruction allocation overhead.

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
                    self.frames[frame_index].capabilities.insert(*name);
                    if let Some(scope) = self.frames[frame_index].capability_scopes.last_mut() {
                        scope.push(*name);
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
                            let label = program
                                .capability_names
                                .get(*need)
                                .cloned()
                                .unwrap_or_else(|| "unknown".to_string());
                            return Err(runtime_error_at(
                                format!("missing capability: {}", label),
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
                Op::CallGenerator(func_id, arg_count) => {
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
                    if func.is_async {
                        return Err(runtime_error_at(
                            "generator function cannot be async".to_string(),
                            span_at(chunk, frame_ip),
                        ));
                    }
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
                            let label = program
                                .capability_names
                                .get(*need)
                                .cloned()
                                .unwrap_or_else(|| "unknown".to_string());
                            return Err(runtime_error_at(
                                format!("missing capability: {}", label),
                                span_at(chunk, frame_ip),
                            ));
                        }
                    }
                    let required_locals = func.params + func.captures;
                    let mut locals = self.take_locals(func.locals.max(required_locals));
                    for (idx, value) in args.iter().cloned().enumerate() {
                        locals[func.captures + idx] = value;
                    }
                    let generator = GeneratorValue {
                        func_id: *func_id,
                        captures: Vec::new(),
                        args,
                        capabilities: self.frames[frame_index].capabilities.clone(),
                        frames: vec![Frame {
                            chunk_id: *func_id,
                            ip: 0,
                            locals,
                            capabilities: self.frames[frame_index].capabilities.clone(),
                            defers: Vec::new(),
                            capability_scopes: Vec::new(),
                        }],
                        stack: Vec::new(),
                        done: false,
                    };
                    self.stack
                        .push(Value::Generator(Rc::new(RefCell::new(generator))));
                }
                Op::Builtin(builtin) => {
                    let result = match builtin {
                        Builtin::TimeNow => {
                            let time_id = Vm::capability_id_for_name("time");
                            if time_id
                                .map(|id| !self.frames[frame_index].capabilities.contains(&id))
                                .unwrap_or(true)
                            {
                                return Err(runtime_error_at(
                                    "missing capability: time".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let now = SystemTime::now()
                                .duration_since(UNIX_EPOCH)
                                .map_err(|_| {
                                    runtime_error_at(
                                        "system clock is before unix epoch".to_string(),
                                        span_at(chunk, frame_ip),
                                    )
                                })?
                                .as_secs() as i64;
                            Value::Int(now)
                        }
                        Builtin::TimeFixed => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::String(timestamp) => {
                                    let seconds = parse_fixed_timestamp(timestamp.as_str()).ok_or_else(
                                        || {
                                            runtime_error_at(
                                                "time.fixed expects RFC3339 UTC like 2026-01-01T00:00:00Z"
                                                    .to_string(),
                                                span_at(chunk, frame_ip),
                                            )
                                        },
                                    )?;
                                    Value::Int(seconds)
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "time.fixed expects string".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::RngDeterministic => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match value {
                                Value::Int(seed) => {
                                    Value::Int(deterministic_random_from_seed(seed))
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "invalid builtin usage".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::RngUuid => {
                            let rng_id = Vm::capability_id_for_name("rng");
                            if rng_id
                                .map(|id| !self.frames[frame_index].capabilities.contains(&id))
                                .unwrap_or(true)
                            {
                                return Err(runtime_error_at(
                                    "missing capability: rng".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            Value::String(Rc::new(Uuid::new_v4().to_string()))
                        }
                        Builtin::Assert => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
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
                            Value::Unit
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
                                Value::Map(entries) => Value::Int(entries.len() as i64),
                                Value::String(value) => Value::Int(value.len() as i64),
                                _ => {
                                    return Err(runtime_error_at(
                                        "len expects array, map, or string".to_string(),
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
                        Builtin::Split => {
                            let delimiter = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let text = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match (text, delimiter) {
                                (Value::String(text), Value::String(delim)) => {
                                    let parts = if delim.is_empty() {
                                        text.chars()
                                            .map(|ch| Value::String(Rc::new(ch.to_string())))
                                            .collect::<Vec<_>>()
                                    } else {
                                        text.split(delim.as_str())
                                            .map(|part| Value::String(Rc::new(part.to_string())))
                                            .collect::<Vec<_>>()
                                    };
                                    Value::Array(Rc::new(parts))
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "split expects string and delimiter".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Trim => {
                            let text = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match text {
                                Value::String(text) => {
                                    Value::String(Rc::new(text.trim().to_string()))
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "trim expects string".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Substring => {
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
                            let text = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let (text, start, end) = match (text, start, end) {
                                (Value::String(text), Value::Int(start), Value::Int(end)) => {
                                    (text, start, end)
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "substring expects string and int bounds".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            };
                            if start < 0 || end < start {
                                return Err(runtime_error_at(
                                    "substring out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let start = start as usize;
                            let end = end as usize;
                            let chars: Vec<char> = text.chars().collect();
                            if end > chars.len() {
                                return Err(runtime_error_at(
                                    "substring out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let result: String = chars[start..end].iter().collect();
                            Value::String(Rc::new(result))
                        }
                        Builtin::CharAt => {
                            let index = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let text = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let (text, index) = match (text, index) {
                                (Value::String(text), Value::Int(index)) => (text, index),
                                _ => {
                                    return Err(runtime_error_at(
                                        "char_at expects string and int".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            };
                            if index < 0 {
                                return Err(runtime_error_at(
                                    "char_at out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                ));
                            }
                            let index = index as usize;
                            let ch = text.chars().nth(index).ok_or_else(|| {
                                runtime_error_at(
                                    "char_at out of bounds".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            Value::String(Rc::new(ch.to_string()))
                        }
                        Builtin::ToUpper => {
                            let text = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match text {
                                Value::String(text) => Value::String(Rc::new(text.to_uppercase())),
                                _ => {
                                    return Err(runtime_error_at(
                                        "to_upper expects string".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::ToLower => {
                            let text = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match text {
                                Value::String(text) => Value::String(Rc::new(text.to_lowercase())),
                                _ => {
                                    return Err(runtime_error_at(
                                        "to_lower expects string".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::ParseInt => {
                            let text = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match text {
                                Value::String(text) => match text.trim().parse::<i64>() {
                                    Ok(value) => Value::Option(Some(Rc::new(Value::Int(value)))),
                                    Err(_) => Value::Option(None),
                                },
                                _ => {
                                    return Err(runtime_error_at(
                                        "parse_int expects string".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::ToString => {
                            let value = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            Value::String(Rc::new(format_value(&value)))
                        }
                        Builtin::Keys => {
                            let map = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match map {
                                Value::Map(entries) => {
                                    let keys =
                                        entries.keys().map(MapKey::to_value).collect::<Vec<_>>();
                                    Value::Array(Rc::new(keys))
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "keys expects map".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Values => {
                            let map = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            match map {
                                Value::Map(entries) => {
                                    let values = entries.values().cloned().collect::<Vec<_>>();
                                    Value::Array(Rc::new(values))
                                }
                                _ => {
                                    return Err(runtime_error_at(
                                        "values expects map".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
                            }
                        }
                        Builtin::Next => {
                            let gen = self.stack.pop().ok_or_else(|| {
                                runtime_error_at(
                                    "stack underflow".to_string(),
                                    span_at(chunk, frame_ip),
                                )
                            })?;
                            let generator = match gen {
                                Value::Generator(gen) => gen,
                                _ => {
                                    return Err(self.runtime_error(
                                        "next expects generator".to_string(),
                                        self.current_span(program),
                                        program,
                                    ))
                                }
                            };
                            let mut generator = generator.borrow_mut();
                            match self.run_generator(program, &mut generator)? {
                                Some(value) => Value::Option(Some(Rc::new(value))),
                                None => Value::Option(None),
                            }
                        }
                    };
                    self.stack.push(result);
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
                            self.frames[frame_index].defers.push(closure);
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
                            // Spawn the future as a task.
                            let frame = self.prepare_future_frame(program, &future)?;
                            let task_id = self.scheduler.spawn(vec![frame], Vec::new());

                            // Try to run it — may complete immediately.
                            match self.run_spawned_task(program, task_id)? {
                                Some(result) => {
                                    self.stack.push(result);
                                }
                                None => {
                                    // Task suspended.  In UntilSuspend mode,
                                    // propagate the suspension so the scheduler
                                    // can context-switch to another task.
                                    if mode == ExecutionMode::UntilSuspend {
                                        // Stay at the Await instruction so it
                                        // re-executes on resume, picks up the
                                        // TaskHandle, and resolves the completed
                                        // value from the scheduler.
                                        self.frames[frame_index].ip = frame_ip;
                                        self.stack.push(Value::TaskHandle(task_id));
                                        return Ok(None);
                                    }
                                    // ToCompletion — drain until done.
                                    let result = self.scheduler_drain_until(program, task_id)?;
                                    self.stack.push(result);
                                }
                            }
                        }
                        Value::TaskHandle(task_id) => {
                            // Resuming after suspension — check completion.
                            if let Some(result) = self.scheduler.completed.remove(&task_id) {
                                self.stack.push(result);
                            } else if mode == ExecutionMode::UntilSuspend {
                                // Still pending — suspend again.
                                self.frames[frame_index].ip = frame_ip;
                                self.stack.push(Value::TaskHandle(task_id));
                                return Ok(None);
                            } else {
                                let result = self.scheduler_drain_until(program, task_id)?;
                                self.stack.push(result);
                            }
                        }
                        other => {
                            return Err(self.runtime_error(
                                format!(
                                    "await expects future or task, got {}",
                                    format_value(&other)
                                ),
                                self.current_span(program),
                                program,
                            ))
                        }
                    }
                }
                Op::Yield => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    if mode == ExecutionMode::UntilYield {
                        self.frames[frame_index].ip = frame_ip + 1;
                        return Ok(Some(value));
                    }
                    self.stack.push(value);
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
                    let mut entries = IndexMap::with_capacity(*count);
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
                        let map_key = MapKey::try_from_value(&key).map_err(|message| {
                            runtime_error_at(message, span_at(chunk, frame_ip))
                        })?;
                        entries.insert(map_key, value);
                    }
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
                    let mut entries = IndexMap::new();
                    for item in chunks {
                        match item {
                            Value::Map(values) => {
                                for (key, value) in values.iter() {
                                    entries.insert(key.clone(), value.clone());
                                }
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
                            let key = MapKey::try_from_value(&index_value).map_err(|message| {
                                runtime_error_at(message, span_at(chunk, frame_ip))
                            })?;
                            let found = entries.get(&key).cloned();
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
                            let mut items = items.clone();
                            let item_values = Rc::make_mut(&mut items);
                            item_values[idx] = value;
                            self.stack.push(Value::Array(items));
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
                            let mut items = items.clone();
                            let item_values = Rc::make_mut(&mut items);
                            item_values[idx] = value;
                            self.stack.push(Value::Tuple(items));
                        }
                        Value::Map(entries) => {
                            let key = MapKey::try_from_value(&index_value).map_err(|message| {
                                runtime_error_at(message, span_at(chunk, frame_ip))
                            })?;
                            let mut entries = entries.clone();
                            let map_entries: &mut IndexMap<MapKey, Value> =
                                Rc::make_mut(&mut entries);
                            map_entries.insert(key, value);
                            self.stack.push(Value::Map(entries));
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
                            let mut items = items.clone();
                            let item_values = Rc::make_mut(&mut items);
                            let mut pending = Some(value);
                            for item in item_values.iter_mut() {
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
                                item_values.push(Value::Tuple(Rc::new(vec![
                                    Value::String(Rc::new(field.clone())),
                                    value,
                                ])));
                            }
                            self.stack.push(Value::Array(items));
                        }
                        Value::Struct(fields) => {
                            let mut fields = fields.clone();
                            Rc::make_mut(&mut fields).insert(field.clone(), value);
                            self.stack.push(Value::Struct(fields));
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
                Op::ToBool => {
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let value = match value {
                        Value::Bool(value) => Value::Bool(value),
                        Value::Int(value) => Value::Bool(value != 0),
                        Value::Float(value) => Value::Bool(value != 0.0),
                        _ => {
                            return Err(runtime_error_at(
                                "invalid boolean operand".to_string(),
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
                                _ => {
                                    return Err(runtime_error_at(
                                        "invalid bitwise operator".to_string(),
                                        span_at(chunk, frame_ip),
                                    ))
                                }
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
                    let value = self.stack.pop().ok_or_else(|| {
                        runtime_error_at("stack underflow".to_string(), span_at(chunk, frame_ip))
                    })?;
                    let defers = std::mem::take(&mut self.frames[frame_index].defers);
                    let parent_capabilities = self.frames[frame_index].capabilities.clone();
                    self.pop_frame();
                    if !defers.is_empty() {
                        for closure in defers.into_iter().rev() {
                            let func = program.functions.get(closure.func_id).ok_or_else(|| {
                                runtime_error_at(
                                    format!("invalid defer function id: {}", closure.func_id),
                                    None,
                                )
                            })?;
                            let required_locals = func.captures + func.params;
                            let mut locals = self.take_locals(func.locals.max(required_locals));
                            for (idx, val) in closure.captures.iter().cloned().enumerate() {
                                locals[idx] = val;
                            }
                            self.frames.push(Frame {
                                chunk_id: closure.func_id,
                                ip: 0,
                                locals,
                                capabilities: parent_capabilities.clone(),
                                defers: Vec::new(),
                                capability_scopes: Vec::new(),
                            });
                            let _ = self
                                .run_with_existing_frames(program, ExecutionMode::ToCompletion)?;
                        }
                    }
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

    fn opcode_name(op: &Op) -> &'static str {
        match op {
            Op::Const(_) => "Const",
            Op::LoadLocal(_) => "LoadLocal",
            Op::StoreLocal(_) => "StoreLocal",
            Op::GrantCapability(_) => "GrantCapability",
            Op::PushCapabilityScope => "PushCapabilityScope",
            Op::PopCapabilityScope => "PopCapabilityScope",
            Op::Call(_, _) => "Call",
            Op::CallAsync(_, _) => "CallAsync",
            Op::CallGenerator(_, _) => "CallGenerator",
            Op::Builtin(_) => "Builtin",
            Op::Try => "Try",
            Op::Throw => "Throw",
            Op::Defer => "Defer",
            Op::Add => "Add",
            Op::Sub => "Sub",
            Op::Mul => "Mul",
            Op::Div => "Div",
            Op::Mod => "Mod",
            Op::Array(_) => "Array",
            Op::ArraySpread(_) => "ArraySpread",
            Op::Tuple(_) => "Tuple",
            Op::Index => "Index",
            Op::StoreIndex => "StoreIndex",
            Op::StoreMember(_) => "StoreMember",
            Op::Struct(_) => "Struct",
            Op::GetMember(_) => "GetMember",
            Op::Enum { .. } => "Enum",
            Op::Closure(_, _) => "Closure",
            Op::CallValue(_) => "CallValue",
            Op::Await => "Await",
            Op::Yield => "Yield",
            Op::Range(_) => "Range",
            Op::Map(_) => "Map",
            Op::MapSpread(_) => "MapSpread",
            Op::IsType(_) => "IsType",
            Op::Cast(_) => "Cast",
            Op::Eq => "Eq",
            Op::Neq => "Neq",
            Op::Lt => "Lt",
            Op::Lte => "Lte",
            Op::Gt => "Gt",
            Op::Gte => "Gte",
            Op::BitAnd => "BitAnd",
            Op::BitOr => "BitOr",
            Op::BitXor => "BitXor",
            Op::Shl => "Shl",
            Op::Shr => "Shr",
            Op::Neg => "Neg",
            Op::Not => "Not",
            Op::ToBool => "ToBool",
            Op::MatchResultOk(_) => "MatchResultOk",
            Op::MatchResultErr(_) => "MatchResultErr",
            Op::MatchOptionSome(_) => "MatchOptionSome",
            Op::MatchOptionNone(_) => "MatchOptionNone",
            Op::MatchStruct(_, _) => "MatchStruct",
            Op::MatchEnum { .. } => "MatchEnum",
            Op::MatchInt(_, _) => "MatchInt",
            Op::MatchBool(_, _) => "MatchBool",
            Op::MatchString(_, _) => "MatchString",
            Op::MatchTuple(_, _) => "MatchTuple",
            Op::MatchFail => "MatchFail",
            Op::JumpIfFalse(_) => "JumpIfFalse",
            Op::Jump(_) => "Jump",
            Op::Return => "Return",
            Op::Pop => "Pop",
            Op::Halt => "Halt",
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

fn span_at(chunk: &Chunk, ip: usize) -> Option<Span> {
    chunk.spans.get(ip).cloned().flatten()
}

/// Create a runtime error with span but without stack trace.
///
/// The stack trace is attached later by the execution loop when it catches
/// the error and has access to the live VM frame state.  This avoids the
/// cost of building a stack trace on every instruction.
fn runtime_error_at(message: String, span: Option<Span>) -> VmError {
    VmError::Runtime {
        message,
        span,
        stack: None,
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
            TypeCheck::Enum(name) => name.to_string(),
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
    // Verify the builtin exists in the shared registry (single source of truth).
    let _meta = at_syntax::lookup_builtin(base, name, args)?;
    // Map the registry entry to the VM's Builtin enum.
    match (base, name) {
        ("time", "now") => Some(Builtin::TimeNow),
        ("time", "fixed") => Some(Builtin::TimeFixed),
        ("rng", "deterministic") => Some(Builtin::RngDeterministic),
        ("rng", "uuid") => Some(Builtin::RngUuid),
        ("", "assert") => Some(Builtin::Assert),
        ("", "assert_eq") => Some(Builtin::AssertEq),
        ("", "print") => Some(Builtin::Print),
        ("", "next") => Some(Builtin::Next),
        ("", "len") => Some(Builtin::Len),
        ("", "append") => Some(Builtin::Append),
        ("", "contains") => Some(Builtin::Contains),
        ("", "slice") => Some(Builtin::Slice),
        ("", "split") => Some(Builtin::Split),
        ("", "trim") => Some(Builtin::Trim),
        ("", "substring") => Some(Builtin::Substring),
        ("", "char_at") => Some(Builtin::CharAt),
        ("", "to_upper") => Some(Builtin::ToUpper),
        ("", "to_lower") => Some(Builtin::ToLower),
        ("", "parse_int") => Some(Builtin::ParseInt),
        ("", "to_string") => Some(Builtin::ToString),
        ("", "keys") => Some(Builtin::Keys),
        ("", "values") => Some(Builtin::Values),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        compile_entry_with_imports, load_lockfile, resolve_cached_path, save_lockfile, Compiler,
        Lockfile, Op, Vm, VmError, LOCKFILE_VERSION,
    };
    use at_parser::parse_module;
    use std::collections::HashMap;
    use std::fs;
    use std::path::PathBuf;

    fn temp_base_dir(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        path.push(format!("at_vm_lockfile_{name}_{nanos}"));
        path
    }

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
        assert!(matches!(chunk.code.first(), Some(super::Op::Const(_))));
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
    fn runtime_time_fixed_parses_timestamp() {
        let source = r#"
assert_eq(time.fixed("2026-01-01T00:00:00Z"), 1767225600);
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
    fn runtime_rng_deterministic_is_stable() {
        let source = r#"
assert_eq(rng.deterministic(7), 3664756328581923162);
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
    fn runtime_errors_on_missing_rng_capability() {
        let source = r#"
fn f() -> string {
    return rng.uuid();
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
                assert!(message.contains("missing capability: rng"));
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

    #[test]
    fn generator_yields_values() {
        let source = r#"
fn gen() {
    yield 1;
    yield 2;
}

let g = gen();
assert_eq(next(g), some(1));
assert_eq(next(g), some(2));
assert_eq(next(g), none());
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
    fn generator_evaluates_yield_expression() {
        let source = r#"
fn gen() {
    let value = 1 + 2;
    yield value;
}

let g = gen();
assert_eq(next(g), some(3));
assert_eq(next(g), none());
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
    fn nested_closure_can_capture_from_grandparent_scope() {
        let source = r#"
fn run() -> int {
    let base = 41;
    return (|_u| (|_v| (|_w| base + 1)(0))(0))(0);
}

assert_eq(run(), 42);
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
    fn lockfile_round_trip_writes_version_header() {
        let base = temp_base_dir("round_trip");
        let mut entries = HashMap::new();
        entries.insert("https://example.com/a.at".to_string(), "aaaa".to_string());
        entries.insert("https://example.com/b.at".to_string(), "bbbb".to_string());
        save_lockfile(
            &base,
            &Lockfile {
                version: LOCKFILE_VERSION,
                entries: entries.clone(),
            },
        )
        .expect("save lockfile");
        let raw = fs::read_to_string(base.join(".at").join("lock")).expect("read lockfile");
        assert_eq!(raw.lines().next(), Some("version 1"));
        let loaded = load_lockfile(&base).expect("load lockfile");
        assert_eq!(loaded.version, LOCKFILE_VERSION);
        assert_eq!(loaded.entries, entries);
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn lockfile_loader_accepts_legacy_format() {
        let base = temp_base_dir("legacy");
        fs::create_dir_all(base.join(".at")).expect("create base");
        fs::write(
            base.join(".at").join("lock"),
            "https://example.com/a.at aaaa\nhttps://example.com/b.at bbbb\n",
        )
        .expect("write lockfile");
        let loaded = load_lockfile(&base).expect("load lockfile");
        assert_eq!(loaded.version, LOCKFILE_VERSION);
        assert_eq!(
            loaded
                .entries
                .get("https://example.com/a.at")
                .map(String::as_str),
            Some("aaaa")
        );
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn lockfile_loader_rejects_newer_version() {
        let base = temp_base_dir("unsupported");
        fs::create_dir_all(base.join(".at")).expect("create base");
        fs::write(base.join(".at").join("lock"), "version 99\n").expect("write lockfile");
        let err = load_lockfile(&base).expect_err("expected error");
        assert!(err.contains("unsupported lockfile version"));
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn resolve_cached_path_rejects_hash_mismatch() {
        let base = temp_base_dir("integrity_mismatch");
        fs::create_dir_all(base.join(".at").join("cache")).expect("create cache");
        let url = "https://example.com/pkg.at";
        let expected_hash = "abcd";
        fs::write(
            base.join(".at")
                .join("cache")
                .join(format!("{expected_hash}.at")),
            "tampered",
        )
        .expect("write cache file");
        fs::write(
            base.join(".at").join("lock"),
            format!("version 1\n{url} {expected_hash}\n"),
        )
        .expect("write lockfile");

        let err = resolve_cached_path(&base, url).expect_err("expected integrity error");
        assert!(err.contains("integrity check failed"));
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn std_import_compiles_outside_repo_layout() {
        let base = temp_base_dir("std_import_compile");
        fs::create_dir_all(&base).expect("create base");
        let file = base.join("main.at");
        fs::write(&file, "import \"std\" as std;\nversion();\n").expect("write source");
        let program =
            compile_entry_with_imports(file.to_string_lossy().as_ref()).expect("compile program");
        assert!(!program.functions.is_empty());
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn runtime_stack_trace_includes_source_path_when_available() {
        let source = r#"
fn fail() {
    let x = none();
    return match x {
        some(v) => v,
    };
}

fail();
"#;
        let mut module = parse_module(source).expect("parse module");
        module.source_path = Some("/tmp/stack_trace_source.at".to_string());
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::new();
        let err = vm.run(&program).expect_err("expected runtime error");
        match err {
            VmError::Runtime { stack, .. } => {
                let stack = stack.expect("stack trace");
                assert_eq!(
                    stack.first().and_then(|frame| frame.source_path.as_deref()),
                    Some("/tmp/stack_trace_source.at")
                );
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn profiler_groups_opcode_counts_by_kind() {
        let source = r#"
fn add(a: int, b: int) -> int {
    return a + b;
}

add(1, 2);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_profiler();
        let result = vm.run(&program).expect("run program");
        assert!(result.is_none());

        let profiler = vm.take_profiler().expect("profiler");
        assert!(profiler.total_instructions > 0);
        assert!(profiler.op_counts.contains_key("Const"));
        assert!(profiler.op_counts.contains_key("Call"));
        assert!(profiler.function_counts.contains_key("<main>"));
        assert!(profiler.function_counts.contains_key("add"));
        assert!(profiler
            .op_counts
            .keys()
            .all(|name| !name.contains('(') && !name.contains('{')));
    }

    #[test]
    fn return_without_stack_value_errors() {
        let source = "fn f() { return 1; }\nf();\n";
        let module = parse_module(source).expect("parse module");
        let mut program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        program.main.code = vec![Op::Return];
        program.main.spans = vec![None];
        program.main.constants.clear();

        let mut vm = Vm::new();
        let err = vm.run(&program).expect_err("expected stack underflow");
        match err {
            VmError::Runtime { message, .. } => assert!(message.contains("stack underflow")),
            other => panic!("unexpected error: {other:?}"),
        }
    }

    // ── Async scheduler tests ───────────────────────────────────────

    #[test]
    fn async_await_returns_value() {
        let source = r#"
async fn compute() -> int {
    return 42;
}
let result = await compute();
print(result);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_output_capture();
        vm.run(&program).expect("run program");
        let output = vm.get_output().unwrap();
        assert_eq!(output, vec!["42"]);
    }

    #[test]
    fn nested_awaits_work() {
        let source = r#"
async fn inner() -> int {
    return 10;
}
async fn outer() -> int {
    let x = await inner();
    return x + 5;
}
let result = await outer();
print(result);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_output_capture();
        vm.run(&program).expect("run program");
        let output = vm.get_output().unwrap();
        assert_eq!(output, vec!["15"]);
    }

    #[test]
    fn await_ordering_is_deterministic() {
        let source = r#"
async fn a() -> int { return 1; }
async fn b() -> int { return 2; }
let fa = a();
let fb = b();
let ra = await fa;
let rb = await fb;
print(ra + rb);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        for _ in 0..10 {
            let mut vm = Vm::with_output_capture();
            vm.run(&program).expect("run program");
            let output = vm.get_output().unwrap();
            assert_eq!(output, vec!["3"]);
        }
    }

    #[test]
    fn max_instructions_respected_across_tasks() {
        let source = r#"
async fn spin() -> int {
    let i = 0;
    while i < 100000 {
        set i = i + 1;
    }
    return i;
}
let result = await spin();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_execution_limit(100);
        let result = vm.run(&program);
        assert!(result.is_err());
    }

    #[test]
    fn scheduler_completes_spawned_tasks() {
        let source = r#"
async fn add(a: int, b: int) -> int {
    return a + b;
}
let result = await add(3, 4);
print(result);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_output_capture();
        vm.run(&program).expect("run program");
        let output = vm.get_output().unwrap();
        assert_eq!(output, vec!["7"]);
    }

    #[test]
    fn async_quantum_preemption_resumes_correctly() {
        // Inner async function does enough work to exceed the scheduler
        // quantum (1024 instructions), forcing the parent task to suspend
        // via the Future-spawned-but-not-yet-complete path of Op::Await.
        // Regression test: before the fix, the parent would store a raw
        // TaskHandle into a local variable instead of the resolved value.
        let source = r#"
async fn heavy() -> int {
    let i = 0;
    while i < 2000 {
        set i = i + 1;
    }
    return i;
}
async fn wrapper() -> int {
    let x = await heavy();
    return x + 1;
}
let result = await wrapper();
print(result);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_output_capture();
        vm.run(&program).expect("run program");
        let output = vm.get_output().unwrap();
        assert_eq!(output, vec!["2001"]);
    }

    #[test]
    fn generator_detects_yield_in_nested_block() {
        let source = r#"
fn gen(x) {
    if x {
        yield 1;
    } else {
        yield 2;
    }
}

let g = gen(true);
assert_eq(next(g), some(1));
assert_eq(next(g), none());
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
    fn filter_and_reduce_builtins() {
        let source = r#"
fn f() {
    let nums = [1, 2, 3, 4, 5];
    let evens = filter(nums, |n| n % 2 == 0);
    assert_eq(len(evens), 2);
    assert_eq(evens[0], 2);
    assert_eq(evens[1], 4);

    let total = reduce(nums, 0, |acc, n| acc + n);
    assert_eq(total, 15);

    let doubled = map(nums, |n| n * 2);
    assert_eq(len(doubled), 5);
    assert_eq(doubled[0], 2);
    assert_eq(doubled[4], 10);
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
    fn spread_in_array_literal() {
        let source = r#"
fn f() {
    let mid = [3, 4];
    let arr = [1, 2, ...mid, 5];
    assert_eq(len(arr), 5);
    assert_eq(arr[0], 1);
    assert_eq(arr[1], 2);
    assert_eq(arr[2], 3);
    assert_eq(arr[3], 4);
    assert_eq(arr[4], 5);
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
    fn spread_in_map_literal() {
        let source = r#"
fn f() {
    let base = map { "a": 1, "b": 2 };
    let merged = map { ...base, "c": 3 };
    assert_eq(len(merged), 3);
    assert_eq(merged["a"], 1);
    assert_eq(merged["c"], 3);
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
    fn string_builtins_work() {
        let source = r#"
fn f() {
    assert_eq(len("hello"), 5);
    assert_eq(trim("  hi  "), "hi");
    assert_eq(to_upper("abc"), "ABC");
    assert_eq(to_lower("XYZ"), "xyz");

    let parts = split("a,b,c", ",");
    assert_eq(len(parts), 3);
    assert_eq(parts[0], "a");
    assert_eq(parts[2], "c");

    assert_eq(contains([1, 2, 3], 2), true);
    assert_eq(contains([1, 2, 3], 9), false);
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
    fn map_keys_and_values() {
        let source = r#"
fn f() {
    let m = map { "x": 10, "y": 20 };
    let k = keys(m);
    let v = values(m);
    assert_eq(len(k), 2);
    assert_eq(len(v), 2);
    assert_eq(contains(k, "x"), true);
    assert_eq(contains(v, 20), true);
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
    fn match_patterns_with_bindings() {
        let source = r#"
fn unwrap_or(opt, default) {
    return match opt {
        some(v) => v,
        none => default,
    };
}

assert_eq(unwrap_or(some(42), 0), 42);
assert_eq(unwrap_or(none(), 0), 0);

fn check_result(r) {
    return match r {
        ok(v) => v + 1,
        err(e) => 0,
    };
}

assert_eq(check_result(ok(9)), 10);
assert_eq(check_result(err("fail")), 0);
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
    fn defer_executes_on_scope_exit() {
        let source = r#"
fn f() {
    defer || print("cleanup");
    print("body");
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_output_capture();
        vm.run(&program).expect("run program");
        let output = vm.get_output().unwrap();
        assert_eq!(output, vec!["body", "cleanup"]);
    }

    #[test]
    fn defer_runs_in_lifo_order() {
        let source = r#"
fn f() {
    defer || print("first");
    defer || print("second");
    defer || print("third");
    print("body");
}
f();
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_output_capture();
        vm.run(&program).expect("run program");
        let output = vm.get_output().unwrap();
        assert_eq!(output, vec!["body", "third", "second", "first"]);
    }

    #[test]
    fn deeply_nested_async_three_levels() {
        let source = r#"
async fn level3() -> int {
    return 10;
}
async fn level2() -> int {
    let x = await level3();
    return x + 20;
}
async fn level1() -> int {
    let x = await level2();
    return x + 30;
}
let result = await level1();
print(result);
"#;
        let module = parse_module(source).expect("parse module");
        let program = Compiler::new()
            .compile_module(&module)
            .expect("compile module");
        let mut vm = Vm::with_output_capture();
        vm.run(&program).expect("run program");
        let output = vm.get_output().unwrap();
        assert_eq!(output, vec!["60"]);
    }

    #[test]
    fn block_body_closure_runs() {
        let source = r#"
let nums = [1, 2, 3, 4, 5];
let result = map(nums, |n| {
    let doubled = n * 2;
    doubled + 1
});
assert_eq(result[0], 3);
assert_eq(result[4], 11);
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
