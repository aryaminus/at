---
name: at-language-expert
description: Essential procedural knowledge and constraints for writing, debugging, and understanding the `at` programming language.
---

# `at` Language Agent Skill

This skill provides the mandatory procedural knowledge required to successfully write and debug `at` code. `at` is a fast, strictly-typed language built specifically for AI agents.

## Design Philosophy

`at` is designed around what makes agents succeed, informed by [A Language For Agents](https://lucumr.pocoo.org/2026/2/9/a-language-for-agents/):

- **Context without LSP:** Types are explicit in function signatures and return types. You never need an LSP to understand what a function accepts or returns. Local variables use inference to save tokens, but the public API is always readable.
- **Braces over whitespace:** `at` uses `{ }` delimiters, not significant whitespace. This avoids the token-efficiency and surgical-edit problems that LLMs have with indentation-sensitive languages.
- **Results over exceptions:** Prefer `Result<T, E>` and `?` propagation. `try/catch/finally` exists as an escape hatch but `Result` is the idiomatic error-handling pattern. Agents should default to returning `Result` types.
- **Explicit effects:** Side effects (network, filesystem, time, randomness) must be declared via `needs { ... }` blocks. This makes mocking trivial in tests and gives agents clear signals about what a function touches.
- **Minimal diffs:** Trailing commas are supported everywhere (arrays, maps, match arms, function params). Format code vertically to keep diffs single-line.
- **Greppable imports:** All imports require aliasing (`import "x" as y;`). Every symbol use is prefixed with its module name (like Go's `context.Context`), making code searchable with basic tools like `grep` or `sed`.
- **Local reasoning:** Each file is self-contained. No implicit globals, no re-exports, no barrel files. If you read a file, you can understand it without loading other files.
- **No macros:** `at` has no macro system. Code generation is unnecessary when the cost of writing code is low.
- **No re-exports or barrel files:** Every import points to the file that defines the symbol. One-to-one mapping from declaration to usage.
- **Dependency-aware builds:** No circular imports. Packages have clear boundaries. Test results are aggressively cached.
- **Deterministic tests:** Tests can pin time and randomness (`using time.fixed; using rand.seeded;`), eliminating flakiness by design.
- **Single failure condition:** `at check` type-checks and lints in one pass. Code either passes or fails — there is no "compiles but has type errors" state. `at test` runs all tests. Two commands, zero ambiguity.

## Critical Constraints (Read First)

1. **Explicit Return Types Required:** Every function MUST declare its return type `fn name() -> type {`.
2. **Mandatory Import Aliasing:** You MUST alias all imports (`import "./utils.at" as utils;`). There are no global imports, no re-exports.
3. **Capability Sandboxing:** If a script touches the network, filesystem, or time, you MUST declare a `needs { ... }` block.
4. **Token Efficiency:** The compiler infers local variables. Use `let x = 5;` instead of `let x: int = 5;` to save tokens.
5. **Mutation uses `set`:** Variables are immutable by default. Use `set x = newValue;` to mutate (not `let mut`).
6. **Semicolons required:** All statements end with `;`.
7. **Match arms use commas:** Match arms are separated by `,` (not newlines).
8. **No macros:** There is no macro system. Write explicit code.
9. **Prefer `Result` over `try/catch`:** Use `Result<T, E>` and `?` for error handling. Avoid `try/catch` unless wrapping FFI or legacy code.

## 1. Syntax Overview

### Variables & Types

```at
let immutable = 10;
let mutable = 20;
set mutable = 30;       // `set` for mutation, not `let mut`

// Type inference handles these automatically
let array = [1, 2, 3];
let empty = [];          // empty array literal
let m = map {};          // empty map literal
let m2 = map { "key": "value" };
```

### Functions & Tests

Tests are colocated and execute at highly-optimized speeds with `at test`.

```at
fn add(a: int, b: int) -> int {
    return a + b;
}

test "adds numbers" {
    assert(add(1, 2) == 3);
}
```

### Control Flow

```at
if a > b {
    // ...
} else {
    // ...
}

for item in array {
    // ...
}

while condition {
    // ...
}
```

### Error Handling

Use `Result<T, E>` and `?` for early returns. This is the idiomatic pattern — agents should default to this over `try/catch`.

```at
fn divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        return Err("Division by zero");
    }
    return Ok(a / b);
}

fn calculate() -> Result<int, string> {
    let result = divide(10, 2)?;
    return Ok(result + 1);
}
```

### Enums & Pattern Matching

```at
enum Shape {
    Circle(float),
    Rect(float, float),   // multi-field variants supported
    Point,                 // no-payload variant
}

fn area(s: Shape) -> float {
    return match s {
        Shape::Circle(r) => 3.14159 * r * r,
        Shape::Rect(w, h) => w * h,
        Shape::Point => 0.0,
    };
}
```

### Higher-Order Functions

`map`, `filter`, and `reduce` are compiler-inlined (not regular builtins):

```at
let nums = [1, 2, 3, 4, 5];
let doubled = map(nums, |x| x * 2);
let evens = filter(nums, |x| x % 2 == 0);
let total = reduce(nums, 0, |acc, x| acc + x);
```

## 2. Builtins Reference

### Core
`print(value)`, `assert(condition)`, `assert_eq(a, b)`, `len(collection)`, `type_of(value)`

### Math
`abs(n)`, `min(a, b)`, `max(a, b)`, `floor(f)`, `ceil(f)`, `round(f)`, `pow(base, exp)`, `sqrt(f)`, `sum(array)`

### String
`contains(haystack, needle)` (works on arrays and strings), `slice(arr, start, end)`, `split(str, delim)`, `trim(str)`, `to_upper(str)`, `to_lower(str)`, `substring(str, start, end)`, `join(array, sep)`, `replace(str, old, new)`, `starts_with(str, prefix)`, `ends_with(str, suffix)`, `repeat(str, n)`, `parse_int(str)`, `parse_float(str)`, `to_string(value)`

### Character
`char_code(str)`, `from_char_code(n)`, `is_digit(str)`, `is_alpha(str)`, `is_upper(str)`, `is_lower(str)`

### Array
`append(arr, value)`, `sort(arr)`, `reverse(arr)`, `index_of(arr, value)`, `count(arr, value)`, `range(start, end)`

### Map
`keys(map)`, `values(map)`

### Option & Result
`some(v)`, `none()`, `is_some(opt)`, `is_none(opt)`, `ok(v)`, `err(v)`, `is_ok(res)`, `is_err(res)`

## 3. Explicit Effects & Sandboxing

Agents execute in a secure sandbox. You cannot perform side-effects without declaring them statically.

```at
needs { network, fs } // MUST be declared if you fetch or read files

import "std/http.at" as http;

fn fetch_data() -> Result<string, string> {
    return http.get("https://example.com");
}
```

For test determinism, declare deterministic environments:

```at
using time.fixed;
using rand.seeded;
```

## 4. Code Generation Checklist for LLMs

Before modifying or generating `at` code, verify:

- [ ] **Mutation**: Use `set x = value;` to mutate, not `let mut`.
- [ ] **Error handling**: Use `Result<T, E>` and `?`, not `try/catch`.
- [ ] **Formatting**: Multi-element arrays, tuples, and maps MUST be formatted vertically to preserve diff-stable trailing commas.
- [ ] **Aliasing**: Did you alias all imports? (`import "x" as y;`)
- [ ] **Effects**: Did you declare `needs { ... }` at the top of the file if accessing external systems?
- [ ] **Tests**: Are you writing colocated `test "name" { assert(...); }` blocks for your functions?
- [ ] **Types**: Did you let the compiler infer types for local variables using `let x = ...`?
- [ ] **Semicolons**: All statements end with `;`.
- [ ] **Match commas**: Match arms separated by `,`.
- [ ] **Mixed arithmetic**: `int` and `float` can be mixed freely (int is promoted to float).
- [ ] **No macros**: Write explicit code. There is no macro system.
- [ ] **Local reasoning**: Each file should be self-contained. No re-exports, no barrel files.

## 5. Development CLI

As an agent, you can use these tools to iteratively validate your code:

- `at check` - Type-check and lint in one pass (instant, catches all errors before runtime)
- `at test` - Run tests (aggressively cached, very fast feedback loop)
- `at run <file>` - Execute a script
- `at fix` - Auto-formats code and fixes lints
