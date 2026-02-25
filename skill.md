---
name: at-language-expert
description: Essential procedural knowledge and constraints for writing, debugging, and understanding the `at` programming language.
---

# `at` Language Agent Skill

This skill provides the mandatory procedural knowledge required to successfully write and debug `at` code. `at` is a fast, strictly-typed language built specifically for AI agents.

## 🚨 Critical Constraints (Read First)

1. **Explicit Return Types Required:** Every function MUST declare its return type `fn name() -> type {`.
2. **Mandatory Import Aliasing:** You MUST alias all imports (`import "./utils.at" as utils;`). There are no global imports.
3. **Capability Sandboxing:** If a script touches the network, filesystem, or time, you MUST declare a `needs { ... }` block at the very top of the file.
4. **Token Efficiency:** The compiler infers local variables. Use `let x = 5;` instead of `let x: int = 5;` to save tokens.
5. **No Macros or Exceptions:** Do not use `try/catch`. All errors must explicitly return `Result<T, E>`.

## 1. Syntax Overview

### Variables & Types

```at
let immutable = 10;
let mut mutable = 20;
mutable = 30;

// Type inference handles these automatically
let array = [1, 2, 3];
let map = { "key": "value" };
```

### Functions & Tests

Tests are colocated and execute at highly-optimized speeds with `at test`.

```at
fn add(a: int, b: int) -> int {
    return a + b;
}

test adds_numbers {
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
```

### Error Handling

No exceptions. Use `Result<T, E>` and `?` for early returns.

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

## 3. Explicit Effects & Sandboxing

Agents execute in a secure sandbox. You cannot perform side-effects without declaring them statically at the top of the file.

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

- [ ] **Formatting**: Multi-element arrays, tuples, and maps MUST be formatted vertically to preserve diff-stable trailing commas.
- [ ] **Aliasing**: Did you alias all imports? (`import "x" as y;`)
- [ ] **No block closures**: If you need a closure, define an explicit function.
- [ ] **Effects**: Did you declare `needs { ... }` at the top of the file if accessing external systems?
- [ ] **Tests**: Are you writing colocated `test test_name { assert(...); }` blocks for your functions?
- [ ] **Types**: Did you let the compiler infer types for local variables using `let x = ...`?

## 5. Development CLI

As an agent, you can use these tools to iteratively validate your code:

- `at run <file>` - Execute a script
- `at test` - Run tests (aggressively cached, very fast feedback loop)
- `at check` - Instantaneous type checking
- `at fix` - Auto-formats code and fixes lints
