# at Language Reference

A programming language designed for AI agents — explicit, readable, and greppable.

## Quick Start

```
# Run a file
at run file.at

# Type-check
at check file.at

# Run tests
at test file.at

# Start REPL
at repl
```

## Types

- `int` — 64-bit signed integer
- `float` — 64-bit floating point
- `bool` — Boolean (`true`, `false`)
- `string` — UTF-8 text
- `unit` — The empty type
- `array<T>` — Array of type T
- `option<T>` — Optional value (`some(T)` or `none`)
- `result<T, E>` — Success or error (`ok(T)` or `err(E)`)

### Notes

- Generic types use angle brackets: `result<int, string>`
- Arrays are immutable; operations return new arrays
- `option` and `result` must use angle brackets with type arguments

## Values and Literals

### Integers
```
0, 42, -7
```
Note: Negative literals are parsed as unary negation: `-7` is `-(7)`.

### Floats
```
3.14, -0.5, 2.0
```
Note: Must have digits after the decimal point. `42.` is parsed as `42` followed by `.`.

### Booleans
```
true, false
```

### Strings
```
"hello world"
```

String interpolation:
```
"hello {name}"
"total = {sum}"
```

String escape sequences:
- `\n` — newline
- `\t` — tab
- `\r` — carriage return
- `\0` — null byte
- `\\` — literal backslash
- `\"` — literal quote

### Comments
```
// Line comment

/* Block comment
   Can span multiple lines */

/* Nested /* block */ comments work */
```

## Variables

```
let x = 1;
let y: int = 2;
set x = x + 1;

using time = time.fixed("2026-01-01T00:00:00Z");
```

### Notes

- `let` defines a new local variable (type optional)
- `set` assigns to an existing local
- `using` declares a capability binding for `needs`
- Variables are block-scoped
- Shadowing is allowed in inner scopes (different from redeclaration in same scope)

## Control Flow

### If Expression

```
if condition { then_expr } else { else_expr }
```

`if` is an expression. `else` is optional when used as a statement.
```
let result = if x < 0 { "neg" } else { if x == 0 { "zero" } else { "pos" } };
```

### While Loop

```
while i < 10 {
    set i = i + 1;
    if i == 5 {
        break;
    }
}
```

### For Loop

```
for item in array {
    print(item);
}
```

### Break and Continue

```
while condition {
    if should_exit {
        break;
    }
    if should_skip {
        continue;
    }
}
```

**Note**: `break` and `continue` must be followed by a semicolon when used as statements.

## Pattern Matching

```
match value {
    ok(v) => handle_success(v),
    err(e) => handle_error(e),
}

match option_value {
    some(v) => v,
    _ => default_value,
}
```

### Patterns

- `ok(ident)` — Match success result, bind value to ident
- `err(ident)` — Match error result, bind value to ident
- `some(ident)` — Match some option, bind value to ident
- `none` — Match none option
- `_` — Wildcard (matches anything, no binding)
- `|` — Or-patterns (e.g. `ok(x) | err(x)`)
- Guards with `if` (e.g. `some(x) if x > 0`)

## Functions

```
fn add(a: int, b: int) -> int {
    return a + b;
}

fn greet(name: string) -> string {
    return "Hello, " + name;
}
```

### Generic Functions

```
fn identity<T>(value: T) -> T {
    return value;
}
```

### Functions with Effects

```
fn get_time() -> int needs { time } {
    return time.now();
}
```

The `needs` clause declares required capabilities.

### Tool Functions

```
tool fn calculate(values: array<int>) -> int {
    let sum = 0;
    for v in values {
        set sum = sum + v;
    }
    return sum;
}
```

Tool functions are exposed via MCP (Model Context Protocol).

## Error Handling

### Result Type

```
fn divide(a: int, b: int) -> result<int, string> {
    return if b == 0 {
        err("division by zero")
    } else {
        ok(a / b)
    };
}

fn use_div() -> result<int, string> {
    let value = divide(10, 2)?;
    return ok(value);
}
```

### Option Type

```
fn safe_get(arr: array<int>, index: int) -> option<int> {
    return if index < len(arr) {
        some(arr[index])
    } else {
        none
    };
}
```

### Checking Types

```
is_some(value)    // Returns bool
is_none(value)    // Returns bool
is_ok(value)      // Returns bool
is_err(value)     // Returns bool
```

## Arrays

```
let values = [1, 2, 3];
let next = append(values, 4);
let mid = slice(next, 1, 3);
let has_two = contains(values, 2);
let first = values[0];
let count = len(values);

for value in values {
    print(value);
}
```

### Array Operations

- `len(array)` — Returns length as int
- `append(array, value)` — Returns new array with value appended
- `contains(array, value)` — Returns bool
- `slice(array, start, end)` — Returns sub-array from start (inclusive) to end (exclusive)
- `array[index]` — Access element at index

## Maps

```
let scores = map { "taylor": 3, "casey": 5 };
let taylor = scores["taylor"];
set scores["taylor"] = 4;
```

### Map Operations

- `map { key: value, ... }` — Map literal
- `map[key]` — Lookup value by key (runtime error if missing)
- `set map[key] = value` — Insert or update a key

## Structs and Enums

```
struct Point<T> {
    x: T,
    y: T,
}

enum Status<T> {
    Ok(T),
    Err(string),
}

let origin = Point { x: 0, y: 0 };
let good = Status::Ok(42);
```

## Imports

```
import "./math.at" as math;
import "https://example.com/lib.at" as remote;
```

Imports load modules and bind them to an alias. All imported names must be accessed through the alias (`math.add`, not just `add`).

**Note**: Remote imports require a lockfile entry or will be fetched and cached.

## Operators and Precedence

Highest to lowest precedence:

```
postfix:  call ( ... ), member (.), index ([ ]), try (?), is, as
unary:    - !
factor:   * / %
term:     + -           (string + string for concatenation)
    compare:  < <= > >=
    equality: == !=
    bit_and:  &
    bit_xor:  ^
    bit_or:   |
    and:      &&
    or:       ||
```

### Operator Details

- `&&` — Logical AND (short-circuit)
- `||` — Logical OR (short-circuit)
- `&`, `|`, `^`, `<<`, `>>` — Bitwise ops (int only)
- `%` — Modulo (remainder)
- `+` — Addition for int/float, concatenation for strings
- `==`, `!=` — Equality works on all types
- `<`, `<=`, `>`, `>=` — Comparison for int/float
- `-` — Negation (unary) or subtraction (binary)
- `!` — Logical NOT

## Built-in Functions

### Assertions
```
assert(condition)
assert_eq(left, right)
```

### I/O
```
print(value)  // Prints value to stdout
```

### Array Operations
```
len(value)                    // Length of array or string
append(array, value)          // Add element to array
contains(array, value)        // Check if array contains value
slice(array, start, end)      // Extract sub-array
```

### Option/Result Constructors
```
some(value)
none()
ok(value)
err(value)
```

### Type Predicates
```
is_some(value)
is_none(value)
is_ok(value)
is_err(value)
```

### Casts and Type Checks

```
let count = value as int;
if value is string {
    print(value);
}
```

### Capabilities
```
time.now()                    // Returns int (requires time capability)
time.fixed(value)             // Returns string (stub)
rng.deterministic(seed)       // Returns seed unchanged (stub)
rng.uuid()                    // Returns string (stub)
```

**Note**: `time.now()` requires the `time` capability via `needs { time }`. Other time/rng functions are currently stubs.

## Test Blocks

```
test "addition works" {
    let result = add(2, 3);
    assert_eq(result, 5);
}

test "with mocked time" {
    using time = time.fixed("2026-01-01T00:00:00Z");
    let now = time.now();
    assert_eq(now, 0);
}
```

Run tests with `at test file.at`.

## Grammar (Simplified)

```
program      ::= { function | stmt }
function     ::= "fn" ident [ "<" ident { "," ident } ">" ] "(" params ")" [ "->" type ] [ "needs" "{" ident { "," ident } "}" ] block
stmt         ::= import | let | using | set | while | for | break | continue | return | test | expr ";"
import       ::= "import" string "as" ident ";"
let          ::= "let" ident [":" type] "=" expr ";"
using        ::= "using" ident [":" type] "=" expr ";"
set          ::= "set" ident "=" expr ";"
while        ::= "while" expr block
for          ::= "for" ident "in" expr block
break        ::= "break" ";"
continue     ::= "continue" ";"
return       ::= "return" [expr] ";"
test         ::= "test" string block
block        ::= "{" { stmt } [expr] "}"

expr         ::= or
or           ::= and { "||" and }
and          ::= bit_or { "&&" bit_or }
bit_or       ::= bit_xor { "|" bit_xor }
bit_xor      ::= bit_and { "^" bit_and }
bit_and      ::= equality { "&" equality }
equality     ::= comparison { ("==" | "!=") comparison }
comparison   ::= term { ("<" | "<=" | ">" | ">=") term }
term         ::= factor { ("+" | "-") factor }
factor       ::= unary { ("*" | "/" | "%") unary }
unary        ::= ("-" | "!") unary | postfix
postfix      ::= primary { ("." ident) | call | index | "?" | "is" type | "as" type }
call         ::= "(" [expr { "," expr }] ")"
index        ::= "[" expr "]"
primary      ::= int | float | string | "true" | "false" | ident | array | map_lit | block | tuple | range | if | match | closure | struct_lit | enum_lit | "(" expr ")"
array        ::= "[" [expr { "," expr }] "]"
map_lit      ::= "map" "{" [expr ":" expr { "," expr ":" expr }] "}"
tuple        ::= "(" expr "," expr { "," expr } ")"
range        ::= expr ".." expr
closure      ::= "|" [ident { "," ident }] "|" expr
struct_lit   ::= ident "{" field_init { "," field_init } "}"
enum_lit     ::= ident "::" ident [ "(" expr ")" ]
if           ::= "if" expr block "else" (block | if)
match        ::= "match" expr "{" { pattern "=>" expr "," } "}"
pattern      ::= "ok" "(" ident ")" | "err" "(" ident ")" | "some" "(" ident ")" | "none" | "_" | pattern "|" pattern | pattern "if" expr

type         ::= ident ["<" type { "," type } ">"]
```

## Design Rationale

This language prioritizes **agent-friendly** features:

1. **Readability without LSP**: Code is understandable without IDE support
2. **Explicit blocks**: Curly braces `{}` not whitespace-sensitive indentation
3. **Explicit effects**: `needs`/`using` make dependencies visible
4. **Results over exceptions**: `result<T, E>` and `option<T>` for error handling
5. **Greppable imports**: No barrel files, no re-exports, mandatory aliases
6. **Minimal diffs**: Trailing commas encouraged for diff stability
7. **No macros**: Reduces hidden behavior, improves local reasoning

## Known Limitations

- `time.fixed()` and `rng.*` functions are stubs (return fixed values)
- Imports are not supported in WASM
- Strings are immutable; concatenation creates new strings

## See Also

- `at --help` — CLI reference
- `docs/cli.md` — Detailed CLI documentation
- `examples/` — Example programs
