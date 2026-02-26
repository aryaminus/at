# at

`at` (from `@`) is a programming language designed for AI agents — explicit, readable, and greppable.

## Design Philosophy

- **Agent-first**: Code should be readable and writable without IDE support
- **Explicit effects**: `needs`/`using` make dependencies visible and mockable
- **Greppable**: Clear imports, no barrel files, no implicit magic
- **Predictable**: Results over exceptions, minimal magic
- **Fast**: Cold start in milliseconds, small binary footprint

## Current Status

**All core features implemented:**

- Types: `int`, `float`, `bool`, `string`, `unit`, `array<T>`, `option<T>`, `result<T,E>`
- Control flow: `if` (expression, optional else), `while`, `for`, `break`, `continue`
- Pattern matching: `match` with `ok`, `err`, `some`, `none`, `_` patterns; multi-field enum variants
- Error handling: `?` (try) operator, `option`, `result`
- Staged deprecation warnings for legacy `throw` and `try { ... } catch { ... }` usage
- Effects: `needs`/`using` for capability management
- Imports: Local paths and URLs with caching
- Comments: `//` line and `/* */` block comments
- String escapes: `\n`, `\t`, `\r`, `\0`, `\\`, `\"`
- Arrays: Immutable arrays with `len`, `append`, `contains`, `slice`
- Mixed arithmetic: `int` and `float` can be mixed freely (int promoted to float)
- Operators: `+`, `-`, `*`, `/`, `%`, `&&`, `||`, `==`, `!=`, `<`, `<=`, `>`, `>=`
- Test blocks: `test "name" { ... }` with `assert` and `assert_eq`; multi-file test discovery (`at test <dir>`)
- Tool functions: `tool fn` for MCP integration
- Regular expressions: `regex_match`, `regex_find`, `regex_replace` with capture group support
- 40+ builtins: math (`abs`, `min`, `max`, `floor`, `ceil`, `round`, `pow`, `sqrt`, `sum`), string (`join`, `replace`, `starts_with`, `ends_with`, `repeat`, `parse_float`), character (`char_code`, `from_char_code`, `is_digit`, `is_alpha`, `is_upper`, `is_lower`), array (`sort`, `reverse`, `index_of`, `count`, `range`), and more

**Staged deprecation status (compatibility mode):**

- `legacy_exception_surface` and `unqualified_import_call` are warning-level in `0.1.x`.
- Planned enforcement timeline:
  - `0.2.x`: optional deny mode for migration testing.
  - `0.3.0` or later: may become hard errors by default.

## Tooling

Comprehensive tooling for development, testing, and deployment:

- **Formatter**: Consistent, stable formatting (`at fmt`)
- **LSP**: Diagnostics, hover, goto-definition, completion, inlay hints (`at lsp`)
- **Type checker**: Full type inference and checking (`at check`)
- **Linter**: Detect duplicate names, unused variables (`at lint`)
- **MCP server**: Expose tools to AI agents (`at mcp-server`)
- **VM**: Stack-based interpreter (`at run`)
- **REPL**: Interactive development (`at repl`)
- **Test runner**: Run test blocks (`at test`)
- **Benchmark**: Performance measurement (`at bench`)
- **Deps**: Show module dependencies (`at deps`)
- **Cache**: Manage cached remote imports (`at cache`)
- **WASM**: Browser-compatible runtime

CI enforces:

- `cargo fmt --check`
- `cargo clippy` correctness gate
- non-correctness clippy warning budget (`.clippy-warning-baseline.toml`)
- workspace tests + example run/check sweep
- release binary size budget (`<= 8,000,000` bytes)
- benchmark stability threshold via `examples/run_bench.sh` + `examples/bench_compare.py`

## Packaging

### npm (WASM)

```bash
npm install -g @aryaminus/at
```

Use `at` binaries for CLI usage; the npm package provides WASM bindings.

## Quick Start

```bash
# Run a program
at run examples/sum.at

# Type-check (fails on errors; warns are non-fatal)
at check file.at

# Run tests (file or directory)
at test file.at
at test tests/

# Start REPL
at repl

# Format code
at fmt file.at

# Run benchmarks
at bench file.at --runs 10

# Show dependencies
at deps file.at

# Lint code
at lint file.at

# Manage cache
at cache list
at cache prune --max-files 100

# Start MCP server
at mcp-server tools.at

# Start LSP server
at lsp

# Get help
at --help
at run --help
```

## Installation

Canonical install docs live in `docs/install.md`.

### Cargo (git)

```bash
cargo install --git https://github.com/aryaminus/at --bin at
```

### Pre-built Binaries

Download from GitHub releases for your platform:

```bash
# macOS (Intel)
curl -L https://github.com/aryaminus/at/releases/latest/download/at-x86_64-apple-darwin.tar.gz | tar xz
sudo mv at-x86_64-apple-darwin /usr/local/bin/at

# macOS (Apple Silicon)
curl -L https://github.com/aryaminus/at/releases/latest/download/at-aarch64-apple-darwin.tar.gz | tar xz
sudo mv at-aarch64-apple-darwin /usr/local/bin/at

# Linux
curl -L https://github.com/aryaminus/at/releases/latest/download/at-x86_64-unknown-linux-gnu.tar.gz | tar xz
sudo mv at-x86_64-unknown-linux-gnu /usr/local/bin/at
```

### Publishing

Releases are automated on merges to `main`. A version bump commit and `vX.Y.Z` tag are created by CI.

### Homebrew

```bash
brew tap aryaminus/at
brew install at
```

### Verify Installation

```bash
at --version
at --help
at run examples/sum.at
at check examples/features.at
```

### Install Smoke Matrix

| Target | Install path | Verification |
|---|---|---|
| Linux/macOS | Release tarball | `at --version`, `at run examples/sum.at`, `at check examples/features.at` |
| Windows | Release zip | `at --version`, `at run examples/sum.at`, `at check examples/features.at` |
| Source | `cargo install --path crates/at_cli` | `at --version`, `at run examples/sum.at` |
| Source (git) | `cargo install --git https://github.com/aryaminus/at --bin at` | `at --version`, `at run examples/sum.at` |
| WASM | `npm install -g @aryaminus/at` | run wrapper smoke in `docs/wasm.md` |

## Example

```at
fn greet(name: string) -> string {
    return "Hello, " + name + "!";
}

fn safe_div(a: int, b: int) -> result<int, string> {
    return if b == 0 {
        err("division by zero")
    } else {
        ok(a / b)
    };
}

fn process(items: array<int>) -> int {
    let sum = 0;
    for item in items {
        set sum = sum + item;
    }
    return sum;
}

test "greeting works" {
    let msg = greet("World");
    assert_eq(msg, "Hello, World!");
}

test "division handles errors" {
    let result = safe_div(10, 0);
    assert(is_err(result));
}

fn main() {
    print(greet("at"));
    
    let numbers = [1, 2, 3, 4, 5];
    let total = process(numbers);
    print(total);
    
    return 0;
}
```

## Documentation

- `docs/lang.md` — Complete language reference
- `docs/cli.md` — CLI commands and options
- `docs/install.md` — Installation and platform notes
- `examples/features.at` — Comprehensive feature demonstration

## Design Goals

From ["A Language For Agents"](https://lucumr.pocoo.org/2026/2/9/a-language-for-agents/):

1. **Context Without LSP**: Readable without IDE support
2. **Braces, Not Whitespace**: No indentation sensitivity
3. **Explicit Flow Context**: `needs`/`using` for effects
4. **Results Over Exceptions**: Typed error handling
5. **Minimal Diffs**: Trailing commas encouraged
6. **Greppable**: No barrel files, explicit imports
7. **Local Reasoning**: No macros, no implicit magic

## License

MIT
