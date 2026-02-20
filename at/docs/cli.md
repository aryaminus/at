# at CLI Reference

Complete reference for the `at` command-line interface.

## Global Options

```
-h, --help      Show help message
-V, --version   Show version information
```

## Commands

### `at <file.at>` or `at run <file.at>`

Execute a file.

```
at run file.at
at run file.at --capability time --capability rng
```

**Options:**
- `-c, --capability <name>` — Grant a capability at runtime

### `at repl`

Start an interactive REPL (Read-Eval-Print Loop).

```
at repl
```

### `at check <file.at>`

Type-check a file. Reports type errors without executing.

```
at check file.at
```

### `at lint <file.at>`

Run the linter on a file. Reports style issues and potential bugs.

```
at lint file.at
```

### `at test <file.at>`

Run test blocks defined in a file.

```
at test file.at
```

**Exit codes:**
- `0` — All tests passed
- `1` — One or more tests failed

### `at fmt <file.at>`

Format a file and print the result to stdout. Does not modify the original file.

```
at fmt file.at > file_formatted.at
```

### `at bench <file.at>`

Run benchmarks for a file. Reports execution time statistics.

```
at bench file.at
```

### `at deps <file.at>`

Show dependencies (imports) used by a file, including transitive dependencies.

```
at deps file.at
```

### `at lsp`

Start the Language Server Protocol server. Used by editors for IDE features.

```
at lsp
```

**Capabilities:**
- Diagnostics (errors and warnings)
- Hover information
- Go to definition
- Auto-completion
- Inlay hints

### `at mcp-server <file.at>`

Start an MCP (Model Context Protocol) server exposing tool functions.

```
at mcp-server file.at
```

**Protocol:** Uses newline-delimited JSON (NDJSON).

**Example request:**
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"sum","arguments":{"values":[1,2,3]}}}
```

### `at cache <subcommand>`

Cache management commands.

#### `at cache add <url>`

Add a remote import to the cache.

```
at cache add https://example.com/lib.at
```

#### `at cache list`

List all cached modules.

```
at cache list
```

#### `at cache show <url>`

Show details about a cached module.

```
at cache show https://example.com/lib.at
```

#### `at cache remove <url>`

Remove a module from the cache.

```
at cache remove https://example.com/lib.at
```

#### `at cache clear`

Clear all cached modules.

```
at cache clear
```

#### `at cache prune [options]`

Prune cache to manage size.

**Options:**
- `--max <n>` or `--max-files <n>` — Maximum number of files to keep (default: 1000)
- `--max-bytes <n>` or `--max-mb <n>` — Maximum total size in MB (default: 100)

```
at cache prune --max-files 500 --max-mb 50
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error / test failure |
| 2 | Parse error |
| 3 | Compile error |
| 4 | Runtime error |

## Environment Variables

- `AT_CACHE_DIR` — Override the default cache directory (default: `./.at/cache`)
- `AT_LOCK_FILE` — Path to lockfile (default: `./.at/lock`)

## Examples

### Run a simple program
```
cat > hello.at << 'EOF'
fn main() {
    print("Hello, World!");
    return 0;
}
EOF
at run hello.at
```

### Run with capabilities
```
cat > time_demo.at << 'EOF'
fn get_time() -> int needs { time } {
    return time.now();
}
EOF
at run time_demo.at --capability time
```

### Type-check before running
```
at check file.at && at run file.at
```

### Run tests
```
cat > test_example.at << 'EOF'
fn add(a: int, b: int) -> int {
    return a + b;
}

test "addition works" {
    assert_eq(add(2, 3), 5);
}
EOF
at test test_example.at
```

### Format code
```
at fmt messy.at > formatted.at
```

### List dependencies
```
cat > deps_example.at << 'EOF'
import "./lib.at" as lib;
import "https://example.com/remote.at" as remote;
EOF
at deps deps_example.at
```

### MCP Server
```
cat > tools.at << 'EOF'
tool fn greet(name: string) -> string {
    return "Hello, " + name + "!";
}
EOF
at mcp-server tools.at
```

Then send requests:
```
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"greet","arguments":{"name":"World"}}}' | at mcp-server tools.at
```

## URL Imports

When you import a URL, the CLI handles it as follows:

1. Check if the module exists in the cache
2. If cached and fresh, use cached version
3. If not cached or stale, fetch from URL
4. Store in cache for future use

**Example:**
```
import "https://example.com/lib.at" as lib;
```

The first time this is encountered, the file is downloaded to `.at/cache/` and a reference is added to `.at/lock`.

## Lockfile Format

The lockfile (`.at/lock`) tracks remote imports:

```
# at lockfile format version 1
https://example.com/lib.at abc123...
https://another.com/utils.at def456...
```

Each line contains a URL and its SHA256 hash.

## Cache Structure

```
.at/
├── cache/
│   ├── abc123...  # cached module content
│   ├── def456...
│   └── ...
└── lock           # lockfile with URLs and hashes
```

## Common Error Messages

### Parse Errors
```
parse error in file.at at 5:12: expected Semicolon, found Int(42)
```

### Type Errors
```
type error in file.at at 10:5: type mismatch: expected int, got string
```

### Runtime Errors
```
runtime error: missing capability: time
```

### Missing Return
```
type error: function 'foo' declares return type 'int' but has no return statement
```

## See Also

- `at --help` — Show help for any command
- `docs/lang.md` — Language reference
- `examples/` — Example programs
