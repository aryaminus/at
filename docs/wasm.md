# WASM Usage Guide

Run the `at` language in the browser or any WASM environment.

## Building

### Prerequisites

Install the WASM target:
```bash
rustup target add wasm32-unknown-unknown
```

### Build the WASM module

```bash
cargo build -p at_wasm --target wasm32-unknown-unknown --release
```

### Build with wasm-pack

```bash
cargo install wasm-pack
wasm-pack build crates/at_wasm --target web
```

The output will be at:
```
target/wasm32-unknown-unknown/release/at_wasm.wasm
```

## API

### JS wrappers

Use the included wrappers for a cleaner API:

- `crates/at_wasm/wrapper_node.js` (Node, CommonJS)
- `crates/at_wasm/wrapper_web.js` (browser, ESM)

Both expose `run(source, options)` where options currently supports `maxInstructions`.

Node example:

```javascript
const { run } = require("./wrapper_node.js");
const result = run("print(1 + 1);", { maxInstructions: 10000 });
console.log(result.status, result.output);
```

Browser example:

```javascript
import { run } from "./wrapper_web.js";
const result = await run("print(1 + 1);", { maxInstructions: 10000 });
console.log(result.status, result.output);
```

Interactive browser example:

```bash
wasm-pack build crates/at_wasm --target web
python -m http.server
```

Open `http://localhost:8000/crates/at_wasm/example.html` for an editable source textarea, optional instruction limit, and JSON output panel.

### Raw WASM export: `run(source: string, max_instructions?: number): string`

Execute AT source code and return a JSON result.

#### Parameters

- `source` (string): AT source code to execute
- `max_instructions` (optional number): Maximum instructions to execute (prevents infinite loops)

#### Returns

JSON object with the following structure:

```typescript
{
  status: "ok" | "parse_error" | "compile_error" | "runtime_error" | "execution_limit",
  value?: string,      // Only when status is "ok"
  output: string[],    // Captured print() output
  error?: string       // Only on error
}
```

#### Example Response

Success:
```json
{
  "status": "ok",
  "value": "42",
  "output": ["Hello", "World"]
}
```

Error:
```json
{
  "status": "parse_error",
  "error": "UnexpectedToken { ... }",
  "output": []
}
```

## JavaScript Usage

### Basic Example

```javascript
// Load the WASM module
const wasm = await WebAssembly.instantiateStreaming(
  fetch("at_wasm.wasm"),
  { env: {} }
);

// Run some code
const source = `
fn greet(name: string) -> string {
    return "Hello, " + name + "!";
}

fn main() {
    print(greet("WASM"));
    return 42;
}
`;

const result = wasm.exports.run(source);
const parsed = JSON.parse(result);

console.log(parsed.status);  // "ok"
console.log(parsed.value);   // "42"
console.log(parsed.output);  // ["Hello, WASM!"]
```

### With Execution Limits

Prevent infinite loops:

```javascript
// Limit to 10000 instructions
const result = wasm.exports.run(source, 10000);
const parsed = JSON.parse(result);

if (parsed.status === "execution_limit") {
  console.error("Code took too long to execute");
}
```

### Error Handling

```javascript
const result = wasm.exports.run("fn main() { return unknown_var; }");
const parsed = JSON.parse(result);

switch (parsed.status) {
  case "ok":
    console.log("Result:", parsed.value);
    console.log("Output:", parsed.output);
    break;
  case "parse_error":
    console.error("Parse error:", parsed.error);
    break;
  case "compile_error":
    console.error("Compile error:", parsed.error);
    break;
  case "runtime_error":
    console.error("Runtime error:", parsed.error);
    console.log("Output before error:", parsed.output);
    break;
  case "execution_limit":
    console.error("Execution limit exceeded:", parsed.error);
    console.log("Partial output:", parsed.output);
    break;
}
```

## Features

### Supported

- All core language features (functions, types, control flow)
- Pattern matching with `match`
- Error handling with `result` and `option`
- Arrays and array operations
- String concatenation
- Captured `print()` output
- Execution limits

### Not Supported

- **Imports**: `import "..."` statements cannot be resolved in WASM
- **File I/O**: No filesystem access
- **Network**: Cannot fetch remote imports
- **Effects**: capability checks are simplified in the browser runtime
- **Capabilities**: `needs` declarations are not enforced

## Playground Example

Complete HTML/JS playground:

```html
<!DOCTYPE html>
<html>
<head>
    <title>AT Playground</title>
    <style>
        body { font-family: monospace; margin: 20px; }
        textarea { width: 100%; height: 200px; }
        #output { background: #f0f0f0; padding: 10px; margin-top: 10px; }
        .error { color: red; }
        .ok { color: green; }
    </style>
</head>
<body>
    <h1>AT Language Playground</h1>
    <textarea id="source">fn main() {
    print("Hello from WASM!");
    return 42;
}</textarea>
    <br>
    <button onclick="run()">Run</button>
    <div id="output"></div>

    <script>
        let wasm;
        
        async function init() {
            const response = await fetch("at_wasm.wasm");
            const bytes = await response.arrayBuffer();
            wasm = await WebAssembly.instantiate(bytes, { env: {} });
        }
        
        function run() {
            const source = document.getElementById("source").value;
            const result = wasm.exports.run(source, 10000);
            const parsed = JSON.parse(result);
            
            const output = document.getElementById("output");
            if (parsed.status === "ok") {
                output.innerHTML = `<div class="ok">Value: ${parsed.value}</div>`;
                output.innerHTML += `<div>Output: ${parsed.output.join("<br>")}</div>`;
            } else {
                output.innerHTML = `<div class="error">${parsed.status}: ${parsed.error}</div>`;
            }
        }
        
        init();
    </script>
</body>
</html>
```

## Limitations

### No Import Resolution

WASM runs in an isolated environment without filesystem or network access:

```at
// This will fail in WASM:
import "./lib.at" as lib;
```

**Workaround**: Include all code in a single file.

### Effects are Stubs

Time and random number functions return fixed values:

```at
fn demo() -> int needs { time } {
    // In WASM, time.now() always returns 0
    let now = time.now();
    
    // rng.uuid() returns the string "rng.uuid"
    let id = rng.uuid();
    
    return now;
}
```

### No REPL Features

The WASM module only supports running complete programs, not interactive sessions.

## Building for Production

### With wasm-pack

```bash
# Install wasm-pack
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Build for bundler
wasm-pack build crates/at_wasm --target bundler --release

# Build for web
wasm-pack build crates/at_wasm --target web --release
```

### Optimizing

```bash
# Strip debug symbols
wasm-strip target/wasm32-unknown-unknown/release/at_wasm.wasm

# Optimize with Binaryen
wasm-opt -O3 -o at_wasm_optimized.wasm target/wasm32-unknown-unknown/release/at_wasm.wasm
```

## Integration with Build Tools

### Webpack

```javascript
import init, { run } from './at_wasm/pkg/at_wasm.js';

async function execute() {
  await init();
  const result = run("fn main() { return 42; }");
  console.log(JSON.parse(result));
}
```

### Vite

```javascript
import init from './at_wasm.wasm?init';

const wasm = await init();
const result = wasm.exports.run("fn main() { return 42; }");
```

## Testing

Run the WASM tests:

```bash
cargo test -p at_wasm --target wasm32-unknown-unknown
```

## See Also

- `docs/lang.md` — Language reference
- `docs/cli.md` — CLI documentation
- `examples/` — Example programs (most work in WASM)
