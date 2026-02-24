# at_wasm

Minimal WASM wrapper for running at source strings.

Build (wasm32 target):

```
rustup target add wasm32-unknown-unknown
cargo build -p at_wasm --target wasm32-unknown-unknown
```

Build (wasm-pack):

```
cargo install wasm-pack
wasm-pack build crates/at_wasm --target nodejs
```

Example (Node):

```
node crates/at_wasm/example.js
```

Node wrapper API (`crates/at_wasm/wrapper_node.js`):

```
run(source: string, options?: { maxInstructions?: number }) -> AtResult
```

Build (browser):

```
wasm-pack build crates/at_wasm --target web
```

Example (browser):

```
python -m http.server
```

Open `http://localhost:8000/crates/at_wasm/example.html`.

The browser example includes:
- editable source textarea
- optional max-instructions input
- run button
- JSON result/output panel

Browser wrapper API (`crates/at_wasm/wrapper_web.js`):

```
run(source: string, options?: { maxInstructions?: number, wasmModule?: ... }) -> Promise<AtResult>
```

Raw WASM API:

```
run(source: &str, max_instructions: Option<usize>) -> String
```
