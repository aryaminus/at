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

Build (browser):

```
wasm-pack build crates/at_wasm --target web
```

Example (browser):

```
python -m http.server
```

Open `http://localhost:8000/crates/at_wasm/example.html`.

API:

```
run(source: &str) -> String
```
