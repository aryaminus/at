# Changelog

All notable changes to this project will be documented in this file.

## [0.1.0] - 2026-02-20

### Added
- Core language: `int`, `float`, `bool`, `string`, `unit`, `array<T>`, `option<T>`, `result<T,E>`
- Control flow: `if` expressions, `while`, `for`, `break`, `continue`
- Pattern matching: `match` with `ok`, `err`, `some`, `none`, `_` patterns
- Error handling: `?` (try) operator, `option`, `result` types
- Effects system: `needs`/`using` for capability management
- Imports: local paths and URL imports with content-hash caching
- Test blocks: `test "name" { ... }` with `assert` and `assert_eq`
- Tool functions: `tool fn` for MCP integration
- CLI: `run`, `repl`, `fmt`, `check`, `lint`, `test`, `bench`, `deps`, `cache`
- LSP server: diagnostics, hover, go-to-definition, completion, inlay hints
- MCP server: expose `tool fn` functions to AI agents
- WASM target: browser and Node.js support
- Code formatter (`at fmt`)
- Type checker (`at check`)
- Linter with auto-fix (`at lint --fix`)
- Benchmark runner (`at bench`)
- Dependency viewer (`at deps --tree`)
- Cache management (`at cache add/list/show/remove/clear/prune`)
- Standard library (`stdlib/std.at`)
