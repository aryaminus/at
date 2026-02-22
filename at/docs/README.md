# at Documentation

Complete documentation for the `at` programming language.

## Quick Links

- [Language Reference](lang.md) — Syntax, types, features
- [CLI Reference](cli.md) — All commands and options  
- [WASM Guide](wasm.md) — Browser and embedded usage
- [Package Manager](package_manager.md) — Lockfile and cache

## Language Overview

`at` is designed for AI agents with these principles:

1. **Agent-first**: Readable without IDE support
2. **Explicit effects**: `needs`/`using` make dependencies visible
3. **Greppable**: Clear imports, no barrel files
4. **Predictable**: Results over exceptions
5. **Fast**: Cold start in milliseconds

## Getting Started

See the [main README](../README.md) for installation and quick start.

## Documentation Index

| Document | Description |
|----------|-------------|
| [lang.md](lang.md) | Complete language reference (types, syntax, features) |
| [cli.md](cli.md) | CLI commands and options reference |
| [wasm.md](wasm.md) | WebAssembly usage guide |
| [package_manager.md](package_manager.md) | Lockfile format and cache management |

## Design

See [Armin Ronacher's blog post](https://armin.dev/blog/2026/02/a-language-for-agents) for design rationale.

Key features:
- Braces-based blocks (not whitespace-sensitive)
- Explicit `result<T, E>` and `option<T>` types
- `needs`/`using` for capability tracking
- Pattern matching with `match`
- Immutable arrays
- First-class MCP tool support

## Examples

See `examples/` directory for working examples:
- `sum.at` — Basic arithmetic
- `primes.at` — Algorithm example
- `features.at` — Comprehensive feature demo
- `calculator.at` — Error handling example
- `mcp_tools.at` — MCP tool definition sample

## License

MIT
