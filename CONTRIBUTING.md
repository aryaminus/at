# Contributing to at

Thanks for your interest in contributing to `at`!

## Getting Started

```bash
git clone https://github.com/your-org/at.git
cd at
cargo build
cargo test --workspace
```

## Project Structure

```
at/
├── crates/
│   ├── at_cli/       # CLI binary (entry point)
│   ├── at_parser/    # Lexer and parser
│   ├── at_syntax/    # AST types and spans
│   ├── at_vm/        # Bytecode compiler and VM
│   ├── at_check/     # Type checker
│   ├── at_lint/      # Linter
│   ├── at_fmt/       # Code formatter
│   ├── at_lsp/       # Language Server Protocol
│   ├── at_mcp/       # Model Context Protocol server
│   └── at_wasm/      # WebAssembly bindings
├── docs/             # Documentation
├── examples/         # Example .at programs
└── stdlib/           # Standard library
```

## Development

### Running Tests

```bash
# All tests
cargo test --workspace

# Specific crate
cargo test -p at_parser

# Integration tests only
cargo test -p at --test '*'
```

### Building

```bash
# Debug build
cargo build

# Release build
cargo build --release

# WASM build
cargo build -p at_wasm --target wasm32-unknown-unknown --release
```

### Code Style

- Run `cargo fmt` before committing
- Run `cargo clippy --workspace` and address warnings
- Keep functions small and focused
- Prefer returning `Result` over panicking

## Pull Requests

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-change`)
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass (`cargo test --workspace`)
6. Run `cargo fmt` and `cargo clippy --workspace`
7. Submit a pull request

## Reporting Issues

- Use GitHub Issues for bug reports and feature requests
- Include a minimal `.at` code snippet that reproduces the issue
- Include the output of `at --version`

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
