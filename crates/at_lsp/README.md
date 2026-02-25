# at_lsp

Minimal LSP server for the at language.

Run:

```
cargo run -p at -- lsp
```

Features:

- Diagnostics: parse, typecheck, lint, compile errors.
- Hover and go-to-definition for local functions and import aliases.
- Completion: local functions, import aliases, builtins, effects, and imported module members.
- Inlay hints for parameter and return types.
- Semantic tokens for syntax highlighting.
- Signature help for function parameters.
- Document symbols and outline.
- Code actions (e.g., quick fixes).
- Document highlights for symbol references.
- Folding ranges for functions, blocks, and comments.
- Formatting via the built-in formatter.

Remote imports:

- If a URL import is present, LSP resolves it from `.at/lock` + `.at/cache`.
- If no lock exists, it falls back to fetching the URL and caching by content hash under `.at/cache`.
- Legacy cache filenames (URL with `/` replaced) are also checked.
