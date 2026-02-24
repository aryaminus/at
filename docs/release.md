# Release Checklist

## Versioning

- `Cargo.toml` workspace version is the canonical version.
- Tag releases with `vX.Y.Z`.

## Before tagging

- Ensure `cargo test` passes
- Ensure GitHub branch protection requires both `ci` and `wasm` checks on PRs to `main`
- Update `CHANGELOG.md` (if used)
- If warning budget changed intentionally, update `.clippy-warning-baseline.toml` with:
  - `cargo clippy --workspace --all-targets --message-format=json --quiet > /tmp/clippy.jsonl`
  - `python3 scripts/check_clippy_warning_budget.py /tmp/clippy.jsonl .clippy-warning-baseline.toml`
- Confirm secrets are set in GitHub Actions:
  - `NPM_TOKEN`
  - `HOMEBREW_TAP_TOKEN`

## Release steps

- Merge changes to `main`.
- CI will bump the version, commit, and tag automatically.

## What the CI does

- Builds and uploads binaries to GitHub Releases (macOS Intel/ARM, Linux, Windows)
- Publishes `@aryaminus/at` to npm (WASM bindings)
- Updates the Homebrew formula in `aryaminus/homebrew-at`
- Runs install smoke checks for packaged binary and `cargo install --path` flows
- Enforces non-correctness clippy warning budget from `.clippy-warning-baseline.toml`
