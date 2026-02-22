# Release Checklist

## Versioning

- `Cargo.toml` workspace version is the canonical version.
- Tag releases with `vX.Y.Z`.

## Before tagging

- Ensure `cargo test` passes
- Update `CHANGELOG.md` (if used)
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
