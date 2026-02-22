# Installation

## Pre-built binaries

Download from GitHub releases for your platform:

```bash
# macOS (Intel)
curl -L https://github.com/aryaminus/at/releases/latest/download/at-x86_64-apple-darwin.tar.gz | tar xz
sudo mv at-x86_64-apple-darwin /usr/local/bin/at

# macOS (Apple Silicon)
curl -L https://github.com/aryaminus/at/releases/latest/download/at-aarch64-apple-darwin.tar.gz | tar xz
sudo mv at-aarch64-apple-darwin /usr/local/bin/at

# Linux
curl -L https://github.com/aryaminus/at/releases/latest/download/at-x86_64-unknown-linux-gnu.tar.gz | tar xz
sudo mv at-x86_64-unknown-linux-gnu /usr/local/bin/at
```

## Homebrew

Homebrew tap is published under `aryaminus/homebrew-at`.

```bash
brew tap aryaminus/at
brew install at
```

## Windows

```powershell
Invoke-WebRequest -Uri https://github.com/aryaminus/at/releases/latest/download/at-x86_64-pc-windows-msvc.zip -OutFile at.zip
Expand-Archive .\at.zip -DestinationPath .
Move-Item .\at-x86_64-pc-windows-msvc.exe C:\Windows\System32\at.exe
```

## Cargo (from source)

```bash
cargo install --path crates/at_cli
```

## npm (WASM)

```bash
npm install -g @aryaminus/at
```

For local development builds:

```bash
npm install
npm run build:wasm
```
