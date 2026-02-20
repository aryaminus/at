#!/bin/sh
set -e

RUNS=${RUNS:-10}
RELEASE=${RELEASE:-0}
PRETTY=${PRETTY:-0}
OUT=${OUT:-}
SHA=${SHA:-}
if [ -z "$SHA" ] && command -v git >/dev/null 2>&1; then
  SHA=$(git rev-parse HEAD 2>/dev/null || true)
fi
TIMESTAMP=${TIMESTAMP:-}
if [ -z "$TIMESTAMP" ] && command -v date >/dev/null 2>&1; then
  TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)
fi
OS=${OS:-}
if [ -z "$OS" ] && command -v uname >/dev/null 2>&1; then
  OS=$(uname -s 2>/dev/null || true)
fi
ARCH=${ARCH:-}
if [ -z "$ARCH" ] && command -v uname >/dev/null 2>&1; then
  ARCH=$(uname -m 2>/dev/null || true)
fi
RUST=${RUST:-}
if [ -z "$RUST" ] && command -v rustc >/dev/null 2>&1; then
  RUST=$(rustc --version 2>/dev/null || true)
fi
PROFILE=${PROFILE:-}
if [ -z "$PROFILE" ]; then
  if [ "$RELEASE" = "1" ]; then
    PROFILE="release"
  else
    PROFILE="debug"
  fi
fi

CARGO_FLAGS=""
if [ "$RELEASE" = "1" ]; then
  CARGO_FLAGS="--release"
fi

run_bench() {
  name="$1"
  file="$2"
  output=$(cargo run $CARGO_FLAGS -p at -- bench "$file" --runs "$RUNS" --json | tail -n 1)
  printf "{\"name\":\"%s\",\"metrics\":%s}" "$name" "$output"
}

first=1
output="["
for entry in \
  "bench examples/bench.at" \
  "primes examples/primes.at" \
  "skip_sum examples/skip_sum.at" \
  "sum examples/sum.at"; do
  name=$(printf "%s" "$entry" | cut -d' ' -f1)
  file=$(printf "%s" "$entry" | cut -d' ' -f2)
  if [ "$first" -eq 0 ]; then
    output="$output,"
  fi
  first=0
  output="$output$(run_bench "$name" "$file")"
done
output="$output]"
if [ -n "$SHA" ] || [ -n "$TIMESTAMP" ] || [ -n "$OS" ] || [ -n "$ARCH" ] || [ -n "$RUST" ] || [ -n "$PROFILE" ]; then
  meta=""
  if [ -n "$SHA" ]; then
    meta="\"sha\":\"$SHA\""
  fi
  if [ -n "$TIMESTAMP" ]; then
    if [ -n "$meta" ]; then
      meta="$meta,"
    fi
    meta="${meta}\"timestamp\":\"$TIMESTAMP\""
  fi
  if [ -n "$OS" ]; then
    if [ -n "$meta" ]; then
      meta="$meta,"
    fi
    meta="${meta}\"os\":\"$OS\""
  fi
  if [ -n "$ARCH" ]; then
    if [ -n "$meta" ]; then
      meta="$meta,"
    fi
    meta="${meta}\"arch\":\"$ARCH\""
  fi
  if [ -n "$RUST" ]; then
    if [ -n "$meta" ]; then
      meta="$meta,"
    fi
    meta="${meta}\"rust\":\"$RUST\""
  fi
  if [ -n "$PROFILE" ]; then
    if [ -n "$meta" ]; then
      meta="$meta,"
    fi
    meta="${meta}\"profile\":\"$PROFILE\""
  fi
  output="{${meta},\"benchmarks\":$output}"
fi
if [ -n "$OUT" ]; then
  printf "%s" "$output" > "$OUT"
fi
if [ "$PRETTY" = "1" ]; then
  printf "%s" "$output" | python -m json.tool
else
  printf "%s\n" "$output"
fi
