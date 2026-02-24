#!/usr/bin/env python3
"""Enforce a no-increase policy for non-correctness clippy warnings."""

from __future__ import annotations

import json
import pathlib
import sys
from collections import Counter

try:
    import tomllib
except ImportError:  # pragma: no cover
    import tomli as tomllib  # type: ignore


def load_baseline(path: pathlib.Path) -> int:
    data = tomllib.loads(path.read_text(encoding="utf-8"))
    value = data.get("total_warnings")
    if not isinstance(value, int):
        raise ValueError(f"{path}: expected integer total_warnings")
    return value


def load_warning_counts(path: pathlib.Path) -> tuple[int, Counter[str]]:
    counts: Counter[str] = Counter()
    total = 0
    with path.open(encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if not line:
                continue
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                continue
            if payload.get("reason") != "compiler-message":
                continue
            message = payload.get("message")
            if not isinstance(message, dict):
                continue
            if message.get("level") != "warning":
                continue
            total += 1
            code = message.get("code")
            lint = code.get("code") if isinstance(code, dict) else None
            counts[lint or "rustc::warning"] += 1
    return total, counts


def main() -> int:
    if len(sys.argv) != 3:
        print(
            "usage: check_clippy_warning_budget.py <clippy-jsonl> <baseline-toml>",
            file=sys.stderr,
        )
        return 2

    jsonl_path = pathlib.Path(sys.argv[1])
    baseline_path = pathlib.Path(sys.argv[2])
    try:
        baseline_total = load_baseline(baseline_path)
        current_total, counts = load_warning_counts(jsonl_path)
    except Exception as exc:  # pragma: no cover
        print(f"error: {exc}", file=sys.stderr)
        return 2

    print(f"clippy warnings: {current_total} (budget: {baseline_total})")
    for lint, count in counts.most_common(10):
        print(f"  {lint}: {count}")

    if current_total > baseline_total:
        print(
            f"error: warning budget exceeded by {current_total - baseline_total}",
            file=sys.stderr,
        )
        print(
            "update fixes or regenerate .clippy-warning-baseline.toml intentionally",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
