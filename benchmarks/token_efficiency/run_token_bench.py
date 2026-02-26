#!/usr/bin/env python3
"""
Token Efficiency Benchmark for the `at` programming language.

NO LLM REQUIRED. Compares hand-written equivalent solutions across
at, Python, JavaScript, Go, and Java on identical problems.
Measures token count using tiktoken (or word-based estimate).

This proves at's core value proposition: equivalent logic requires
fewer output tokens than traditional languages.
"""

import json
import os
import sys
import subprocess
import argparse
from pathlib import Path

try:
    import tiktoken
    _encoder = tiktoken.get_encoding("cl100k_base")
    def count_tokens(text: str) -> int:
        if not text:
            return 0
        return len(_encoder.encode(text))
    TOKEN_METHOD = "tiktoken (cl100k_base)"
except ImportError:
    def count_tokens(text: str) -> int:
        if not text:
            return 0
        return int(len(text.split()) * 1.3)
    TOKEN_METHOD = "word estimate (install tiktoken for accuracy)"


LANGUAGES = ["at", "python", "javascript", "go", "java"]
SCRIPT_DIR = Path(__file__).resolve().parent
PROBLEMS_FILE = SCRIPT_DIR / "problems.jsonl"
PROJECT_ROOT = SCRIPT_DIR.parent.parent


def load_problems(path: Path) -> list[dict]:
    problems = []
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                problems.append(json.loads(line))
    return problems


def validate_at_solution(problem: dict, at_binary: Path) -> tuple[bool, float]:
    """Run `at test` on the at solution. Returns (passed, duration_ms)."""
    import time
    import tempfile

    code = problem["solutions"]["at"]
    with tempfile.NamedTemporaryFile(mode="w", suffix=".at", delete=False) as f:
        f.write(code)
        f.flush()
        temp_path = f.name

    try:
        start = time.perf_counter()
        res = subprocess.run(
            [str(at_binary), "test", temp_path],
            capture_output=True, text=True, timeout=30
        )
        duration = (time.perf_counter() - start) * 1000
        return res.returncode == 0, duration
    except subprocess.TimeoutExpired:
        return False, 30000
    finally:
        os.unlink(temp_path)


def find_at_binary() -> Path | None:
    for path in [
        PROJECT_ROOT / "target" / "release" / "at",
        PROJECT_ROOT / "target" / "debug" / "at",
    ]:
        if path.exists():
            return path
    import shutil
    which = shutil.which("at")
    return Path(which) if which else None


def main():
    parser = argparse.ArgumentParser(
        description="Token Efficiency Benchmark: at vs Python/JS/Go/Java"
    )
    parser.add_argument(
        "--validate", action="store_true",
        help="Also run `at test` to verify at solutions pass"
    )
    parser.add_argument(
        "--json", action="store_true",
        help="Output results as JSON"
    )
    args = parser.parse_args()

    problems = load_problems(PROBLEMS_FILE)

    at_binary = None
    if args.validate:
        at_binary = find_at_binary()
        if not at_binary:
            print("Error: --validate requires `at` binary. Build with: cargo build --release")
            sys.exit(1)

    print("=" * 70)
    print("  Token Efficiency Benchmark: `at` vs Python / JS / Go / Java")
    print(f"  Token counting: {TOKEN_METHOD}")
    print(f"  Problems: {len(problems)}")
    print("=" * 70)

    # Per-language totals
    totals = {lang: 0 for lang in LANGUAGES}
    results = []

    for prob in problems:
        pid = prob["id"]
        name = prob["name"]
        solutions = prob["solutions"]

        counts = {}
        for lang in LANGUAGES:
            if lang in solutions:
                counts[lang] = count_tokens(solutions[lang])
                totals[lang] += counts[lang]

        at_count = counts.get("at", 0)
        validated = None
        runtime_ms = None

        if args.validate and at_binary:
            passed, dur = validate_at_solution(prob, at_binary)
            validated = passed
            runtime_ms = dur

        result = {
            "id": pid,
            "name": name,
            "tokens": counts,
            "validated": validated,
            "runtime_ms": round(runtime_ms, 2) if runtime_ms else None,
        }
        results.append(result)

        # Print per-problem results
        print(f"\n  {name} ({pid})")
        for lang in LANGUAGES:
            if lang in counts:
                tok = counts[lang]
                if lang == "at":
                    marker = " <--"
                    if validated is not None:
                        marker = f" {'PASS' if validated else 'FAIL'} ({runtime_ms:.0f}ms)"
                else:
                    savings = ((tok - at_count) / tok * 100) if tok > 0 else 0
                    marker = f" ({savings:+.1f}% vs at)" if at_count > 0 else ""
                print(f"    {lang:>12}: {tok:>5} tokens{marker}")

    # Summary
    print("\n" + "=" * 70)
    print("  AGGREGATE RESULTS")
    print("=" * 70)

    at_total = totals.get("at", 1)
    summary = {"token_method": TOKEN_METHOD, "problems": len(problems), "languages": {}}

    for lang in LANGUAGES:
        total = totals[lang]
        ratio = total / at_total if at_total > 0 else 0
        savings = ((total - at_total) / total * 100) if total > 0 else 0

        summary["languages"][lang] = {
            "total_tokens": total,
            "ratio_vs_at": round(ratio, 3),
            "savings_pct": round(savings, 1),
        }

        if lang == "at":
            print(f"    {'at':>12}: {total:>6} tokens (baseline)")
        else:
            print(f"    {lang:>12}: {total:>6} tokens ({savings:+.1f}% vs at, {ratio:.2f}x)")

    print(f"\n  Token Efficiency Ratio (lower = more efficient):")
    for lang in LANGUAGES:
        ratio = summary["languages"][lang]["ratio_vs_at"]
        bar = "#" * int(ratio * 20)
        print(f"    {lang:>12}: {bar} {ratio:.2f}x")

    if args.json:
        output = {
            "summary": summary,
            "problems": results,
        }
        results_dir = SCRIPT_DIR.parent / "results"
        results_dir.mkdir(exist_ok=True)
        out_file = results_dir / "token_efficiency.json"
        with open(out_file, "w") as f:
            json.dump(output, f, indent=2)
        print(f"\n  Results written to: {out_file}")


if __name__ == "__main__":
    main()
