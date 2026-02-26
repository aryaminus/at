#!/usr/bin/env python3
"""
Unified benchmark runner for the `at` programming language.

Runs all available benchmarks and produces a combined report.
Benchmarks that require no LLM are run by default.
"""

import subprocess
import sys
import argparse
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent

BENCHMARKS = {
    "tokens": {
        "name": "Token Efficiency",
        "script": SCRIPT_DIR / "token_efficiency" / "run_token_bench.py",
        "requires_llm": False,
        "description": "Compare at token usage vs Python/JS/Go/Java",
    },
    "speed": {
        "name": "Execution Speed",
        "script": SCRIPT_DIR / "execution_speed" / "run_speed_bench.py",
        "requires_llm": False,
        "description": "Measure at test feedback loop latency vs Python/Node.js",
    },
    "determinism": {
        "name": "Determinism",
        "script": SCRIPT_DIR / "determinism" / "run_determinism_bench.py",
        "requires_llm": False,
        "description": "Prove zero flaky test failures across N runs",
    },
    "autocodebench": {
        "name": "AutoCodeBenchmark",
        "script": SCRIPT_DIR / "autocodebench" / "run_autocodebench.py",
        "requires_llm": True,
        "description": "LLM-based evaluation using Tencent AutoCodeBenchmark",
    },
    "humaneval": {
        "name": "HumanEval-at",
        "script": SCRIPT_DIR / "humaneval_at" / "run_humaneval.py",
        "requires_llm": True,
        "description": "HumanEval pass@1 evaluation translated to at syntax",
    },
}


def main():
    parser = argparse.ArgumentParser(
        description="Run all `at` benchmarks"
    )
    parser.add_argument(
        "--suite", type=str, default=None,
        choices=list(BENCHMARKS.keys()),
        help="Run only a specific benchmark suite"
    )
    parser.add_argument(
        "--include-llm", action="store_true",
        help="Also run LLM-based benchmarks (requires openai package + API key)"
    )
    parser.add_argument(
        "--json", action="store_true",
        help="Pass --json to each benchmark for machine-readable output"
    )
    parser.add_argument(
        "--validate", action="store_true",
        help="Pass --validate to token bench to verify at solutions"
    )
    parser.add_argument(
        "--determinism-runs", type=int, default=50,
        help="Number of runs for determinism benchmark (default: 50)"
    )
    args = parser.parse_args()

    suites = [args.suite] if args.suite else list(BENCHMARKS.keys())

    print("=" * 70)
    print("  `at` Language Benchmark Suite")
    print("=" * 70)
    print()

    for suite_id in suites:
        bench = BENCHMARKS[suite_id]

        if bench["requires_llm"] and not args.include_llm:
            print(f"  Skipping {bench['name']} (requires --include-llm)")
            continue

        if not bench["script"].exists():
            print(f"  Skipping {bench['name']} (script not found: {bench['script']})")
            continue

        print(f"\n{'#' * 70}")
        print(f"  Running: {bench['name']}")
        print(f"  {bench['description']}")
        print(f"{'#' * 70}\n")

        cmd = [sys.executable, str(bench["script"])]

        if args.json:
            cmd.append("--json")

        if suite_id == "tokens" and args.validate:
            cmd.append("--validate")

        if suite_id == "humaneval" and args.validate:
            cmd.append("--validate")

        if suite_id == "determinism":
            cmd.extend(["--runs", str(args.determinism_runs)])

        if suite_id == "autocodebench":
            cmd.extend(["--dry-run"])  # Default to dry-run for safety

        result = subprocess.run(cmd)

        if result.returncode != 0:
            print(f"\n  WARNING: {bench['name']} exited with code {result.returncode}")

    print(f"\n{'=' * 70}")
    print("  Benchmark suite complete.")
    print(f"{'=' * 70}")


if __name__ == "__main__":
    main()
