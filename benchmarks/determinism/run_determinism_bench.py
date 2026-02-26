#!/usr/bin/env python3
"""
Determinism Benchmark for the `at` programming language.

NO LLM REQUIRED. Runs each test suite N times and verifies zero flaky failures.
This proves at's deterministic execution model produces reliable agent feedback.

Inspired by findings that Python test suites exhibit 2-16% flakiness rates
(see: "An Empirical Study of Flaky Tests", Luo et al. 2014).
"""

import json
import os
import sys
import subprocess
import time
import argparse
import tempfile
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
PROBLEMS_FILE = SCRIPT_DIR.parent / "token_efficiency" / "problems.jsonl"


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
        description="Determinism Benchmark: Zero Flakiness Proof"
    )
    parser.add_argument(
        "--runs", type=int, default=100,
        help="Number of runs per test (default: 100)"
    )
    parser.add_argument(
        "--json", action="store_true",
        help="Output results as JSON"
    )
    args = parser.parse_args()

    at_binary = find_at_binary()
    if not at_binary:
        print("Error: `at` binary not found. Build with: cargo build --release")
        sys.exit(1)

    # Load problems
    problems = []
    with open(PROBLEMS_FILE, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                problems.append(json.loads(line))

    print("=" * 70)
    print("  Determinism Benchmark: Zero Flakiness Proof")
    print("=" * 70)
    print(f"  at binary: {at_binary}")
    print(f"  Runs per test: {args.runs}")
    print(f"  Test suites: {len(problems)}")
    total_executions = len(problems) * args.runs
    print(f"  Total executions: {total_executions}")
    print()

    results = []
    total_flaky = 0
    total_pass = 0
    total_fail = 0

    for prob in problems:
        pid = prob["id"]
        name = prob["name"]
        code = prob["solutions"]["at"]

        # Write to temp file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".at", delete=False) as f:
            f.write(code)
            f.flush()
            temp_path = f.name

        passes = 0
        failures = 0
        times = []

        # Find and clear the test cache directory
        cache_dir = PROJECT_ROOT / ".at" / "test-cache"

        try:
            for i in range(args.runs):
                # Clear test cache to force re-execution each run
                if cache_dir.exists():
                    import shutil
                    shutil.rmtree(cache_dir, ignore_errors=True)

                start = time.perf_counter()
                res = subprocess.run(
                    [str(at_binary), "test", temp_path],
                    capture_output=True, text=True, timeout=30
                )
                dur = (time.perf_counter() - start) * 1000
                times.append(dur)

                if res.returncode == 0:
                    passes += 1
                else:
                    failures += 1
        except subprocess.TimeoutExpired:
            failures += 1
        finally:
            os.unlink(temp_path)

        is_flaky = passes > 0 and failures > 0
        status = "FLAKY" if is_flaky else ("PASS" if passes == args.runs else "FAIL")

        if is_flaky:
            total_flaky += 1
        total_pass += passes
        total_fail += failures

        times.sort()
        median_ms = times[len(times) // 2] if times else 0
        stddev = (sum((t - sum(times)/len(times))**2 for t in times) / len(times))**0.5 if times else 0

        result = {
            "id": pid,
            "name": name,
            "runs": args.runs,
            "passes": passes,
            "failures": failures,
            "flaky": is_flaky,
            "median_ms": round(median_ms, 2),
            "stddev_ms": round(stddev, 2),
        }
        results.append(result)

        marker = "FLAKY!" if is_flaky else ("OK" if status == "PASS" else "CONSISTENTLY FAILING")
        print(f"  {name:.<40} {passes}/{args.runs} passed  {median_ms:>6.1f}ms +/- {stddev:.1f}ms  [{marker}]")

    # Summary
    print("\n" + "=" * 70)
    print("  RESULTS")
    print("=" * 70)
    print(f"  Total executions:  {total_executions}")
    print(f"  Total passes:      {total_pass}")
    print(f"  Total failures:    {total_fail}")
    print(f"  Flaky test suites: {total_flaky}/{len(problems)}")
    flakiness_rate = (total_flaky / len(problems) * 100) if problems else 0
    print(f"  Flakiness rate:    {flakiness_rate:.1f}%")

    if total_flaky == 0:
        print(f"\n  ZERO flaky failures across {total_executions} executions.")
        print(f"  at's deterministic VM guarantees reliable agent feedback loops.")
    else:
        print(f"\n  WARNING: {total_flaky} flaky test suite(s) detected.")

    if args.json:
        summary = {
            "total_executions": total_executions,
            "total_passes": total_pass,
            "total_failures": total_fail,
            "flaky_suites": total_flaky,
            "flakiness_rate_pct": round(flakiness_rate, 2),
            "problems": results,
        }
        results_dir = SCRIPT_DIR.parent / "results"
        results_dir.mkdir(exist_ok=True)
        out_file = results_dir / "determinism.json"
        with open(out_file, "w") as f:
            json.dump(summary, f, indent=2)
        print(f"\n  Results written to: {out_file}")


if __name__ == "__main__":
    main()
