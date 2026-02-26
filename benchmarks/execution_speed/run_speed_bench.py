#!/usr/bin/env python3
"""
Execution Speed Benchmark for the `at` programming language.

NO LLM REQUIRED. Measures the time for `at test` to execute test suites
across multiple problem files, demonstrating at's fast feedback loop
for agent-driven development.

Also compares against Python and Node.js execution times when available.
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


def find_runtime(name: str) -> str | None:
    import shutil
    return shutil.which(name)


def time_execution(cmd: list[str], code: str, suffix: str, runs: int = 5) -> dict:
    """Run code `runs` times and return timing stats."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=suffix, delete=False) as f:
        f.write(code)
        f.flush()
        temp_path = f.name

    durations = []
    passed = True
    try:
        for _ in range(runs):
            start = time.perf_counter()
            res = subprocess.run(
                cmd + [temp_path],
                capture_output=True, text=True, timeout=30
            )
            dur = (time.perf_counter() - start) * 1000  # ms
            durations.append(dur)
            if res.returncode != 0:
                passed = False
    except subprocess.TimeoutExpired:
        passed = False
        durations.append(30000)
    finally:
        os.unlink(temp_path)

    if not durations:
        return {"available": False}

    durations.sort()
    return {
        "available": True,
        "passed": passed,
        "runs": len(durations),
        "min_ms": round(min(durations), 2),
        "max_ms": round(max(durations), 2),
        "median_ms": round(durations[len(durations) // 2], 2),
        "mean_ms": round(sum(durations) / len(durations), 2),
    }


def main():
    parser = argparse.ArgumentParser(
        description="Execution Speed Benchmark: at test vs Python/Node.js"
    )
    parser.add_argument(
        "--runs", type=int, default=5,
        help="Number of runs per problem per language (default: 5)"
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

    python_bin = find_runtime("python3") or find_runtime("python")
    node_bin = find_runtime("node")

    # Load problems (reuse token efficiency problems)
    problems = []
    with open(PROBLEMS_FILE, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                problems.append(json.loads(line))

    print("=" * 70)
    print("  Execution Speed Benchmark: Agent Feedback Loop Latency")
    print("=" * 70)
    print(f"  at binary:  {at_binary}")
    print(f"  Python:     {python_bin or 'not found'}")
    print(f"  Node.js:    {node_bin or 'not found'}")
    print(f"  Runs/prob:  {args.runs}")
    print(f"  Problems:   {len(problems)}")
    print()

    results = []
    at_times = []
    py_times = []
    node_times = []

    for prob in problems:
        pid = prob["id"]
        name = prob["name"]
        sols = prob["solutions"]

        print(f"  {name} ({pid})")

        # at
        at_result = time_execution(
            [str(at_binary), "test"], sols["at"], ".at", args.runs
        )
        print(f"    {'at':>12}: {at_result['median_ms']:>8.2f}ms median", end="")
        if at_result.get("passed"):
            at_times.append(at_result["median_ms"])
            print()
        else:
            print(" (FAIL)")

        # Python
        py_result = {"available": False}
        if python_bin and "python" in sols:
            py_result = time_execution(
                [python_bin], sols["python"], ".py", args.runs
            )
            print(f"    {'python':>12}: {py_result['median_ms']:>8.2f}ms median", end="")
            if py_result.get("passed"):
                py_times.append(py_result["median_ms"])
                speedup = py_result["median_ms"] / at_result["median_ms"] if at_result["median_ms"] > 0 else 0
                print(f" ({speedup:.1f}x slower)")
            else:
                print(" (FAIL)")

        # Node.js
        node_result = {"available": False}
        if node_bin and "javascript" in sols:
            node_result = time_execution(
                [node_bin], sols["javascript"], ".js", args.runs
            )
            print(f"    {'node.js':>12}: {node_result['median_ms']:>8.2f}ms median", end="")
            if node_result.get("passed"):
                node_times.append(node_result["median_ms"])
                speedup = node_result["median_ms"] / at_result["median_ms"] if at_result["median_ms"] > 0 else 0
                print(f" ({speedup:.1f}x slower)")
            else:
                print(" (FAIL)")

        results.append({
            "id": pid, "name": name,
            "at": at_result,
            "python": py_result,
            "node": node_result,
        })

    # Summary
    print("\n" + "=" * 70)
    print("  AGGREGATE RESULTS (median of medians)")
    print("=" * 70)

    def median(lst):
        if not lst:
            return 0
        s = sorted(lst)
        return s[len(s) // 2]

    at_median = median(at_times)
    py_median = median(py_times)
    node_median = median(node_times)

    print(f"    {'at':>12}: {at_median:>8.2f}ms (baseline)")
    if py_times:
        speedup = py_median / at_median if at_median > 0 else 0
        print(f"    {'python':>12}: {py_median:>8.2f}ms ({speedup:.1f}x slower)")
    if node_times:
        speedup = node_median / at_median if at_median > 0 else 0
        print(f"    {'node.js':>12}: {node_median:>8.2f}ms ({speedup:.1f}x slower)")

    print(f"\n  For agent feedback loops:")
    print(f"    at test completes in ~{at_median:.0f}ms = instant feedback")
    if py_times:
        print(f"    Python takes ~{py_median:.0f}ms = {py_median / at_median:.0f}x more latency per iteration")

    if args.json:
        summary = {
            "at_median_ms": round(at_median, 2),
            "python_median_ms": round(py_median, 2) if py_times else None,
            "node_median_ms": round(node_median, 2) if node_times else None,
            "problems": results,
        }
        results_dir = SCRIPT_DIR.parent / "results"
        results_dir.mkdir(exist_ok=True)
        out_file = results_dir / "execution_speed.json"
        with open(out_file, "w") as f:
            json.dump(summary, f, indent=2)
        print(f"\n  Results written to: {out_file}")


if __name__ == "__main__":
    main()
