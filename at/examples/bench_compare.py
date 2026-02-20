#!/usr/bin/env python3
import json
import sys


def load(path):
    with open(path, "r", encoding="utf-8") as handle:
        data = json.load(handle)
    if isinstance(data, dict) and "benchmarks" in data:
        return data["benchmarks"]
    return data


def index_by_name(items):
    return {item["name"]: item for item in items}


def main():
    if len(sys.argv) < 3 or len(sys.argv) > 4:
        print("usage: bench_compare.py <baseline.json> <current.json> [threshold_pct]")
        return 1

    threshold = None
    if len(sys.argv) == 4:
        try:
            threshold = float(sys.argv[3])
        except ValueError:
            print("threshold_pct must be a number")
            return 1

    baseline = index_by_name(load(sys.argv[1]))
    current = index_by_name(load(sys.argv[2]))

    names = sorted(set(baseline.keys()) | set(current.keys()))
    failures = 0
    for name in names:
        if name not in baseline:
            print(f"{name}: new")
            continue
        if name not in current:
            print(f"{name}: missing")
            continue

        base = baseline[name]["metrics"]
        now = current[name]["metrics"]
        base_ns = base.get("run_ns")
        now_ns = now.get("run_ns")
        if base_ns is None or now_ns is None:
            print(f"{name}: missing run_ns")
            continue

        delta = now_ns - base_ns
        if base_ns == 0:
            print(f"{name}: base run_ns is zero")
            continue

        pct = (delta / base_ns) * 100.0
        sign = "+" if delta >= 0 else ""
        print(f"{name}: {sign}{pct:.2f}% ({base_ns} -> {now_ns})")
        if threshold is not None and pct > threshold:
            failures += 1

    if failures > 0:
        print(f"regressions over {threshold:.2f}%: {failures}")
        return 2

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
