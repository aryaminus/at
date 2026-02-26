#!/usr/bin/env python3
"""
HumanEval-at: Pass@1 evaluation for the `at` programming language.

Two modes of operation:

1. --validate: Runs the hand-translated seed problems (problems.jsonl) through
   `at test` to verify canonical solutions. No LLM needed.

2. --model <name>: Downloads the full OpenAI HumanEval dataset (164 Python
   problems) and uses an LLM to translate each problem into `at` code.
   The LLM receives the Python prompt, Python tests, and skill.md context,
   then produces a complete `at` file (function + tests). The generated code
   is validated via `at test`.

Dataset source:
  https://github.com/openai/human-eval (HumanEval.jsonl.gz, 164 problems)

Best practices:
  - HumanEval (OpenAI): JSONL format, pass@k, per-instance logging
  - SkillsBench (arXiv 2602.12670): Context provisioning via skill.md

Usage:
  # Validate hand-translated canonical solutions (no LLM needed):
  python3 run_humaneval.py --validate

  # Run pass@1 with an LLM on the full 164-problem set:
  pip install openai
  export OPENAI_BASE_URL="http://localhost:11434/v1"
  export OPENAI_API_KEY="ollama"
  python3 run_humaneval.py --model "qwen2.5-coder:7b"

  # Limit to first N problems:
  python3 run_humaneval.py --model "gpt-4o" --count 20
"""

import gzip
import json
import os
import shutil
import sys
import subprocess
import time
import argparse
import tempfile
import urllib.request
from pathlib import Path
from datetime import datetime, timezone

# ---------------------------------------------------------------------------
# Optional dependencies
# ---------------------------------------------------------------------------
try:
    import openai
    _has_openai = True
except ImportError:
    _has_openai = False

try:
    import tiktoken
    _encoder = tiktoken.get_encoding("cl100k_base")
    def count_tokens(text: str) -> int:
        return len(_encoder.encode(text)) if text else 0
except ImportError:
    def count_tokens(text: str) -> int:
        return len(text.split()) if text else 0

# ---------------------------------------------------------------------------
# Paths & constants
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent
SEED_PROBLEMS_FILE = SCRIPT_DIR / "problems.jsonl"
SKILL_FILE = PROJECT_ROOT / "skill.md"

HUMANEVAL_URL = (
    "https://github.com/openai/human-eval/raw/master/data/HumanEval.jsonl.gz"
)
HUMANEVAL_CACHE = SCRIPT_DIR / "HumanEval.jsonl.gz"


# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

def find_at_binary() -> Path | None:
    for path in [
        PROJECT_ROOT / "target" / "release" / "at",
        PROJECT_ROOT / "target" / "debug" / "at",
    ]:
        if path.exists():
            return path
    which = shutil.which("at")
    return Path(which) if which else None


def load_seed_problems() -> list[dict]:
    """Load hand-translated at problems (for --validate)."""
    problems = []
    with open(SEED_PROBLEMS_FILE, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                problems.append(json.loads(line))
    return problems


def download_humaneval() -> Path:
    """Download the full HumanEval dataset from GitHub (once, cached)."""
    if HUMANEVAL_CACHE.exists():
        return HUMANEVAL_CACHE
    print(f"  Downloading HumanEval.jsonl.gz from GitHub...")
    req = urllib.request.Request(
        HUMANEVAL_URL,
        headers={"User-Agent": "at-bench/1.0"},
    )
    with urllib.request.urlopen(req, timeout=120) as resp:
        with open(HUMANEVAL_CACHE, "wb") as f:
            shutil.copyfileobj(resp, f)
    size_kb = HUMANEVAL_CACHE.stat().st_size / 1024
    print(f"  Saved to {HUMANEVAL_CACHE} ({size_kb:.1f} KB)")
    return HUMANEVAL_CACHE


def load_full_humaneval() -> list[dict]:
    """Load the full 164-problem HumanEval dataset."""
    cache = download_humaneval()
    problems = []
    with gzip.open(cache, "rt", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                problems.append(json.loads(line))
    return problems


def run_at_test(at_binary: Path, code: str, timeout_s: int = 10) -> dict:
    """Write code to a temp file, run `at test`, return outcome."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".at", delete=False) as f:
        f.write(code)
        f.flush()
        temp_path = f.name

    try:
        start = time.perf_counter()
        res = subprocess.run(
            [str(at_binary), "test", temp_path],
            capture_output=True, text=True, timeout=timeout_s
        )
        elapsed_ms = (time.perf_counter() - start) * 1000
        passed = res.returncode == 0 and "fail" not in res.stdout.lower()
        return {
            "passed": passed,
            "elapsed_ms": round(elapsed_ms, 2),
            "stdout": res.stdout.strip(),
            "stderr": res.stderr.strip(),
            "returncode": res.returncode,
        }
    except subprocess.TimeoutExpired:
        return {
            "passed": False,
            "elapsed_ms": timeout_s * 1000,
            "stdout": "",
            "stderr": "TIMEOUT",
            "returncode": -1,
        }
    finally:
        os.unlink(temp_path)


def extract_code(text: str) -> str:
    """Extract code from markdown fences if present."""
    if "```" not in text:
        return text.strip()
    lines = text.split("\n")
    code_lines = []
    in_block = False
    for line in lines:
        if line.strip().startswith("```"):
            in_block = not in_block
            continue
        if in_block:
            code_lines.append(line)
    return "\n".join(code_lines).strip()


def load_skill_context() -> str:
    """Load skill.md for LLM context provisioning."""
    if SKILL_FILE.exists():
        return SKILL_FILE.read_text(encoding="utf-8")
    return ""


# ---------------------------------------------------------------------------
# Validate canonical (seed) solutions
# ---------------------------------------------------------------------------

def validate_canonical(at_binary: Path, problems: list[dict]) -> bool:
    """Validate that all hand-translated canonical solutions pass their tests."""
    print("=" * 70)
    print("  HumanEval-at: Validating Canonical Solutions")
    print("=" * 70)
    print(f"  at binary: {at_binary}")
    print(f"  Problems:  {len(problems)}")
    print()

    all_pass = True
    for prob in problems:
        code = prob["canonical_solution"] + "\n\n" + prob["test"]
        result = run_at_test(at_binary, code)
        status = "PASS" if result["passed"] else "FAIL"
        if not result["passed"]:
            all_pass = False
        print(f"  {prob['task_id']:.<40} [{status}]  {result['elapsed_ms']:.1f}ms")
        if not result["passed"]:
            print(f"    stdout: {result['stdout'][:200]}")
            print(f"    stderr: {result['stderr'][:200]}")

    print()
    if all_pass:
        print(f"  All {len(problems)} canonical solutions pass.")
    else:
        print(f"  WARNING: Some canonical solutions failed!")
    return all_pass


# ---------------------------------------------------------------------------
# LLM pass@1 evaluation (full HumanEval dataset)
# ---------------------------------------------------------------------------

def run_llm_evaluation(
    at_binary: Path,
    problems: list[dict],
    model: str,
    count: int | None = None,
    resume_file: Path | None = None,
):
    """
    Run pass@1 evaluation using an LLM to translate Python HumanEval problems
    into `at` code. Each problem's Python prompt, canonical solution, and test
    are given to the LLM which must produce equivalent `at` code.
    """
    if not _has_openai:
        print("Error: `openai` package required. Install with: pip install openai")
        sys.exit(1)

    client = openai.OpenAI()
    skill_context = load_skill_context()

    # Resume support
    completed_ids = set()
    if resume_file and resume_file.exists():
        with open(resume_file, "r") as f:
            for line in f:
                if line.strip():
                    entry = json.loads(line)
                    completed_ids.add(entry["task_id"])
        print(f"  Resuming: {len(completed_ids)} already completed")

    subset = problems[:count] if count else problems
    remaining = [p for p in subset if p["task_id"] not in completed_ids]

    results_dir = SCRIPT_DIR.parent / "results"
    results_dir.mkdir(exist_ok=True)

    run_id = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    if not resume_file:
        resume_file = results_dir / f"humaneval_{run_id}.jsonl"

    print("=" * 70)
    print("  HumanEval-at: Pass@1 Evaluation (Full Dataset)")
    print("=" * 70)
    print(f"  Model:     {model}")
    print(f"  at binary: {at_binary}")
    print(f"  Problems:  {len(remaining)} remaining / {len(subset)} total")
    print(f"  Results:   {resume_file}")
    print()

    passes = 0
    failures = 0
    errors = 0

    for prob in remaining:
        task_id = prob["task_id"]
        py_prompt = prob["prompt"]
        py_solution = prob.get("canonical_solution", "")
        py_test = prob.get("test", "")
        entry_point = prob.get("entry_point", "")

        system_msg = (
            "You are an expert translator from Python to the `at` programming language.\n"
            "You will be given a Python function signature with docstring, its canonical\n"
            "Python solution, and the Python test code. Your job is to produce a COMPLETE\n"
            "`at` file that:\n"
            "  1. Implements the equivalent function in `at` syntax\n"
            "  2. Includes `at` test blocks that verify the same behavior\n\n"
            "Rules:\n"
            "  - Output ONLY valid `at` code. No explanations, no markdown.\n"
            "  - Every function MUST have an explicit return type: fn name(...) -> type\n"
            "  - Use `set` for mutation (not `let mut`)\n"
            "  - Semicolons are required at end of statements\n"
            "  - Match arms separated by commas\n"
            "  - Tests use: test \"name\" { assert(...); }\n"
            "  - The function name must be: " + entry_point + "\n"
        )
        if skill_context:
            system_msg += f"\n--- at Language Reference ---\n{skill_context}\n"

        user_msg = (
            f"Translate this Python problem to `at`. Return a complete .at file.\n\n"
            f"--- Python prompt ---\n```python\n{py_prompt}\n```\n\n"
            f"--- Python solution ---\n```python\n{py_solution}\n```\n\n"
            f"--- Python tests ---\n```python\n{py_test}\n```\n"
        )

        try:
            response = client.chat.completions.create(
                model=model,
                messages=[
                    {"role": "system", "content": system_msg},
                    {"role": "user", "content": user_msg},
                ],
                temperature=0.0,
                max_tokens=2048,
            )
            generated = response.choices[0].message.content or ""
            generated = extract_code(generated)

            result = run_at_test(at_binary, generated)

            outcome = "pass" if result["passed"] else "fail"
            if result["passed"]:
                passes += 1
            else:
                failures += 1

            entry = {
                "task_id": task_id,
                "model": model,
                "outcome": outcome,
                "elapsed_ms": result["elapsed_ms"],
                "generated_tokens": count_tokens(generated),
                "python_tokens": count_tokens(py_prompt + py_solution),
                "generated": generated[:2000],
                "stdout": result["stdout"][:500],
                "stderr": result["stderr"][:500],
            }

        except Exception as e:
            errors += 1
            entry = {
                "task_id": task_id,
                "model": model,
                "outcome": "error",
                "error": str(e),
            }
            outcome = "error"

        # Write result
        with open(resume_file, "a") as f:
            f.write(json.dumps(entry) + "\n")

        status_char = "+" if outcome == "pass" else ("-" if outcome == "fail" else "!")
        print(f"  [{status_char}] {task_id}: {outcome}")

    # Summary
    total = passes + failures + errors + len(completed_ids)
    print()
    print("=" * 70)
    print("  RESULTS")
    print("=" * 70)
    print(f"  Total:   {total}")
    print(f"  Passed:  {passes}")
    print(f"  Failed:  {failures}")
    print(f"  Errors:  {errors}")
    if passes + failures > 0:
        pass_rate = passes / (passes + failures) * 100
        print(f"  Pass@1:  {pass_rate:.1f}%")

    # Write summary
    summary = {
        "model": model,
        "total": total,
        "passed": passes,
        "failed": failures,
        "errors": errors,
        "pass_at_1": round(passes / max(passes + failures, 1) * 100, 2),
        "timestamp": datetime.now(timezone.utc).isoformat(),
    }
    summary_file = resume_file.with_suffix(".json").with_name(
        resume_file.stem + "_summary.json"
    )
    with open(summary_file, "w") as f:
        json.dump(summary, f, indent=2)
    print(f"\n  Results: {resume_file}")
    print(f"  Summary: {summary_file}")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="HumanEval-at: Pass@1 evaluation for the at language"
    )
    parser.add_argument(
        "--validate", action="store_true",
        help="Validate hand-translated canonical solutions (no LLM needed)"
    )
    parser.add_argument(
        "--model", type=str, default=None,
        help="LLM model name for pass@1 evaluation on full HumanEval (164 problems)"
    )
    parser.add_argument(
        "--count", type=int, default=None,
        help="Limit evaluation to first N problems"
    )
    parser.add_argument(
        "--resume", type=str, default=None,
        help="Resume from an existing results JSONL file"
    )
    args = parser.parse_args()

    at_binary = find_at_binary()
    if not at_binary:
        print("Error: `at` binary not found. Build with: cargo build --release")
        sys.exit(1)

    if args.validate:
        problems = load_seed_problems()
        ok = validate_canonical(at_binary, problems)
        sys.exit(0 if ok else 1)

    if args.model:
        problems = load_full_humaneval()
        print(f"  Loaded {len(problems)} problems from HumanEval dataset")
        resume_path = Path(args.resume) if args.resume else None
        run_llm_evaluation(at_binary, problems, args.model, args.count, resume_path)
    else:
        print("Usage:")
        print("  Validate canonical: python3 run_humaneval.py --validate")
        print("  Run with LLM:      python3 run_humaneval.py --model <name>")
        print("  Run subset:         python3 run_humaneval.py --model <name> --count 20")
        sys.exit(1)


if __name__ == "__main__":
    main()
