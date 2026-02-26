#!/usr/bin/env python3
"""
AutoCodeBenchmark Evaluation Harness for the `at` programming language.

Downloads problems from HuggingFace's tencent/AutoCodeBenchmark dataset,
uses an LLM to translate solutions + tests into `at`, then runs `at test`
to validate correctness. Measures pass@1 and token savings vs. canonical solutions.

Best practices modeled after HumanEval (OpenAI) and SWE-bench (Princeton NLP):
  - Full dataset cached locally, subset selected at runtime (not on download)
  - Per-instance JSONL result logging with resume support
  - Execution timeout to prevent hangs
  - Structured outcome categorization (pass / fail / timeout / error)
  - Aggregate summary written to both stdout and JSON file
"""

import urllib.request
import json
import os
import subprocess
import time
import argparse
import sys
import hashlib
import shutil
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
        if not text:
            return 0
        return len(_encoder.encode(text))
except ImportError:
    def count_tokens(text: str) -> int:
        """Rough estimate: ~1.3 tokens per whitespace-delimited word."""
        if not text:
            return 0
        return int(len(text.split()) * 1.3)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DATASET_URL = (
    "https://huggingface.co/datasets/tencent/AutoCodeBenchmark"
    "/resolve/main/autocodebench_lite.jsonl"
)
DATASET_FILENAME = "autocodebench_lite.jsonl"

REQUIRED_FIELDS = {"question", "canonical_solution", "demo_test_func",
                   "full_test_func", "language", "difficulty"}

# Timeout (seconds) for a single `at test` execution
DEFAULT_EXEC_TIMEOUT = 30

# Outcome categories (following HumanEval conventions)
OUTCOME_PASS = "passed"
OUTCOME_FAIL = "failed"
OUTCOME_TIMEOUT = "timed out"
OUTCOME_ERROR = "error"


# ---------------------------------------------------------------------------
# Paths — resolved relative to this script, not the working directory
# ---------------------------------------------------------------------------

def resolve_paths():
    """Return key paths relative to the script's location."""
    script_dir = Path(__file__).resolve().parent
    project_root = script_dir.parent

    return {
        "script_dir": script_dir,
        "project_root": project_root,
        "dataset": script_dir / DATASET_FILENAME,
        "skill_md": project_root / "skill.md",
        "at_release": project_root / "target" / "release" / "at",
        "at_debug": project_root / "target" / "debug" / "at",
        "temp_dir": script_dir / "temp",
        "results_dir": script_dir / "results",
    }


# ---------------------------------------------------------------------------
# Dataset management
# ---------------------------------------------------------------------------

def download_dataset(dest: Path) -> None:
    """Download the full dataset JSONL from HuggingFace (once)."""
    if dest.exists():
        return

    print(f"Downloading {DATASET_FILENAME} from HuggingFace...")
    req = urllib.request.Request(DATASET_URL, headers={"User-Agent": "at-bench/1.0"})
    try:
        with urllib.request.urlopen(req, timeout=120) as resp:
            with open(dest, "wb") as f:
                shutil.copyfileobj(resp, f)
        print(f"Saved to {dest} ({dest.stat().st_size / 1024 / 1024:.1f} MB)")
    except Exception as e:
        # Clean up partial download
        if dest.exists():
            dest.unlink()
        print(f"Error downloading dataset: {e}")
        sys.exit(1)


def load_dataset(path: Path, count: int | None = None,
                 difficulty: str | None = None,
                 language: str | None = None) -> list[dict]:
    """Load and optionally filter the dataset."""
    problems = []
    with open(path, "r", encoding="utf-8") as f:
        for lineno, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            try:
                record = json.loads(line)
            except json.JSONDecodeError as e:
                print(f"Warning: skipping malformed line {lineno}: {e}")
                continue

            # Validate schema
            missing = REQUIRED_FIELDS - set(record.keys())
            if missing:
                print(f"Warning: line {lineno} missing fields {missing}, skipping")
                continue

            # Apply filters
            if difficulty and record.get("difficulty") != difficulty:
                continue
            if language and record.get("language") != language:
                continue

            problems.append(record)

    # Subsample
    if count is not None and count < len(problems):
        problems = problems[:count]

    return problems


# ---------------------------------------------------------------------------
# LLM interaction
# ---------------------------------------------------------------------------

def setup_client(model_id: str):
    """Create an OpenAI-compatible client."""
    if not _has_openai:
        print("Error: openai package required. Install with: pip install openai")
        sys.exit(1)
    base_url = os.environ.get("OPENAI_BASE_URL", "http://localhost:11434/v1")
    api_key = os.environ.get("OPENAI_API_KEY", "ollama")
    client = openai.OpenAI(base_url=base_url, api_key=api_key)
    return client, model_id


def generate_at_code(client, model: str, problem: dict,
                     skill_context: str) -> tuple[str, int]:
    """Ask the LLM to translate a problem + tests into `at` code."""
    question = problem.get("question", "")
    full_tests = problem.get("full_test_func", "")

    system_prompt = (
        "You are an expert autonomous coder specializing in the `at` programming language.\n"
        "You are given a problem description and test assertions written in another language.\n"
        "Your job is to:\n"
        "1. Write the solution in `at` language.\n"
        "2. Translate the test assertions into native `at` `test \"name\" { ... }` blocks beneath your solution.\n\n"
        "DO NOT output markdown formatting (like ```at). Output purely executable `at` code.\n\n"
        "Here is the official skill.md guide for the `at` language. Follow its rules strictly!\n\n"
        f"====== skill.md ======\n{skill_context}\n======================"
    )

    user_prompt = (
        f"### Problem Description\n{question}\n\n"
        f"### Original Test Assertions To Translate\n{full_tests}"
    )

    try:
        response = client.chat.completions.create(
            model=model,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            temperature=0.0,
        )
        code = response.choices[0].message.content or ""

        # Strip markdown fences if the LLM added them despite instructions
        if code.startswith("```"):
            lines = code.split("\n")
            if lines[0].startswith("```"):
                lines = lines[1:]
            if lines and lines[-1].strip().startswith("```"):
                lines = lines[:-1]
            code = "\n".join(lines)

        return code, count_tokens(code)
    except Exception as e:
        print(f"  LLM error: {e}")
        return "", 0


# ---------------------------------------------------------------------------
# Execution
# ---------------------------------------------------------------------------

def find_at_binary(paths: dict) -> Path | None:
    """Find the `at` binary, preferring release over debug."""
    if paths["at_release"].exists():
        return paths["at_release"]
    if paths["at_debug"].exists():
        return paths["at_debug"]
    # Try PATH as last resort
    which = shutil.which("at")
    if which:
        return Path(which)
    return None


def execute_at_code(code: str, problem_id: int, at_binary: Path,
                    temp_dir: Path, timeout: int) -> tuple[str, float, str]:
    """
    Write code to a temp file and run `at test` on it.
    Returns (outcome, duration_secs, output_text).
    """
    temp_dir.mkdir(parents=True, exist_ok=True)
    filepath = temp_dir / f"prob_{problem_id}.at"
    filepath.write_text(code, encoding="utf-8")

    start = time.perf_counter()
    try:
        res = subprocess.run(
            [str(at_binary), "test", str(filepath)],
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        duration = time.perf_counter() - start
        output = (res.stdout or "") + (res.stderr or "")

        if res.returncode == 0:
            return OUTCOME_PASS, duration, output
        else:
            return OUTCOME_FAIL, duration, output

    except subprocess.TimeoutExpired:
        duration = time.perf_counter() - start
        return OUTCOME_TIMEOUT, duration, f"Timed out after {timeout}s"

    except Exception as e:
        duration = time.perf_counter() - start
        return OUTCOME_ERROR, duration, str(e)


# ---------------------------------------------------------------------------
# Results I/O
# ---------------------------------------------------------------------------

def make_run_id(models: list[str], count: int) -> str:
    """Generate a unique run identifier."""
    ts = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    model_slug = "_".join(m.replace("/", "-") for m in models)
    return f"{model_slug}_{count}_{ts}"


def load_completed_results(results_file: Path) -> dict[str, dict]:
    """Load already-completed results for resume support. Key: 'model::idx'."""
    completed = {}
    if not results_file.exists():
        return completed
    with open(results_file, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                record = json.loads(line)
                key = f"{record['model']}::{record['problem_idx']}"
                completed[key] = record
            except (json.JSONDecodeError, KeyError):
                continue
    return completed


def append_result(results_file: Path, record: dict) -> None:
    """Append a single result record to the JSONL file."""
    results_file.parent.mkdir(parents=True, exist_ok=True)
    with open(results_file, "a", encoding="utf-8") as f:
        f.write(json.dumps(record, ensure_ascii=False) + "\n")


def write_summary(summary_file: Path, summary: dict) -> None:
    """Write the aggregate summary as pretty-printed JSON."""
    summary_file.parent.mkdir(parents=True, exist_ok=True)
    with open(summary_file, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="AutoCodeBenchmark evaluation harness for the `at` language"
    )
    parser.add_argument(
        "--models", type=str, default="glm-4.5",
        help="Comma-separated list of model IDs (default: glm-4.5)"
    )
    parser.add_argument(
        "--count", type=int, default=10,
        help="Number of problems to evaluate (default: 10)"
    )
    parser.add_argument(
        "--difficulty", type=str, default=None,
        choices=["easy", "medium", "hard"],
        help="Filter problems by difficulty"
    )
    parser.add_argument(
        "--language", type=str, default=None,
        help="Filter problems by canonical language (e.g. python, cpp)"
    )
    parser.add_argument(
        "--timeout", type=int, default=DEFAULT_EXEC_TIMEOUT,
        help=f"Per-problem execution timeout in seconds (default: {DEFAULT_EXEC_TIMEOUT})"
    )
    parser.add_argument(
        "--resume", action="store_true",
        help="Resume from existing results file (skip completed problems)"
    )
    parser.add_argument(
        "--run-id", type=str, default=None,
        help="Explicit run ID (for resume). Auto-generated if not provided."
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Validate setup without calling the LLM or running code"
    )
    parser.add_argument(
        "--clean", action="store_true",
        help="Remove temp files after each problem"
    )
    args = parser.parse_args()

    paths = resolve_paths()
    models = [m.strip() for m in args.models.split(",")]

    # -- Header --
    print("=" * 60)
    print("  AutoCodeBenchmark -> `at` Evaluation Harness")
    print("=" * 60)

    # -- Find at binary --
    at_binary = find_at_binary(paths)
    if at_binary is None:
        print("\nError: Could not find the `at` binary.")
        print("Build it with: cargo build --release")
        sys.exit(1)
    print(f"\nat binary: {at_binary}")

    # -- Download & load dataset --
    download_dataset(paths["dataset"])
    dataset = load_dataset(
        paths["dataset"],
        count=args.count,
        difficulty=args.difficulty,
        language=args.language,
    )
    if not dataset:
        print("Error: No problems matched the given filters.")
        sys.exit(1)
    print(f"Selected {len(dataset)} problems", end="")
    if args.difficulty:
        print(f" [difficulty={args.difficulty}]", end="")
    if args.language:
        print(f" [language={args.language}]", end="")
    print()

    # -- Load skill.md --
    skill_context = ""
    if paths["skill_md"].exists():
        skill_context = paths["skill_md"].read_text(encoding="utf-8")
        print(f"Loaded skill.md ({len(skill_context)} chars)")
    else:
        print(f"Warning: {paths['skill_md']} not found. Proceeding without skill context.")

    # -- Dry run check --
    if args.dry_run:
        print("\n[dry-run] Setup validated successfully. Exiting.")
        print(f"  Dataset: {paths['dataset']} ({paths['dataset'].stat().st_size / 1024 / 1024:.1f} MB)")
        print(f"  Problems: {len(dataset)}")
        print(f"  Models: {models}")
        print(f"  Timeout: {args.timeout}s")
        print(f"  at binary: {at_binary}")
        if skill_context:
            print(f"  skill.md: loaded")
        # Show a sample problem
        sample = dataset[0]
        print(f"\n  Sample problem [0]:")
        print(f"    language:   {sample['language']}")
        print(f"    difficulty: {sample['difficulty']}")
        print(f"    question:   {sample['question'][:120]}...")
        sys.exit(0)

    # -- Setup results --
    run_id = args.run_id or make_run_id(models, len(dataset))
    results_file = paths["results_dir"] / f"{run_id}.jsonl"
    summary_file = paths["results_dir"] / f"{run_id}_summary.json"

    completed = {}
    if args.resume:
        completed = load_completed_results(results_file)
        if completed:
            print(f"Resuming: {len(completed)} results already completed")

    print(f"Run ID: {run_id}")
    print(f"Results: {results_file}")
    print()

    # -- Initialize per-model stats --
    stats = {
        model: {
            "passed": 0, "failed": 0, "timed_out": 0, "errors": 0,
            "at_tokens_total": 0, "canon_tokens_total": 0, "time_total": 0.0,
        }
        for model in models
    }

    # -- Evaluate --
    total_tasks = len(dataset) * len(models)
    task_num = 0

    for idx, prob in enumerate(dataset):
        canon_lang = prob["language"]
        canon_tokens = count_tokens(prob["canonical_solution"])
        difficulty = prob["difficulty"]

        print(f"\n--- Problem {idx + 1}/{len(dataset)} [{difficulty}, {canon_lang}] ---")
        print(f"  Canonical tokens: {canon_tokens}")

        for model in models:
            task_num += 1
            resume_key = f"{model}::{idx}"

            # Skip if already completed
            if resume_key in completed:
                prev = completed[resume_key]
                print(f"  [{model}] Skipped (already {prev.get('outcome', '?')})")
                # Replay stats
                if prev.get("outcome") == OUTCOME_PASS:
                    stats[model]["passed"] += 1
                    stats[model]["at_tokens_total"] += prev.get("at_tokens", 0)
                    stats[model]["canon_tokens_total"] += prev.get("canon_tokens", 0)
                    stats[model]["time_total"] += prev.get("duration", 0)
                elif prev.get("outcome") == OUTCOME_TIMEOUT:
                    stats[model]["timed_out"] += 1
                elif prev.get("outcome") == OUTCOME_ERROR:
                    stats[model]["errors"] += 1
                else:
                    stats[model]["failed"] += 1
                continue

            progress = f"[{task_num}/{total_tasks}]"
            print(f"  {progress} {model}: generating `at` code...")

            client, model_id = setup_client(model)
            at_code, at_tokens = generate_at_code(
                client, model_id, prob, skill_context
            )

            if not at_code:
                record = {
                    "run_id": run_id,
                    "problem_idx": idx,
                    "model": model,
                    "language": canon_lang,
                    "difficulty": difficulty,
                    "outcome": OUTCOME_ERROR,
                    "at_tokens": 0,
                    "canon_tokens": canon_tokens,
                    "duration": 0,
                    "output": "LLM returned empty response",
                    "timestamp": datetime.now(timezone.utc).isoformat(),
                }
                append_result(results_file, record)
                stats[model]["errors"] += 1
                print(f"  {progress} {model}: ERROR (empty LLM response)")
                continue

            outcome, duration, output = execute_at_code(
                at_code, idx, at_binary, paths["temp_dir"], args.timeout
            )

            record = {
                "run_id": run_id,
                "problem_idx": idx,
                "model": model,
                "language": canon_lang,
                "difficulty": difficulty,
                "outcome": outcome,
                "at_tokens": at_tokens,
                "canon_tokens": canon_tokens,
                "duration": round(duration, 4),
                "output": output[:2000],  # Truncate large outputs
                "timestamp": datetime.now(timezone.utc).isoformat(),
            }
            append_result(results_file, record)

            if outcome == OUTCOME_PASS:
                stats[model]["passed"] += 1
                stats[model]["at_tokens_total"] += at_tokens
                stats[model]["canon_tokens_total"] += canon_tokens
                stats[model]["time_total"] += duration
                savings = canon_tokens - at_tokens
                pct = (savings / canon_tokens * 100) if canon_tokens > 0 else 0
                print(f"  {progress} {model}: PASS ({duration * 1000:.1f}ms, {at_tokens} tokens, {pct:+.1f}% savings)")
            elif outcome == OUTCOME_TIMEOUT:
                stats[model]["timed_out"] += 1
                print(f"  {progress} {model}: TIMEOUT (>{args.timeout}s)")
            elif outcome == OUTCOME_FAIL:
                stats[model]["failed"] += 1
                print(f"  {progress} {model}: FAIL")
            else:
                stats[model]["errors"] += 1
                print(f"  {progress} {model}: ERROR ({output[:100]})")

            # Optionally clean temp files
            if args.clean:
                temp_file = paths["temp_dir"] / f"prob_{idx}.at"
                if temp_file.exists():
                    temp_file.unlink()

    # -- Summary --
    print("\n" + "=" * 60)
    print("  Final Results")
    print("=" * 60)

    summary = {
        "schema_version": 1,
        "run_id": run_id,
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "dataset": DATASET_FILENAME,
        "total_problems": len(dataset),
        "filters": {
            "difficulty": args.difficulty,
            "language": args.language,
        },
        "timeout_seconds": args.timeout,
        "models": {},
    }

    for model in models:
        s = stats[model]
        total = len(dataset)
        passed = s["passed"]

        model_summary = {
            "total": total,
            "passed": passed,
            "failed": s["failed"],
            "timed_out": s["timed_out"],
            "errors": s["errors"],
            "pass_at_1": round(passed / total, 4) if total > 0 else 0,
        }

        print(f"\nModel: {model}")
        print(f"  Pass@1: {passed}/{total} ({model_summary['pass_at_1'] * 100:.1f}%)")
        print(f"  Failed: {s['failed']}  Timed out: {s['timed_out']}  Errors: {s['errors']}")

        if passed > 0:
            avg_at = s["at_tokens_total"] / passed
            avg_canon = s["canon_tokens_total"] / passed
            avg_time = s["time_total"] / passed * 1000
            token_savings = ((avg_canon - avg_at) / avg_canon * 100) if avg_canon > 0 else 0

            model_summary["avg_at_tokens"] = round(avg_at, 1)
            model_summary["avg_canon_tokens"] = round(avg_canon, 1)
            model_summary["avg_runtime_ms"] = round(avg_time, 1)
            model_summary["token_savings_pct"] = round(token_savings, 1)

            print(f"  Avg `at` tokens:    {avg_at:.1f}")
            print(f"  Avg canonical tokens: {avg_canon:.1f}")
            print(f"  Token savings:      {token_savings:+.1f}%")
            print(f"  Avg runtime:        {avg_time:.1f}ms")

        summary["models"][model] = model_summary

    # Write summary file
    write_summary(summary_file, summary)
    print(f"\nResults:  {results_file}")
    print(f"Summary:  {summary_file}")


if __name__ == "__main__":
    main()
