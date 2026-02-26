# Benchmark Suite for `at`

Empirical benchmarks proving `at` is the optimal language for AI agent code generation. The suite measures three key claims without requiring any LLM, plus an LLM-based evaluation using the AutoCodeBenchmark dataset.

## Benchmarks

### 1. Token Efficiency (`token_efficiency/`)

**No LLM required.** Compares hand-written equivalent solutions across `at`, Python, JavaScript, Go, and Java on 11 identical problems. Measures token count using tiktoken.

```bash
python3 token_efficiency/run_token_bench.py --validate
```

Results: `at` achieves comparable token counts to Python/JS while providing strict types, deterministic execution, and native error handling (Result/Option). Java and Go consistently require more tokens.

### 2. Execution Speed (`execution_speed/`)

**No LLM required.** Measures wall-clock time for `at test` vs Python and Node.js on identical test suites. Demonstrates the fast feedback loop critical for agent-driven development.

```bash
python3 execution_speed/run_speed_bench.py --runs 5
```

Results: `at test` completes in ~3-4ms per problem. Python takes ~22ms (6x slower). Node.js takes ~56ms (16x slower). This means agents iterating with `at` get feedback 6-16x faster.

### 3. Determinism (`determinism/`)

**No LLM required.** Runs each test suite N times and verifies zero flaky failures. Proves `at`'s deterministic VM produces reliable feedback for agent reflexion loops.

```bash
python3 determinism/run_determinism_bench.py --runs 100
```

Results: 0% flakiness rate across 1,100 executions. Every test produces identical results on every run.

### 4. AutoCodeBenchmark (`run_autocodebench.py`)

**Requires LLM + openai package.** Downloads problems from [Tencent AutoCodeBenchmark](https://huggingface.co/datasets/tencent/AutoCodeBenchmark) (1,590 problems, 20 languages), uses an LLM to translate solutions into `at`, and measures Pass@1 and token savings.

```bash
pip install openai tiktoken
export OPENAI_BASE_URL="http://localhost:11434/v1"
export OPENAI_API_KEY="ollama"
python3 run_autocodebench.py --models "glm-4.5" --count 10
```

## Unified Runner

Run all non-LLM benchmarks at once:

```bash
python3 run_all.py --validate --json
```

Include LLM benchmarks:

```bash
python3 run_all.py --include-llm --json
```

## Problem Set

The `token_efficiency/problems.jsonl` file contains 11 hand-written problems with verified solutions in all 5 languages:

| Problem | Category | Key Feature Tested |
|---------|----------|-------------------|
| FizzBuzz | Loops | String building, conditionals |
| Two Sum | Search | Option types, nested loops |
| Palindrome Check | Strings | Character access, early return |
| Maximum Subarray | Algorithms | If-expressions as values |
| Binary Search | Algorithms | Option return, mutation |
| Functional Pipeline | HOF | map/filter/reduce, closures |
| Error Handling | Safety | Result types, ? operator |
| Fibonacci | Math | Iterative computation |
| Word Frequency | Maps | Map operations, string processing |
| Matrix Multiplication | Math | Nested arrays, triple loop |
| Pattern Matching | Types | Enums, structs, match expressions |

All `at` solutions are verified passing with `at test` before being included.

## Output

Results are written to the `results/` directory as JSON files:

- `results/token_efficiency.json` -- Token counts per language per problem
- `results/execution_speed.json` -- Timing data per language per problem
- `results/determinism.json` -- Pass/fail counts across N runs
- `results/<run_id>.jsonl` -- Per-instance AutoCodeBench results
- `results/<run_id>_summary.json` -- Aggregate AutoCodeBench metrics

## Methodology

The benchmarks follow best practices from established evaluation frameworks:

- **HumanEval** (OpenAI): JSONL format, pass@k metrics, per-instance result logging
- **SWE-bench** (Princeton NLP): Resume support, structured outcomes, aggregate summaries
- **SkillsBench** (arXiv 2602.12670): Context provisioning via `skill.md`, validating that curated procedural knowledge raises pass rates
