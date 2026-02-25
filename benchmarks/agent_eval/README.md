# LLM Benchmarking Harness for `at`

This is a fully automated evaluation pipeline designed to benchmark how well Large Language Models can write code in the `at` programming language. It is modeled after the evaluation methodologies used by **AutoCodeBenchmark**, **HumanEval**, and **SkillsBench**.

## How it Works

1. **Dataset Loading**: It loads a dataset of algorithmic programming prompts (e.g., adapted from AutoCodeBenchmark) along with hidden test cases (`dataset.json`).
2. **Context Provisioning**: Following the findings of **SkillsBench**, it prepends the agent's system prompt with the `skill.md` procedural guidelines to strictly enforce the grammatical structure of `at`.
3. **Generation**: It invokes the LLM (via an OpenAI-compatible API) to generate the `at` code solution.
4. **Execution & V&V**: It dynamically writes the LLM's output to a temporary `.at` file, appends the hidden assertions, and executes it via the native `at test` VM.
5. **Metrics**: It calculates the **Pass@1** rate and models **Token Efficiency** (using `tiktoken`) to prove that `at` requires fewer tokens per successful task.

## Running the Eval

You will need the `openai` and `tiktoken` python packages installed:

```bash
pip install openai tiktoken
```

By default, the script defaults to a local instance of `ollama` (useful for evaluating models like `llama3.2` or `qwen2.5-coder` locally).

```bash
cd benchmarks/agent_eval
python3 eval_harness.py
```

### Using External APIs (OpenAI / Groq / OpenRouter)

Because it uses the standard OpenAI python client, you can point it to any compatible provider.

**OpenAI (GPT-4o)**

```bash
export OPENAI_BASE_URL="https://api.openai.com/v1"
export OPENAI_API_KEY="sk-..."
export EVAL_MODEL="gpt-4o"
python3 eval_harness.py
```

**Groq (Llama 3.3 70B)**

```bash
export OPENAI_BASE_URL="https://api.groq.com/openai/v1"
export OPENAI_API_KEY="gsk_..."
export EVAL_MODEL="llama-3.3-70b-versatile"
python3 eval_harness.py
```

### Scaling Up

To run the full **AutoCodeBenchmark** suite:

1. Download `autocodebench_lite.jsonl` from HuggingFace.
2. Translate the C++ `full_test_func` payloads into `at` assertions.
3. Add them to `dataset.json` and run the harness.
