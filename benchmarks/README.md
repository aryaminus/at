# Multi-Language LLM Code Generation Benchmark

This benchmark suite evaluates the code generation capabilities of Large Language Models across multiple programming languages, specifically targeting a comparative analysis between **Python**, **C++**, and **`at`**.

It is modeled after multi-language benchmarks like AutoCodeBenchmark and HumanEval, but introduces an operational pipeline to evaluate novel languages.

## Goal

To empirically measure two things:

1. **Pass@1 Rate**: The primary metric measuring functional correctness on the first attempt.
2. **Token Efficiency**: The average number of output tokens expended by the LLM to successfully solve the problem.

This suite is built to prove the hypothesis that `at`—when provided with its `skill.md` procedural context—results in higher Pass@1 rates and lower token expenditure than mainstream languages due to strict functional constraints and lack of boilerplate.

---

## Directory Structure

- `dataset.json`: The benchmark problem set. Each problem includes a `prompt` and hidden unit tests for `at`, `python`, and `cpp`.
- `run_eval.py`: The evaluation harness. It connects to any OpenAI-compatible API to generate inference and executes the tests dynamically.
- `temp/`: A transient directory where generated solutions are buffered, compiled, and executed.

---

## How to Run

1. **Install Dependencies**

   ```bash
   pip install openai tiktoken
   ```

2. **Set your API Keys**
   The harness uses the standard OpenAI python SDK, meaning it can connect to OpenAI, Anthropic (via wrappers), Groq, OpenRouter, or local servers like Ollama.

   *Example: Local Ollama*

   ```bash
   export OPENAI_BASE_URL="http://localhost:11434/v1"
   export OPENAI_API_KEY="ollama"
   ```

   *Example: OpenAI*

   ```bash
   export OPENAI_BASE_URL="https://api.openai.com/v1"
   export OPENAI_API_KEY="sk-..."
   ```

3. **Execute the Benchmark**
   Run the evaluation script. You can optionally specify a comma-separated list of models and languages.

   ```bash
   python3 run_eval.py --models "gpt-4o,llama3.2" --langs "at,python,cpp"
   ```

## Extending the Dataset

To add more problems (e.g., from AutoCodeBenchmark or HumanEval), simply append to `dataset.json`. Ensure you provide testing logic tailored to the native assertions of each language:

- **`at`**: `test "name" { assert(...); }`
- **`python`**: `if __name__ == "__main__": assert(...)`
- **`cpp`**: `int main() { assert(...); return 0; }`

By continuously expanding the dataset, we can rigorously track the zero-shot capabilities of frontier models in adapting to the `at` language!
