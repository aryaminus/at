import urllib.request
import json
import os
import subprocess
import time
import argparse
import sys

try:
    import openai
except ImportError:
    print("Please install openai: pip install openai")
    sys.exit(1)

try:
    import tiktoken
    encoder = tiktoken.get_encoding("cl100k_base")
    def count_tokens(text):
        if not text: return 0
        return len(encoder.encode(text))
except ImportError:
    def count_tokens(text):
        if not text: return 0
        return int(len(text.split()) * 1.3)

DATASET_URL = "https://huggingface.co/datasets/tencent/AutoCodeBenchmark/resolve/main/autocodebench_lite.jsonl"
LOCAL_DATASET = "autocodebench_lite.jsonl"

def download_dataset(num_lines=10):
    if not os.path.exists(LOCAL_DATASET):
        print(f"Downloading AutoCodeBenchmark (saving first {num_lines} lines)...")
        req = urllib.request.Request(DATASET_URL, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req) as response, open(LOCAL_DATASET, 'w', encoding='utf-8') as out_file:
            lines_saved = 0
            for line in response:
                out_file.write(line.decode('utf-8'))
                lines_saved += 1
                if lines_saved >= num_lines:
                    break
    
    dataset = []
    with open(LOCAL_DATASET, 'r', encoding='utf-8') as f:
        for line in f:
            if line.strip():
                dataset.append(json.loads(line))
    return dataset

def setup_client(model_id):
    base_url = os.environ.get("OPENAI_BASE_URL", "http://localhost:11434/v1")
    api_key = os.environ.get("OPENAI_API_KEY", "ollama")
    return openai.OpenAI(base_url=base_url, api_key=api_key), model_id

def generate_at_solution_and_tests(client, model, problem, skill_context):
    question = problem.get("question", "")
    full_tests = problem.get("full_test_func", "")
    
    system_prompt = f"""You are an expert autonomous coder specializing in the `at` programming language.
You are given a problem description and test assertions written in another language.
Your job is to:
1. Write the solution in `at` language.
2. Translate the test assertions into native `at` `test "name" {{ ... }}` blocks beneath your solution.

DO NOT output markdown formatting (like ```at). Output purely executable code.

Here is the official `skill.md` guide for the `at` language. Follow its rules strictly!

====== `skill.md` ======
{skill_context}
======================"""

    user_prompt = f"### Problem Description\n{question}\n\n### Original Test Assertions To Translate\n{full_tests}"

    try:
        response = client.chat.completions.create(
            model=model,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ],
            temperature=0.0
        )
        code = response.choices[0].message.content
        if code.startswith("```"):
            lines = code.split("\n")
            if lines[0].startswith("```"):
                lines = lines[1:]
            if lines[-1].startswith("```"):
                lines = lines[:-1]
            code = "\n".join(lines)
            
        return code, count_tokens(code)
    except Exception as e:
        print(f"Error calling LLM: {e}")
        return "", 0

def execute_at_code(code, problem_id):
    if not os.path.exists("temp"):
        os.makedirs("temp")
    
    filepath = f"temp/prob_{problem_id}.at"
    with open(filepath, "w", encoding='utf-8') as f:
        f.write(code)
        
    start = time.perf_counter()
    # Assume at binary is compiled in release mode in the rust workspace root
    res = subprocess.run(["../target/release/at", "test", filepath], capture_output=True, text=True)
    duration = time.perf_counter() - start
    
    if res.returncode != 0:
        # Fallback to debug build if release isn't there
        res = subprocess.run(["../target/debug/at", "test", filepath], capture_output=True, text=True)
        duration = time.perf_counter() - start
        
    return res.returncode == 0, duration, res.stdout + res.stderr

def main():
    parser = argparse.ArgumentParser(description="End-to-End AutoCodeBench Eval for `at`")
    parser.add_argument("--models", type=str, default="glm-4.5", help="Comma-separated models")
    parser.add_argument("--count", type=int, default=3, help="Number of problems to evaluate")
    args = parser.parse_args()

    models = [m.strip() for m in args.models.split(",")]
    
    print("==================================================")
    print("  AutoCodeBenchmark -> `at` Evaluation Harness")
    print("==================================================\n")
    
    dataset = download_dataset(args.count)
    print(f"Loaded {len(dataset)} problems from AutoCodeBenchmark.\n")

    skill_md_path = "../skill.md"
    skill_context = ""
    if os.path.exists(skill_md_path):
        with open(skill_md_path, "r", encoding='utf-8') as f:
            skill_context = f.read()
    else:
        print("Warning: ../skill.md not found. Proceeding without explicit skills context.")

    # Initialize stats
    stats = {model: {"pass": 0, "at_tokens": 0, "canon_tokens": 0, "time": 0.0} for model in models}

    for idx, prob in enumerate(dataset):
        canon_solution = prob.get("canonical_solution", "")
        canon_lang = prob.get("language", "unknown")
        canon_tokens = count_tokens(canon_solution)
        
        print(f"\n--- Problem {idx+1}/{len(dataset)} ---")
        print(f"Canonical Language: {canon_lang}")
        print(f"Canonical Solution Tokens: {canon_tokens}")
        print("-" * 30)

        for model in models:
            client, model_id = setup_client(model)
            print(f"➤ Model: {model_id} generating `at`...")
            
            at_code, at_tokens = generate_at_solution_and_tests(client, model_id, prob, skill_context)
            if not at_code:
                continue
                
            is_pass, duration, out = execute_at_code(at_code, idx)
            
            if is_pass:
                print(f"  ✅ PASS! (Runtime: {duration*1000:.1f}ms, `at` Output Tokens: {at_tokens})")
                stats[model]["pass"] += 1
                stats[model]["at_tokens"] += at_tokens
                stats[model]["canon_tokens"] += canon_tokens
                stats[model]["time"] += duration
                
                savings = canon_tokens - at_tokens
                if savings > 0:
                    pct = (savings / canon_tokens) * 100
                    print(f"  🏆 `at` used {savings} FEWER tokens than the base {canon_lang} solution ({pct:.1f}% savings)!")
            else:
                print(f"  ❌ FAIL! The generated `at` code failed test assertions.")
                # print(out[:300]) # uncomment to debug

    print("\n==================================================")
    print(" Final Dynamic Evaluation Results")
    print("==================================================")
    for model in models:
        passed = stats[model]["pass"]
        if passed > 0:
            avg_at = stats[model]["at_tokens"] / passed
            avg_canon = stats[model]["canon_tokens"] / passed
            avg_time = stats[model]["time"] / passed * 1000
            print(f"Model: {model}")
            print(f"  Pass@1:     {passed}/{len(dataset)} ({(passed/len(dataset))*100:.1f}%)")
            print(f"  Avg AT Toks: {avg_at:.1f}")
            print(f"  Avg Original Toks ({canon_lang} etc): {avg_canon:.1f}")
            pct = ((avg_canon - avg_at) / avg_canon) * 100 if avg_canon > 0 else 0
            print(f"  Result: `at` token savings = {pct:.1f}%")
            print(f"  Avg Test Runtime: {avg_time:.1f}ms")
        else:
            print(f"Model: {model} - 0 tests passed.")

if __name__ == "__main__":
    main()
