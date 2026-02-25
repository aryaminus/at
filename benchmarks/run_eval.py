import os
import sys
import json
import subprocess
import time
import argparse

try:
    import openai
except ImportError:
    print("Please install openai: pip install openai")
    sys.exit(1)

try:
    import tiktoken
    encoder = tiktoken.get_encoding("cl100k_base")
    def count_tokens(text):
        return len(encoder.encode(text))
except ImportError:
    def count_tokens(text):
        return int(len(text.split()) * 1.3)

def setup_client(model_id):
    base_url = os.environ.get("OPENAI_BASE_URL", "http://localhost:11434/v1")
    api_key = os.environ.get("OPENAI_API_KEY", "ollama")
    return openai.OpenAI(base_url=base_url, api_key=api_key), model_id

def read_file(path):
    if not os.path.exists(path):
        return ""
    with open(path, "r") as f:
        return f.read()

def generate_code(client, model, prompt, language, skill_context=""):
    system_prompt = f"You are an expert coding assistant. Generate the optimal, concise solution in {language} for the given prompt."
    
    if language == "at":
        system_prompt += f"""
Here is the official `skill.md` guide for the `at` language. Follow its rules strictly!

====== `skill.md` ======
{skill_context}
======================
"""

    system_prompt += "\nOutput ONLY valid code. No markdown formatting (like ```) or explanations."

    try:
        response = client.chat.completions.create(
            model=model,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": prompt}
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

def execute_code(language, code, tests, problem_id):
    combined = code + "\n\n" + tests
    
    if language == "at":
        filepath = f"temp/{problem_id}.at"
        with open(filepath, "w") as f: f.write(combined)
        cmd = ["../target/debug/at", "test", filepath]
    
    elif language == "python":
        filepath = f"temp/{problem_id}.py"
        with open(filepath, "w") as f: f.write(combined)
        cmd = ["python3", filepath]
        
    elif language == "cpp":
        filepath = f"temp/{problem_id}.cpp"
        # wrap cpp code if necessary? We assume tests contain main() and code is just functions.
        combined = "#include <cmath>\n#include <vector>\n" + code + "\n\n" + tests
        with open(filepath, "w") as f: f.write(combined)
        out_bin = f"temp/{problem_id}.out"
        compile_res = subprocess.run(["g++", "-O3", "-std=c++17", filepath, "-o", out_bin], capture_output=True, text=True)
        if compile_res.returncode != 0:
            return False, 0, compile_res.stderr
        cmd = [f"./{out_bin}"]
    else:
        return False, 0, "Unsupported language"

    start = time.perf_counter()
    res = subprocess.run(cmd, capture_output=True, text=True)
    duration = time.perf_counter() - start
    
    return res.returncode == 0, duration, res.stdout + res.stderr

def main():
    parser = argparse.ArgumentParser(description="Multi-Language LLM Benchmark")
    parser.add_argument("--models", type=str, default="llama3.2", help="Comma-separated list of models to evaluate")
    parser.add_argument("--langs", type=str, default="at,python,cpp", help="Comma-separated list of languages to evaluate")
    args = parser.parse_args()

    models = [m.strip() for m in args.models.split(",")]
    languages = [l.strip() for l in args.langs.split(",")]
    
    print("===========================================")
    print(" Cross-Language AutoCodeBench Evaluation")
    print("===========================================\n")
    
    skill_context = read_file("../skill.md")

    with open("dataset.json", "r") as f:
        dataset = json.load(f)

    if not os.path.exists("temp"):
        os.makedirs("temp")

    results = {}

    for model in models:
        print(f"\nEvaluating Model: {model}")
        client, model_id = setup_client(model)
        results[model] = {}
        
        for lang in languages:
            print(f"  Target Language: {lang}")
            
            passed = 0
            total_tokens = 0
            total_time = 0
            
            for item in dataset:
                gen_code, tokens = generate_code(client, model_id, item['prompt'], lang, skill_context)
                if not gen_code: continue
                
                total_tokens += tokens
                
                tests = item.get(f'tests_{lang}', "")
                if not tests:
                    print(f"Skipping {item['id']} for {lang}, no tests found.")
                    continue
                    
                is_pass, duration, output = execute_code(lang, gen_code, tests, item['id'])
                
                if is_pass:
                    passed += 1
                    total_time += duration
                    print(f"    - {item['id']}: ✅ PASS ({tokens} tokens, {duration*1000:.1f}ms)")
                else:
                    print(f"    - {item['id']}: ❌ FAIL ({tokens} tokens)")
                    # print(output[:200]) # Comment out for cleaner output
            
            results[model][lang] = {
                "pass_rate": passed / len(dataset),
                "avg_tokens": total_tokens / len(dataset),
                "avg_exec_ms": (total_time / passed * 1000) if passed > 0 else 0
            }

    print("\n\n===========================================")
    print(" Final Benchmark Report")
    print("===========================================")
    
    for model in models:
        print(f"\nModel: {model}")
        print(f"{'Language':<10} | {'Pass Rate':<10} | {'Avg Output Tokens':<18} | {'Avg Exec Time':<15}")
        print("-" * 65)
        for lang in languages:
            res = results[model][lang]
            print(f"{lang:<10} | {res['pass_rate']*100:>8.1f}% | {res['avg_tokens']:>17.1f} | {res['avg_exec_ms']:>12.1f}ms")

if __name__ == "__main__":
    main()
