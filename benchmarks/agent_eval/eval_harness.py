import os
import json
import subprocess
import time

try:
    import openai
except ImportError:
    print("Please install the openai python package: pip install openai")
    exit(1)

try:
    import tiktoken
    encoder = tiktoken.get_encoding("cl100k_base")
    def count_tokens(text):
        return len(encoder.encode(text))
except ImportError:
    def count_tokens(text):
        return len(text.split()) * 1.3

def setup_client():
    # Supports Ollama out of the box (e.g., http://localhost:11434/v1)
    # Allows falling back to actual OpenAI/Groq/Gemini if env vars are set
    base_url = os.environ.get("OPENAI_BASE_URL", "http://localhost:11434/v1")
    api_key = os.environ.get("OPENAI_API_KEY", "ollama")
    model = os.environ.get("EVAL_MODEL", "llama3.2")
    
    return openai.OpenAI(base_url=base_url, api_key=api_key), model

def get_skill_md():
    with open("../../skill.md", "r") as f:
        return f.read()

def generate_at_code(client, model, skill_context, prompt):
    system_prompt = f"""You are an autonomous AI coding agent writing code in the `at` programming language.
You MUST write the solution to the user's prompt in valid `at` language.
Here is the official `skill.md` guide for the `at` language. Follow its rules strictly!

====== `skill.md` ======
{skill_context}
======================

Output ONLY the code required to solve the problem. Do not output markdown code blocks (like ```) or explanations. Just valid `at` code."""

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
        # clean up any markdown if it hallucinated it
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

def run_at_test(filepath):
    # Ensure compiling in debug so it's faster to iterate, or release if built
    res = subprocess.run(["../../target/debug/at", "test", filepath], capture_output=True, text=True, cwd=os.path.dirname(filepath))
    return res.returncode == 0, res.stdout + res.stderr

def main():
    print("===========================================")
    print(" LLM Benchmark Harness for `at` Language")
    print("===========================================\n")
    
    client, model = setup_client()
    print(f"Using Model: {model} at {client.base_url}")
    
    try:
        skill_context = get_skill_md()
    except FileNotFoundError:
        print("Could not find skill.md. Run from the benchmarks/agent_eval directory.")
        return

    with open("dataset.json", "r") as f:
        dataset = json.load(f)

    if not os.path.exists("temp"):
        os.makedirs("temp")

    total_problems = len(dataset)
    passed = 0
    total_tokens = 0

    for item in dataset:
        print(f"\n[Problem: {item['id']}]")
        print("Generating `at` code...")
        
        start_time = time.perf_counter()
        code, tokens = generate_at_code(client, model, skill_context, item['prompt'])
        gen_time = time.perf_counter() - start_time
        
        if not code:
            print("❌ Generation failed.")
            continue
            
        total_tokens += tokens
        
        # bundle code with tests
        combined_code = code + "\n\n" + item['tests']
        eval_file = f"temp/{item['id']}.at"
        with open(eval_file, "w") as f:
            f.write(combined_code)
            
        print(f"Executing tests in `{eval_file}`...")
        
        is_pass, output = run_at_test(eval_file)
        
        if is_pass:
            print(f"✅ Pass@1! (Generated in {gen_time:.2f}s, {tokens} tokens)")
            passed += 1
        else:
            print(f"❌ Failed (Generated in {gen_time:.2f}s, {tokens} tokens)")
            print("--- Output ---")
            print(output.strip()[:500] + "...")
            print("--------------")

    print("\n===========================================")
    print(" Overall Results")
    print("===========================================")
    print(f"Pass@1 Rate:      {passed}/{total_problems} ({(passed/total_problems)*100:.1f}%)")
    print(f"Total Tokens:     {total_tokens}")
    if passed > 0:
        print(f"Avg Tokens/Pass:  {total_tokens/passed:.1f}")

if __name__ == "__main__":
    main()
