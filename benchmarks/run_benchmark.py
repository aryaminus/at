import os
import subprocess
import time

try:
    import tiktoken
    encoder = tiktoken.get_encoding("cl100k_base")
    def count_tokens(text):
        return len(encoder.encode(text))
except ImportError:
    print("tiktoken not found, using word-based approximation")
    def count_tokens(text):
        # Rough approximation for tokens
        import re
        words = re.findall(r"\w+|[^\w\s]", text)
        return int(len(words) * 1.3)

def run_at_benchmark():
    start = time.perf_counter()
    res = subprocess.run(["./target/debug/at", "test", "benchmarks/number_analyzer.at"], capture_output=True, text=True)
    duration = time.perf_counter() - start
    return res.returncode == 0, duration, res.stdout + res.stderr

def run_py_benchmark():
    start = time.perf_counter()
    res = subprocess.run(["python3", "benchmarks/number_analyzer.py"], capture_output=True, text=True)
    duration = time.perf_counter() - start
    return res.returncode == 0, duration, res.stdout + res.stderr

def read_file(path):
    with open(path, "r") as f:
        return f.read()

def main():
    print("=== AutoCodeBench: Number Analyzer Evaluation ===")
    
    at_code = read_file("benchmarks/number_analyzer.at")
    py_code = read_file("benchmarks/number_analyzer.py")
    
    at_tokens = count_tokens(at_code)
    py_tokens = count_tokens(py_code)
    
    print(f"\n[Token Efficiency]")
    print(f"at solution tokens:     {at_tokens}")
    print(f"Python solution tokens: {py_tokens}")
    if at_tokens < py_tokens:
        print(f"Result: `at` uses {py_tokens - at_tokens} fewer tokens!")
    else:
        print(f"Result: `Python` uses {at_tokens - py_tokens} fewer tokens!")
        
    print(f"\n[Execution Pass@1]")
    # ensure build
    subprocess.run(["cargo", "build", "--quiet"])
    
    at_pass, at_time, at_out = run_at_benchmark()
    py_pass, py_time, py_out = run_py_benchmark()
    
    print(f"`at` Pass@1:     {'✅' if at_pass else '❌'}  (Executed tests in {at_time*1000:.2f}ms)")
    if not at_pass: print(at_out)
    
    print(f"Python Pass@1:   {'✅' if py_pass else '❌'}  (Executed tests in {py_time*1000:.2f}ms)")
    if not py_pass: print(py_out)

if __name__ == "__main__":
    main()
