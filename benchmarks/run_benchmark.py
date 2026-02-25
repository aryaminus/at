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
        import re
        words = re.findall(r"\w+|[^\w\s]", text)
        return int(len(words) * 1.3)

def run_at_benchmark():
    start = time.perf_counter()
    res = subprocess.run(["./target/release/at", "test", "benchmarks/number_analyzer.at"], capture_output=True, text=True)
    duration = time.perf_counter() - start
    return res.returncode == 0, duration, res.stdout + res.stderr

def run_java_benchmark():
    # First compile
    compile_res = subprocess.run(["javac", "benchmarks/NumberAnalyzer.java"], capture_output=True, text=True)
    if compile_res.returncode != 0:
        return False, 0, compile_res.stderr

    # Then run with assertions enabled
    start = time.perf_counter()
    res = subprocess.run(["java", "-ea", "-cp", "benchmarks", "NumberAnalyzer"], capture_output=True, text=True)
    duration = time.perf_counter() - start
    return res.returncode == 0, duration, res.stdout + res.stderr

def read_file(path):
    with open(path, "r") as f:
        return f.read()

def main():
    print("=== AutoCodeBench: Number Analyzer Evaluation ===")
    
    at_code = read_file("benchmarks/number_analyzer.at")
    java_code = read_file("benchmarks/NumberAnalyzer.java")
    
    at_tokens = count_tokens(at_code)
    java_tokens = count_tokens(java_code)
    
    print(f"\n[Token Efficiency]")
    print(f"at solution tokens:   {at_tokens}")
    print(f"Java solution tokens: {java_tokens}")
    if at_tokens < java_tokens:
        pct = (java_tokens - at_tokens) / java_tokens * 100
        print(f"Result: `at` is {pct:.1f}% more token-efficient than Java ({java_tokens - at_tokens} fewer tokens)!")
    else:
        print(f"Result: `Java` uses {at_tokens - java_tokens} fewer tokens!")
        
    print(f"\n[Execution Pass@1]")
    subprocess.run(["cargo", "build", "--release", "--quiet"])
    
    at_pass, at_time, at_out = run_at_benchmark()
    java_pass, java_time, java_out = run_java_benchmark()
    
    print(f"`at` Pass@1:   {'✅' if at_pass else '❌'}  (Executed tests in {at_time*1000:.2f}ms)")
    if not at_pass: print(at_out)
    
    print(f"Java Pass@1:   {'✅' if java_pass else '❌'}  (Executed tests in {java_time*1000:.2f}ms)")
    if not java_pass: print(java_out)

if __name__ == "__main__":
    main()
