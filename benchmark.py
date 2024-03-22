import ast
from time import time
import subprocess
import random
import argparse

NUM_TESTS = 1000
TEST_FILE = "tests/big.py"

class Benchmark:
    def __init__(self, num_tests=NUM_TESTS, test_file=TEST_FILE) -> None:
        self.num_tests = num_tests
        self.test_file = test_file
    def run(self):
        python = self.num_tests
        rust = self.num_tests
        total_cpython = 0
        total_rust = 0
        print("Running benchmark...")
        while python and rust:
            if random.random() < 1/2:
                total_cpython += parse_time_cpython(self.test_file)
                python -= 1
            else:
                total_rust += parse_time_rust(self.test_file)
                rust -= 1
        remaining, function = (python, parse_time_cpython) if python else (rust, parse_time_rust)
        total = 0
        for _ in range(remaining):
            total += function(self.test_file)
        if python:
            total_cpython += total
        else:
            total_rust += total
        total_cpython /= self.num_tests
        total_rust /= self.num_tests
        print(f"Average time CPython: {total_cpython:.0f}ms")
        print(f"Average time Rust: {total_rust:.0f}ms")

def parse_time_cpython(test_file):
    now = time()
    with open(test_file) as f:
        code = f.read()
    ast.parse(code)
    return 1000 * (time() - now)

def parse_time_rust(test_file):
    time_str = subprocess.check_output(["./target/release/prometheus", test_file])
    elapsed = float(time_str.decode().replace("ms", ""))
    return elapsed

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Benchmark script: our parser vs. the native CPython parser")
    parser.add_argument("-f", "--file", type=str, default="tests/big.py", help="Path to the file to benchmark")
    parser.add_argument("-n", "--num_tests", type=int, default=1000, help="Number of tests to run")
    args = parser.parse_args()
    Benchmark(args.num_tests, args.file).run()

