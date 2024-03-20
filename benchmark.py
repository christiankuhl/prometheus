import ast
from time import time
import subprocess

NUM_TESTS = 1000
TEST_FILE = "tests/big.py"

def parse_time_cpython():
    now = time()
    with open(TEST_FILE) as f:
        code = f.read()
    ast.parse(code)
    return time() - now

def parse_time_rust():
    time_str = subprocess.check_output(["./target/release/prometheus", TEST_FILE])
    elapsed = float(time_str.decode().replace("ms", ""))
    return elapsed

if __name__ == "__main__":
    print("Running benchmark: CPython parser...")
    total_cpython = 0
    for _ in range(NUM_TESTS):
        total_cpython += parse_time_cpython()
    total_cpython /= NUM_TESTS
    print(f"Average time: {1000*total_cpython:.0f}ms")
    print("Running benchmark: Rust parser...")
    total_rust = 0
    for _ in range(NUM_TESTS):
        total_rust += parse_time_rust()
    total_rust /= NUM_TESTS
    print(f"Average time: {total_rust:.0f}ms")

