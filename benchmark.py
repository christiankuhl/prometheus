import ast
from time import time
import subprocess
import random
import argparse
import tqdm
import subprocess
from statistics import mean, stdev

NUM_TESTS = 1000
TEST_FILE = "tests/big.py"


class Benchmark:
    def __init__(self, num_tests=NUM_TESTS, test_file=TEST_FILE) -> None:
        self.num_tests = num_tests
        self.test_file = test_file

    def run(self, rust_only=False):
        subprocess.run(["cargo", "build", "--release"])
        if rust_only:
            print("Running Rust parser...")
            total = 0
            for _ in range(self.num_tests):
                total += parse_time_rust(self.test_file)
            print(f"Average time Rust: {total:.0f}ms")
            return
        python = self.num_tests
        rust = self.num_tests
        total_cpython = 0
        total_rust = 0
        print("Running benchmark...")
        prog = tqdm.tqdm(total=2 * self.num_tests)
        py_results = []
        rs_results = []
        while python and rust:
            if random.random() < 1 / 2:
                res = parse_time_cpython(self.test_file)
                total_cpython += res
                py_results.append(res)
                python -= 1
                prog.update(1)
            else:
                res = parse_time_rust(self.test_file)
                total_rust += res
                rs_results.append(res)
                rust -= 1
                prog.update(1)
            if total_cpython > 0 and total_rust > 0:
                qp = total_cpython / (self.num_tests - python)
                qr = total_rust / (self.num_tests - rust)
                prog.set_description(
                    f"CPython {qp:.0f}ms / Rust {qr:.0f}ms ~ {qr/qp:.2f}x"
                )
        remaining, function = (
            (python, parse_time_cpython) if python else (rust, parse_time_rust)
        )
        rest = []
        for _ in range(remaining):
            rest.append(function(self.test_file))
            prog.update(1)
        prog.close()
        python = stats(py_results)
        rust = stats(rs_results)
        print(
            f"CPython: mean: {python[0]:.0f}ms, stdev: {python[1]:.2f}ms, min: {python[2]:.0f}ms, max: {python[3]:.0f}ms"
        )
        print(
            f"Rust: mean: {rust[0]:.0f}ms, stdev: {rust[1]:.2f}ms, min: {rust[2]:.0f}ms, max: {rust[3]:.0f}ms"
        )


def stats(results):
    return mean(results), stdev(results), min(results), max(results)


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
    parser = argparse.ArgumentParser(
        description="Benchmark script: our parser vs. the native CPython parser"
    )
    parser.add_argument(
        "-f",
        "--file",
        type=str,
        default="tests/big.py",
        help="Path to the file to benchmark",
    )
    parser.add_argument(
        "-n", "--num_tests", type=int, default=1000, help="Number of tests to run"
    )
    parser.add_argument(
        "-r",
        "--rust_only",
        default=False,
        help="Only run Rust parser",
        action="store_true",
    )
    args = parser.parse_args()
    Benchmark(args.num_tests, args.file).run(args.rust_only)
