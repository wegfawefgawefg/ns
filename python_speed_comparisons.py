# speed_test_equivalents.py
import timeit
import sys

# Set higher recursion limit for Python if needed for deep recursion tests
# sys.setrecursionlimit(2000) # Default is often 1000

# --- Python Equivalents for Benchmarks ---


def py_fib(n):
    if n <= 1:
        return n
    else:
        return py_fib(n - 1) + py_fib(n - 2)


def py_sum_recursive(n):
    if n == 0:
        return 0
    else:
        return n + py_sum_recursive(n - 1)


def py_list_creation_and_sum(size):
    # Create list
    lst = list(range(1, size + 1))
    # Sum list (iteratively for Python, as direct sum() is too optimized)
    current_sum = 0
    for item in lst:
        current_sum += item
    return current_sum


class PyStruct:
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def __repr__(self):
        return f"PyStruct(a={self.a}, b={self.b})"


def py_struct_creation_loop(n_structs):
    # This function in Python will just create N structs.
    # The NotScheme version returns one as a final result.
    # To make it comparable, we can return the last one or just time the loop.
    # For simplicity, let's just do the loop and return a dummy value or the last struct.
    s = None
    for i in range(n_structs):
        s = PyStruct(i, i + 10)
    return s  # Return the last created struct


def run_python_benchmark(test_name, func, *args):
    print(f"\n--- Python Benchmark: {test_name} ---")

    # Using timeit for more stable measurements
    # Number of loops for timeit can be adjusted
    number_of_runs = 10
    if "Fibonacci(20)" in test_name:  # Fib(20) is a bit slow
        number_of_runs = 10
    elif "Recursive Sum(100)" in test_name:
        number_of_runs = 1000
    elif "List Creation & Sum (50)" in test_name:
        number_of_runs = 10000
    elif "Struct Creation Loop (100)" in test_name:
        number_of_runs = 10000

    # Wrap func call in a lambda for timeit setup
    stmt_lambda = lambda: func(*args)

    try:
        # Warm-up (optional, but can help for very short functions)
        # stmt_lambda()

        total_time = timeit.timeit(stmt_lambda, number=number_of_runs)
        avg_time_per_run = total_time / number_of_runs

        # Get one result for display
        result = func(*args)

        print(
            f"Python Execution Time (avg of {number_of_runs} runs): {avg_time_per_run:.6f} seconds"
        )
        print(f"Result: {result}")

    except RecursionError:
        print(f"Python Execution: RecursionError for {test_name} with args {args}")
    except Exception as e:
        print(f"Python Execution Error for {test_name}: {e}")
    print("------------------------------------")


if __name__ == "__main__":
    print("Running Python Speed Test Equivalents...\n")

    # Fibonacci Test
    fib_n_py = 20
    run_python_benchmark(f"Recursive Fibonacci({fib_n_py})", py_fib, fib_n_py)

    # Recursive Sum Test
    sum_n_py = 100
    # Note: Python's default recursion limit is ~1000.
    # For larger N, you'd hit RecursionError or need to increase limit.
    # Your NotScheme VM might have a different effective limit based on its call stack.
    try:
        sys.setrecursionlimit(sum_n_py + 200)  # Ensure enough headroom
        run_python_benchmark(f"Recursive Sum({sum_n_py})", py_sum_recursive, sum_n_py)
    except RecursionError:
        print(
            f"Could not run Recursive Sum({sum_n_py}) due to Python recursion limits, even after trying to increase."
        )
    finally:
        sys.setrecursionlimit(1000)  # Reset to default or original

    # List Creation and Summation Test
    list_size_py = 50
    run_python_benchmark(
        f"List Creation & Sum ({list_size_py})", py_list_creation_and_sum, list_size_py
    )

    # Struct Creation Loop Test
    struct_count_py = 100
    run_python_benchmark(
        f"Struct Creation Loop ({struct_count_py})",
        py_struct_creation_loop,
        struct_count_py,
    )

    print("\nPython Speed Test Equivalents Finished.")
