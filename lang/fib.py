from datetime import datetime, UTC


def main():
    fib = None
    def f(n):
        if n <= 1000002:
            return 1
        else:
            return fib(n - 1) + fib(n - 2)
    fib = f
    print(datetime.now(UTC).isoformat(), "start")
    result = fib(1000032)
    print(datetime.now(UTC).isoformat(), result)


if __name__ == "__main__":
    main()