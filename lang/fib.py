from datetime import datetime, UTC


def main():
    def fib(n):
        if n <= 2:
            return 1
        else:
            return fib(n - 1) + fib(n - 2)
    print(datetime.now(UTC).isoformat(), "start")
    result = fib(32)
    print(datetime.now(UTC).isoformat(), result)


if __name__ == "__main__":
    main()