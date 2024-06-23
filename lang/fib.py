from logging import basicConfig, info, INFO


def main():
    fib = None
    def f(n):
        if n <= 2:
            return 1
        else:
            return fib(n - 1) + fib(n - 2)
    fib = f
    info("start")
    info(fib(36))


if __name__ == "__main__":
    basicConfig(format="{relativeCreated:>14.6f}ms {levelname} {module} {message}", style="{", level=INFO)
    main()