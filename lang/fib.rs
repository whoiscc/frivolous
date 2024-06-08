fn fib(n: i32) -> i32 {
    if n <= 2 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    tracing_subscriber::fmt::init();
    tracing::info!("start");
    let result = fib(36);
    tracing::info!("{result}")
}
