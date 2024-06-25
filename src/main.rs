use frivolous::parse::Source;
use tracing_subscriber::fmt::time::Uptime;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_timer(Uptime::default())
        .init();

    println!("{:#?}", Source::new(include_str!("../lang/fib.txt"))?);
    Ok(())
}
