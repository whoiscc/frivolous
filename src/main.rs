pub mod loader;
pub mod machine;
pub mod memory;

use loader::Loader;
use machine::{Code, CodeExecutable::Interpreted, Machine};
use memory::Memory;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();
    use machine::{Instruction::*, NumericalOperator2::*};
    let instructions = [
        // 0 captured self reference, 1 argument
        LoadInt(2),
        IntOperator2(LessEqual, 1, 2),
        Rewind(2),
        JumpUnless(2, 6),
        LoadInt(1),
        Return(3),
        // jump to here
        LoadInt(1),
        IntOperator2(Sub, 1, 3),
        Call(0, vec![4]),
        Rewind(3),
        LoadInt(2),
        IntOperator2(Sub, 1, 4),
        Call(0, vec![5]),
        Rewind(4),
        IntOperator2(Add, 3, 4),
        Return(5),
    ];
    let fib = Code {
        hints: "fib".into(),
        captures: Default::default(),
        num_parameter: 1,
        executable: Interpreted(instructions.into()),
    };
    let instructions = [
        LoadUnit,
        LoadCode(Box::new(fib), vec![0]),
        Set(0, 1),
        LoadIntrinsic("trace".into()),
        LoadString("start".into()),
        Call(2, vec![3]),
        Rewind(1),
        // LoadInt(10),
        LoadInt(32),
        Call(0, vec![2]),
        Rewind(0),
        LoadIntrinsic("int_display_format".into()),
        Call(1, vec![0]),
        Rewind(0),
        LoadIntrinsic("trace".into()),
        Call(1, vec![0]),
        Rewind(0),
        Return(0),
    ];
    let code = Code {
        hints: "<main>".into(),
        captures: Default::default(),
        num_parameter: 0,
        executable: Interpreted(instructions.into()),
    };
    let mut memory = Memory::new();
    let mut loader = Loader::new();
    loader.load_builtin(&mut memory);
    let code_address = memory.allocate_any(Box::new(code));
    Machine::new().entry_execute(code_address, &mut memory, &loader)
}
