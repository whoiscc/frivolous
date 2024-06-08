pub mod loader;
pub mod machine;
pub mod memory;

use anyhow::Context;
use loader::Loader;
use machine::{Code, InstructionFunction, Machine};
use memory::Memory;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let mut memory = Memory::new();
    let mut loader = Loader::new();
    loader.inject_builtin(&mut memory);

    use machine::{Instruction::*, NumericalOperator2::*};
    let instructions = [
        // 0 captured self reference, 1 argument
        LoadInt(2),                    // -> 2
        IntOperator2(LessEqual, 1, 2), // -> 3
        JumpUnless(3, 5),
        LoadInt(1), // -> 4
        Return(4),
        // jump to here
        LoadInt(1),              // -> 4
        IntOperator2(Sub, 1, 4), // -> 5
        Call(0, vec![5]),        // -> 6
        LoadInt(2),              // -> 7
        IntOperator2(Sub, 1, 7), // -> 8
        Call(0, vec![8]),        // -> 9
        IntOperator2(Add, 6, 9), // -> 10
        Return(10),
    ];
    let fib = InstructionFunction {
        hints: "fib".into(),
        num_capture: 1,
        num_parameter: 1,
        id: loader.add_code(instructions.into()),
    };
    let instructions = [
        LoadUnit,
        LoadFunction(Box::new(fib), vec![0]),
        Set(0, 1),
        LoadInjection("trace".into()),
        LoadString("start".into()),
        Call(2, vec![3]),
        Rewind(1),
        // LoadInt(10),
        LoadInt(36),
        Call(0, vec![2]),
        Rewind(0),
        LoadInjection("int_display_format".into()),
        Call(1, vec![0]),
        Rewind(0),
        LoadInjection("trace".into()),
        Call(1, vec![0]),
        Rewind(0),
        Return(0),
    ];
    let code = Code::new_interpreted(
        &InstructionFunction {
            hints: "<main>".into(),
            num_capture: 0,
            num_parameter: 0,
            id: loader.add_code(instructions.into()),
        },
        Default::default(),
    )?;
    let code_address = memory.allocate_any(Box::new(code));
    let mut machine = Machine::new();
    machine
        .entry_execute(code_address, &mut memory, &loader)
        .context(machine.backtrace())
}
