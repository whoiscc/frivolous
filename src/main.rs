pub mod loader;
pub mod machine;
pub mod memory;

use anyhow::Context;
use loader::{Loader, LoaderCode};
use machine::{Code, InstructionFunction, Machine};
use memory::Memory;
use tracing_subscriber::fmt::time::Uptime;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().with_timer(Uptime::default()).init();

    let mut memory = Memory::new();
    let mut loader = Loader::new();
    loader.inject_builtin(&mut memory);

    use machine::{ExtendedInstruction::*, Instruction::*, NumericalOperator2::*};
    let mut code = LoaderCode::new();
    // 0 captured self reference, 1 argument
    code.add(LoadInt(2)); // -> 2
    code.add(IntOperator2(LessEqual, 1, 2)); // -> 3
    code.add(JumpUnless(3, 5));
    code.add(LoadInt(1)); // -> 4
    code.add(Return(4));
    // jump to here
    code.add(LoadInt(1)); // -> 4
    code.add(IntOperator2(Sub, 1, 4)); // -> 5
    code.add_call(0, vec![5]); // -> 6
    code.add(LoadInt(2)); // -> 7
    code.add(IntOperator2(Sub, 1, 7)); // -> 8
    code.add_call(0, vec![8]); // -> 9
    code.add(IntOperator2(Add, 6, 9)); // -> 10
    code.add(Return(10));
    let fib = InstructionFunction {
        hints: "fib".into(),
        num_capture: 1,
        num_parameter: 1,
        id: loader.add_code(code),
    };
    let mut code = LoaderCode::new();
    code.add(LoadUnit); // -> 0
    code.add_extended(LoadFunction(Box::new(fib), vec![0])); // -> 1
    code.add(Set(0, 1));
    code.add_extended(LoadInjection("trace".into())); // -> 2
    code.add_extended(LoadString("start".into())); // -> 3
    code.add_call(2, vec![3]); // -> 4
    code.add(LoadInt(36)); // -> 5
    code.add_call(0, vec![5]); // -> 6
    code.add_extended(LoadInjection("int_display_format".into())); // -> 7
    code.add_call(7, vec![6]); // -> 8
    code.add_extended(LoadInjection("trace".into())); // -> 9
    code.add_call(9, vec![8]); // -> 10
    code.add(Return(10));
    let code = Code::new_interpreted(
        &InstructionFunction {
            hints: "<main>".into(),
            num_capture: 0,
            num_parameter: 0,
            id: loader.add_code(code),
        },
        Default::default(),
    )?;
    let code_address = memory.allocate_any(Box::new(code));
    let mut machine = Machine::new();
    machine
        .entry_execute(code_address, &mut memory, &loader)
        .context(machine.backtrace())
}
