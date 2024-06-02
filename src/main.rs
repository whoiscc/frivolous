pub mod loader;
pub mod machine;
pub mod memory;

use loader::Loader;
use machine::{Code, CodeExecutable::Interpreted, Machine};
use memory::Memory;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();
    use machine::Instruction::*;
    let instructions = [
        LoadIntrinsic("trace".into()),
        LoadString("This is frivolous".into()),
        Call(0, vec![1]),
        Return(2),
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
