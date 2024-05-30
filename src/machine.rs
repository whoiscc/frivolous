use crate::memory::{Address, Memory};

pub type CodeId = usize;
pub type StackOffset = usize;
pub type InstructionOffset = usize;

#[derive(Debug)]
pub enum Instruction {
    LoadUnit,
    LoadBool(bool),
    LoadInt(i32),
    LoadString(String),
    LoadCode(Box<Code>),
    LoadLayout(Vec<String>),
    LoadType(StackOffset), // of layout
    LoadRecord(StackOffset, Vec<(String, StackOffset)>),
    LoadTrait(Vec<String>),

    // copy current offset to the index, reset current offset to the index
    Rewind(StackOffset),

    RecordGet(StackOffset, String),
    TypeGet(StackOffset, String),
    TraitGet(StackOffset, Vec<StackOffset>, String), // (trait, impl types, name)
    Operator2(InstructionOperator2, StackOffset, StackOffset),
    Call(StackOffset, Vec<StackOffset>),
    Match(StackOffset, StackOffset), // (matched, type)

    Set(StackOffset, StackOffset),
    RecordSet(StackOffset, String, StackOffset),
    TypeSet(StackOffset, String, StackOffset),
    Impl(StackOffset, Vec<StackOffset>, Vec<(String, StackOffset)>),

    Jump(InstructionOffset),
    JumpIf(StackOffset, InstructionOffset),
    Return(StackOffset),
}

#[derive(Debug)]
pub enum InstructionOperator2 {
    Add,
    Sub,
    BitAnd,
    BitOr,
    ShiftLeft,
    ShiftRight,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug)]
pub struct Code {
    pub hints: String,
    pub captures: Vec<Address>,
    pub num_parameter: usize,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Default)]
pub struct Machine {
    stack: Vec<Address>,
    frames: Vec<Frame>,
}

#[derive(Debug)]
struct Frame {
    code_address: Address,
    base_offset: StackOffset,
    instruction_offset: InstructionOffset,
}

impl Machine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn entry_execute(
        &mut self,
        code_address: Address,
        memory: &mut Memory,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(self.stack.is_empty());
        anyhow::ensure!(self.frames.is_empty());
        self.push_frame(code_address, Vec::new(), memory)?;
        self.evaluate(memory)
    }

    pub fn push_frame(
        &mut self,
        code_address: Address,
        arguments: Vec<Address>,
        memory: &mut Memory,
    ) -> anyhow::Result<()> {
        let frame = Frame {
            code_address,
            base_offset: self.stack.len(),
            instruction_offset: 0,
        };
        let code = memory
            .get_any_ref(frame.code_address)?
            .downcast_ref::<Code>()
            .ok_or(anyhow::format_err!("expecting Code object"))?;
        anyhow::ensure!(code.num_parameter == arguments.len());
        self.stack.extend(code.captures.clone());
        self.stack.extend(arguments);
        Ok(())
    }

    fn evaluate(&mut self, memory: &mut Memory) -> anyhow::Result<()> {
        let frame = self
            .frames
            .last_mut()
            .ok_or(anyhow::format_err!("evaluating frame is missing"))?;
        let code = memory
            .get_any_ref(frame.code_address)?
            .downcast_ref::<Code>()
            .ok_or(anyhow::format_err!("expecting Code object"))?;
        for instruction in &code.instructions[frame.instruction_offset..] {
            frame.instruction_offset += 1;
            use Instruction::*;
            match instruction {
                Jump(offset) => {
                    frame.instruction_offset = *offset;
                    break;
                }
                JumpIf(i, offset) => {
                    if memory.get_bool(self.stack[frame.base_offset + *i])? {
                        frame.instruction_offset = *offset;
                        break;
                    }
                }
                _ => todo!(),
            }
        }
        Ok(())
    }
}
