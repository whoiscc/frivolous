use std::{
    cmp::Ordering::{Greater, Less},
    collections::BTreeMap,
    sync::Arc,
};

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
    LoadCode(Box<Code>, Vec<StackOffset>),
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
    Match(StackOffset, StackOffset), // (matched, type)

    Set(StackOffset, StackOffset),
    RecordSet(StackOffset, String, StackOffset),
    TypeSet(StackOffset, String, StackOffset),
    Impl(StackOffset, Vec<StackOffset>, Vec<(String, StackOffset)>),

    Jump(InstructionOffset),
    JumpIf(StackOffset, InstructionOffset),

    Call(StackOffset, Vec<StackOffset>),
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

#[derive(Debug, Clone)]
pub struct Code {
    pub hints: String,
    pub captures: Vec<Address>,
    pub num_parameter: usize,
    pub instructions: Arc<[Instruction]>,
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
    return_offset: StackOffset,
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
        self.push_frame(code_address, Vec::new())?;
        self.evaluate(memory)
    }

    pub fn push_frame(
        &mut self,
        code_address: Address,
        arguments: Vec<Address>,
    ) -> anyhow::Result<()> {
        let Some(code) = code_address.get_any_ref()?.downcast_ref::<Code>() else {
            anyhow::bail!("expecting Code object")
        };
        anyhow::ensure!(code.num_parameter == arguments.len());
        let captures = code.captures.clone();
        let frame = Frame {
            code_address,
            base_offset: self.stack.len(),
            return_offset: captures.len() + arguments.len(),
            instruction_offset: 0,
        };
        self.frames.push(frame);
        self.stack.extend(captures.clone());
        self.stack.extend(arguments);
        Ok(())
    }

    fn evaluate(&mut self, memory: &mut Memory) -> anyhow::Result<()> {
        'nonlocal_jump: loop {
            let frame = self
                .frames
                .last_mut()
                .ok_or(anyhow::format_err!("evaluating frame is missing"))?;
            let code = frame
                .code_address
                .get_any_ref()?
                .downcast_ref::<Code>()
                .ok_or(anyhow::format_err!("expecting Code object"))?;
            'local_jump: loop {
                for instruction in &code.instructions[frame.instruction_offset..] {
                    frame.instruction_offset += 1;
                    use Instruction::*;
                    match instruction {
                        Jump(offset) => {
                            frame.instruction_offset = *offset;
                            continue 'local_jump;
                        }
                        JumpIf(i, offset) => {
                            if self.stack[frame.base_offset + *i].get_bool()? {
                                frame.instruction_offset = *offset;
                                continue 'local_jump;
                            }
                        }
                        Call(code_i, args_i) => {
                            let code_address = self.stack[frame.base_offset + *code_i].clone();
                            let arguments = args_i
                                .iter()
                                .map(|i| self.stack[frame.base_offset + *i].clone())
                                .collect();
                            frame.return_offset = self.stack.len();
                            self.push_frame(code_address, arguments)?;
                            continue 'nonlocal_jump;
                        }
                        Return(i) => {
                            let address = self.stack[frame.base_offset + *i].clone();
                            self.frames.pop();
                            if let Some(frame) = self.frames.last_mut() {
                                self.stack.truncate(frame.return_offset);
                                self.stack.push(address);
                                continue 'nonlocal_jump;
                            } else {
                                self.stack[0] = address;
                                return Ok(());
                            };
                        }

                        Rewind(i) => {
                            anyhow::ensure!(self.stack.len() > frame.base_offset + *i);
                            let address = self.stack.last().unwrap().clone();
                            // `splice` probably can do but less readable
                            self.stack.truncate(frame.base_offset + *i);
                            self.stack.push(address)
                        }

                        LoadUnit => self.stack.push(memory.allocate_unit()),
                        LoadBool(b) => self.stack.push(memory.allocate_bool(*b)),
                        LoadInt(int) => self.stack.push(memory.allocate_int(*int)),
                        LoadString(string) => self
                            .stack
                            .push(memory.allocate_any(Box::new(string.clone()))),
                        LoadCode(code, captures) => {
                            anyhow::ensure!(code.captures.is_empty());
                            let mut code = code.clone();
                            code.captures = captures
                                .iter()
                                .map(|i| self.stack[frame.base_offset + *i].clone())
                                .collect();
                            self.stack.push(memory.allocate_any(code))
                        }
                        LoadLayout(layout) => self
                            .stack
                            .push(memory.allocate_any(Box::new(RecordLayout(layout.clone())))),
                        LoadType(layout_i) => {
                            let t = Type {
                                layout: self.stack[frame.base_offset + *layout_i].clone(),
                                attributes: Default::default(),
                            };
                            self.stack.push(memory.allocate_any(Box::new(t)))
                        }
                        LoadRecord(type_i, fields) => {
                            let Some(t) = self.stack[frame.base_offset + *type_i]
                                .get_any_ref()?
                                .downcast_ref::<Type>()
                            else {
                                anyhow::bail!("expecting type object")
                            };
                            let Some(RecordLayout(layout)) = t.layout.get_any_ref()?.downcast_ref()
                            else {
                                anyhow::bail!("type layout is not a Layout object")
                            };
                            let mut addresses = Vec::new();
                            for name in layout {
                                let i = fields
                                    .iter()
                                    .find_map(
                                        |(other_name, i)| {
                                            if other_name == name {
                                                Some(*i)
                                            } else {
                                                None
                                            }
                                        },
                                    )
                                    .ok_or(anyhow::format_err!(
                                        "missing field {name} in record initialization"
                                    ))?;
                                addresses.push(self.stack[frame.base_offset + i].clone())
                            }
                            let record = Record {
                                layout: t.layout.clone(),
                                addresses,
                            };
                            self.stack.push(memory.allocate_any(Box::new(record)))
                        }
                        LoadTrait(layout) => {
                            let t = Trait {
                                layout: layout.clone(),
                                implementations: Default::default(),
                            };
                            self.stack.push(memory.allocate_any(Box::new(t)))
                        }

                        Set(i, source_i) => {
                            match i.cmp(source_i) {
                                Less => {
                                    let (stack, remaining_stack) = self.stack.split_at_mut(*i + 1);
                                    stack
                                        .last_mut()
                                        .unwrap()
                                        .copy_from(&remaining_stack[source_i - i - 1])?
                                }
                                Greater => {
                                    let (stack, remaining_stack) =
                                        self.stack.split_at_mut(*source_i + 1);
                                    remaining_stack[i - source_i - 1]
                                        .copy_from(stack.last().unwrap())?
                                }
                                _ => {} // would be some silly case like `set x = x`
                            }
                        }
                        RecordGet(i, name) => {
                            let Some(record) = self.stack[frame.base_offset + *i]
                                .get_any_ref()?
                                .downcast_ref::<Record>()
                            else {
                                anyhow::bail!("expecting record object")
                            };
                            let Some(RecordLayout(layout)) =
                                record.layout.get_any_ref()?.downcast_ref()
                            else {
                                anyhow::bail!("record layout is not a Layout object")
                            };
                            let p = layout
                                .iter()
                                .position(|other_name| other_name == name)
                                .ok_or(anyhow::format_err!("unexpected field name {name}"))?;
                            self.stack.push(record.addresses[p].clone())
                        }
                        RecordSet(i, name, j) => {
                            let address = self.stack[frame.base_offset + *j].clone();
                            let Some(record) = self.stack[frame.base_offset + *i]
                                .get_any_mut()?
                                .downcast_mut::<Record>()
                            else {
                                anyhow::bail!("expecting record object")
                            };
                            let Some(RecordLayout(layout)) =
                                record.layout.get_any_ref()?.downcast_ref()
                            else {
                                anyhow::bail!("record layout is not a Layout object")
                            };
                            let p = layout
                                .iter()
                                .position(|other_name| other_name == name)
                                .ok_or(anyhow::format_err!("unexpected field name {name}"))?;
                            record.addresses[p] = address
                        }
                        TypeGet(i, name) => {
                            let Some(t) = self.stack[frame.base_offset + *i]
                                .get_any_ref()?
                                .downcast_ref::<Type>()
                            else {
                                anyhow::bail!("expecting type object")
                            };
                            let Some(address) = t.attributes.get(name) else {
                                anyhow::bail!("there is no attribute {name} on type object")
                            };
                            self.stack.push(address.clone())
                        }
                        TypeSet(i, name, j) => {
                            let address = self.stack[frame.base_offset + *j].clone();
                            let Some(t) = self.stack[frame.base_offset + *i]
                                .get_any_mut()?
                                .downcast_mut::<Type>()
                            else {
                                anyhow::bail!("expecting type object")
                            };
                            let replaced = t.attributes.insert(name.clone(), address);
                            anyhow::ensure!(
                                replaced.is_none(),
                                "duplicated setting attribute {name} on type object"
                            )
                        }
                        _ => todo!(),
                    }
                }
                anyhow::bail!("Code instructions not end with Return")
            }
        }
    }
}

#[derive(Debug)]
struct Record {
    layout: Address,
    addresses: Vec<Address>,
}

#[derive(Debug)]
struct RecordLayout(Vec<String>);

#[derive(Debug)]
struct Type {
    layout: Address,
    attributes: BTreeMap<String, Address>,
}

#[derive(Debug)]
struct Trait {
    layout: Vec<String>,
    implementations: BTreeMap<Vec<Address>, Vec<Address>>,
}
