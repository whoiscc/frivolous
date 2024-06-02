use std::{
    cmp::Ordering::{Greater, Less},
    collections::BTreeMap,
    fmt::Display,
    sync::Arc,
};

use anyhow::Context;
use derive_more::Deref;

use crate::{
    loader::Loader,
    memory::{Address, Memory},
};

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
    LoadType(Option<StackOffset>), // of layout, None for native types
    LoadRecord(StackOffset, Vec<String>, Vec<(String, StackOffset)>),
    LoadTrait(Vec<String>),
    LoadIntrinsic(String),

    // copy current offset to the index, reset current offset to the index
    Rewind(StackOffset),

    RecordGet(StackOffset, String),
    TypeGet(StackOffset, String),
    TraitGet(StackOffset, Vec<StackOffset>, String), // (trait, impl types, name)
    IntOperator2(NumericalOperator2, StackOffset, StackOffset),
    BitwiseOperator2(BitwiseOperator2, StackOffset, StackOffset),
    Is(StackOffset, StackOffset),

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
pub enum NumericalOperator2 {
    Add,
    Sub,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug)]
pub enum BitwiseOperator2 {
    And,
    Or,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone)]
pub struct Code {
    pub hints: String,
    pub captures: Vec<Address>,
    pub num_parameter: usize,
    pub executable: CodeExecutable,
}

#[derive(Debug, Clone)]
pub enum CodeExecutable {
    Interpreted(Arc<[Instruction]>),
    Native(unsafe fn(&[Address], &mut Memory) -> anyhow::Result<Address>),
}

#[derive(Debug, Default)]
pub struct Machine {
    stack: Vec<Address>,
    frames: Vec<Frame>,
    type_id_counter: TypeId,
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
        loader: &Loader,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(self.stack.is_empty());
        anyhow::ensure!(self.frames.is_empty());
        self.push_frame(code_address, Vec::new())?;
        self.evaluate_with_backtrace(memory, loader)
    }

    fn push_frame(&mut self, code_address: Address, arguments: Vec<Address>) -> anyhow::Result<()> {
        let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
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
}

#[derive(Debug)]
pub struct BacktraceLine {
    pub code_hints: String,
    pub instruction_offset: usize,
}

impl Display for BacktraceLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`{}` (instruction {})",
            self.code_hints, self.instruction_offset
        )
    }
}

impl Frame {
    unsafe fn backtrace_line(&self) -> BacktraceLine {
        let code = unsafe { self.code_address.get_downcast_ref::<Code>() }.unwrap();
        BacktraceLine {
            code_hints: code.hints.clone(),
            instruction_offset: self.instruction_offset,
        }
    }
}

#[derive(Debug, Deref)]
pub struct Backtrace(pub Vec<BacktraceLine>);

impl Display for Backtrace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut prefix = "";
        for line in &**self {
            write!(f, "{prefix}{line}")?;
            prefix = "\n"
        }
        Ok(())
    }
}

impl Machine {
    fn evaluate_with_backtrace(
        &mut self,
        memory: &mut Memory,
        loader: &Loader,
    ) -> anyhow::Result<()> {
        self.evaluate(memory, loader).with_context(|| {
            Backtrace(
                self.frames
                    .iter()
                    .map(|frame| unsafe { frame.backtrace_line() })
                    .collect(),
            )
        })
    }

    fn evaluate(&mut self, memory: &mut Memory, loader: &Loader) -> anyhow::Result<()> {
        'nonlocal_jump: loop {
            let Some(frame) = self.frames.last_mut() else {
                anyhow::bail!("evaluating frame is missing")
            };
            let CodeExecutable::Interpreted(instructions) =
                &unsafe { frame.code_address.get_downcast_ref::<Code>() }?.executable
            else {
                todo!()
            };
            'local_jump: loop {
                for instruction in &instructions[frame.instruction_offset..] {
                    frame.instruction_offset += 1;
                    use Instruction::*;
                    match instruction {
                        Jump(offset) => {
                            frame.instruction_offset = *offset;
                            continue 'local_jump;
                        }
                        JumpIf(i, offset) => {
                            if unsafe { self.stack[frame.base_offset + *i].get_bool() }? {
                                frame.instruction_offset = *offset;
                                continue 'local_jump;
                            }
                        }
                        Call(code_i, args_i) => {
                            let code_address = self.stack[frame.base_offset + *code_i].clone();
                            let arguments = args_i
                                .iter()
                                .map(|i| self.stack[frame.base_offset + *i].clone())
                                .collect::<Vec<_>>();
                            // i don't like every interpreted call must be downcast three times (one
                            // here, one in `push_frame`, one after nonlocal jump)
                            // hopefully compiler will optimize them away
                            let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
                            if let CodeExecutable::Native(function) = code.executable {
                                anyhow::ensure!(code.captures.is_empty());
                                anyhow::ensure!(arguments.len() == code.num_parameter);
                                let address = unsafe { function(&arguments, memory) }?;
                                self.stack.push(address)
                            } else {
                                frame.return_offset = self.stack.len();
                                self.push_frame(code_address, arguments)?;
                                continue 'nonlocal_jump;
                            }
                        }
                        Return(i) => {
                            let address = self.stack.remove(frame.base_offset + *i);
                            self.frames.pop();
                            if let Some(frame) = self.frames.last_mut() {
                                self.stack.truncate(frame.return_offset);
                                self.stack.push(address);
                                continue 'nonlocal_jump;
                            } else {
                                unsafe { address.get_unit() }?;
                                self.stack.clear();
                                return Ok(());
                            };
                        }

                        Rewind(i) => {
                            anyhow::ensure!(self.stack.len() > frame.base_offset + *i);
                            let address = self.stack.pop().unwrap();
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
                        LoadLayout(layout) => self.stack.push(
                            memory.allocate_any(Box::new(RecordLayout(layout.clone().into()))),
                        ),
                        LoadType(layout_i) => {
                            self.type_id_counter += 1;
                            let t = Type {
                                id: self.type_id_counter,
                                layout_address: layout_i
                                    .map(|i| self.stack[frame.base_offset + i].clone()),
                                attributes: Default::default(),
                            };
                            self.stack.push(memory.allocate_any(Box::new(t)))
                        }
                        LoadRecord(type_i, variants, fields) => {
                            let mut t = unsafe {
                                self.stack[frame.base_offset + *type_i].get_downcast_ref::<Type>()
                            }?;
                            for variant in variants {
                                let Some(layout_address) = &t.layout_address else {
                                    anyhow::bail!("attempting to construct record for native type")
                                };
                                t = unsafe {
                                    layout_address
                                        .get_downcast_ref::<Record>()?
                                        .get_ref(variant)?
                                        .get_downcast_ref()
                                }?
                            }
                            let Some(layout_address) = &t.layout_address else {
                                anyhow::bail!("attempting to construct record for native type")
                            };
                            let RecordLayout(layout) =
                                unsafe { layout_address.get_downcast_ref() }?;
                            let mut addresses = Vec::new();
                            for name in &**layout {
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
                                type_id: t.id,
                                layout: layout.clone(),
                                addresses: addresses.into(),
                            };
                            self.stack.push(memory.allocate_any(Box::new(record)))
                        }
                        LoadTrait(layout) => {
                            let t = Trait {
                                layout: layout.clone().into(),
                                implementations: Default::default(),
                            };
                            self.stack.push(memory.allocate_any(Box::new(t)))
                        }
                        LoadIntrinsic(key) => {
                            let Some(address) = loader.intrinsics.get(key) else {
                                anyhow::bail!("intrinsic {key} not found")
                            };
                            self.stack.push(address.clone())
                        }

                        Set(i, source_i) => {
                            match i.cmp(source_i) {
                                Less => {
                                    let (stack, remaining_stack) = self.stack.split_at_mut(*i + 1);
                                    unsafe {
                                        stack
                                            .last_mut()
                                            .unwrap()
                                            .copy_from(&remaining_stack[source_i - i - 1])
                                    }
                                }
                                Greater => {
                                    let (stack, remaining_stack) =
                                        self.stack.split_at_mut(*source_i + 1);
                                    unsafe {
                                        remaining_stack[i - source_i - 1]
                                            .copy_from(stack.last().unwrap())
                                    }
                                }
                                _ => {} // would be some silly case like `set x = x`
                            }
                        }
                        RecordGet(i, name) => {
                            let record = unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_ref::<Record>()
                            }?;
                            self.stack.push(record.get_ref(name)?.clone())
                        }
                        RecordSet(i, name, j) => {
                            let address = self.stack[frame.base_offset + *j].clone();
                            *unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_mut::<Record>()
                            }?
                            .get_mut(name)? = address
                        }
                        TypeGet(i, name) => {
                            let t = unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_ref::<Type>()
                            }?;
                            let Some(address) = t.attributes.get(name) else {
                                anyhow::bail!("there is no attribute {name} on type object")
                            };
                            self.stack.push(address.clone())
                        }
                        TypeSet(i, name, j) => {
                            let address = self.stack[frame.base_offset + *j].clone();
                            let t = unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_mut::<Type>()
                            }?;
                            let replaced = t.attributes.insert(name.clone(), address);
                            anyhow::ensure!(
                                replaced.is_none(),
                                "duplicated setting attribute {name} on Type object"
                            )
                        }
                        TraitGet(i, implementor, name) => {
                            let t = unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_ref::<Trait>()
                            }?;
                            let Some(p) = t.layout.iter().position(|other_name| other_name == name)
                            else {
                                anyhow::bail!("field `{name}` not found in trait layout")
                            };
                            let mut k = Vec::new();
                            for i in implementor {
                                let t = unsafe {
                                    self.stack[frame.base_offset + *i].get_downcast_ref::<Type>()
                                }?;
                                k.push(t.id)
                            }
                            let Some(addresses) = t.implementations.get(&k) else {
                                anyhow::bail!("implementation not found")
                            };
                            self.stack.push(addresses[p].clone())
                        }
                        Impl(i, implementor, fields) => {
                            let t = unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_ref::<Trait>()
                            }?;
                            let mut k = Vec::new();
                            for i in implementor {
                                let t = unsafe {
                                    self.stack[frame.base_offset + *i].get_downcast_ref::<Type>()
                                }?;
                                k.push(t.id)
                            }
                            let mut addresses = Vec::new();
                            for name in &*t.layout {
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
                                        "missing field `{name}` in trait implementation"
                                    ))?;
                                addresses.push(self.stack[frame.base_offset + i].clone())
                            }
                            let replaced = unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_mut::<Trait>()
                            }
                            .unwrap()
                            .implementations
                            .insert(k, addresses);
                            anyhow::ensure!(
                                replaced.is_none(),
                                "duplicated implementation of trait"
                            )
                        }

                        Is(i, j) => {
                            let r = unsafe {
                                self.stack[frame.base_offset + *j].get_downcast_ref::<Type>()
                            }?;
                            // TODO support other types
                            let l = unsafe {
                                self.stack[frame.base_offset + *i].get_downcast_ref::<Record>()
                            }?;
                            self.stack.push(memory.allocate_bool(l.type_id == r.id))
                        }
                        IntOperator2(op, i, j) => {
                            let l = unsafe { self.stack[frame.base_offset + *i].get_int() }?;
                            let r = unsafe { self.stack[frame.base_offset + *j].get_int() }?;
                            use NumericalOperator2::*;
                            let address = match op {
                                Add => memory.allocate_int(l + r),
                                Sub => memory.allocate_int(l + r),
                                Equal => memory.allocate_bool(l == r),
                                NotEqual => memory.allocate_bool(l != r),
                                Less => memory.allocate_bool(l < r),
                                LessEqual => memory.allocate_bool(l <= r),
                                Greater => memory.allocate_bool(l > r),
                                GreaterEqual => memory.allocate_bool(l >= r),
                            };
                            self.stack.push(address)
                        }
                        BitwiseOperator2(op, i, j) => {
                            let l = unsafe { self.stack[frame.base_offset + *i].get_int() }?;
                            let r = unsafe { self.stack[frame.base_offset + *j].get_int() }?;
                            use crate::machine::BitwiseOperator2::*;
                            let address = match op {
                                And => memory.allocate_int(l & r),
                                Or => memory.allocate_int(l | r),
                                ShiftLeft => memory.allocate_int(l << r),
                                ShiftRight => memory.allocate_int(l >> r),
                            };
                            self.stack.push(address)
                        }
                    }
                }
                anyhow::bail!("Code instructions not end with Return")
            }
        }
    }
}

#[derive(Debug)]
struct Record {
    type_id: TypeId,
    // switching to `Arc<Vec<String>>` save 8 bytes, but incur one more indirection
    // guess computation is more sensitive than memory here
    layout: Arc<[String]>,
    addresses: Box<[Address]>,
}

impl Record {
    fn position(&self, name: &str) -> anyhow::Result<usize> {
        self.layout
            .iter()
            .position(|other_name| other_name == name)
            .ok_or(anyhow::format_err!(
                "field `{name}` not found in record layout"
            ))
    }

    fn get_ref(&self, name: &str) -> anyhow::Result<&Address> {
        Ok(&self.addresses[self.position(name)?])
    }

    fn get_mut(&mut self, name: &str) -> anyhow::Result<&mut Address> {
        Ok(&mut self.addresses[self.position(name)?])
    }
}

#[derive(Debug)]
struct RecordLayout(Arc<[String]>);

type TypeId = u32;

#[derive(Debug)]
struct Type {
    id: TypeId,
    layout_address: Option<Address>, // either RecordLayout or Record
    attributes: BTreeMap<String, Address>,
}

#[derive(Debug)]
struct Trait {
    layout: Arc<[String]>,
    implementations: BTreeMap<Vec<TypeId>, Vec<Address>>,
}
