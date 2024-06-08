use std::{any::type_name, collections::BTreeMap, fmt::Display, sync::Arc};

use derive_more::Deref;

use crate::{
    loader::{InterpretedCodeId, Loader},
    memory::{Address, Memory, TypeError},
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
    LoadFunction(Box<InstructionFunction>, Vec<StackOffset>),
    LoadLayout(Vec<String>),
    LoadType(Option<StackOffset>), // of layout, None for native types
    LoadRecord(StackOffset, Vec<String>, Vec<(String, StackOffset)>),
    LoadTrait(Vec<String>),
    LoadInjection(String),

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
    JumpUnless(StackOffset, InstructionOffset),

    Call(StackOffset, Vec<StackOffset>),
    Return(StackOffset),
}

#[derive(Debug)]
pub struct InstructionFunction {
    pub hints: String,
    pub num_capture: usize,
    pub num_parameter: usize,
    pub id: InterpretedCodeId,
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

#[derive(Debug, Default)]
pub struct Machine {
    stack: Stack,
    frames: Vec<Frame>,
    type_id_counter: TypeId,
}

#[derive(Debug, Default)]
struct Stack {
    offsets: Vec<StackVariable>,
    locals: Vec<Local>,
}

#[derive(Debug, Clone)]
enum StackVariable {
    Address(Address),
    Local(usize),
}

#[derive(Debug, Clone, Copy)]
enum Local {
    Unit,
    Bool(bool),
    Int(i32),
}

#[derive(Debug)]
struct Frame {
    code_offset: StackOffset,
    code_id: InterpretedCodeId,
    stack_offset: StackOffset,
    instruction_offset: InstructionOffset,
    local_offset: usize,
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
        anyhow::ensure!(self.stack.offsets.is_empty());
        assert!(self.stack.locals.is_empty());
        anyhow::ensure!(self.frames.is_empty());
        let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
        anyhow::ensure!(code.num_parameter == 0);
        let CodeSource::Interpreted(source) = &code.source else {
            anyhow::bail!("unsupported native code entry")
        };
        let code_id = source.id;
        self.stack.offsets.extend(
            source
                .captures
                .iter()
                .map(|address| StackVariable::Address(address.clone())),
        );
        self.stack
            .offsets
            .insert(0, StackVariable::Address(code_address));
        let frame = Frame {
            code_offset: 0,
            stack_offset: 1,
            instruction_offset: 0,
            local_offset: 0,
            code_id,
        };
        self.frames.push(frame);
        self.evaluate(memory, loader)
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
    unsafe fn backtrace_line(&self, stack: &Stack) -> BacktraceLine {
        let code = unsafe { stack.get_downcast_ref::<Code>(self.code_offset) }.unwrap();
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
        if self.0.is_empty() {
            return write!(f, "(empty backtrace)");
        }
        let mut prefix = "";
        for line in &**self {
            write!(f, "{prefix}{line}")?;
            prefix = "\n"
        }
        Ok(())
    }
}

impl Machine {
    pub fn backtrace(&self) -> Backtrace {
        Backtrace(
            self.frames
                .iter()
                .map(|frame| unsafe { frame.backtrace_line(&self.stack) })
                .collect(),
        )
    }

    fn evaluate(&mut self, memory: &mut Memory, loader: &Loader) -> anyhow::Result<()> {
        'nonlocal_jump: loop {
            let Some(frame) = self.frames.last_mut() else {
                anyhow::bail!("evaluating frame is missing")
            };
            let instructions = &loader.code[frame.code_id];
            'local_jump: loop {
                for instruction in &instructions[frame.instruction_offset..] {
                    tracing::trace!("{instruction:?}");
                    frame.instruction_offset += 1;
                    use {Instruction::*, StackVariable::*};
                    match instruction {
                        Jump(offset) => {
                            frame.instruction_offset = *offset;
                            continue 'local_jump;
                        }
                        JumpUnless(i, offset) => {
                            if !unsafe { self.stack.get_bool(frame.stack_offset + *i) }? {
                                frame.instruction_offset = *offset;
                                continue 'local_jump;
                            }
                        }
                        Call(code_i, args_i) => {
                            let StackVariable::Address(code_address) =
                                self.stack.offsets[frame.stack_offset + *code_i].clone()
                            else {
                                anyhow::bail!(TypeError {
                                    expected: type_name::<Code>(),
                                    content: format!(
                                        "{:?}",
                                        self.stack.offsets[frame.stack_offset + *code_i]
                                    )
                                })
                            };
                            let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
                            anyhow::ensure!(args_i.len() == code.num_parameter);
                            match &code.source {
                                &CodeSource::Native(function) => {
                                    let arguments = args_i
                                        .iter()
                                        .map(|i| self.stack.escape(frame.stack_offset + *i, memory))
                                        .collect::<Vec<_>>();
                                    let address = unsafe { function(&arguments, memory) }?;
                                    self.stack.offsets.push(Address(address))
                                }
                                CodeSource::Interpreted(source) => {
                                    let stack_offset = self.stack.offsets.len();
                                    let local_offset = self.stack.locals.len();
                                    let code_id = source.id;

                                    for address in &source.captures {
                                        self.stack
                                            .offsets
                                            .push(StackVariable::Address(address.clone()))
                                    }
                                    for i in args_i {
                                        self.stack.offsets.push(
                                            self.stack.offsets[frame.stack_offset + *i].clone(),
                                        )
                                    }

                                    let frame = Frame {
                                        code_id,
                                        code_offset: frame.stack_offset + *code_i,
                                        stack_offset,
                                        instruction_offset: 0,
                                        local_offset,
                                    };
                                    self.frames.push(frame);
                                    continue 'nonlocal_jump;
                                }
                            }
                        }
                        Return(i) => {
                            let popped_frame = self.frames.pop().unwrap();
                            if !self.frames.is_empty() {
                                let mut variable =
                                    self.stack.offsets[popped_frame.stack_offset + *i].clone();
                                let mut locals_len = popped_frame.local_offset;
                                if let Local(index) = variable {
                                    if index >= locals_len {
                                        self.stack.locals[locals_len] = self.stack.locals[index];
                                        variable = Local(locals_len);
                                        locals_len += 1;
                                    }
                                }
                                self.stack.offsets.truncate(popped_frame.stack_offset);
                                self.stack.offsets.push(variable);
                                self.stack.locals.truncate(locals_len);
                                continue 'nonlocal_jump;
                            } else {
                                unsafe { self.stack.get_unit(popped_frame.stack_offset + *i) }?;
                                self.stack.offsets.clear();
                                self.stack.locals.clear();
                                return Ok(());
                            };
                        }

                        Rewind(i) => {
                            anyhow::ensure!(self.stack.offsets.len() >= *i);
                            let variable = self.stack.offsets.pop().unwrap();
                            self.stack
                                .offsets
                                .splice(frame.stack_offset + *i.., [variable]);
                        }

                        LoadUnit => self.stack.push_local(crate::machine::Local::Unit),
                        LoadBool(b) => self.stack.push_local(crate::machine::Local::Bool(*b)),
                        LoadInt(int) => self.stack.push_local(crate::machine::Local::Int(*int)),
                        LoadString(string) => self
                            .stack
                            .offsets
                            .push(Address(memory.allocate_any(Box::new(string.clone())))),
                        LoadFunction(function, captures_i) => {
                            anyhow::ensure!(captures_i.len() == function.num_capture);
                            let captures = captures_i
                                .iter()
                                .map(|i| self.stack.escape(frame.stack_offset + *i, memory))
                                .collect();
                            self.stack
                                .offsets
                                .push(Address(memory.allocate_any(Box::new(
                                    Code::new_interpreted(function, captures)?,
                                ))))
                        }
                        LoadLayout(layout) => self.stack.offsets.push(Address(
                            memory.allocate_any(Box::new(RecordLayout(layout.clone().into()))),
                        )),
                        LoadType(layout_i) => {
                            self.type_id_counter += 1;
                            let t = Type {
                                id: self.type_id_counter,
                                layout_address: layout_i
                                    .map(|i| self.stack.escape(frame.stack_offset + i, memory)),
                                attributes: Default::default(),
                            };
                            self.stack
                                .offsets
                                .push(Address(memory.allocate_any(Box::new(t))))
                        }
                        LoadRecord(type_i, variants, fields) => {
                            let mut t = unsafe {
                                self.stack
                                    .get_downcast_ref::<Type>(frame.stack_offset + *type_i)
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
                            let offsets =
                                layout
                                    .iter()
                                    .map(|name| {
                                        fields
                                            .iter()
                                            .find_map(|(other_name, i)| {
                                                if other_name == name {
                                                    Some(*i)
                                                } else {
                                                    None
                                                }
                                            })
                                            .ok_or(anyhow::format_err!(
                                                "missing field {name} in record initialization"
                                            ))
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;
                            let record = Record {
                                type_id: t.id,
                                layout: layout.clone(),
                                addresses: offsets
                                    .into_iter()
                                    .map(|i| self.stack.escape(frame.stack_offset + i, memory))
                                    .collect(),
                            };
                            self.stack
                                .offsets
                                .push(Address(memory.allocate_any(Box::new(record))))
                        }
                        LoadTrait(layout) => {
                            let t = Trait {
                                layout: layout.clone().into(),
                                implementations: Default::default(),
                            };
                            self.stack
                                .offsets
                                .push(Address(memory.allocate_any(Box::new(t))))
                        }
                        LoadInjection(key) => {
                            let Some(address) = loader.injections.get(key) else {
                                anyhow::bail!("intrinsic {key} not found")
                            };
                            self.stack.offsets.push(Address(address.clone()))
                        }

                        Set(i, source_i) => {
                            if let (Local(index), Local(source_index)) = (
                                &self.stack.offsets[frame.stack_offset + *i],
                                &self.stack.offsets[frame.stack_offset + *source_i],
                            ) {
                                self.stack.locals[*index] = self.stack.locals[*source_index]
                            } else {
                                // TODO optimize some single side local cases
                                unsafe {
                                    self.stack
                                        .escape(frame.stack_offset + *i, memory)
                                        .copy_from(
                                            &self
                                                .stack
                                                .escape(frame.stack_offset + *source_i, memory),
                                        )
                                }
                            }
                        }
                        RecordGet(i, name) => {
                            let record = unsafe {
                                self.stack
                                    .get_downcast_ref::<Record>(frame.stack_offset + *i)
                            }?;
                            self.stack
                                .offsets
                                .push(Address(record.get_ref(name)?.clone()))
                        }
                        RecordSet(i, name, j) => {
                            let address = self.stack.escape(frame.stack_offset + *j, memory);
                            *unsafe {
                                self.stack
                                    .get_downcast_mut::<Record>(frame.stack_offset + *i)
                            }?
                            .get_mut(name)? = address
                        }
                        TypeGet(i, name) => {
                            let Some(address) = unsafe {
                                self.stack.get_downcast_ref::<Type>(frame.stack_offset + *i)
                            }?
                            .attributes
                            .get(name) else {
                                anyhow::bail!("there is no attribute {name} on type object")
                            };
                            self.stack.offsets.push(Address(address.clone()))
                        }
                        TypeSet(i, name, j) => {
                            let address = self.stack.escape(frame.stack_offset + *j, memory);
                            let replaced = unsafe {
                                self.stack.get_downcast_mut::<Type>(frame.stack_offset + *i)
                            }?
                            .attributes
                            .insert(name.clone(), address);
                            anyhow::ensure!(
                                replaced.is_none(),
                                "duplicated setting attribute {name} on Type object"
                            )
                        }
                        TraitGet(i, implementor, name) => {
                            let t = unsafe {
                                self.stack
                                    .get_downcast_ref::<Trait>(frame.stack_offset + *i)
                            }?;
                            let Some(p) = t.layout.iter().position(|other_name| other_name == name)
                            else {
                                anyhow::bail!("field `{name}` not found in trait layout")
                            };
                            let mut k = Vec::new();
                            for i in implementor {
                                let ty = unsafe {
                                    self.stack.get_downcast_ref::<Type>(frame.stack_offset + *i)
                                }?;
                                k.push(ty.id)
                            }
                            let Some(addresses) = t.implementations.get(&k) else {
                                anyhow::bail!("implementation not found")
                            };
                            self.stack.offsets.push(Address(addresses[p].clone()))
                        }
                        Impl(i, implementor, fields) => {
                            let t = unsafe {
                                self.stack
                                    .get_downcast_ref::<Trait>(frame.stack_offset + *i)
                            }?;
                            let mut k = Vec::new();
                            for i in implementor {
                                let t = unsafe {
                                    self.stack.get_downcast_ref::<Type>(frame.stack_offset + *i)
                                }?;
                                k.push(t.id)
                            }
                            let offsets =
                                t.layout
                                    .iter()
                                    .map(|name| {
                                        fields
                                            .iter()
                                            .find_map(|(other_name, i)| {
                                                if other_name == name {
                                                    Some(*i)
                                                } else {
                                                    None
                                                }
                                            })
                                            .ok_or(anyhow::format_err!(
                                                "missing field `{name}` in trait implementation"
                                            ))
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;
                            let addresses = offsets
                                .into_iter()
                                .map(|i| self.stack.escape(frame.stack_offset + i, memory))
                                .collect();
                            let replaced = unsafe {
                                self.stack
                                    .get_downcast_mut::<Trait>(frame.stack_offset + *i)
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
                                self.stack.get_downcast_ref::<Type>(frame.stack_offset + *j)
                            }?;
                            // TODO support other types
                            let l = unsafe {
                                self.stack
                                    .get_downcast_ref::<Record>(frame.stack_offset + *i)
                            }?;
                            self.stack
                                .push_local(crate::machine::Local::Bool(l.type_id == r.id))
                        }
                        IntOperator2(op, i, j) => {
                            let l = unsafe { self.stack.get_int(frame.stack_offset + *i) }?;
                            let r = unsafe { self.stack.get_int(frame.stack_offset + *j) }?;
                            tracing::debug!("  {op:?} {l} {r}");
                            use {crate::machine::Local::*, NumericalOperator2::*};
                            let local = match op {
                                Add => Int(l + r),
                                Sub => Int(l - r),
                                Equal => Bool(l == r),
                                NotEqual => Bool(l != r),
                                Less => Bool(l < r),
                                LessEqual => Bool(l <= r),
                                Greater => Bool(l > r),
                                GreaterEqual => Bool(l >= r),
                            };
                            self.stack.push_local(local)
                        }
                        BitwiseOperator2(op, i, j) => {
                            let l = unsafe { self.stack.get_int(frame.stack_offset + *i) }?;
                            let r = unsafe { self.stack.get_int(frame.stack_offset + *j) }?;
                            use crate::machine::{BitwiseOperator2::*, Local::*};
                            let local = match op {
                                And => Int(l & r),
                                Or => Int(l | r),
                                ShiftLeft => Int(l << r),
                                ShiftRight => Int(l >> r),
                            };
                            self.stack.push_local(local)
                        }
                    }
                }
                anyhow::bail!("Code instructions not end with Return")
            }
        }
    }
}

impl Stack {
    fn push_local(&mut self, local: Local) {
        let index = self.locals.len();
        self.locals.push(local);
        self.offsets.push(StackVariable::Local(index))
    }

    fn escape(&mut self, i: usize, memory: &mut Memory) -> Address {
        let variable = &mut self.offsets[i];
        match variable {
            StackVariable::Address(address) => address.clone(),
            StackVariable::Local(index) => {
                let address = match &self.locals[*index] {
                    Local::Unit => memory.allocate_unit(),
                    Local::Bool(b) => memory.allocate_bool(*b),
                    Local::Int(int) => memory.allocate_int(*int),
                };
                *variable = StackVariable::Address(address.clone());
                address
            }
        }
    }

    unsafe fn get_unit(&self, i: usize) -> anyhow::Result<()> {
        match &self.offsets[i] {
            StackVariable::Address(address) => unsafe { address.get_unit() },
            StackVariable::Local(index) => {
                let Local::Unit = &self.locals[*index] else {
                    anyhow::bail!(TypeError {
                        expected: "bool",
                        content: format!("{:?}", self.offsets[i])
                    })
                };
                Ok(())
            }
        }
    }

    unsafe fn get_bool(&self, i: usize) -> anyhow::Result<bool> {
        match &self.offsets[i] {
            StackVariable::Address(address) => unsafe { address.get_bool() },
            StackVariable::Local(index) => {
                let Local::Bool(b) = &self.locals[*index] else {
                    anyhow::bail!(TypeError {
                        expected: "bool",
                        content: format!("{:?}", self.offsets[i])
                    })
                };
                Ok(*b)
            }
        }
    }

    unsafe fn get_int(&self, i: usize) -> anyhow::Result<i32> {
        match &self.offsets[i] {
            StackVariable::Address(address) => unsafe { address.get_int() },
            StackVariable::Local(index) => {
                let Local::Int(int) = &self.locals[*index] else {
                    anyhow::bail!(TypeError {
                        expected: "int",
                        content: format!("{:?}", self.offsets[i])
                    })
                };
                Ok(*int)
            }
        }
    }

    unsafe fn get_downcast_ref<T: 'static>(&self, i: usize) -> anyhow::Result<&T> {
        let StackVariable::Address(address) = &self.offsets[i] else {
            anyhow::bail!(TypeError {
                expected: type_name::<T>(),
                content: format!("{:?}", self.offsets[i])
            })
        };
        address.get_downcast_ref()
    }

    unsafe fn get_downcast_mut<T: 'static>(&self, i: usize) -> anyhow::Result<&mut T> {
        let StackVariable::Address(address) = &self.offsets[i] else {
            anyhow::bail!(TypeError {
                expected: type_name::<T>(),
                content: format!("{:?}", self.offsets[i])
            })
        };
        address.get_downcast_mut()
    }
}

#[derive(Debug)]
pub struct Code {
    hints: String,
    num_parameter: usize,
    source: CodeSource,
}

#[derive(Debug)]
enum CodeSource {
    Interpreted(Interpreted),
    Native(unsafe fn(&[Address], &mut Memory) -> anyhow::Result<Address>),
}

#[derive(Debug)]
struct Interpreted {
    captures: Vec<Address>,
    id: InterpretedCodeId,
}

impl Code {
    pub fn new_interpreted(
        function: &InstructionFunction,
        captures: Vec<Address>,
    ) -> anyhow::Result<Self> {
        anyhow::ensure!(captures.len() == function.num_capture);
        Ok(Self {
            hints: function.hints.clone(),
            num_parameter: function.num_parameter,
            source: CodeSource::Interpreted(Interpreted {
                captures,
                id: function.id,
            }),
        })
    }

    pub fn new_native(
        hints: String,
        num_parameter: usize,
        function: unsafe fn(&[Address], &mut Memory) -> anyhow::Result<Address>,
    ) -> Self {
        Self {
            hints,
            num_parameter,
            source: CodeSource::Native(function),
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
