use std::{any::type_name, collections::BTreeMap, fmt::Display, sync::Arc};

use derive_more::Deref;

use crate::{
    loader::{InterpretedCodeId, Loader},
    memory::{Address, Memory, TypeError},
};

pub type CodeId = usize;
pub type StackOffset = u8;
pub type InstructionOffset = usize;

#[derive(Debug)]
pub enum Instruction {
    LoadUnit,
    LoadBool(bool),
    LoadInt(i32),

    IntOperator2(NumericalOperator2, StackOffset, StackOffset),
    BitwiseOperator2(BitwiseOperator2, StackOffset, StackOffset),

    Set(StackOffset, StackOffset),
    // copy current offset to the index, reset current offset to the index
    Rewind(StackOffset),

    Jump(InstructionOffset),
    JumpUnless(StackOffset, InstructionOffset),

    Call(StackOffset, Box<[StackOffset; 16]>, u8),
    Return(StackOffset),

    Extended(Box<ExtendedInstruction>),
}

impl Instruction {
    pub fn call(code: StackOffset, args_i: Vec<StackOffset>) -> Self {
        let mut call_args_i = Box::<[_; 16]>::default();
        let num_arg = args_i.len() as _;
        for (i, source_i) in call_args_i.iter_mut().zip(args_i) {
            *i = source_i
        }
        Self::Call(code, call_args_i, num_arg)
    }
}

#[derive(Debug)]
pub enum ExtendedInstruction {
    LoadString(String),
    LoadFunction(Box<InstructionFunction>, Vec<StackOffset>),
    LoadLayout(Vec<String>),
    LoadType(Option<StackOffset>), // of layout, None for native types
    LoadRecord(StackOffset, Vec<String>, Vec<(String, StackOffset)>),
    LoadTrait(Vec<String>),
    LoadInjection(String),

    RecordGet(StackOffset, String),
    TypeGet(StackOffset, String),
    TraitGet(StackOffset, Vec<StackOffset>, String), // (trait, impl types, name)
    Is(StackOffset, StackOffset),

    RecordSet(StackOffset, String, StackOffset),
    TypeSet(StackOffset, String, StackOffset),
    Impl(StackOffset, Vec<StackOffset>, Vec<(String, StackOffset)>),
}

impl From<ExtendedInstruction> for Instruction {
    fn from(value: ExtendedInstruction) -> Self {
        Self::Extended(Box::new(value))
    }
}

#[derive(Debug)]
pub struct InstructionFunction {
    pub hints: String,
    pub num_capture: u8,
    pub num_parameter: u8,
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
    variables: Vec<Variable>,
    locals: Vec<Local>,
}

#[derive(Debug, Clone)]
enum Variable {
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
    code_offset: usize,
    code_id: InterpretedCodeId,
    variable_offset: usize,
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
        anyhow::ensure!(self.stack.variables.is_empty());
        assert!(self.stack.locals.is_empty());
        anyhow::ensure!(self.frames.is_empty());
        let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
        anyhow::ensure!(code.num_parameter == 0);
        let CodeSource::Interpreted(source) = &code.source else {
            anyhow::bail!("unsupported native code entry")
        };
        let code_id = source.id;
        self.stack.variables.extend(
            source
                .captures
                .iter()
                .map(|address| Variable::Address(address.clone())),
        );
        self.stack
            .variables
            .insert(0, Variable::Address(code_address));
        let frame = Frame {
            code_offset: 0,
            variable_offset: 1,
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
        let code = unsafe { stack.get_downcast_ref::<Code>(self.code_offset as _) }.unwrap();
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
                    use {ExtendedInstruction::*, Instruction::*, Variable::*};
                    match instruction {
                        Jump(offset) => {
                            frame.instruction_offset = *offset;
                            continue 'local_jump;
                        }
                        JumpUnless(i, offset) => {
                            if !unsafe { self.stack.get_bool(frame.variable_offset + *i as usize) }?
                            {
                                frame.instruction_offset = *offset;
                                continue 'local_jump;
                            }
                        }
                        Call(code_i, args_i, num_arg) => {
                            let Address(code_address) = self.stack.variables
                                [frame.variable_offset + *code_i as usize]
                                .clone()
                            else {
                                anyhow::bail!(TypeError {
                                    expected: type_name::<Code>(),
                                    content: format!(
                                        "{:?}",
                                        self.stack.variables
                                            [frame.variable_offset + *code_i as usize]
                                    )
                                })
                            };
                            let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
                            anyhow::ensure!(*num_arg == code.num_parameter);
                            match &code.source {
                                &CodeSource::Native(function) => {
                                    let arguments = args_i
                                        .iter()
                                        .map(|i| {
                                            self.stack
                                                .escape(frame.variable_offset + *i as usize, memory)
                                        })
                                        .collect::<Vec<_>>();
                                    let address = unsafe { function(&arguments, memory) }?;
                                    self.stack.variables.push(Address(address))
                                }
                                CodeSource::Interpreted(source) => {
                                    let stack_offset = self.stack.variables.len();
                                    let local_offset = self.stack.locals.len();
                                    let code_id = source.id;

                                    for address in &source.captures {
                                        self.stack.variables.push(Address(address.clone()))
                                    }
                                    for i in &args_i[..*num_arg as usize] {
                                        self.stack.variables.push(
                                            self.stack.variables
                                                [frame.variable_offset + *i as usize]
                                                .clone(),
                                        )
                                    }

                                    let frame = Frame {
                                        code_id,
                                        code_offset: frame.variable_offset + *code_i as usize,
                                        variable_offset: stack_offset as _,
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
                                let mut variable = self.stack.variables
                                    [popped_frame.variable_offset + *i as usize]
                                    .clone();
                                let mut locals_len = popped_frame.local_offset;
                                if let Local(index) = variable {
                                    if index >= locals_len {
                                        self.stack.locals[locals_len] = self.stack.locals[index];
                                        variable = Local(locals_len);
                                        locals_len += 1;
                                    }
                                }
                                self.stack.variables.truncate(popped_frame.variable_offset);
                                self.stack.variables.push(variable);
                                self.stack.locals.truncate(locals_len);
                                continue 'nonlocal_jump;
                            } else {
                                unsafe {
                                    self.stack
                                        .get_unit(popped_frame.variable_offset + *i as usize)
                                }?;
                                self.stack.variables.clear();
                                self.stack.locals.clear();
                                return Ok(());
                            };
                        }

                        Set(i, source_i) => {
                            if let (Local(index), Local(source_index)) = (
                                &self.stack.variables[frame.variable_offset + *i as usize],
                                &self.stack.variables[frame.variable_offset + *source_i as usize],
                            ) {
                                self.stack.locals[*index] = self.stack.locals[*source_index]
                            } else {
                                // TODO optimize some single side local cases
                                unsafe {
                                    self.stack
                                        .escape(frame.variable_offset + *i as usize, memory)
                                        .copy_from(&self.stack.escape(
                                            frame.variable_offset + *source_i as usize,
                                            memory,
                                        ))
                                }
                            }
                        }

                        Rewind(i) => {
                            anyhow::ensure!(self.stack.variables.len() >= *i as _);
                            let variable = self.stack.variables.pop().unwrap();
                            self.stack
                                .variables
                                .splice(frame.variable_offset + *i as usize.., [variable]);
                        }

                        LoadUnit => self.stack.push_local(crate::machine::Local::Unit),
                        LoadBool(b) => self.stack.push_local(crate::machine::Local::Bool(*b)),
                        LoadInt(int) => self.stack.push_local(crate::machine::Local::Int(*int)),

                        Extended(instruction) => match &**instruction {
                            LoadString(string) => self
                                .stack
                                .variables
                                .push(Address(memory.allocate_any(Box::new(string.clone())))),
                            LoadFunction(function, captures_i) => {
                                anyhow::ensure!(captures_i.len() == function.num_capture as _);
                                let captures = captures_i
                                    .iter()
                                    .map(|i| {
                                        self.stack
                                            .escape(frame.variable_offset + *i as usize, memory)
                                    })
                                    .collect();
                                self.stack
                                    .variables
                                    .push(Address(memory.allocate_any(Box::new(
                                        Code::new_interpreted(function, captures)?,
                                    ))))
                            }
                            LoadLayout(layout) => self.stack.variables.push(Address(
                                memory.allocate_any(Box::new(RecordLayout(layout.clone().into()))),
                            )),
                            LoadType(layout_i) => {
                                self.type_id_counter += 1;
                                let t = Type {
                                    id: self.type_id_counter,
                                    layout_address: layout_i.map(|i| {
                                        self.stack
                                            .escape(frame.variable_offset + i as usize, memory)
                                    }),
                                    attributes: Default::default(),
                                };
                                self.stack
                                    .variables
                                    .push(Address(memory.allocate_any(Box::new(t))))
                            }
                            LoadRecord(type_i, variants, fields) => {
                                let mut t = unsafe {
                                    self.stack.get_downcast_ref::<Type>(
                                        frame.variable_offset + *type_i as usize,
                                    )
                                }?;
                                for variant in variants {
                                    let Some(layout_address) = &t.layout_address else {
                                        anyhow::bail!(
                                            "attempting to construct record for native type"
                                        )
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
                                let offsets = layout
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
                                        .map(|i| {
                                            self.stack
                                                .escape(frame.variable_offset + i as usize, memory)
                                        })
                                        .collect(),
                                };
                                self.stack
                                    .variables
                                    .push(Address(memory.allocate_any(Box::new(record))))
                            }
                            LoadTrait(layout) => {
                                let t = Trait {
                                    layout: layout.clone().into(),
                                    implementations: Default::default(),
                                };
                                self.stack
                                    .variables
                                    .push(Address(memory.allocate_any(Box::new(t))))
                            }
                            LoadInjection(key) => {
                                let Some(address) = loader.injections.get(key) else {
                                    anyhow::bail!("intrinsic {key} not found")
                                };
                                self.stack.variables.push(Address(address.clone()))
                            }

                            RecordGet(i, name) => {
                                let record = unsafe {
                                    self.stack.get_downcast_ref::<Record>(
                                        frame.variable_offset + *i as usize,
                                    )
                                }?;
                                self.stack
                                    .variables
                                    .push(Address(record.get_ref(name)?.clone()))
                            }
                            RecordSet(i, name, j) => {
                                let address = self
                                    .stack
                                    .escape(frame.variable_offset + *j as usize, memory);
                                *unsafe {
                                    self.stack.get_downcast_mut::<Record>(
                                        frame.variable_offset + *i as usize,
                                    )
                                }?
                                .get_mut(name)? = address
                            }
                            TypeGet(i, name) => {
                                let Some(address) = unsafe {
                                    self.stack.get_downcast_ref::<Type>(
                                        frame.variable_offset + *i as usize,
                                    )
                                }?
                                .attributes
                                .get(name) else {
                                    anyhow::bail!("there is no attribute {name} on type object")
                                };
                                self.stack.variables.push(Address(address.clone()))
                            }
                            TypeSet(i, name, j) => {
                                let address = self
                                    .stack
                                    .escape(frame.variable_offset + *j as usize, memory);
                                let replaced = unsafe {
                                    self.stack.get_downcast_mut::<Type>(
                                        frame.variable_offset + *i as usize,
                                    )
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
                                    self.stack.get_downcast_ref::<Trait>(
                                        frame.variable_offset + *i as usize,
                                    )
                                }?;
                                let Some(p) =
                                    t.layout.iter().position(|other_name| other_name == name)
                                else {
                                    anyhow::bail!("field `{name}` not found in trait layout")
                                };
                                let mut k = Vec::new();
                                for i in implementor {
                                    let ty = unsafe {
                                        self.stack.get_downcast_ref::<Type>(
                                            frame.variable_offset + *i as usize,
                                        )
                                    }?;
                                    k.push(ty.id)
                                }
                                let Some(addresses) = t.implementations.get(&k) else {
                                    anyhow::bail!("implementation not found")
                                };
                                self.stack.variables.push(Address(addresses[p].clone()))
                            }
                            Impl(i, implementor, fields) => {
                                let t = unsafe {
                                    self.stack.get_downcast_ref::<Trait>(
                                        frame.variable_offset + *i as usize,
                                    )
                                }?;
                                let mut k = Vec::new();
                                for i in implementor {
                                    let t = unsafe {
                                        self.stack.get_downcast_ref::<Type>(
                                            frame.variable_offset + *i as usize,
                                        )
                                    }?;
                                    k.push(t.id)
                                }
                                let offsets = t
                                    .layout
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
                                    .map(|i| {
                                        self.stack
                                            .escape(frame.variable_offset + i as usize, memory)
                                    })
                                    .collect();
                                let replaced = unsafe {
                                    self.stack.get_downcast_mut::<Trait>(
                                        frame.variable_offset + *i as usize,
                                    )
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
                                    self.stack.get_downcast_ref::<Type>(
                                        frame.variable_offset + *j as usize,
                                    )
                                }?;
                                // TODO support other types
                                let l = unsafe {
                                    self.stack.get_downcast_ref::<Record>(
                                        frame.variable_offset + *i as usize,
                                    )
                                }?;
                                self.stack
                                    .push_local(crate::machine::Local::Bool(l.type_id == r.id))
                            }
                        },

                        IntOperator2(op, i, j) => {
                            let l =
                                unsafe { self.stack.get_int(frame.variable_offset + *i as usize) }?;
                            let r =
                                unsafe { self.stack.get_int(frame.variable_offset + *j as usize) }?;
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
                            let l =
                                unsafe { self.stack.get_int(frame.variable_offset + *i as usize) }?;
                            let r =
                                unsafe { self.stack.get_int(frame.variable_offset + *j as usize) }?;
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
        self.variables.push(Variable::Local(index))
    }

    fn escape(&mut self, i: usize, memory: &mut Memory) -> Address {
        let variable = &mut self.variables[i];
        match variable {
            Variable::Address(address) => address.clone(),
            Variable::Local(index) => {
                let address = match &self.locals[*index] {
                    Local::Unit => memory.allocate_unit(),
                    Local::Bool(b) => memory.allocate_bool(*b),
                    Local::Int(int) => memory.allocate_int(*int),
                };
                *variable = Variable::Address(address.clone());
                address
            }
        }
    }

    unsafe fn get_unit(&self, i: usize) -> anyhow::Result<()> {
        match &self.variables[i] {
            Variable::Address(address) => unsafe { address.get_unit() },
            Variable::Local(index) => {
                let Local::Unit = &self.locals[*index] else {
                    anyhow::bail!(TypeError {
                        expected: "bool",
                        content: format!("{:?}", self.variables[i])
                    })
                };
                Ok(())
            }
        }
    }

    unsafe fn get_bool(&self, i: usize) -> anyhow::Result<bool> {
        match &self.variables[i] {
            Variable::Address(address) => unsafe { address.get_bool() },
            Variable::Local(index) => {
                let Local::Bool(b) = &self.locals[*index] else {
                    anyhow::bail!(TypeError {
                        expected: "bool",
                        content: format!("{:?}", self.variables[i])
                    })
                };
                Ok(*b)
            }
        }
    }

    unsafe fn get_int(&self, i: usize) -> anyhow::Result<i32> {
        match &self.variables[i] {
            Variable::Address(address) => unsafe { address.get_int() },
            Variable::Local(index) => {
                let Local::Int(int) = &self.locals[*index] else {
                    anyhow::bail!(TypeError {
                        expected: "int",
                        content: format!("{:?}", self.variables[i])
                    })
                };
                Ok(*int)
            }
        }
    }

    unsafe fn get_downcast_ref<T: 'static>(&self, i: usize) -> anyhow::Result<&T> {
        let Variable::Address(address) = &self.variables[i] else {
            anyhow::bail!(TypeError {
                expected: type_name::<T>(),
                content: format!("{:?}", self.variables[i])
            })
        };
        address.get_downcast_ref()
    }

    unsafe fn get_downcast_mut<T: 'static>(&self, i: usize) -> anyhow::Result<&mut T> {
        let Variable::Address(address) = &self.variables[i] else {
            anyhow::bail!(TypeError {
                expected: type_name::<T>(),
                content: format!("{:?}", self.variables[i])
            })
        };
        address.get_downcast_mut()
    }
}

#[derive(Debug)]
pub struct Code {
    hints: String,
    num_parameter: u8,
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
        anyhow::ensure!(captures.len() == function.num_capture as _);
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
        num_parameter: u8,
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
