use std::{
    any::type_name,
    cmp::Ordering::{Equal, Greater, Less},
    collections::BTreeMap,
    fmt::Display,
    sync::Arc,
};

use derive_more::Deref;

use crate::{
    loader::{InterpretedCodeId, Loader},
    memory::{Address, Memory, TypeError},
};

pub type StackOffset = u16;
pub type InstructionOffset = u16;

#[derive(Debug)]
pub enum Instruction {
    LoadUnit,
    LoadBool(bool),
    LoadInt(i32),

    IntOperator2(NumericalOperator2, StackOffset, StackOffset),
    BitwiseOperator2(BitwiseOperator2, StackOffset, StackOffset),
    Is(StackOffset, StackOffset),

    Set(StackOffset, StackOffset),
    // copy current offset to the index, reset current offset to the index
    Rewind(StackOffset),

    Jump(InstructionOffset),
    JumpUnless(StackOffset, InstructionOffset),

    Call(StackOffset, u16, u8),
    Return(StackOffset),

    Extended(InstructionOffset),
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

    RecordSet(StackOffset, String, StackOffset),
    TypeSet(StackOffset, String, StackOffset),
    Impl(StackOffset, Vec<StackOffset>, Vec<(String, StackOffset)>),
}

#[derive(Debug)]
pub struct InstructionFunction {
    pub hints: String,
    pub num_capture: u8,
    pub num_parameter: u8,
    pub id: InterpretedCodeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    variable_indexes: Vec<usize>,
    variables: Vec<Variable>,
}

#[derive(Debug, Clone)]
enum Variable {
    Address(Address),
    Inline(Inline),
}

#[derive(Debug, Clone, Copy)]
enum Inline {
    Unit,
    Bool(bool),
    Int(i32),
}

#[derive(Debug)]
struct Frame {
    code_id: InterpretedCodeId,
    // offsets into Stack's `variable_indexes`
    // these are the base offsets of `StackOffset` types, but themselves are not `StackOffset` typed
    // because `StackOffset` accounts for offset within one frame, while these are accumulated
    // offsets over the whole frame stack
    code_offset: usize,
    stack_offset: usize,
    // offset into LoaderCode's instructions
    instruction_offset: InstructionOffset,
    // offset into Stack's `variables`
    variable_offset: usize,
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
        anyhow::ensure!(self.stack.variable_indexes.is_empty());
        assert!(self.stack.variables.is_empty());
        anyhow::ensure!(self.frames.is_empty());
        let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
        anyhow::ensure!(code.num_parameter == 0);
        let CodeSource::Interpreted(source) = &code.source else {
            anyhow::bail!("unsupported native code entry")
        };
        let code_id = source.id;
        self.stack.push_address(code_address.clone());
        for address in &source.captures {
            self.stack.push_address(address.clone())
        }
        let frame = Frame {
            code_offset: 0,
            stack_offset: 1,
            instruction_offset: 0,
            variable_offset: 0,
            code_id,
        };
        self.frames.push(frame);
        self.evaluate(memory, loader)
    }
}

#[derive(Debug)]
pub struct BacktraceLine {
    pub code_hints: String,
    pub instruction_offset: InstructionOffset,
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
        // TODO decide whether necessary to enable this optimization
        // let mut arguments = Vec::new(); // reused buffer for native calls
        'nonlocal_jump: loop {
            let Some(frame) = self.frames.last_mut() else {
                anyhow::bail!("evaluating frame is missing")
            };
            let code = &loader.code[frame.code_id];
            'local_jump: loop {
                for instruction in &code.instructions[frame.instruction_offset as usize..] {
                    tracing::trace!("{instruction:?}");
                    frame.instruction_offset += 1;
                    use {ExtendedInstruction::*, Instruction::*};
                    match instruction {
                        Jump(offset) => {
                            frame.instruction_offset = *offset;
                            continue 'local_jump;
                        }
                        JumpUnless(i, offset) => {
                            if !unsafe { self.stack.get_bool(frame.stack_offset + *i as usize) }? {
                                frame.instruction_offset = *offset;
                                continue 'local_jump;
                            }
                        }
                        Call(code_i, arg_offsets_i, num_arg) => {
                            let args_i = &code.arg_offsets[*arg_offsets_i as usize
                                ..*arg_offsets_i as usize + *num_arg as usize];
                            let code_address = self
                                .stack
                                // this escape is not expected to actually escape anything
                                // just abusing it to clone the address
                                .escape(frame.stack_offset + *code_i as usize, memory);
                            // this should catch any mis-escape without significant damage
                            let code = unsafe { code_address.get_downcast_ref::<Code>() }?;
                            anyhow::ensure!(*num_arg == code.num_parameter);
                            match &code.source {
                                &CodeSource::Native(function) => {
                                    // arguments.extend(args_i
                                    //     .iter()
                                    //     .map(|i| {
                                    //         self.stack
                                    //             .escape(frame.stack_offset + *i as usize, memory)
                                    //     }));
                                    let arguments = args_i
                                        .iter()
                                        .map(|i| {
                                            self.stack
                                                .escape(frame.stack_offset + *i as usize, memory)
                                        })
                                        .collect::<Vec<_>>();
                                    let address = unsafe { function(&arguments, memory) }?;
                                    // arguments.drain(..);
                                    self.stack.push_address(address)
                                }
                                CodeSource::Interpreted(source) => {
                                    let stack_offset = self.stack.variable_indexes.len();
                                    let code_id = source.id;

                                    for address in &source.captures {
                                        self.stack.push_address(address.clone())
                                    }
                                    for i in args_i {
                                        self.stack.variable_indexes.push(
                                            self.stack.variable_indexes
                                                [frame.stack_offset + *i as usize],
                                        )
                                    }

                                    let frame = Frame {
                                        code_id,
                                        code_offset: frame.stack_offset + *code_i as usize,
                                        stack_offset,
                                        instruction_offset: 0,
                                        variable_offset: self.stack.variables.len(),
                                    };
                                    self.frames.push(frame);
                                    continue 'nonlocal_jump;
                                }
                            }
                        }
                        Return(i) => {
                            let popped_frame = self.frames.pop().unwrap();
                            if !self.frames.is_empty() {
                                let mut return_offset = self.stack.variable_indexes
                                    [popped_frame.stack_offset + *i as usize];
                                let mut variables_len = popped_frame.variable_offset;
                                if return_offset >= popped_frame.variable_offset {
                                    self.stack.variables[variables_len] =
                                        self.stack.variables[return_offset].clone();
                                    return_offset = variables_len;
                                    variables_len += 1
                                }
                                self.stack.variables.truncate(variables_len);
                                // this is slower, not sure why
                                // self.stack
                                //     .variable_indexes
                                //     .splice(popped_frame.stack_offset.., [return_offset]);
                                self.stack
                                    .variable_indexes
                                    .truncate(popped_frame.stack_offset);
                                self.stack.variable_indexes.push(return_offset);
                                continue 'nonlocal_jump;
                            } else {
                                unsafe {
                                    self.stack.get_unit(popped_frame.stack_offset + *i as usize)
                                }?;
                                self.stack.variable_indexes.clear();
                                self.stack.variables.clear();
                                return Ok(());
                            };
                        }

                        Set(i, source_i) => {
                            let index =
                                self.stack.variable_indexes[frame.stack_offset + *i as usize];
                            let source_index = self.stack.variable_indexes
                                [frame.stack_offset + *source_i as usize];
                            'set: {
                                let (variable, source_variable) = match index.cmp(&source_index) {
                                    Less => {
                                        let (variables, other_variables) =
                                            self.stack.variables.split_at_mut(index + 1);
                                        (
                                            variables.last_mut().unwrap(),
                                            &other_variables[source_index - index - 1],
                                        )
                                    }
                                    Greater => {
                                        let (variables, other_variables) =
                                            self.stack.variables.split_at_mut(source_index + 1);
                                        (
                                            &mut other_variables[index - source_index - 1],
                                            variables.last().unwrap(),
                                        )
                                    }
                                    Equal => break 'set, // no-op silly case, probably never happen
                                };
                                if let (Variable::Inline(inline), Variable::Inline(source_inline)) =
                                    (variable, source_variable)
                                {
                                    *inline = *source_inline
                                } else {
                                    // TODO optimize for single inline cases
                                    unsafe {
                                        self.stack
                                            .escape(frame.stack_offset + *i as usize, memory)
                                            .copy_from(&self.stack.escape(
                                                frame.stack_offset + *source_i as usize,
                                                memory,
                                            ))
                                    }
                                }
                            }
                        }

                        Rewind(i) => {
                            let len = self.stack.variable_indexes.len();
                            anyhow::ensure!(len >= frame.stack_offset + *i as usize);
                            self.stack
                                .variable_indexes
                                .truncate(frame.stack_offset + *i as usize)
                        }

                        LoadUnit => self.stack.push_local(Inline::Unit),
                        LoadBool(b) => self.stack.push_local(Inline::Bool(*b)),
                        LoadInt(int) => self.stack.push_local(Inline::Int(*int)),

                        Extended(offset) => match &code.extended_instructions[*offset as usize] {
                            LoadString(string) => self
                                .stack
                                .push_address(memory.allocate_any(Box::new(string.clone()))),
                            LoadFunction(function, captures_i) => {
                                anyhow::ensure!(captures_i.len() == function.num_capture as _);
                                let captures = captures_i
                                    .iter()
                                    .map(|i| {
                                        self.stack.escape(frame.stack_offset + *i as usize, memory)
                                    })
                                    .collect();
                                self.stack.push_address(memory.allocate_any(Box::new(
                                    Code::new_interpreted(function, captures)?,
                                )))
                            }
                            LoadLayout(layout) => self.stack.push_address(
                                memory.allocate_any(Box::new(RecordLayout(layout.clone().into()))),
                            ),
                            LoadType(layout_i) => {
                                self.type_id_counter += 1;
                                let t = Type {
                                    id: self.type_id_counter,
                                    layout_address: layout_i.map(|i| {
                                        self.stack.escape(frame.stack_offset + i as usize, memory)
                                    }),
                                    attributes: Default::default(),
                                };
                                self.stack.push_address(memory.allocate_any(Box::new(t)))
                            }
                            LoadRecord(type_i, variants, fields) => {
                                let mut t = unsafe {
                                    self.stack.get_downcast_ref::<Type>(
                                        frame.stack_offset + *type_i as usize,
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
                                                .escape(frame.stack_offset + i as usize, memory)
                                        })
                                        .collect(),
                                };
                                self.stack
                                    .push_address(memory.allocate_any(Box::new(record)))
                            }
                            LoadTrait(layout) => {
                                let t = Trait {
                                    layout: layout.clone().into(),
                                    implementations: Default::default(),
                                };
                                self.stack.push_address(memory.allocate_any(Box::new(t)))
                            }
                            LoadInjection(key) => {
                                let Some(address) = loader.injections.get(key) else {
                                    anyhow::bail!("intrinsic {key} not found")
                                };
                                self.stack.push_address(address.clone())
                            }

                            RecordGet(i, name) => {
                                let record = unsafe {
                                    self.stack.get_downcast_ref::<Record>(
                                        frame.stack_offset + *i as usize,
                                    )
                                }?;
                                self.stack.push_address(record.get_ref(name)?.clone())
                            }
                            RecordSet(i, name, j) => {
                                let address =
                                    self.stack.escape(frame.stack_offset + *j as usize, memory);
                                *unsafe {
                                    self.stack.get_downcast_mut::<Record>(
                                        frame.stack_offset + *i as usize,
                                    )
                                }?
                                .get_mut(name)? = address
                            }
                            TypeGet(i, name) => {
                                let Some(address) = unsafe {
                                    self.stack
                                        .get_downcast_ref::<Type>(frame.stack_offset + *i as usize)
                                }?
                                .attributes
                                .get(name) else {
                                    anyhow::bail!("there is no attribute {name} on type object")
                                };
                                self.stack.push_address(address.clone())
                            }
                            TypeSet(i, name, j) => {
                                let address =
                                    self.stack.escape(frame.stack_offset + *j as usize, memory);
                                let replaced = unsafe {
                                    self.stack
                                        .get_downcast_mut::<Type>(frame.stack_offset + *i as usize)
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
                                        .get_downcast_ref::<Trait>(frame.stack_offset + *i as usize)
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
                                            frame.stack_offset + *i as usize,
                                        )
                                    }?;
                                    k.push(ty.id)
                                }
                                let Some(addresses) = t.implementations.get(&k) else {
                                    anyhow::bail!("implementation not found")
                                };
                                self.stack.push_address(addresses[p].clone())
                            }
                            Impl(i, implementor, fields) => {
                                let t = unsafe {
                                    self.stack
                                        .get_downcast_ref::<Trait>(frame.stack_offset + *i as usize)
                                }?;
                                let mut k = Vec::new();
                                for i in implementor {
                                    let t = unsafe {
                                        self.stack.get_downcast_ref::<Type>(
                                            frame.stack_offset + *i as usize,
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
                                        self.stack.escape(frame.stack_offset + i as usize, memory)
                                    })
                                    .collect();
                                let replaced = unsafe {
                                    self.stack
                                        .get_downcast_mut::<Trait>(frame.stack_offset + *i as usize)
                                }
                                .unwrap()
                                .implementations
                                .insert(k, addresses);
                                anyhow::ensure!(
                                    replaced.is_none(),
                                    "duplicated implementation of trait"
                                )
                            }
                        },

                        Is(i, j) => {
                            let r = unsafe {
                                self.stack
                                    .get_downcast_ref::<Type>(frame.stack_offset + *j as usize)
                            }?;
                            // TODO support other types
                            let l = unsafe {
                                self.stack
                                    .get_downcast_ref::<Record>(frame.stack_offset + *i as usize)
                            }?;
                            self.stack
                                .push_local(crate::machine::Inline::Bool(l.type_id == r.id))
                        }
                        IntOperator2(op, i, j) => {
                            let l =
                                unsafe { self.stack.get_int(frame.stack_offset + *i as usize) }?;
                            let r =
                                unsafe { self.stack.get_int(frame.stack_offset + *j as usize) }?;
                            tracing::trace!("  {op:?} {l} {r}");
                            use {Inline::*, NumericalOperator2::*};
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
                                unsafe { self.stack.get_int(frame.stack_offset + *i as usize) }?;
                            let r =
                                unsafe { self.stack.get_int(frame.stack_offset + *j as usize) }?;
                            use crate::machine::{BitwiseOperator2::*, Inline::*};
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
    fn push_local(&mut self, inline: Inline) {
        self.variable_indexes.push(self.variables.len());
        self.variables.push(Variable::Inline(inline))
    }

    fn push_address(&mut self, address: Address) {
        self.variable_indexes.push(self.variables.len());
        self.variables.push(Variable::Address(address))
    }

    fn escape(&mut self, i: usize, memory: &mut Memory) -> Address {
        let variable = &mut self.variables[self.variable_indexes[i]];
        match variable {
            Variable::Address(address) => address.clone(),
            Variable::Inline(inline) => {
                let address = match inline {
                    Inline::Unit => memory.allocate_unit(),
                    Inline::Bool(b) => memory.allocate_bool(*b),
                    Inline::Int(int) => memory.allocate_int(*int),
                };
                *variable = Variable::Address(address.clone());
                address
            }
        }
    }

    unsafe fn get_unit(&self, i: usize) -> anyhow::Result<()> {
        let variable = &self.variables[self.variable_indexes[i]];
        match variable {
            Variable::Address(address) => unsafe { address.get_unit() },
            Variable::Inline(Inline::Unit) => Ok(()),
            _ => anyhow::bail!(TypeError {
                expected: "Unit",
                content: format!("{variable:?}")
            }),
        }
    }

    unsafe fn get_bool(&self, i: usize) -> anyhow::Result<bool> {
        let variable = &self.variables[self.variable_indexes[i]];
        match variable {
            Variable::Address(address) => unsafe { address.get_bool() },
            Variable::Inline(Inline::Bool(b)) => Ok(*b),
            _ => anyhow::bail!(TypeError {
                expected: "Bool",
                content: format!("{variable:?}")
            }),
        }
    }

    unsafe fn get_int(&self, i: usize) -> anyhow::Result<i32> {
        let variable = &self.variables[self.variable_indexes[i]];
        match variable {
            Variable::Address(address) => unsafe { address.get_int() },
            Variable::Inline(Inline::Int(int)) => Ok(*int),
            _ => anyhow::bail!(TypeError {
                expected: "Int",
                content: format!("{variable:?}")
            }),
        }
    }

    unsafe fn get_downcast_ref<T: 'static>(&self, i: usize) -> anyhow::Result<&T> {
        let variable = &self.variables[self.variable_indexes[i]];
        let Variable::Address(address) = variable else {
            anyhow::bail!(TypeError {
                expected: type_name::<T>(),
                content: format!("{variable:?}")
            })
        };
        unsafe { address.get_downcast_ref() }
    }

    unsafe fn get_downcast_mut<T: 'static>(&self, i: usize) -> anyhow::Result<&mut T> {
        let variable = &self.variables[self.variable_indexes[i]];
        let Variable::Address(address) = variable else {
            anyhow::bail!(TypeError {
                expected: type_name::<T>(),
                content: format!("{variable:?}")
            })
        };
        unsafe { address.get_downcast_mut() }
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
