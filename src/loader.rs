use std::collections::BTreeMap;

use crate::{
    machine::{Code, ExtendedInstruction, Instruction, StackOffset},
    memory::{Address, Memory},
};

mod builtin {
    use crate::memory::{Address, Memory};

    pub unsafe fn int_debug_format(
        args: &[Address],
        memory: &mut Memory,
    ) -> anyhow::Result<Address> {
        let int = unsafe { args[0].get_int() }?;
        Ok(memory.allocate_any(Box::new(format!("{int:?}"))))
    }

    pub unsafe fn int_display_format(
        args: &[Address],
        memory: &mut Memory,
    ) -> anyhow::Result<Address> {
        let int = unsafe { args[0].get_int() }?;
        Ok(memory.allocate_any(Box::new(format!("{int}"))))
    }

    pub unsafe fn string_debug_format(
        args: &[Address],
        memory: &mut Memory,
    ) -> anyhow::Result<Address> {
        let string = unsafe { args[0].get_downcast_ref::<String>() }?;
        Ok(memory.allocate_any(Box::new(format!("{string:?}"))))
    }

    pub unsafe fn string_append(args: &[Address], memory: &mut Memory) -> anyhow::Result<Address> {
        let string1 = unsafe { args[0].get_downcast_mut::<String>() }?;
        let string2 = unsafe { args[1].get_downcast_ref::<String>() }?;
        string1.push_str(string2);
        Ok(memory.allocate_unit())
    }

    pub unsafe fn trace(args: &[Address], memory: &mut Memory) -> anyhow::Result<Address> {
        let string = unsafe { args[0].get_downcast_ref::<String>() }?;
        tracing::info!("{string}");
        Ok(memory.allocate_unit())
    }
}

#[derive(Debug, Default)]
pub struct Loader {
    pub injections: BTreeMap<String, Address>,
    pub code: Vec<LoaderCode>,
}

#[derive(Debug, Default)]
pub struct LoaderCode {
    pub instructions: Vec<Instruction>,
    pub extended_instructions: Vec<ExtendedInstruction>,
    pub arg_offsets: Vec<StackOffset>,
}

impl LoaderCode {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    pub fn add_call(&mut self, code_i: StackOffset, args_i: Vec<StackOffset>) {
        let args_offset = self.arg_offsets.len() as _;
        let num_arg = args_i.len() as _;
        self.arg_offsets.extend(args_i);
        self.instructions
            .push(Instruction::Call(code_i, args_offset, num_arg))
    }

    pub fn add_extended(&mut self, instruction: ExtendedInstruction) {
        self.instructions
            .push(Instruction::Extended(self.extended_instructions.len() as _));
        self.extended_instructions.push(instruction)
    }
}

impl Loader {
    pub fn new() -> Self {
        Self::default()
    }

    fn inject_builtin_code(
        &mut self,
        memory: &mut Memory,
        key: &str,
        num_parameter: u8,
        function: unsafe fn(&[Address], &mut Memory) -> anyhow::Result<Address>,
    ) {
        let address = memory.allocate_any(Box::new(Code::new_native(
            format!("<builtin {key}>"),
            num_parameter,
            function,
        )));
        let replaced = self.injections.insert(key.into(), address);
        assert!(replaced.is_none())
    }

    pub fn inject_builtin(&mut self, memory: &mut Memory) {
        use builtin::*;
        self.inject_builtin_code(memory, "int_debug_format", 1, int_debug_format);
        self.inject_builtin_code(memory, "int_display_format", 1, int_display_format);
        self.inject_builtin_code(memory, "string_debug_format", 1, string_debug_format);
        self.inject_builtin_code(memory, "string_append", 2, string_append);
        self.inject_builtin_code(memory, "trace", 1, trace);
    }
}

pub type InterpretedCodeId = usize;

impl Loader {
    pub fn add_code(&mut self, code: LoaderCode) -> InterpretedCodeId {
        let id = self.code.len();
        self.code.push(code);
        id
    }
}
