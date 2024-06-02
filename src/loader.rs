use std::collections::BTreeMap;

use crate::{
    machine::{Code, CodeExecutable::Native},
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
    pub intrinsics: BTreeMap<String, Address>,
}

impl Loader {
    pub fn new() -> Self {
        Self::default()
    }

    fn load_builtin_code(
        &mut self,
        memory: &mut Memory,
        key: &str,
        num_parameter: usize,
        function: unsafe fn(&[Address], &mut Memory) -> anyhow::Result<Address>,
    ) {
        let address = memory.allocate_any(Box::new(Code {
            hints: format!("<builtin {key}>"),
            captures: Default::default(),
            num_parameter,
            executable: Native(function),
        }));
        let replaced = self.intrinsics.insert(key.into(), address);
        assert!(replaced.is_none())
    }

    pub fn load_builtin(&mut self, memory: &mut Memory) {
        use builtin::*;
        self.load_builtin_code(memory, "int_debug_format", 1, int_debug_format);
        self.load_builtin_code(memory, "int_display_format", 1, int_display_format);
        self.load_builtin_code(memory, "string_debug_format", 1, string_debug_format);
        self.load_builtin_code(memory, "string_append", 2, string_append);
        self.load_builtin_code(memory, "trace", 1, trace);
    }
}
