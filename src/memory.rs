use std::any::Any;

// not impl Copy and Default intentionally
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address(u64);

#[derive(Debug, Default)]
pub struct Memory {
    //
}

#[allow(unused_variables)]
impl Memory {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn allocate_unit(&mut self) -> Address {
        Address(0)
    }

    pub fn allocate_bool(&mut self, b: bool) -> Address {
        Address(0)
    }

    pub fn allocate_int(&mut self, int: i32) -> Address {
        Address(0)
    }

    pub fn allocate_any(&mut self, any: Box<dyn Any>) -> Address {
        Address(0)
    }
}

impl Address {
    #[allow(unused_variables)]
    pub fn copy_from(&mut self, source_address: &Address) -> anyhow::Result<()> {
        Ok(())
    }

    pub fn get_unit(&self) -> anyhow::Result<()> {
        Ok(())
    }

    pub fn get_bool(&self) -> anyhow::Result<bool> {
        todo!()
    }

    pub fn get_int(&self) -> anyhow::Result<i32> {
        todo!()
    }

    pub fn get_any_ref(&self) -> anyhow::Result<&dyn Any> {
        todo!()
    }

    pub fn get_any_mut(&mut self) -> anyhow::Result<&mut dyn Any> {
        todo!()
    }
}
