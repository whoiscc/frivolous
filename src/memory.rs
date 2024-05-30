use std::any::Any;

pub type Address = u64;

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
        0
    }

    pub fn allocate_bool(&mut self, b: bool) -> Address {
        0
    }

    pub fn allocate_int(&mut self, int: i32) -> Address {
        0
    }

    pub fn allocate_any(&mut self, any: Box<dyn Any>) -> Address {
        0
    }

    pub fn copy(&mut self, address: Address, source_address: Address) -> anyhow::Result<()> {
        Ok(())
    }

    pub fn get_unit(&self, address: Address) -> anyhow::Result<()> {
        Ok(())
    }

    pub fn get_bool(&self, address: Address) -> anyhow::Result<bool> {
        todo!()
    }

    pub fn get_int(&self, address: Address) -> anyhow::Result<i32> {
        todo!()
    }

    pub fn get_any_ref(&self, address: Address) -> anyhow::Result<&dyn Any> {
        todo!()
    }

    pub fn get_any_mut(&self, address: Address) -> anyhow::Result<&mut dyn Any> {
        todo!()
    }
}
