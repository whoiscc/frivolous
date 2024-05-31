use std::{any::Any, sync::Arc};

// not impl Copy and Default intentionally
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address(u64);

#[derive(Debug, Default)]
pub struct Memory {
    previous_address: u64,
}

#[derive(Debug)]
struct Block {
    #[allow(unused)]
    previous_address: u64,
    content: BlockContent,
}

#[derive(Debug, Clone)]
enum BlockContent {
    Unit,
    Bool(bool),
    Int(i32),
    Any(Arc<BlockContentAny>),
}

#[derive(Debug)]
struct BlockContentAny {
    data: *mut dyn Any,
    // TODO concurrency control
}

unsafe impl Send for BlockContentAny {}
unsafe impl Sync for BlockContentAny {}

impl Memory {
    pub fn new() -> Self {
        Self::default()
    }

    fn allocate(&mut self, content: BlockContent) -> Address {
        let block = Block {
            previous_address: self.previous_address,
            content,
        };
        let address = Box::leak(Box::new(block)) as *mut _ as _;
        self.previous_address = address;
        Address(address)
    }

    pub fn allocate_unit(&mut self) -> Address {
        self.allocate(BlockContent::Unit)
    }

    pub fn allocate_bool(&mut self, b: bool) -> Address {
        self.allocate(BlockContent::Bool(b))
    }

    pub fn allocate_int(&mut self, int: i32) -> Address {
        self.allocate(BlockContent::Int(int))
    }

    pub fn allocate_any(&mut self, any: Box<dyn Any>) -> Address {
        self.allocate(BlockContent::Any(Arc::new(BlockContentAny::new(any))))
    }
}

#[allow(clippy::missing_safety_doc)]
impl Address {
    unsafe fn content_ref(&self) -> &BlockContent {
        &unsafe { &*(self.0 as *const Block) }.content
    }

    unsafe fn content_mut(&mut self) -> &mut BlockContent {
        &mut unsafe { &mut *(self.0 as *mut Block) }.content
    }

    pub unsafe fn copy_from(&mut self, source_address: &Address) {
        unsafe { self.content_mut() }.clone_from(unsafe { source_address.content_ref() })
    }

    pub unsafe fn get_unit(&self) -> anyhow::Result<()> {
        let BlockContent::Unit = (unsafe { self.content_ref() }) else {
            anyhow::bail!("expecting Unit object")
        };
        Ok(())
    }

    pub unsafe fn get_bool(&self) -> anyhow::Result<bool> {
        let BlockContent::Bool(b) = (unsafe { self.content_ref() }) else {
            anyhow::bail!("expecting Bool object")
        };
        Ok(*b)
    }

    pub unsafe fn get_int(&self) -> anyhow::Result<i32> {
        let BlockContent::Int(int) = (unsafe { self.content_ref() }) else {
            anyhow::bail!("expecting Int object")
        };
        Ok(*int)
    }

    pub unsafe fn get_any_ref(&self) -> anyhow::Result<&dyn Any> {
        let BlockContent::Any(any) = (unsafe { self.content_ref() }) else {
            anyhow::bail!("expecting dynamical typed object")
        };
        any.get_ref()
    }

    // need to rethink about mutability rules
    pub unsafe fn get_any_mut(&mut self) -> anyhow::Result<&mut dyn Any> {
        let BlockContent::Any(any) = (unsafe { self.content_ref() }) else {
            anyhow::bail!("expecting dynamical typed object")
        };
        any.get_mut()
    }
}

impl BlockContentAny {
    fn new(any: Box<dyn Any>) -> Self {
        Self {
            data: Box::leak(any) as _,
        }
    }

    fn get_ref(&self) -> anyhow::Result<&dyn Any> {
        Ok(&self.data)
    }

    fn get_mut(&self) -> anyhow::Result<&mut dyn Any> {
        // TODO
        Ok(unsafe { &mut *self.data })
    }
}

impl Drop for BlockContentAny {
    fn drop(&mut self) {
        drop(unsafe { Box::from_raw(self.data) })
    }
}
