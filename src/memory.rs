use std::{
    any::{type_name, Any},
    sync::Arc,
};

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

#[derive(Debug, derive_more::Display, derive_more::Error)]
#[display(fmt = "TypeError: expected {expected} from content {content}")]
pub struct TypeError {
    pub expected: &'static str,
    pub content: String,
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
        let content = unsafe { self.content_ref() };
        let BlockContent::Unit = content else {
            anyhow::bail!(TypeError {
                expected: "Unit",
                content: format!("{content:?}")
            })
        };
        Ok(())
    }

    pub unsafe fn get_bool(&self) -> anyhow::Result<bool> {
        let content = unsafe { self.content_ref() };
        let BlockContent::Bool(b) = content else {
            anyhow::bail!(TypeError {
                expected: "Bool",
                content: format!("{content:?}")
            })
        };
        Ok(*b)
    }

    pub unsafe fn get_int(&self) -> anyhow::Result<i32> {
        let content = unsafe { self.content_ref() };
        let BlockContent::Int(int) = content else {
            anyhow::bail!(TypeError {
                expected: "Int",
                content: format!("{content:?}")
            })
        };
        Ok(*int)
    }

    pub unsafe fn get_any_ref(&self) -> anyhow::Result<&dyn Any> {
        let content = unsafe { self.content_ref() };
        let BlockContent::Any(any) = content else {
            anyhow::bail!(TypeError {
                expected: "(dynamical typed)",
                content: format!("{content:?}")
            })
        };
        any.get_ref()
    }

    // need to rethink about mutability rules
    pub unsafe fn get_any_mut(&self) -> anyhow::Result<&mut dyn Any> {
        let content = unsafe { self.content_ref() };
        let BlockContent::Any(any) = content else {
            anyhow::bail!(TypeError {
                expected: "(dynamical typed)",
                content: format!("{content:?}")
            })
        };
        any.get_mut()
    }

    pub unsafe fn get_downcast_ref<T: 'static>(&self) -> anyhow::Result<&T> {
        let any = unsafe { self.get_any_ref() }.map_err(|mut err| {
            if let Some(err) = err.downcast_mut::<TypeError>() {
                err.expected = type_name::<T>()
            }
            err
        })?;
        any.downcast_ref().ok_or(
            TypeError {
                expected: type_name::<T>(),
                content: format!("Any(id = {:?})", any.type_id()),
            }
            .into(),
        )
    }

    pub unsafe fn get_downcast_mut<T: 'static>(&self) -> anyhow::Result<&mut T> {
        let any = unsafe { self.get_any_mut() }.map_err(|mut err| {
            if let Some(err) = err.downcast_mut::<TypeError>() {
                err.expected = type_name::<T>()
            }
            err
        })?;
        let type_error = TypeError {
            expected: type_name::<T>(),
            content: format!("Any(id = {:?})", (any as &dyn Any).type_id()),
        };
        any.downcast_mut().ok_or(type_error.into())
    }
}

impl BlockContentAny {
    fn new(any: Box<dyn Any>) -> Self {
        Self {
            data: Box::leak(any) as _,
        }
    }

    fn get_ref(&self) -> anyhow::Result<&dyn Any> {
        Ok(unsafe { &*self.data })
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
