pub type FunctionId = usize;
pub type StackOffset = usize;
pub type InstructionOffset = usize;

#[derive(Debug)]
pub enum Instruction {
    LoadUnit,
    LoadBool(bool),
    LoadInt(i32),
    LoadString(String),
    LoadFunction(FunctionId),
    LoadLayout(Vec<String>),
    LoadType(StackOffset), // of layout
    LoadRecord(StackOffset, Vec<(String, StackOffset)>),
    LoadTrait(Vec<String>),

    // copy current offset to the index, reset current offset to the index
    Rewind(StackOffset),

    RecordGet(StackOffset, String),
    TypeGet(StackOffset, String),
    TraitGet(StackOffset, Vec<StackOffset>, String), // (trait, impl types, name)
    Operator2(InstructionOperator2, StackOffset, StackOffset),
    Call(StackOffset, Vec<StackOffset>),
    Match(StackOffset, StackOffset), // (matched, type)

    Set(StackOffset, StackOffset),
    RecordSet(StackOffset, String, StackOffset),
    TypeSet(StackOffset, String, StackOffset),
    Impl(StackOffset, Vec<StackOffset>, Vec<(String, StackOffset)>),

    Jump(InstructionOffset),
    JumpIf(StackOffset, InstructionOffset),
    Return(StackOffset),
}

#[derive(Debug)]
pub enum InstructionOperator2 {
    Add,
    Sub,
    BitAnd,
    BitOr,
    ShiftLeft,
    ShiftRight,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}
