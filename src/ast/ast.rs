use std::path::PathBuf;

#[derive(Debug, PartialEq, Eq)]
pub enum ASTNodeType {
    Call,
    IntFlags,
    StrFlags,
    Resource,
    Struct,
    TypeDef,
}

#[derive(Debug, Clone)]
pub struct Pos {
    pub file: PathBuf,
    pub line: usize,
    pub col: usize,
}

pub trait ASTNode {
    fn get_type(&self) -> ASTNodeType;

    fn get_name(&self) -> &str;

    fn get_pos(&self) -> &Pos;
}

#[derive(Debug)]
pub struct Ident {
    pub pos: Pos,
    pub name: String,
}

#[derive(Debug)]
pub struct Type {
    pub pos: Pos,
    pub value: u64,
    pub value_fmt: IntFmt,
    pub ident: String,
    pub string: Option<String>,
    pub string_fmt: Option<StrFmt>,
    pub colon: Vec<Type>,
    pub args: Vec<Type>,
}

#[derive(Debug)]
pub struct Field {
    pos: Pos,
    name: Ident,
    typ: Type,
    attrs: Vec<Type>,
    new_block: bool,
}

impl Field {
    pub fn get_name(&self) -> &str {
        &self.name.name
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IntFmt {
    Dec,
    Neg,
    Hex,
    Char,
}

#[derive(Debug)]
pub struct ASTInt {
    pub pos: Pos,
    // one of the value, ident, cexpr is filled
    pub value: Option<(u64, IntFmt)>,
    pub ident: Option<String>,
    pub cexpr: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StrFmt {
    Raw,
    Hex,
}

#[derive(Debug)]
pub struct ASTString {
    pub pos: Pos,
    pub value: Vec<u8>,
    pub fmt: StrFmt,
}

#[derive(Debug)]
pub struct IntFlagsNode {
    pub pos: Pos,
    pub name: Ident,
    pub values: Vec<ASTInt>,
}

#[derive(Debug)]
pub struct StrFlagsNode {
    pub pos: Pos,
    pub name: Ident,
    pub values: Vec<ASTString>,
}

impl ASTNode for StrFlagsNode {
    fn get_name(&self) -> &str {
        &self.name.name
    }

    fn get_pos(&self) -> &Pos {
        &self.pos
    }

    fn get_type(&self) -> ASTNodeType {
        ASTNodeType::StrFlags
    }
}

impl ASTNode for IntFlagsNode {
    fn get_name(&self) -> &str {
        &self.name.name
    }

    fn get_pos(&self) -> &Pos {
        &self.pos
    }

    fn get_type(&self) -> ASTNodeType {
        ASTNodeType::IntFlags
    }
}

#[derive(Debug)]
pub struct ResourceNode {
    pub pos: Pos,
    pub name: Ident,
    pub base: Type,
    pub values: Vec<ASTInt>,
}

impl ASTNode for ResourceNode {
    fn get_name(&self) -> &str {
        &self.name.name
    }

    fn get_pos(&self) -> &Pos {
        &self.pos
    }

    fn get_type(&self) -> ASTNodeType {
        ASTNodeType::Resource
    }
}

#[derive(Debug)]
pub struct CallNode {
    pub pos: Pos,
    pub name: Ident,
    pub call_name: String,
    pub idx: usize,
    pub args: Vec<Field>,
    pub ret: Type,
    pub attrs: Vec<Type>,
}

impl ASTNode for CallNode {
    fn get_name(&self) -> &str {
        &self.name.name
    }

    fn get_pos(&self) -> &Pos {
        &self.pos
    }

    fn get_type(&self) -> ASTNodeType {
        ASTNodeType::Call
    }
}

#[derive(Debug)]
pub struct StructNode {
    pub pos: Pos,
    pub name: Ident,
    pub fields: Vec<Field>,
    pub attrs: Vec<Type>,
    pub is_union: bool,
}

impl ASTNode for StructNode {
    fn get_name(&self) -> &str {
        &self.name.name
    }

    fn get_pos(&self) -> &Pos {
        &self.pos
    }

    fn get_type(&self) -> ASTNodeType {
        ASTNodeType::Struct
    }
}

#[derive(Debug)]
pub struct TypeDefNode {
    pub pos: Pos,
    pub name: Ident,
    pub args: Vec<Ident>,
    pub typ: Type,
    pub struc: StructNode,
}

impl ASTNode for TypeDefNode {
    fn get_name(&self) -> &str {
        &self.name.name
    }

    fn get_pos(&self) -> &Pos {
        &self.pos
    }

    fn get_type(&self) -> ASTNodeType {
        ASTNodeType::TypeDef
    }
}
