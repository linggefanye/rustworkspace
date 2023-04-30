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
pub struct ASTType {
    pub pos: Pos,
    pub value: u64,
    pub value_fmt: IntFmt,
    pub ident: String,
    pub string: Option<String>,
    pub string_fmt: Option<StrFmt>,
    pub colon: Vec<ASTType>,
    pub args: Vec<ASTType>,
}

#[derive(Debug)]
pub struct ASTField {
    pub pos: Pos,
    pub name: Ident,
    pub typ: ASTType,
    pub attrs: Vec<ASTType>,
    pub new_block: bool,
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
    pub base: ASTType,
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
    pub args: Vec<ASTField>,
    pub ret: ASTType,
    pub attrs: Vec<ASTType>,
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
    pub fields: Vec<ASTField>,
    pub attrs: Vec<ASTType>,
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
    pub typ: ASTType,
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
