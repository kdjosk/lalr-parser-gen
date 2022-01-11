pub struct Program {
    pub stmt_seq: Vec<Stmt>,
}
impl Program {
    pub fn new(stmt_seq: Vec<Stmt>) -> Program {
        Program {
            stmt_seq,
        }
    }
}

pub enum Stmt {
    If(IfBlock),
    Expr(Expr),
    Assignment(Name, Expr),
    FunDef(FunDef),
    VarDecl(Name, Expr),
    ForLoop(ForLoopBlock),
    Return(Expr)
}

#[derive(Debug)]
pub struct Name {
    value: String,
}
impl Name {
    pub fn new(value: String) -> Name {
        Name { value }
    }
}

pub struct ForLoopBlock {
    pub iterator: Name,
    pub iterable: Expr,
    pub stmt_seq: Vec<Stmt>,
}
impl ForLoopBlock {
    pub fn new(iterator: Name, iterable: Expr, stmt_seq: Vec<Stmt>) -> ForLoopBlock {
        ForLoopBlock {
            iterator,
            iterable,
            stmt_seq,
        }
    }
}

pub struct IfBlock {
    pub expr: Expr,
    pub stmt_seq: Vec<Stmt>,
    pub else_tail: Option<ElseTail>,
}
impl IfBlock {
    pub fn new(expr: Expr, stmt_seq: Vec<Stmt>, else_tail: Option<ElseTail>) -> IfBlock {
        IfBlock {
            expr,
            stmt_seq,
            else_tail,
        }
    }
}

pub struct ElseTail {
    pub else_if_block: Option<Box<IfBlock>>,
    pub else_block: Option<ElseBlock>,
}
impl ElseTail {
    pub fn new(else_if_block: Option<Box<IfBlock>>, else_block: Option<ElseBlock>) -> ElseTail {
        ElseTail {
            else_if_block,
            else_block,
        }
    }
}

pub struct ElseBlock {
    pub stmt_seq: Vec<Stmt>,
}
impl ElseBlock {
    pub fn new(stmt_seq: Vec<Stmt>) -> ElseBlock {
        ElseBlock { stmt_seq }
    }
}

#[derive(Debug)]
pub enum Type {
    Int32,
    Int64,
    Uint32,
    Uint64,
    Uint8,
    Float32,
    Float64,
    String,
    Bool,
    UserDefined(Name),
}

pub struct Param {
    pub name: Name,
    pub ptype: Type,
}
impl Param {
    pub fn new(name: Name, ptype: Type) -> Param {
        Param { name, ptype }
    }
}

pub struct Arg {
    pub kword: Option<Name>,
    pub expr: Expr,
}
impl Arg {
    pub fn new(kword: Option<Name>, expr: Expr) -> Arg {
        Arg { kword, expr }
    }
}

pub struct FunDef {
    pub name: Name,
    pub params: Vec<Param>,
    pub ret_type: Type,
    pub stmt_seq: Vec<Stmt>,
}
impl FunDef {
    pub fn new(name: Name, params: Vec<Param>, ret_type: Type, stmt_seq: Vec<Stmt>) -> FunDef {
        FunDef {
            name,
            params,
            ret_type,
            stmt_seq,
        }
    }
}

pub struct CallExpr {
    pub name: Name,
    pub args: Vec<Arg>,
}
impl CallExpr {
    pub fn new(name: Name, args: Vec<Arg>) -> CallExpr {
        CallExpr { name, args }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    GreaterEq,
    LessEq,
    Greater,
    Less,
    Eq,
    LogicOr,
    LogicAnd,
    LogicNot,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Minus,
    Not,
}

pub enum Expr {
    IntLit(u64),
    StrLit(String),
    FloatLit(f64),
    BoolLit(bool),
    Identifier(Name),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Call(CallExpr),
}
