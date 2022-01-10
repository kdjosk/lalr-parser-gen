pub struct Program {
    stmt_seq: Vec<Stmt>,
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

pub struct Name {
    value: String,
}
impl Name {
    pub fn new(value: String) -> Name {
        Name { value }
    }
}

pub struct ForLoopBlock {
    iterator: Name,
    iterable: Expr,
    stmt_seq: Vec<Stmt>,
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
    expr: Expr,
    stmt_seq: Vec<Stmt>,
    else_tail: Option<ElseTail>,
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
    else_if_block: Option<Box<IfBlock>>,
    else_block: Option<ElseBlock>,
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
    stmt_seq: Vec<Stmt>,
}
impl ElseBlock {
    pub fn new(stmt_seq: Vec<Stmt>) -> ElseBlock {
        ElseBlock { stmt_seq }
    }
}

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
    name: Name,
    typev: Type,
}
impl Param {
    pub fn new(name: Name, typev: Type) -> Param {
        Param { name, typev }
    }
}

pub struct Arg {
    kword: Option<Name>,
    expr: Expr,
}
impl Arg {
    pub fn new(kword: Option<Name>, expr: Expr) -> Arg {
        Arg { kword, expr }
    }
}

pub struct FunDef {
    name: Name,
    params: Vec<Param>,
    ret_type: Type,
    stmt_seq: Vec<Stmt>,
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
    name: Name,
    args: Vec<Arg>,
}
impl CallExpr {
    pub fn new(name: Name, args: Vec<Arg>) -> CallExpr {
        CallExpr { name, args }
    }
}

pub enum Expr {
    IntLit(u64),
    StrLit(String),
    FloatLit(f64),
    BoolLit(bool),
    Identifier(Name),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    LessEq(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    LogicOr(Box<Expr>, Box<Expr>),
    LogicAnd(Box<Expr>, Box<Expr>),
    LogicNot(Box<Expr>),
    UnaryMinus(Box<Expr>),
    Call(CallExpr),
}
