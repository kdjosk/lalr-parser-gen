pub struct Program {
    stmt_seq: Vec<Stmt>,
}
impl Program {
    pub fn new() -> Program {
        Program { stmt_seq: Vec::new() }
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmt_seq.push(stmt);
    }
}

pub enum Stmt {
    If(Expr, TrueBlock, FalseBlock),
    Expr(Expr),
    Assignment(Name, Expr),
    FunDef(FunDef),
}

pub struct Name {
    value: String,
}
impl Name {
    pub fn new(value: String) -> Name {
        Name {
            value
        }
    }
}

pub struct TrueBlock {
    stmt_seq: Vec<Stmt>,
}
impl TrueBlock {
    pub fn new(stmt_seq: Vec<Stmt>) -> TrueBlock {
        TrueBlock {
            stmt_seq,
        }
    }
}

pub struct FalseBlock {
    else_if_blocks: Vec<ElseIfBlock>,
    else_block: ElseBlock,
}
impl FalseBlock {
    pub fn new(else_if_blocks: Vec<ElseIfBlock>, else_block: ElseBlock) -> FalseBlock {
        FalseBlock {
            else_if_blocks,
            else_block,
        }
    }
}


pub struct ElseIfBlock {
    condition: Expr,
    stmt_seq: Vec<Stmt>,
}
impl ElseIfBlock {
    pub fn new(condition: Expr, stmt_seq: Vec<Stmt>) -> ElseIfBlock {
        ElseIfBlock {
            condition,
            stmt_seq
        }
    }
}

pub struct ElseBlock {
    stmt_seq: Vec<Stmt>,
}
impl ElseBlock {
    pub fn new(stmt_seq: Vec<Stmt>) -> ElseBlock {
        ElseBlock {
            stmt_seq
        }
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
        Param {
            name,
            typev,
        }
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
        CallExpr {
            name,
            args,
        }
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