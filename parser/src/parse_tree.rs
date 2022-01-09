use std::{fmt::{Display, self}};

// TODO(kjoskowi) This whole file should be automatically generated from grammar description
pub trait ParseTreeVisitor {
    fn visit_stmt_seq(&mut self, stmt_seq: NonterminalNode<StmtSeq>);
    fn visit_stmt(&mut self, stmt: NonterminalNode<Stmt>);
    fn visit_expr_stmt(&mut self, expr_stmt: NonterminalNode<ExprStmt>);
    fn visit_assignment_stmt(&mut self, assignment_stmt: NonterminalNode<AssignmentStmt>);
    fn visit_if_stmt(&mut self, if_stmt: NonterminalNode<IfStmt>);
    fn visit_else_tail(&mut self, else_tail: NonterminalNode<ElseTail>);
    fn visit_var_decl_stmt(&mut self, var_decl_stmt: NonterminalNode<VarDeclStmt>);
    fn visit_fun_def_stmt(&mut self, fun_def_stmt: NonterminalNode<FunDefStmt>);
    fn visit_param_seq(&mut self, param_seq: NonterminalNode<ParamSeq>);
    fn visit_param_seq_tail(&mut self, param_seq_tail: NonterminalNode<ParamSeqTail>);
    fn visit_param(&mut self, param: NonterminalNode<Param>);
    fn visit_return_declaration(&mut self, return_declaration: NonterminalNode<ReturnDeclaration>);
    fn visit_return_stmt(&mut self, return_stmt: NonterminalNode<ReturnStmt>);
    fn visit_type_specifier(&mut self, type_specifier: NonterminalNode<TypeSpecifier>);
    fn visit_primitive_type_specifier(&mut self, primitive_type_specifier: NonterminalNode<PrimitiveTypeSpecifier>);
    fn visit_for_loop_stmt(&mut self, for_loop_stmt: NonterminalNode<ForLoopStmt>);
    fn visit_unary_operator(&mut self, unary_operator: NonterminalNode<UnaryOperator>);
    fn visit_expr(&mut self, expr: NonterminalNode<Expr>);
    fn visit_disjunction(&mut self, disjunction: NonterminalNode<Disjunction>);
    fn visit_conjunction_seq(&mut self, conjunction_seq: NonterminalNode<ConjunctionSeq>);
    fn visit_conjunction(&mut self, conjunction: NonterminalNode<Conjunction>);
    fn visit_inversion_seq(&mut self, inversion_seq: NonterminalNode<InversionSeq>);
    fn visit_inversion(&mut self, inversion: NonterminalNode<Inversion>);
    fn visit_comparison(&mut self, comparison: NonterminalNode<Comparison>);
    fn visit_sum(&mut self, sum: NonterminalNode<Sum>);
    fn visit_term(&mut self, term: NonterminalNode<Term>);
    fn visit_factor(&mut self, factor: NonterminalNode<Factor>);
    fn visit_call_expr(&mut self, call_expr: NonterminalNode<CallExpr>);
    fn visit_arg_seq(&mut self, arg_seq: NonterminalNode<ArgSeq>);
    fn visit_arg_seq_tail(&mut self, arg_seq_tail: NonterminalNode<ArgSeqTail>);
    fn visit_arg(&mut self, arg: NonterminalNode<Arg>);
    fn visit_primary(&mut self, primary: NonterminalNode<Primary>);
    fn visit_atom(&mut self, atom: NonterminalNode<Atom>);
    fn visit_rel_operator(&mut self, rel_operator: NonterminalNode<RelOperator>);
    fn visit_mult_operator(&mut self, mult_operator: NonterminalNode<MultOperator>);
    fn visit_add_operator(&mut self, add_operator: NonterminalNode<AddOperator>);
    fn visit_literal(&mut self, literal: NonterminalNode<Literal>);
    fn visit_boolean_literal(&mut self, boolean_literal: NonterminalNode<BooleanLiteral>);
    fn visit_semi(&mut self, semi: TerminalNode<Semi>);
    fn visit_coma(&mut self, coma: TerminalNode<Coma>);
    fn visit_return(&mut self, ret: TerminalNode<Return>);
    fn visit_r_arrow(&mut self, r_arrow: TerminalNode<RArrow>);
    fn visit_identifier(&mut self, identifier: TerminalNode<Identifier>);
    fn visit_assign(&mut self, assign: TerminalNode<Assign>);
    fn visit_if(&mut self, ifword: TerminalNode<If>);
    fn visit_else(&mut self, elseword: TerminalNode<Else>);
    fn visit_l_brace(&mut self, l_brace: TerminalNode<LBrace>);
    fn visit_r_brace(&mut self, r_brace: TerminalNode<RBrace>);
    fn visit_l_paren(&mut self, l_paren: TerminalNode<LParen>);
    fn visit_r_paren(&mut self, r_paren: TerminalNode<RParen>);
    fn visit_or(&mut self, or: TerminalNode<Or>);
    fn visit_fun(&mut self, fun: TerminalNode<Fun>);
    fn visit_and(&mut self, and: TerminalNode<And>);
    fn visit_not(&mut self, not: TerminalNode<Not>);
    fn visit_less(&mut self, less: TerminalNode<Less>);
    fn visit_greater(&mut self, greater: TerminalNode<Greater>);
    fn visit_less_equal(&mut self, less_equal: TerminalNode<LessEqual>);
    fn visit_greater_equal(&mut self, greater_equal: TerminalNode<GreaterEqual>);
    fn visit_equal(&mut self, equal: TerminalNode<Equal>);
    fn visit_not_equal(&mut self, not_equal: TerminalNode<NotEqual>);
    fn visit_star(&mut self, star: TerminalNode<Star>);
    fn visit_div(&mut self, div: TerminalNode<Div>);
    fn visit_minus(&mut self, minus: TerminalNode<Minus>);
    fn visit_plus(&mut self, plus: TerminalNode<Plus>);
    fn visit_integer_literal(&mut self, integer_literal: TerminalNode<IntegerLiteral>);
    fn visit_floating_literal(&mut self, floating_literal: TerminalNode<FloatingLiteral>);
    fn visit_string_literal(&mut self, string_literal: TerminalNode<StringLiteral>);
    fn visit_true(&mut self, trueword: TerminalNode<True>);
    fn visit_false(&mut self, falseword: TerminalNode<False>);
    fn visit_colon(&mut self, colon: TerminalNode<Colon>);
    fn visit_u32(&mut self, u32: TerminalNode<U32>);
    fn visit_i32(&mut self, i32: TerminalNode<I32>);
    fn visit_f32(&mut self, f32: TerminalNode<F32>);
    fn visit_u64(&mut self, u64: TerminalNode<U64>);
    fn visit_i64(&mut self, i64: TerminalNode<I64>);
    fn visit_f64(&mut self, f64: TerminalNode<F64>);
    fn visit_u8(&mut self, u8: TerminalNode<U8>);
    fn visit_bool(&mut self, bool: TerminalNode<Bool>);
    fn visit_string_type(&mut self, string_type: TerminalNode<StringType>);
    fn visit_let(&mut self, letword: TerminalNode<Let>);
    fn visit_for(&mut self, forword: TerminalNode<For>);
    fn visit_in(&mut self, inword: TerminalNode<In>);
}

pub enum ParseTreeNode {
    Leaf(Box<dyn TerminalNodeT>),
    Internal(Box<dyn NonterminalNodeT>)
}

pub trait Labeled {
    fn get_label(&self) -> String;
}

pub trait Nonterminal {
    fn get_children_ref(&self) -> &Vec<ParseTreeNode>;
    fn add_child(&mut self, child: ParseTreeNode); 
}

pub trait Terminal {
    fn get_lexical_value(&self) -> LexicalValue;
}


pub trait NonterminalNodeT: Nonterminal + Labeled {}
impl<T> NonterminalNodeT for T where
    T: Nonterminal + Labeled {}
pub trait TerminalNodeT: Terminal + Labeled {}
impl<T>  TerminalNodeT for T where
    T: Terminal + Labeled {}


pub struct NonterminalNode<T> {
    children: Vec<ParseTreeNode>,
    label: String,
    t: T,
}

impl<T: SyntaxFunction> NonterminalNode<T> {
    pub fn new(label: &str, t: T) -> NonterminalNode<T> {
        let label = label.to_string();
        NonterminalNode{
            children: Vec::new(),
            label,
            t,
        }
    }
}
impl<T: SyntaxFunction> Labeled for NonterminalNode<T> {
    fn get_label(&self) -> String {
        self.label.clone()
    }
}
impl<T: SyntaxFunction> Nonterminal for NonterminalNode<T> {
    fn get_children_ref(&self) -> &Vec<ParseTreeNode> {
        &self.children
    }
    fn add_child(&mut self, child: ParseTreeNode) {
        self.children.push(child);
    }
}

#[derive(Clone)]
pub enum LexicalValue {
    StringType(String),
    IntegerType(u64),
    FloatingType(f64),
    BooleanType(bool),
    IdentifierType(String),
    NoLexicalValue,
}
impl Display for LexicalValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexicalValue::StringType(v) => write!(f, "{}", v),
            LexicalValue::IntegerType(v) => write!(f, "{}", v),
            LexicalValue::FloatingType(v) => write!(f, "{}", v),
            LexicalValue::BooleanType(v) => write!(f, "{}", v),
            LexicalValue::IdentifierType(v) => write!(f, "{}", v),
            LexicalValue::NoLexicalValue => write!(f, ""),
        }
    }
}

pub struct TerminalNode<T> {
    lex_value: LexicalValue,
    label: String,
    t: T,
}
impl<T: SyntaxFunction> TerminalNode<T> {
    pub fn new(label: &str, lex_value: LexicalValue, t: T) -> TerminalNode<T> {
        let label = label.to_string();
        TerminalNode{
            lex_value,
            label,
            t,
        }
    }
}
impl<T: SyntaxFunction> Labeled for TerminalNode<T> {
    fn get_label(&self) -> String {
        format!("{}({})", self.label.clone(), self.lex_value)
    }
}
impl<T: SyntaxFunction> Terminal for TerminalNode<T> {
    fn get_lexical_value(&self) -> LexicalValue {
        self.lex_value.clone()
    }
}

pub trait SyntaxFunction{}
pub struct Program {}
impl SyntaxFunction for Program {}
pub struct StmtSeq {}
impl SyntaxFunction for StmtSeq {}
pub struct Stmt {}
impl SyntaxFunction for Stmt {}
pub struct ExprStmt {}
impl SyntaxFunction for ExprStmt {}
pub struct AssignmentStmt {}
impl SyntaxFunction for AssignmentStmt {}
pub struct IfStmt {}
impl SyntaxFunction for IfStmt {}
pub struct ElseTail {}
impl SyntaxFunction for ElseTail {}
pub struct VarDeclStmt {}
impl SyntaxFunction for VarDeclStmt {}
pub struct FunDefStmt {}
impl SyntaxFunction for FunDefStmt {}
pub struct ParamSeq {}
impl SyntaxFunction for ParamSeq {}
pub struct ParamSeqTail {}
impl SyntaxFunction for ParamSeqTail {}
pub struct Param {}
impl SyntaxFunction for Param {}
pub struct ReturnDeclaration {}
impl SyntaxFunction for ReturnDeclaration {}
pub struct ReturnStmt {}
impl SyntaxFunction for ReturnStmt {}
pub struct TypeSpecifier {}
impl SyntaxFunction for TypeSpecifier {}
pub struct PrimitiveTypeSpecifier {}
impl SyntaxFunction for PrimitiveTypeSpecifier {}
pub struct ForLoopStmt {}
impl SyntaxFunction for ForLoopStmt {}
pub struct Let {}
impl SyntaxFunction for Let {}
pub struct For {}
impl SyntaxFunction for For {}
pub struct In {}
impl SyntaxFunction for In {}
pub struct Expr {}
impl SyntaxFunction for Expr {}
pub struct Disjunction {}
impl SyntaxFunction for Disjunction {}
pub struct ConjunctionSeq {}
impl SyntaxFunction for ConjunctionSeq {}
pub struct Conjunction {}
impl SyntaxFunction for Conjunction {}
pub struct InversionSeq {}
impl SyntaxFunction for InversionSeq {}
pub struct Inversion {}
impl SyntaxFunction for Inversion {}
pub struct Comparison {}
impl SyntaxFunction for Comparison {}
pub struct Sum {}
impl SyntaxFunction for Sum {}
pub struct Term {}
impl SyntaxFunction for Term {}
pub struct Factor {}
impl SyntaxFunction for Factor {}
pub struct CallExpr {}
impl SyntaxFunction for CallExpr {}
pub struct Fun {}
impl SyntaxFunction for Fun {}
pub struct ArgSeq {}
impl SyntaxFunction for ArgSeq {}
pub struct ArgSeqTail {}
impl SyntaxFunction for ArgSeqTail {}
pub struct Arg {}
impl SyntaxFunction for Arg {}
pub struct Primary {}
impl SyntaxFunction for Primary {}
pub struct Atom {}
impl SyntaxFunction for Atom {}
pub struct RelOperator {}
impl SyntaxFunction for RelOperator {}
pub struct MultOperator {}
impl SyntaxFunction for MultOperator {}
pub struct AddOperator {}
impl SyntaxFunction for AddOperator {}
pub struct Literal {}
impl SyntaxFunction for Literal {}
pub struct BooleanLiteral {}
impl SyntaxFunction for BooleanLiteral {}
pub struct Semi {}
impl SyntaxFunction for Semi {}
pub struct Coma {}
impl SyntaxFunction for Coma {}
pub struct Return {}
impl SyntaxFunction for Return {}
pub struct RArrow {}
impl SyntaxFunction for RArrow {}
pub struct Identifier {}
impl SyntaxFunction for Identifier {}
pub struct Assign {}
impl SyntaxFunction for Assign {}
pub struct If {}
impl SyntaxFunction for If {}
pub struct Else {}
impl SyntaxFunction for Else {}
pub struct LBrace {}
impl SyntaxFunction for LBrace {}
pub struct RBrace {}
impl SyntaxFunction for RBrace {}
pub struct LParen {}
impl SyntaxFunction for LParen {}
pub struct RParen {}
impl SyntaxFunction for RParen {}
pub struct Or {}
impl SyntaxFunction for Or {}
pub struct And {}
impl SyntaxFunction for And {}
pub struct Not {}
impl SyntaxFunction for Not {}
pub struct Less {}
impl SyntaxFunction for Less {}
pub struct Greater {}
impl SyntaxFunction for Greater {}
pub struct LessEqual {}
impl SyntaxFunction for LessEqual {}
pub struct GreaterEqual {}
impl SyntaxFunction for GreaterEqual {}
pub struct Equal {}
impl SyntaxFunction for Equal {}
pub struct NotEqual {}
impl SyntaxFunction for NotEqual {}
pub struct Star {}
impl SyntaxFunction for Star {}
pub struct Div {}
impl SyntaxFunction for Div {}
pub struct Minus {}
impl SyntaxFunction for Minus {}
pub struct Plus {}
impl SyntaxFunction for Plus {}
pub struct IntegerLiteral {}
impl SyntaxFunction for IntegerLiteral {}
pub struct FloatingLiteral {}
impl SyntaxFunction for FloatingLiteral {}
pub struct StringLiteral {}
impl SyntaxFunction for StringLiteral {}
pub struct True {}
impl SyntaxFunction for True {}
pub struct False {}
impl SyntaxFunction for False {}
pub struct Colon {}
impl SyntaxFunction for Colon {}
pub struct UnaryOperator {}
impl SyntaxFunction for UnaryOperator {}
pub struct U32 {}
impl SyntaxFunction for U32 {}
pub struct I32 {}
impl SyntaxFunction for I32 {}
pub struct F32 {}
impl SyntaxFunction for F32 {}
pub struct U64 {}
impl SyntaxFunction for U64 {}
pub struct I64 {}
impl SyntaxFunction for I64 {}
pub struct F64 {}
impl SyntaxFunction for F64 {}
pub struct U8 {}
impl SyntaxFunction for U8 {}
pub struct Bool {}
impl SyntaxFunction for Bool {}
pub struct StringType {}
impl SyntaxFunction for StringType {}
