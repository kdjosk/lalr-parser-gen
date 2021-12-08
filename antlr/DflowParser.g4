grammar DflowParser;

program:
  stmtSeq
  EOF;
  
module : 
  Module
  Identifier
  LBrace
  typeDefinitionStmtSeq?
  RBrace;

library : 
  Library
  Identifier
  LBrace
  functionDefinitionStmtSeq?
  RBrace;

functionDefinitionStmt :
  Priv?
  Fn
  Identifier
  LParen
  paramSeq?
  RParen
  returnDeclaration?
  contract
  LBrace
  stmtSeq?
  RBrace;

contract :
  (
    Colon
    Pre
    LParen
    exprSeq?
    RParen
  )?
  (
    Colon
    Post
    LParen
    exprSeq?
    RParen
  )?;

arg :
  expr
  | Identifier Assign expr;

returnDeclaration :
  RArrow
  typeSpecifierSeq;

typeDefinitionStmt :
  dflowDefinition
  | componentDefinition
  | implDefinition
  | structDefinition;

dflowDefinition :
  Dflow 
  Identifier
  LBrace
  stageSeq?
  RBrace;

componentDefinition :
  Compnt
  Identifier
  LBrace
  initMethod
  callMethod
  memberDeclarationSeq?
  RBrace;

implDefinition :
  Impl
  Identifier
  As
  Identifier
  LBrace
  stageAssignmentSeq?
  RBrace;

structDefinition :
  Struct
  Identifier
  (Colon Bind Identifier)?
  LBrace
  fieldDeclarationSeq?
  RBrace;

initMethod :
  Init
  LParen
  paramSeq?
  RParen
  contract
  LBrace
  stmtSeq?
  RBrace;

callMethod :
  Call
  LParen
  paramSeq?
  RParen
  returnDeclaration?
  contract
  LBrace
  stmtSeq?
  RBrace;

typeDefinitionStmtSeq : typeDefinitionStmt+;
functionDefinitionStmtSeq : functionDefinitionStmt+;
typeSpecifierSeq : typeSpecifier (Coma typeSpecifier)*;
identifierSeq : Identifier (Coma Identifier)*;
argSeq : arg (Coma arg)*;
paramSeq : param | paramSeq Coma param;
exprSeq : expr (Coma expr)*;
stmtSeq : stmt+;
stageSeq : stage+;
fieldDeclarationSeq : fieldDeclaration+;
stageAssignmentSeq : stageAssignment+;
memberDeclarationSeq : memberDeclaration+;

param:
  Identifier Colon typeSpecifier;

memberDeclaration :
  Identifier Colon typeSpecifier Semi
  | functionDefinitionStmt;

stage :
  Identifier LParen identifierSeq? RParen (RArrow identifierSeq)? Semi;

stageAssignment :
  Identifier Colon Identifier Semi;

fieldDeclaration :
  Identifier Colon typeSpecifier Semi;

stmt :
  simpleStmt
  | compoundStmt;

simpleStmt :
  assignmentStmt
  | exprStmt
  | returnStmt
  | loopStmt
  | execStmt
  | Break Semi
  | Continue Semi;

compoundStmt :
  loopStmt
  | typeDefinitionStmt
  | functionDefinitionStmt
  | ifElseStmt
  | library
  | module;

assignmentStmt : 
  Let Mut? Identifier Assign exprStmt
  | primary Assign exprStmt;

exprStmt :
  expr Semi;

definitionStmt :
  typeDefinitionStmt
  | functionDefinitionStmt;

returnStmt :
  Return expr Semi;

loopStmt :
  (While expr 
  | For (Identifier | Identifier Coma Identifier) In (Identifier | rangeExpr))
  LBrace stmtSeq RBrace;

execStmt :
  Exec LParen Identifier RParen Semi;

rangeExpr :
  Range LParen expr Coma expr (Coma expr)? RParen;

ifElseStmt :
  ifStmt (Else ifStmt)* (Else LBrace stmtSeq RBrace)?;

ifStmt :
  If expr LBrace stmtSeq RBrace;

expr :
  conjunction (Or conjunction)+
  | conjunction
  | arrayTypeInitializer;

conjunction :
  inversion (And inversion)+
  | inversion;

inversion :
  Not inversion
  | comparison;

comparison :
  sum relOperator sum
  | sum;

sum:
  sum addOperator term
  | term;

term:
  term multOperator factor
  | factor;

factor:
  addOperator factor
  | primary;

primary:
  M Dot primary
  | primary Dot primary
  | primary LParen argSeq? RParen
  | primary LBracket expr RBracket
  | atom;

atom:
  Identifier
  | literal;

unaryOperator :
  Minus
  | Not;

addOperator :
  Plus 
  | Minus;

multOperator :
  Star
  | Div;

relOperator:
  Less
  | Greater
  | LessEqual
  | GreaterEqual
  | Equal
  | NotEqual;

literal:
	IntegerLiteral
	| FloatingLiteral
	| StringLiteral
	| BooleanLiteral;


typeSpecifier :
  Identifier
  | simpleTypeSpecifier
  | arrayTypeSpecifier;


simpleTypeSpecifier :
  U32 
  | I32 
  | F32 
  | U64
  | I64
  | F64
  | U8
  | Bool
  | String;

arrayTypeSpecifier :
  unknownSizeArrayTypeSpecifier
  | fixedSizeArrayTypeSpecifier;

unknownSizeArrayTypeSpecifier :
  Vector Less typeSpecifier Greater;

fixedSizeArrayTypeSpecifier:
  Vector Less typeSpecifier Coma primary Greater;

arrayTypeInitializer :
  unknownSizeArrayTypeSpecifier LParen exprSeq? RParen
  | fixedSizeArrayTypeSpecifier LParen RParen;


/* Lexer rules */

Dflow : 'dflow';
Module : 'module';
Library : 'library';
Compnt : 'compnt';
Return : 'return';
Fn : 'fn';
Let : 'let';
While : 'while';
For : 'for';
In : 'in';
Range : 'range';
If : 'if';
Else : 'else';
Struct : 'struct';
Init : 'init';
Call : 'call';
Impl : 'impl';
As : 'as';
Priv : 'priv';
Pre : 'pre';
Post : 'post';
Break: 'break';
Continue: 'continue';
Exec: 'exec';
Bind: 'bind';
Mut: 'mut';
Or: 'or';
And: 'and';
Not : 'not';
U32 : 'u32'; 
I32 : 'i32'; 
F32 : 'f32'; 
U64 : 'u64'; 
I64 : 'i64'; 
F64 : 'f64'; 
U8 : 'u8'; 
Vector : 'vec'; 
Bool : 'bool'; 
String : 'str';
LBrace : '{';
RBrace : '}';
LParen : '(';
RParen : ')';
LBracket : '[';
RBracket : ']';
Plus: '+';
Minus: '-';
Star : '*';
Div : '/';
Colon : ':';
Semi : ';';
Coma : ',';
Dot : '.';
Assign : '=';
Less : '<';
Greater : '>';
LessEqual : '<=';
GreaterEqual : '>=';
Equal : '==';
NotEqual : '!=';
RArrow : '->';

BooleanLiteral : 'true' | 'false';

StringLiteral :
  '"' (EscapeSeq | ~[\\"])* '"';

fragment EscapeSeq :
	'\\\''
	| '\\"'
	| '\\?'
	| '\\\\'
	| '\\a'
	| '\\b'
	| '\\f'
	| '\\n'
	| '\\r'
	| ('\\' ('\r' '\n'? | '\n'))
	| '\\t'
	| '\\v';

IntegerLiteral : SIGN? NONZERODIGIT ('\''? DIGIT)* | SIGN? DIGIT;
Identifier : NONDIGIT (DIGIT | NONDIGIT)*;

FloatingLiteral:
	FractionalConstant Exponentpart?
	| DigitSeq Exponentpart;


fragment FractionalConstant:
	SIGN? (DigitSeqNotStartingWithZero? '.' DigitSeq
	| DigitSeq '.');

fragment Exponentpart :
	'e' SIGN? DigitSeq
	'E' SIGN? DigitSeq;

fragment DigitSeq: DIGIT ('\''? DIGIT)*;
fragment DigitSeqNotStartingWithZero: NONZERODIGIT ('\''? DIGIT)*;
fragment NONDIGIT: [a-zA-Z_];
fragment NONZERODIGIT : [1-9];
fragment DIGIT : [0-9];
fragment SIGN : [+-];

Whitespace: [ \t\r\n]+ -> skip;
