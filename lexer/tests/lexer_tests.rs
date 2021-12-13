use float_cmp::assert_approx_eq;
use lexer::{Lexer, StringSource, Token};
#[test]
fn integer_literal_test() {
    lexer_test("123232", &Token::IntegerLiteral(123232));
    lexer_test("0", &Token::IntegerLiteral(0));
    lexer_test("1", &Token::IntegerLiteral(1));
}

#[test]
#[should_panic(expected = "Invalid number prefix 0[0-9]")]
fn invalid_integer_literal_test() {
    panic_lexer_test("0123232");
}

#[test]
fn floating_point_literal_test() {
    f64_lexer_test("0.4234", &Token::FloatingLiteral(0.4234));
    f64_lexer_test("2.4234", &Token::FloatingLiteral(2.4234));
}

#[test]
fn floating_point_literal_with_negative_exponent_test() {
    f64_lexer_test("0.4234e-2", &Token::FloatingLiteral(0.004234));
    f64_lexer_test("2.4234e-2", &Token::FloatingLiteral(0.024234));
}

#[test]
fn floating_point_literal_with_plus_sign_after_exponent_test() {
    f64_lexer_test("0.4234e+2", &Token::FloatingLiteral(42.34));
    f64_lexer_test("2.4234e+2", &Token::FloatingLiteral(242.34));
}

#[test]
fn floating_literal_with_no_dot_and_positive_exponent_test() {
    f64_lexer_test("34e+2", &Token::FloatingLiteral(3400.0));
}

#[test]
fn floating_literal_with_no_dot_and_negative_exponent_test() {
    f64_lexer_test("34e-2", &Token::FloatingLiteral(0.34));
}

#[test]
fn string_literal_test() {
    lexer_test(
        r#""aaabbbccc""#,
        &Token::StringLiteral(String::from("aaabbbccc")),
    );
    lexer_test(
        r#"'aaabbbccc'"#,
        &Token::StringLiteral(String::from("aaabbbccc")),
    );
}

#[test]
#[should_panic]
fn invalid_string_literal_test() {
    panic_lexer_test(r#""aaabbbccc"#);
    panic_lexer_test(r#"'aaabbbccc"#);
    panic_lexer_test(r#"'aaabbbc'cc"#);
}

fn lbrace_test() {
    lexer_test("{", &Token::LBrace);
}

#[test]
fn rbrace_test() {
    lexer_test("}", &Token::RBrace);
}

#[test]
fn lparen_test() {
    lexer_test("(", &Token::LParen);
}

#[test]
fn rparen_test() {
    lexer_test(")", &Token::RParen);
}

#[test]
fn lbracket_test() {
    lexer_test("[", &Token::LBracket);
}

#[test]
fn rbracket_test() {
    lexer_test("]", &Token::RBracket);
}

#[test]
fn plus_test() {
    lexer_test("+", &Token::Plus);
}

#[test]
fn star_test() {
    lexer_test("*", &Token::Star);
}

#[test]
fn div_test() {
    lexer_test("/", &Token::Div);
}

#[test]
fn colon_test() {
    lexer_test(":", &Token::Colon);
}

#[test]
fn semi_test() {
    lexer_test(";", &Token::Semi);
}

#[test]
fn coma_test() {
    lexer_test(",", &Token::Coma);
}

#[test]
fn dot_test() {
    lexer_test(".", &Token::Dot);
}

#[test]
fn notequal_test() {
    lexer_test("!=", &Token::NotEqual);
}

#[test]
fn minus_test() {
    lexer_test("-", &Token::Minus);
}

#[test]
fn assign_test() {
    lexer_test("=", &Token::Assign);
}

#[test]
fn less_test() {
    lexer_test("<", &Token::Less);
}

#[test]
fn greater_test() {
    lexer_test(">", &Token::Greater);
}

#[test]
fn rarrow_test() {
    lexer_test("->", &Token::RArrow);
}

#[test]
fn equal_test() {
    lexer_test("==", &Token::Equal);
}

#[test]
fn lessequal_test() {
    lexer_test("<=", &Token::LessEqual);
}

#[test]
fn greaterequal_test() {
    lexer_test(">=", &Token::GreaterEqual);
}

#[test]

#[test]
fn space_test() {
    lexer_test(" ", &Token::Space);
}

#[test]
fn tab_test() {
    lexer_test("\t", &Token::Tab);
}

#[test]
fn newline_test() {
    lexer_test("\n", &Token::Newline);
}

#[test]
fn dflow_test() {
    lexer_test("dflow", &Token::Dflow);
}

#[test]
fn module_test() {
    lexer_test("module", &Token::Module);
}

#[test]
fn library_test() {
    lexer_test("library", &Token::Library);
}

#[test]
fn component_test() {
    lexer_test("component", &Token::Component);
}

#[test]
fn return_test() {
    lexer_test("return", &Token::Return);
}

#[test]
fn fn_test() {
    lexer_test("fn", &Token::Fn);
}

#[test]
fn let_test() {
    lexer_test("let", &Token::Let);
}

#[test]
fn while_test() {
    lexer_test("while", &Token::While);
}

#[test]
fn for_test() {
    lexer_test("for", &Token::For);
}

#[test]
fn in_test() {
    lexer_test("in", &Token::In);
}

#[test]
fn range_test() {
    lexer_test("range", &Token::Range);
}

#[test]
fn if_test() {
    lexer_test("if", &Token::If);
}

#[test]
fn else_test() {
    lexer_test("else", &Token::Else);
}

#[test]
fn struct_test() {
    lexer_test("struct", &Token::Struct);
}

#[test]
fn init_test() {
    lexer_test("init", &Token::Init);
}

#[test]
fn call_test() {
    lexer_test("call", &Token::Call);
}

#[test]
fn impl_test() {
    lexer_test("impl", &Token::Impl);
}

#[test]
fn as_test() {
    lexer_test("as", &Token::As);
}

#[test]
fn priv_test() {
    lexer_test("priv", &Token::Priv);
}

#[test]
fn pre_test() {
    lexer_test("pre", &Token::Pre);
}

#[test]
fn post_test() {
    lexer_test("post", &Token::Post);
}

#[test]
fn break_test() {
    lexer_test("break", &Token::Break);
}

#[test]
fn continue_test() {
    lexer_test("continue", &Token::Continue);
}

#[test]
fn exec_test() {
    lexer_test("exec", &Token::Exec);
}

#[test]
fn bind_test() {
    lexer_test("bind", &Token::Bind);
}

#[test]
fn mut_test() {
    lexer_test("mut", &Token::Mut);
}

#[test]
fn or_test() {
    lexer_test("or", &Token::Or);
}

#[test]
fn and_test() {
    lexer_test("and", &Token::And);
}

#[test]
fn not_test() {
    lexer_test("not", &Token::Not);
}

#[test]
fn u32_test() {
    lexer_test("u32", &Token::U32);
}

#[test]
fn i32_test() {
    lexer_test("i32", &Token::I32);
}

#[test]
fn f32_test() {
    lexer_test("f32", &Token::F32);
}

#[test]
fn u64_test() {
    lexer_test("u64", &Token::U64);
}

#[test]
fn i64_test() {
    lexer_test("i64", &Token::I64);
}

#[test]
fn f64_test() {
    lexer_test("f64", &Token::F64);
}

#[test]
fn u8_test() {
    lexer_test("u8", &Token::U8);
}

#[test]
fn vec_test() {
    lexer_test("vec", &Token::Vec);
}

#[test]
fn bool_test() {
    lexer_test("bool", &Token::Bool);
}

#[test]
fn str_test() {
    lexer_test("str", &Token::String);
}

#[test]
fn false_test() {
    lexer_test("false", &Token::False);
}

#[test]
fn true_test() {
    lexer_test("true", &Token::True);
}

fn lexer_test(string: &str, expected: &Token) {
    let source = StringSource::new(string.to_string());
    let mut lexer = Lexer::new(source);
    assert_eq!(lexer.next_token(), *expected);
}

fn panic_lexer_test(string: &str) {
    let source = StringSource::new(string.to_string());
    let mut lexer = Lexer::new(source);
    lexer.next_token();
}

fn f64_lexer_test(string: &str, expected: &Token) {
    let source = StringSource::new(string.to_string());
    let mut lexer = Lexer::new(source);
    match lexer.next_token() {
        Token::FloatingLiteral(val1) => match expected {
            Token::FloatingLiteral(val2) => {
                assert_approx_eq!(f64, val1, *val2);
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}
