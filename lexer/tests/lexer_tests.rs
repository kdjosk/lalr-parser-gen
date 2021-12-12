use lexer::{Lexer, StringSource, Token as T};

#[test]
fn integer_literal_test() {
    let source = StringSource::new("123232".to_string());
    let mut lexer = Lexer::new(source);
    assert_eq!(lexer.next_token(), T::IntegerLiteral(123232));
}

#[test]
fn floating_point_literal_test() {
    let source = StringSource::new("0.4234".to_string());
    let mut lexer = Lexer::new(source);
    assert_eq!(lexer.next_token(), T::IntegerLiteral(123232));
}