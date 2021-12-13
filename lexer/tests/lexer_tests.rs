use lexer::{Lexer, StringSource, Token as T};
use float_cmp::assert_approx_eq;
#[test]
fn integer_literal_test() {
    lexer_test("123232", &T::IntegerLiteral(123232));
}

#[test]
fn floating_point_literal_test() {
    f64_lexer_test("0.4234", &T::FloatingLiteral(0.4234));
    f64_lexer_test("2.4234", &T::FloatingLiteral(2.4234));
}

#[test]
fn floating_point_literal_with_negative_exponent_test() {
    f64_lexer_test("0.4234e-2", &T::FloatingLiteral(0.004234));
    f64_lexer_test("2.4234e-2", &T::FloatingLiteral(0.024234));
}

#[test]
fn floating_point_literal_with_plus_sign_after_exponent_test() {
    f64_lexer_test("0.4234e+2", &T::FloatingLiteral(42.34));
    f64_lexer_test("2.4234e+2", &T::FloatingLiteral(242.34));
}


fn lexer_test(string: &str, expected: &T) {
    let source = StringSource::new(string.to_string());
    let mut lexer = Lexer::new(source);
    assert_eq!(lexer.next_token(), *expected);
}

fn f64_lexer_test(string: &str, expected: &T) {
    let source = StringSource::new(string.to_string());
    let mut lexer = Lexer::new(source);
    match lexer.next_token() {
        T::FloatingLiteral(val1) => {
            match expected {
                T::FloatingLiteral(val2) => {
                    assert_approx_eq!(f64, val1, *val2);
                }
                _ => unreachable!(),
            }  
        }
        _ => unreachable!(),
    }
}