use super::*;
use crate::lexer::Lexer;

macro_rules! match_ast {
    ($( $input: expr => $ast: pat $( if $cond: expr )? ),+ $(,)? => $method: ident) => {
        $(
            let tokens = Lexer::new($input).tokenize()?;
            let mut parser = Parser::new($input, tokens);
            let expr = parser.$method()?;
            assert!(matches!(expr, $ast $( if $cond )?));
        )+
    };
}

#[test]
fn test_primary_expression() -> Result<(), ParseError> {
    match_ast!(
        "1" => Expr::Literal(WithToken(Literal::Integer(1), _)),
        "name" => Expr::Var(WithToken(name, _)) if name == "name".to_string(),
        => primary
    );

    Ok(())
}
