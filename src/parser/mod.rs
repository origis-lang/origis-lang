mod ast;

#[derive(pest_derive::Parser)]
#[grammar = "./grammar.pest"]
pub struct Parser;

#[cfg(test)]
mod tests {
    use pest::Parser as _;

    use super::*;

    #[test]
    fn parse_int_test() {
        use from_pest::FromPest;

        let source = String::from("0b11");
        let mut parse_tree = Parser::parse(Rule::integer, &source).unwrap();
        let ast = ast::Integer::from_pest(&mut parse_tree).unwrap();

        assert!(matches!(
            &ast,
            ast::Integer::Bin(ast::IntegerBin { val: 3 })
        ));
    }

    #[test]
    fn parse_float_test() {
        use from_pest::FromPest;

        let source = String::from("3.45");
        let mut parse_tree = Parser::parse(Rule::float, &source).unwrap();
        let ast = ast::Float::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.val, 3.45);
    }

    #[test]
    fn parse_tuple_test() {
        use from_pest::FromPest;

        let source = String::from("(1,)");
        let mut parse_tree = Parser::parse(Rule::tuple, &source).unwrap();
        let ast = ast::Tuple::from_pest(&mut parse_tree).unwrap();
        assert!(matches!(ast.params.0[0], ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
            ast::Integer::Dec(ast::IntegerDec { val: 1 })
        )))));
    }

    #[test]
    fn parse_op_test() {
        use from_pest::FromPest;

        let source = String::from("-");
        let mut parse_tree = Parser::parse(Rule::operator, &source).unwrap();
        let ast = ast::Operator::from_pest(&mut parse_tree).unwrap();
        assert!(matches!(ast, ast::Operator::Sub(_)));
    }

    #[test]
    fn parse_char_test() {
        use from_pest::FromPest;

        let source = String::from(r#"'a'"#);
        let mut parse_tree = Parser::parse(Rule::char, &source).unwrap();
        let ast = ast::Char::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.val, 'a');
    }

    #[test]
    fn parse_string_test() {
        use from_pest::FromPest;

        let source = String::from(r#""origis""#);
        let mut parse_tree = Parser::parse(Rule::string, &source).unwrap();
        let ast = ast::StringLiteral::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.val, "origis");
    }

    #[test]
    fn parse_ident_test() {
        use from_pest::FromPest;

        let source = String::from(r#"Hello_世界"#);
        let mut parse_tree = Parser::parse(Rule::ident, &source).unwrap();
        let ast = ast::Ident::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.0, "Hello_世界");
    }

    #[test]
    fn parse_params_test() {
        use from_pest::FromPest;

        let source = String::from(r#"[1, 0xf1,]"#);
        let mut parse_tree = Parser::parse(Rule::array, &source).unwrap();
        let ast = ast::Array::from_pest(&mut parse_tree).unwrap();
        let params = ast.params.0;
        assert!(matches!(
            params[0],
            ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
                ast::Integer::Dec(ast::IntegerDec { val: 1 })
            )))
        ));
        assert!(matches!(
            params[1],
            ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
                ast::Integer::Hex(ast::IntegerHex { val: 241 })
            )))
        ));
    }

    #[test]
    fn parse_op_expr_test() {
        use from_pest::FromPest;

        let source = String::from(r#"1 + (2 - 3)"#);
        let mut parse_tree = Parser::parse(Rule::expr, &source).unwrap();
        let ast = ast::Expr::from_pest(&mut parse_tree).unwrap();
        assert!(matches!(
            ast,
            ast::Expr::OpExpr(box ast::OpExpr {
                lhs: ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
                    ast::Integer::Dec(ast::IntegerDec { val: 1 })
                ))),
                op: ast::Operator::Add(_),
                rhs: ast::Expr::OpExpr(box ast::OpExpr {
                    lhs: ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
                        ast::Integer::Dec(ast::IntegerDec { val: 2 })
                    ))),
                    op: ast::Operator::Sub(_),
                    rhs: ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
                        ast::Integer::Dec(ast::IntegerDec { val: 3 })
                    )))
                })
            })
        ));
    }

    #[test]
    fn parse_var_def_test() {
        use from_pest::FromPest;

        let source = String::from(r#"let pi = 3.14;"#);
        let mut parse_tree = Parser::parse(Rule::var_def, &source).unwrap();
        let ast = ast::VarDef::from_pest(&mut parse_tree).unwrap();
        match ast {
            ast::VarDef {
                name: ast::Ident("pi"),
                val: ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Float(float))),
            } => {
                assert_eq!(float.val, 3.14)
            }
            _ => unreachable!("assert error"),
        }
    }
}
