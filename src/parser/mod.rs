use from_pest::FromPest;
use pest::Parser as _;

pub mod ast;

#[derive(pest_derive::Parser)]
#[grammar = "./grammar.pest"]
pub struct Parser;

pub fn parse_module(source: &str) -> ast::Module {
    let mut parse_tree = Parser::parse(Rule::module, source).unwrap();
    let ast = ast::Module::from_pest(&mut parse_tree).unwrap();
    ast
}

#[cfg(test)]
mod tests {
    use from_pest::FromPest;
    use pest::Parser as _;

    use super::*;

    #[test]
    fn parse_int_test() {
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
        let source = String::from("3.45");
        let mut parse_tree = Parser::parse(Rule::float, &source).unwrap();
        let ast = ast::Float::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.val, 3.45);
    }

    #[test]
    fn parse_tuple_test() {
        let source = String::from("(1,)");
        let mut parse_tree = Parser::parse(Rule::tuple, &source).unwrap();
        let ast = ast::Tuple::from_pest(&mut parse_tree).unwrap();
        assert!(matches!(
            ast.params.0[0],
            ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
                ast::Integer::Dec(ast::IntegerDec { val: 1 })
            )))
        ));
    }

    #[test]
    fn parse_unit_type_test() {
        let source = String::from("()");
        let mut parse_tree = Parser::parse(Rule::_type, &source).unwrap();
        let ast = ast::Type::from_pest(&mut parse_tree).unwrap();
        assert!(matches!(ast, ast::Type::Unit(_)));
    }

    #[test]
    fn parse_op_test() {
        let source = String::from("-");
        let mut parse_tree = Parser::parse(Rule::operator, &source).unwrap();
        let ast = ast::Operator::from_pest(&mut parse_tree).unwrap();
        assert!(matches!(ast, ast::Operator::Sub(_)));
    }

    #[test]
    fn parse_char_test() {
        let source = String::from(r#"'a'"#);
        let mut parse_tree = Parser::parse(Rule::char, &source).unwrap();
        let ast = ast::Char::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.val, 'a');
    }

    #[test]
    fn parse_string_test() {
        let source = String::from(r#""origis""#);
        let mut parse_tree = Parser::parse(Rule::string, &source).unwrap();
        let ast = ast::StringLiteral::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.val, "origis");
    }

    #[test]
    fn parse_ident_test() {
        let source = String::from(r#"Hello_世界"#);
        let mut parse_tree = Parser::parse(Rule::ident, &source).unwrap();
        let ast = ast::Ident::from_pest(&mut parse_tree).unwrap();
        assert_eq!(ast.0, "Hello_世界");
    }

    #[test]
    fn parse_params_test() {
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
        let source = String::from(r#"let pi: float = 3.14;"#);
        let mut parse_tree = Parser::parse(Rule::var_def, &source).unwrap();
        let ast = ast::VarDef::from_pest(&mut parse_tree).unwrap();
        match ast {
            ast::VarDef {
                name: ast::Ident(s),
                ty: ast::Type::Float(_),
                val: ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Float(float))),
            } => {
                assert_eq!(s, "pi");
                assert_eq!(float.val, 3.14)
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn parse_fn_def_test() {
        let source = String::from(r#"fn sum(a, b: int) int { a + b }"#);
        let mut parse_tree = Parser::parse(Rule::func_def, &source).unwrap();
        let ast = ast::FuncDef::from_pest(&mut parse_tree).unwrap();
        match &ast {
            ast::FuncDef {
                name: ast::Ident(name),
                params: ast::FuncDefParams(params), // A
                ret_type: ast::Type::Int(_),
                body:
                    ast::FuncDefBody {
                        stmts: _,
                        ret:
                            Some(ast::Expr::OpExpr(box ast::OpExpr {
                                lhs: ast::Expr::Ident(ast::Ident(a)),
                                op: ast::Operator::Add(_),
                                rhs: ast::Expr::Ident(ast::Ident(b)),
                            })),
                    },
            } => {
                assert_eq!(name, &"sum");
                assert_eq!(a, &"a");
                assert_eq!(b, &"b");
                assert!(matches!(
                    params.as_slice(),
                    [
                        ast::FuncDefParam {
                            name: ast::Ident(_),
                            ty: ast::Type::Int(_)
                        },
                        ast::FuncDefParam {
                            name: ast::Ident(_),
                            ty: ast::Type::Int(_)
                        },
                    ]
                ))
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn parse_fn_call_test() {
        let source = String::from(r#"sum(a, 3)"#);
        let mut parse_tree = Parser::parse(Rule::func_call, &source).unwrap();
        let ast = ast::FuncCall::from_pest(&mut parse_tree).unwrap();
        match &ast {
            ast::FuncCall {
                name: ast::Ident(name),
                args,
            } => {
                assert_eq!(name, &"sum");
                assert!(matches!(
                    args.0.as_slice(),
                    [
                        ast::Expr::Ident(ast::Ident(_)),
                        ast::Expr::Value(ast::Value::Primitive(ast::PrimitiveValue::Integer(
                            ast::Integer::Dec(ast::IntegerDec { val: 3 })
                        )))
                    ]
                ))
            }
            _ => unreachable!(),
        }
    }
}
