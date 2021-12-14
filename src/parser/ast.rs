use std::num::ParseIntError;
use std::str::FromStr;

use from_pest::pest::iterators::Pairs;
use from_pest::pest::Span;
use from_pest::{ConversionError, FromPest, Void};
use pest_ast::FromPest;

use super::Rule;

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::main))]
pub struct MainProgram<'p> {
    pub stmts: Vec<Stmt<'p>>,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::stmt))]
pub enum Stmt<'p> {
    VarDef(VarDef<'p>),
    Expr(Expr<'p>),
}

#[derive(Debug, Clone)]
pub enum Expr<'p> {
    Value(Value<'p>),
    Ident(Ident<'p>),
    OpExpr(Box<OpExpr<'p>>),
}

impl<'p> FromPest<'p> for Expr<'p> {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pairs: &mut Pairs<'p, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        pairs
            .peek()
            .filter(|pair| pair.as_rule() == Rule::expr || pair.as_rule() == Rule::op_term)
            .ok_or(::from_pest::ConversionError::NoMatch)?;
        let mut pairs = pairs.next().unwrap().into_inner();
        let pair = pairs.peek().unwrap();
        Ok(match pair.as_rule() {
            Rule::op_expr => Expr::OpExpr(box OpExpr::from_pest(&mut pairs)?),
            Rule::value => Expr::Value(Value::from_pest(&mut pairs)?),
            Rule::ident => Expr::Ident(Ident::from_pest(&mut pairs)?),
            _ => return Err(ConversionError::NoMatch),
        })
    }
}

#[derive(Debug, Clone)]
pub struct OpExpr<'p> {
    pub lhs: Expr<'p>,
    pub op: Operator,
    pub rhs: Expr<'p>,
}

impl<'p> FromPest<'p> for OpExpr<'p> {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pairs: &mut Pairs<'p, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        pairs
            .peek()
            .filter(|pair| pair.as_rule() == Rule::op_expr)
            .ok_or(::from_pest::ConversionError::NoMatch)?;
        let mut pairs = pairs.next().unwrap().into_inner();

        let lhs = Expr::from_pest(&mut pairs)?;
        let op = Operator::from_pest(&mut pairs)?;

        let mut rhs = Expr::from_pest(&mut pairs)?;

        loop {
            match Operator::from_pest(&mut pairs) {
                Ok(op) => {
                    let new_rhs = Expr::from_pest(&mut pairs)?;
                    rhs = Expr::OpExpr(box OpExpr {
                        lhs: rhs,
                        op,
                        rhs: new_rhs,
                    });
                }
                Err(ConversionError::NoMatch) => break,
                Err(err @ ConversionError::Malformed(_)) => return Err(err),
            }
        }

        Ok(OpExpr { lhs, op, rhs })
    }
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::func_def))]
pub struct FuncDef<'p> {
    pub name: Ident<'p>,
    pub params: FuncDefParams<'p>,
    pub ret_type: Type,
    pub body: FuncDefBody<'p>,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::func_def_body))]
pub struct FuncDefBody<'p> {
    pub stmts: Vec<Stmt<'p>>,
    pub ret: Option<Expr<'p>>,
}

#[derive(Debug, Clone)]
pub struct FuncDefParam<'p> {
    pub name: Ident<'p>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FuncDefParams<'p>(pub Vec<FuncDefParam<'p>>);

impl<'p> FromPest<'p> for FuncDefParams<'p> {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pairs: &mut Pairs<'p, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        pairs
            .peek()
            .filter(|pair| pair.as_rule() == Rule::func_def_params)
            .ok_or(::from_pest::ConversionError::NoMatch)?;
        let mut pairs = pairs.next().unwrap().into_inner();
        let mut params = Vec::with_capacity(3);
        let mut ids = Vec::with_capacity(3);
        loop {
            let next = if let Some(next) = pairs.peek() {
                next
            } else {
                break;
            };
            match next.as_rule() {
                Rule::_type => {
                    let ty = Type::from_pest(&mut pairs)?;
                    params.extend(ids.iter().map(|id| FuncDefParam { name: *id, ty }));
                    ids.clear();
                }
                Rule::ident => {
                    let id = Ident::from_pest(&mut pairs)?;
                    ids.push(id);
                }
                _ => unreachable!(),
            }
        }
        Ok(FuncDefParams(params))
    }
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::var_def))]
pub struct VarDef<'p> {
    pub name: Ident<'p>,
    pub val: Expr<'p>,
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::_type))]
pub enum Type {
    Int(IntType),
    Float(FloatType),
    Char(CharType),
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::int_type))]
pub struct IntType;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::float_type))]
pub struct FloatType;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::char_type))]
pub struct CharType;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::ident))]
pub struct Ident<'p>(#[pest_ast(outer(with(span_into_str)))] pub &'p str);

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::value))]
pub enum Value<'p> {
    Primitive(PrimitiveValue),
    String(StringLiteral<'p>),
    Array(Array<'p>),
    Tuple(Tuple<'p>),
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::primitive_value))]
pub enum PrimitiveValue {
    Char(Char),
    Integer(Integer),
    Float(Float),
}

#[derive(Debug, Clone)]
pub struct Params<'p>(pub Vec<Expr<'p>>);

impl<'p> FromPest<'p> for Params<'p> {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pairs: &mut Pairs<'p, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut params = Vec::with_capacity(3);
        loop {
            match Expr::from_pest(pairs) {
                Ok(expr) => {
                    params.push(expr);
                }
                Err(ConversionError::NoMatch) => break,
                Err(err) => return Err(err),
            }
        }
        Ok(Params(params))
    }
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::tuple))]
pub struct Tuple<'p> {
    pub params: Params<'p>,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::array))]
pub struct Array<'p> {
    pub params: Params<'p>,
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::string))]
pub struct StringLiteral<'p> {
    #[pest_ast(outer(with(span_into_str), with(str_to_str_lit)))]
    pub val: &'p str,
}

fn str_to_str_lit(s: &str) -> &str {
    s.trim_matches('\"')
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::char))]
pub struct Char {
    #[pest_ast(outer(with(span_into_str), with(str_to_char)))]
    pub val: char,
}

fn str_to_char(s: &str) -> char {
    s.trim_matches('\'').chars().next().unwrap()
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::operator))]
pub enum Operator {
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
    Pow(Pow),
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::op_add))]
pub struct Add;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::op_sub))]
pub struct Sub;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::op_mul))]
pub struct Mul;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::op_div))]
pub struct Div;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::op_pow))]
pub struct Pow;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::float))]
pub struct Float {
    #[pest_ast(outer(with(span_into_str), with(str::parse), with(Result::unwrap)))]
    pub val: f64,
}

fn span_into_float(span: Span) -> f64 {
    let (integer, decimal) = span.as_str().split_once(".").unwrap();
    let mut float_str = match &integer[..2] {
        "0b" | "0B" => bin_str_to_int(&integer[2..]).unwrap().to_string(),
        "0o" | "0O" => oct_str_to_int(&integer[2..]).unwrap().to_string(),
        "0x" | "0X" => hex_str_to_int(&integer[2..]).unwrap().to_string(),
        other => other.to_owned(),
    };
    float_str.push('.');
    float_str.push_str(decimal);
    f64::from_str(&float_str).unwrap()
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer))]
pub enum Integer {
    Dec(IntegerDec),
    Bin(IntegerBin),
    Oct(IntegerOct),
    Hex(IntegerHex),
}

impl Integer {
    pub fn as_i64(&self) -> i64 {
        match self {
            Integer::Dec(i) => i.val,
            Integer::Bin(i) => i.val,
            Integer::Oct(i) => i.val,
            Integer::Hex(i) => i.val,
        }
    }
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer_dec))]
pub struct IntegerDec {
    #[pest_ast(outer(with(span_into_str), with(str::parse), with(Result::unwrap)))]
    pub val: i64,
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer_bin))]
pub struct IntegerBin {
    #[pest_ast(outer(with(span_into_str), with(bin_str_to_int), with(Result::unwrap)))]
    pub val: i64,
}

fn bin_str_to_int(s: &str) -> Result<i64, ParseIntError> {
    i64::from_str_radix(&s[2..], 2)
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer_oct))]
pub struct IntegerOct {
    #[pest_ast(outer(with(span_into_str), with(oct_str_to_int), with(Result::unwrap)))]
    pub val: i64,
}

fn oct_str_to_int(s: &str) -> Result<i64, ParseIntError> {
    i64::from_str_radix(&s[2..], 8)
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer_hex))]
pub struct IntegerHex {
    #[pest_ast(outer(with(span_into_str), with(hex_str_to_int), with(Result::unwrap)))]
    pub val: i64,
}

fn hex_str_to_int(s: &str) -> Result<i64, ParseIntError> {
    i64::from_str_radix(&s[2..], 16)
}

fn span_into_str(span: Span) -> &str {
    span.as_str()
}
