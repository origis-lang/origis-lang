use std::fmt::{Display, Formatter};
use std::num::ParseIntError;
use std::str::FromStr;

use compact_str::CompactStr;
use from_pest::pest::iterators::Pairs;
use from_pest::pest::Span;
use from_pest::{ConversionError, FromPest, Void};
use pest_ast::FromPest;

use super::Rule;

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::module))]
pub struct Module {
    pub defs: Vec<Definition>,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::definition))]
pub enum Definition {
    FuncDef(FuncDef),
    UseDef(UseDef),
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::stmt))]
pub enum Stmt {
    VarDef(VarDef),
    Definition(Definition),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Value),
    Ident(Ident),
    OpExpr(Box<OpExpr>),
    Call(FuncCall),
    If(Box<IfExpr>),
}

impl<'p> FromPest<'p> for Expr {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pairs: &mut Pairs<'p, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        pairs
            .peek()
            .filter(|pair| {
                pair.as_rule() == Rule::expr
                    || pair.as_rule() == Rule::op_term
            })
            .ok_or(::from_pest::ConversionError::NoMatch)?;
        let mut pairs = pairs.next().unwrap().into_inner();
        let pair = pairs.peek().unwrap();
        Ok(match pair.as_rule() {
            Rule::op_expr => {
                Expr::OpExpr(box OpExpr::from_pest(&mut pairs)?)
            }
            Rule::value => Expr::Value(Value::from_pest(&mut pairs)?),
            Rule::ident => Expr::Ident(Ident::from_pest(&mut pairs)?),
            Rule::func_call => {
                Expr::Call(FuncCall::from_pest(&mut pairs)?)
            }
            Rule::if_expr => {
                Expr::If(box IfExpr::from_pest(&mut pairs)?)
            }
            _ => return Err(ConversionError::NoMatch),
        })
    }
}

#[derive(Debug, Clone)]
pub struct OpExpr {
    pub lhs: Expr,
    pub op: Operator,
    pub rhs: Expr,
}

impl<'p> FromPest<'p> for OpExpr {
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
                Err(err @ ConversionError::Malformed(_)) => {
                    return Err(err)
                }
            }
        }

        Ok(OpExpr { lhs, op, rhs })
    }
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::if_expr))]
pub struct IfExpr {
    pub condition: Expr,
    pub body: FuncDefBody,
    pub else_ifs: Vec<ElseIf>,
    pub r#else: Option<Else>,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::else_if))]
pub struct ElseIf {
    pub condition: Expr,
    pub body: FuncDefBody,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::_else))]
pub struct Else(pub FuncDefBody);

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::use_def))]
pub struct UseDef(pub ModulePath);

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::module_path))]
pub struct ModulePath(pub Vec<Ident>);

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::func_def))]
pub struct FuncDef {
    pub name: Ident,
    pub params: FuncDefParams,
    pub ret_type: Type,
    pub body: FuncDefBody,
}

#[derive(Debug, Clone)]
pub struct FuncDefBody {
    pub stmts: Vec<Stmt>,
    pub ret: Option<Expr>,
}

impl<'p> FromPest<'p> for FuncDefBody {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pairs: &mut Pairs<'p, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        pairs
            .peek()
            .filter(|pair| pair.as_rule() == Rule::func_def_body)
            .ok_or(ConversionError::NoMatch)?;
        let mut pairs = pairs.next().unwrap().into_inner();
        let mut body = Vec::with_capacity(5);
        let mut ret = None;
        loop {
            let next = if let Some(next) = pairs.peek() {
                next
            } else {
                break;
            };
            match next.as_rule() {
                Rule::stmt => {
                    body.push(Stmt::from_pest(&mut pairs)?);
                }
                Rule::expr => {
                    ret = Some(Expr::from_pest(&mut pairs)?);
                }
                _ => unreachable!(),
            }
        }
        Ok(FuncDefBody { stmts: body, ret })
    }
}

#[derive(Debug, Clone)]
pub struct FuncDefParam {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FuncDefParams(pub Vec<FuncDefParam>);

impl<'p> FromPest<'p> for FuncDefParams {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pairs: &mut Pairs<'p, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        pairs
            .peek()
            .filter(|pair| pair.as_rule() == Rule::func_def_params)
            .ok_or(ConversionError::NoMatch)?;
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
                    params.extend(ids.iter().map(|id| {
                        FuncDefParam {
                            name: Ident::clone(id),
                            ty: ty.clone(),
                        }
                    }));
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
#[pest_ast(rule(Rule::func_call))]
pub struct FuncCall {
    pub name: Ident,
    pub args: FuncCallArgs,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::func_call_args))]
pub struct FuncCallArgs(pub Vec<Expr>);

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::var_def))]
pub struct VarDef {
    pub name: Ident,
    pub ty: Type,
    pub val: Expr,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::_type))]
pub enum Type {
    Unit(UnitType),
    Int(IntType),
    Float(FloatType),
    Char(CharType),
    Custom(CustomType),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit(_) => write!(f, "unit"),
            Type::Int(_) => write!(f, "int"),
            Type::Float(_) => write!(f, "float"),
            Type::Char(_) => write!(f, "char"),
            Type::Custom(ty) => write!(f, "{}", ty.0),
        }
    }
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::custom_type))]
pub struct CustomType(pub Ident);

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::unit_type))]
pub struct UnitType;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::int_type))]
pub struct IntType;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::float_type))]
pub struct FloatType;

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::char_type))]
pub struct CharType;

#[derive(Debug, FromPest, Clone, PartialEq)]
#[pest_ast(rule(Rule::ident))]
pub struct Ident(
    #[pest_ast(outer(
        with(span_into_str),
        with(str_to_compact_str)
    ))]
    pub CompactStr,
);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::value))]
pub enum Value {
    Primitive(PrimitiveValue),
    String(StringLiteral),
    Array(Array),
    Tuple(Tuple),
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::primitive_value))]
pub enum PrimitiveValue {
    Char(Char),
    Integer(Integer),
    Float(Float),
    Bool(Bool),
}

#[derive(Debug, Clone)]
pub struct Params(pub Vec<Expr>);

impl<'p> FromPest<'p> for Params {
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
pub struct Tuple {
    pub params: Params,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::array))]
pub struct Array {
    pub params: Params,
}

#[derive(Debug, FromPest, Clone)]
#[pest_ast(rule(Rule::string))]
pub struct StringLiteral {
    #[pest_ast(outer(
        with(span_into_str),
        with(str_to_str_lit),
        with(str_to_compact_str)
    ))]
    pub val: CompactStr,
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

macro_rules! def_ops {
    ($($rule:ident: $name:ident),*) => {
        #[derive(Debug, FromPest, Copy, Clone)]
        #[pest_ast(rule(Rule::operator))]
        pub enum Operator {
            $($name($name)),*
        }

        $(
            #[derive(Debug, FromPest, Copy, Clone)]
            #[pest_ast(rule(Rule::$rule))]
            pub struct $name;
        )*
    };
}

def_ops! {
    op_add: Add,
    op_sub: Sub,
    op_mul: Mul,
    op_div: Div,
    op_pow: Pow,
    op_xor: Xor,
    op_eq: Eq,
    op_not_eq: NotEq,
    op_less: Less,
    op_greater: Greater,
    op_less_or_eq: LessOrEq,
    op_greater_or_eq: GreaterOrEq,
    op_rem: Rem,
    op_add_assign: AddAssign
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::boolean))]
pub struct Bool(
    #[pest_ast(outer(with(span_into_str), with(str_to_bool)))]
    pub  bool,
);

fn str_to_bool(s: &str) -> bool {
    s == "true" || s == "TRUE"
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::float))]
pub struct Float {
    #[pest_ast(outer(
        with(span_into_str),
        with(str::parse),
        with(Result::unwrap)
    ))]
    pub val: f64,
}

fn span_into_float(span: Span) -> f64 {
    let (integer, decimal) = span.as_str().split_once(".").unwrap();
    let mut float_str = match &integer[..2] {
        "0b" | "0B" => {
            bin_str_to_int(&integer[2..]).unwrap().to_string()
        }
        "0o" | "0O" => {
            oct_str_to_int(&integer[2..]).unwrap().to_string()
        }
        "0x" | "0X" => {
            hex_str_to_int(&integer[2..]).unwrap().to_string()
        }
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
    #[pest_ast(outer(
        with(span_into_str),
        with(str::parse),
        with(Result::unwrap)
    ))]
    pub val: i64,
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer_bin))]
pub struct IntegerBin {
    #[pest_ast(outer(
        with(span_into_str),
        with(bin_str_to_int),
        with(Result::unwrap)
    ))]
    pub val: i64,
}

fn bin_str_to_int(s: &str) -> Result<i64, ParseIntError> {
    i64::from_str_radix(&s[2..], 2)
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer_oct))]
pub struct IntegerOct {
    #[pest_ast(outer(
        with(span_into_str),
        with(oct_str_to_int),
        with(Result::unwrap)
    ))]
    pub val: i64,
}

fn oct_str_to_int(s: &str) -> Result<i64, ParseIntError> {
    i64::from_str_radix(&s[2..], 8)
}

#[derive(Debug, FromPest, Copy, Clone)]
#[pest_ast(rule(Rule::integer_hex))]
pub struct IntegerHex {
    #[pest_ast(outer(
        with(span_into_str),
        with(hex_str_to_int),
        with(Result::unwrap)
    ))]
    pub val: i64,
}

fn hex_str_to_int(s: &str) -> Result<i64, ParseIntError> {
    i64::from_str_radix(&s[2..], 16)
}

fn str_to_compact_str(s: &str) -> CompactStr {
    CompactStr::from(s)
}

fn span_into_str(span: Span) -> &str {
    span.as_str()
}
