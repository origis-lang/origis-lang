use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Div, Mul, Rem, Sub};

use compact_str::CompactStr;

use crate::interpreter::func::Func;
use crate::interpreter::gc::Gc;
use crate::interpreter::InterpretationError;

#[derive(Debug, PartialEq, PartialOrd, gc::Trace, gc::Finalize)]
pub enum Value {
    Primitive(#[unsafe_ignore_trace] PrimitiveValue),
    Gc(Gc<GcValue>),
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
pub enum PrimitiveValue {
    Unit,
    Integer(i64),
    Float(f64),
    Char(char),
    Bool(bool),
}

#[derive(Debug, PartialEq, PartialOrd, gc::Trace, gc::Finalize)]
pub enum GcValue {
    String(#[unsafe_ignore_trace] CompactStr),
    Array(Vec<Value>),
    Func(#[unsafe_ignore_trace] Func),
}

impl GcValue {
    pub fn type_name(&self) -> &'static str {
        match self {
            GcValue::String(_) => "string",
            GcValue::Array(_) => "array",
            GcValue::Func(_) => "function",
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Primitive(PrimitiveValue::Unit) => "unit",
            Value::Primitive(PrimitiveValue::Integer(_)) => "integer",
            Value::Primitive(PrimitiveValue::Float(_)) => "float",
            Value::Primitive(PrimitiveValue::Char(_)) => "char",
            Value::Primitive(PrimitiveValue::Bool(_)) => "bool",
            Value::Gc(val) => val.borrow().type_name(),
        }
    }

    pub fn expect_unit(&self) -> Result<(), InterpretationError> {
        match self {
            Value::Primitive(PrimitiveValue::Unit) => Ok(()),
            _ => Err(InterpretationError::TypeNotMatch(
                "unit",
                self.type_name(),
            )),
        }
    }

    pub fn expect_int(&self) -> Result<i64, InterpretationError> {
        match self {
            Value::Primitive(PrimitiveValue::Integer(i)) => Ok(*i),
            _ => Err(InterpretationError::TypeNotMatch(
                "integer",
                self.type_name(),
            )),
        }
    }

    pub fn expect_float(&self) -> Result<f64, InterpretationError> {
        match self {
            Value::Primitive(PrimitiveValue::Float(f)) => Ok(*f),
            _ => Err(InterpretationError::TypeNotMatch(
                "float",
                self.type_name(),
            )),
        }
    }

    pub fn expect_char(&self) -> Result<char, InterpretationError> {
        match self {
            Value::Primitive(PrimitiveValue::Char(c)) => Ok(*c),
            _ => Err(InterpretationError::TypeNotMatch(
                "char",
                self.type_name(),
            )),
        }
    }

    pub fn expect_str(
        &self,
    ) -> Result<CompactStr, InterpretationError> {
        match self {
            Value::Gc(s) => match &*s.borrow() {
                GcValue::String(s) => return Ok(s.clone()),
                _ => {}
            },
            _ => {}
        };
        Err(InterpretationError::TypeNotMatch(
            "string",
            self.type_name(),
        ))
    }

    pub fn expect_bool(&self) -> Result<bool, InterpretationError> {
        match self {
            Value::Primitive(PrimitiveValue::Bool(b)) => Ok(*b),
            _ => Err(InterpretationError::TypeNotMatch(
                "boolean",
                self.type_name(),
            )),
        }
    }

    #[inline]
    pub fn unit() -> Self {
        Value::Primitive(PrimitiveValue::Unit)
    }

    #[inline]
    pub fn int(i: i64) -> Self {
        Value::Primitive(PrimitiveValue::Integer(i))
    }

    #[inline]
    pub fn float(f: f64) -> Self {
        Value::Primitive(PrimitiveValue::Float(f))
    }

    #[inline]
    pub fn char(c: char) -> Self {
        Value::Primitive(PrimitiveValue::Char(c))
    }

    #[inline]
    pub fn bool(b: bool) -> Self {
        Value::Primitive(PrimitiveValue::Bool(b))
    }

    #[inline]
    pub fn str<S>(s: S) -> Self
    where
        S: Into<CompactStr>,
    {
        Value::Gc(Gc::new(GcValue::String(s.into())))
    }

    #[inline]
    pub fn func(f: Func) -> Self {
        Value::Gc(Gc::new(GcValue::Func(f)))
    }

    #[inline]
    pub fn arr(arr: Vec<Value>) -> Self {
        Value::Gc(Gc::new(GcValue::Array(arr)))
    }
}

impl Add for Value {
    type Output = Result<Self, InterpretationError>;

    fn add(self, rhs: Self) -> Self::Output {
        macro_rules! mismatched_operation {
            () => {
                return Err(InterpretationError::MismatchedOperation(
                    self.type_name(),
                    "+",
                    rhs.type_name(),
                ))
            };
        }
        Ok(match &self {
            Value::Primitive(PrimitiveValue::Integer(l_int)) => {
                match &rhs {
                    Value::Primitive(PrimitiveValue::Integer(
                        r_int,
                    )) => Value::int(l_int + r_int),
                    Value::Primitive(PrimitiveValue::Float(
                        r_float,
                    )) => Value::float(*l_int as f64 + r_float),
                    _ => mismatched_operation!(),
                }
            }
            Value::Primitive(PrimitiveValue::Float(l_float)) => {
                match &rhs {
                    Value::Primitive(PrimitiveValue::Integer(i)) => {
                        Value::float(l_float + *i as f64)
                    }
                    Value::Primitive(PrimitiveValue::Float(
                        r_float,
                    )) => Value::float(l_float + r_float),
                    _ => mismatched_operation!(),
                }
            }
            Value::Primitive(PrimitiveValue::Char(l_char)) => {
                match &rhs {
                    Value::Primitive(PrimitiveValue::Char(
                        r_char,
                    )) => Value::str(CompactStr::from_iter([
                        l_char, r_char,
                    ])),
                    Value::Gc(val) => match &*val.borrow() {
                        GcValue::String(r_str) => {
                            Value::str(CompactStr::from(format!(
                                "{}{}",
                                l_char, r_str
                            )))
                        }
                        _ => mismatched_operation!(),
                    },
                    _ => mismatched_operation!(),
                }
            }
            Value::Gc(val) => match &*val.borrow() {
                GcValue::String(l_str) => match &rhs {
                    Value::Primitive(PrimitiveValue::Char(
                        r_char,
                    )) => Value::str(CompactStr::from(format!(
                        "{}{}",
                        l_str, r_char
                    ))),
                    Value::Gc(val) => match &*val.borrow() {
                        GcValue::String(r_str) => {
                            Value::str(CompactStr::from(format!(
                                "{}{}",
                                l_str, r_str
                            )))
                        }
                        _ => mismatched_operation!(),
                    },
                    _ => mismatched_operation!(),
                },
                GcValue::Array(arr) => {
                    let mut arr = arr.clone();
                    arr.push(rhs);
                    Value::arr(arr)
                }
                _ => mismatched_operation!(),
            },
            _ => mismatched_operation!(),
        })
    }
}

impl Value {
    pub fn add_assign(
        &mut self,
        rhs: Self,
    ) -> Result<(), InterpretationError> {
        macro_rules! mismatched_operation {
            () => {
                return Err(InterpretationError::MismatchedOperation(
                    self.type_name(),
                    "+=",
                    rhs.type_name(),
                ))
            };
        }
        match self {
            Value::Primitive(PrimitiveValue::Integer(l_int)) => {
                match &rhs {
                    Value::Primitive(PrimitiveValue::Integer(
                        r_int,
                    )) => *l_int += r_int,
                    Value::Primitive(PrimitiveValue::Float(
                        r_float,
                    )) => *l_int += *r_float as i64,
                    _ => mismatched_operation!(),
                }
            }
            Value::Primitive(PrimitiveValue::Float(l_float)) => {
                match &rhs {
                    Value::Primitive(PrimitiveValue::Integer(i)) => {
                        *l_float += *i as f64
                    }
                    Value::Primitive(PrimitiveValue::Float(
                        r_float,
                    )) => *l_float += r_float,
                    _ => mismatched_operation!(),
                }
            }
            Value::Gc(val) => match &mut *val.clone().borrow_mut() {
                GcValue::String(l_str) => match &rhs {
                    Value::Primitive(PrimitiveValue::Char(
                        r_char,
                    )) => {
                        *l_str = CompactStr::from(format!(
                            "{}{}",
                            l_str, r_char
                        ))
                    }
                    Value::Gc(val) => match &*val.borrow() {
                        GcValue::String(r_str) => {
                            *l_str = CompactStr::from(format!(
                                "{}{}",
                                l_str, r_str
                            ))
                        }
                        _ => mismatched_operation!(),
                    },
                    _ => mismatched_operation!(),
                },
                GcValue::Array(arr) => {
                    arr.push(rhs);
                }
                _ => mismatched_operation!(),
            },
            _ => mismatched_operation!(),
        };
        Ok(())
    }
}

macro_rules! impl_op {
    ($trait_name:ident, $fn_name:ident, $op:tt) => {
        impl $trait_name for Value {
            type Output = Result<Self, InterpretationError>;

            fn $fn_name(self, rhs: Self) -> Self::Output {
                macro_rules! mismatched_operation {
                    () => {
                        return Err(InterpretationError::MismatchedOperation(
                            self.type_name(),
                            stringify!($op),
                            rhs.type_name(),
                        ))
                    };
                }
                Ok(match &self {
                    Value::Primitive(PrimitiveValue::Integer(l_int)) => match &rhs {
                        Value::Primitive(PrimitiveValue::Integer(r_int)) => Value::int(l_int $op r_int),
                        Value::Primitive(PrimitiveValue::Float(r_float)) => Value::float(*l_int as f64 $op r_float),
                        _ => mismatched_operation!(),
                    },
                    Value::Primitive(PrimitiveValue::Float(l_float)) => match &rhs {
                        Value::Primitive(PrimitiveValue::Integer(r_int)) => Value::float(l_float $op *r_int as f64),
                        Value::Primitive(PrimitiveValue::Float(r_float)) => Value::float(l_float $op r_float),
                        _ => mismatched_operation!(),
                    },
                    _ => mismatched_operation!(),
                })
            }
        }
    };
}

impl_op!(Sub, sub, -);
impl_op!(Mul, mul, *);
impl_op!(Div, div, /);
impl_op!(Rem, rem, %);

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::unit()
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::str(CompactStr::from(s))
    }
}

impl<'a> From<&'a str> for Value {
    fn from(s: &'a str) -> Self {
        Value::str(CompactStr::from(s))
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::int(i)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::float(f)
    }
}

impl From<char> for Value {
    fn from(ch: char) -> Self {
        Value::char(ch)
    }
}

impl<const N: usize> From<[Value; N]> for Value {
    fn from(arr: [Value; N]) -> Self {
        Value::arr(arr.to_vec())
    }
}

impl From<Vec<Value>> for Value {
    fn from(arr: Vec<Value>) -> Self {
        Value::arr(arr)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Primitive(PrimitiveValue::Unit) => {
                write!(f, "()")?
            }
            Value::Primitive(PrimitiveValue::Integer(i)) => {
                write!(f, "{}", i)?
            }
            Value::Primitive(PrimitiveValue::Float(fl)) => {
                write!(f, "{}", fl)?
            }
            Value::Primitive(PrimitiveValue::Char(c)) => {
                write!(f, "{}", c)?
            }
            Value::Primitive(PrimitiveValue::Bool(b)) => {
                write!(f, "{}", b)?
            }
            Value::Gc(val) => match &*val.borrow() {
                GcValue::String(s) => write!(f, "{}", s)?,
                GcValue::Array(arr) => {
                    write!(f, "[")?;
                    arr.iter().try_for_each(|val| {
                        Display::fmt(val, f)?;
                        write!(f, ", ")
                    })?;
                    write!(f, "]")?;
                }
                GcValue::Func(func) => {
                    Debug::fmt(func, f)?;
                }
            },
        }
        Ok(())
    }
}

impl Clone for Value {
    #[inline]
    fn clone(&self) -> Self {
        match self {
            Value::Primitive(val) => Value::Primitive(*val),
            Value::Gc(val) => Value::Gc(Gc::clone(val)),
        }
    }
}
