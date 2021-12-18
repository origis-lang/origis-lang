use std::ops::Add;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Value::Unit => {
                rhs
            }
            Value::Integer(l_int) => {
                match rhs {
                    Value::Unit => self,
                    Value::Integer(r_int) => Value::Integer(l_int + r_int)
                }
            }
        }
    }
}
