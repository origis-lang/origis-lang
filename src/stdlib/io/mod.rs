use crate::interpreter::{
    FuncArgs, InterpretationError, Interpreter, ModuleCtx, Value,
};

pub fn print(
    _: &Interpreter,
    _: &ModuleCtx,
    args: FuncArgs,
) -> Result<Value, InterpretationError> {
    args.into_iter().for_each(|arg| {
        print!("{}", arg.1);
    });
    Ok(Value::unit())
}
