use std::collections::HashMap;

use crate::{
    interpreter::{Interpreter, Result},
    span::{Span, UNKNOWN_SPAN},
    Val,
};

use super::super::{power_nums, Num, Nums, Value};

pub enum Folder {
    Op(Box<dyn NativeFolder>),
    User(Val, Val),
}

impl Clone for Folder {
    fn clone(&self) -> Self {
        match self {
            Self::Op(arg0) => Self::Op(arg0.restart()),
            Self::User(arg0, arg1) => Self::User(arg0.clone(), arg1.clone()),
        }
    }
}

impl std::fmt::Debug for Folder {
    fn fmt<'a>(&'a self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Op(arg0) => f.debug_tuple("Op").field(&arg0.kind()).finish(),
            Self::User(arg0, arg1) => f.debug_tuple("User").field(arg0).field(arg1).finish(),
        }
    }
}

pub trait NativeFolder {
    fn start() -> Box<dyn NativeFolder>
    where
        Self: Sized;
    fn restart(&self) -> Box<dyn NativeFolder>;

    fn seed(&mut self, v: Val) -> Result<()>;

    fn accumulate(&mut self, value: &Val, interpreter: &mut Interpreter) -> Result<()>;
    fn finish(&self, span: &Span) -> Result<Val>;

    fn int_accumulate(&mut self, value: i64, interpreter: &mut Interpreter) -> Result<()> {
        self.accumulate(&Value::new_number(value, &*UNKNOWN_SPAN), interpreter)
    }

    fn kind(&self) -> &'static str;
}

macro_rules! int_folder {
    ($name:ident, $op:tt, $default:expr) => {
        pub(in crate::interpreter) struct $name {
            current: i64,
        }

        impl NativeFolder for $name {
            fn start() -> Box<dyn NativeFolder> {
                Box::new(Self {
                    current: $default,
                })
            }

            fn restart(&self) -> Box<dyn NativeFolder> {
                Self::start()
            }

            fn seed(&mut self, v: Val) -> Result<()> {
                self.current = v.borrow().cast_number(&*v.borrow())?;
                Ok(())
            }

            fn accumulate(&mut self, value: &Val, inter: &mut Interpreter) -> Result<()> {
                let v = value.borrow().cast_number(&*value.borrow())?;
                self.int_accumulate(v, inter)
            }

            fn int_accumulate(&mut self, value: i64, _: &mut Interpreter) -> Result<()> {
                self.current $op value;
                Ok(())
            }

            fn finish(&self, span: &Span) -> Result<Val> {
                Ok(Value::new_number(self.current, span))
            }

            fn kind(&self) -> &'static str {
                stringify!($name)
            }
        }
    };
}

int_folder! {BitwiseOrFolder, |=, 0}
int_folder! {BitwiseAndFolder, &=, -1}
int_folder! {IntDivideFolder, /=, 0}
int_folder! {LeftShiftFolder, <<=, 0}
int_folder! {RightShiftFolder, >>=, 0}
int_folder! {ModuloFolder, %=, 0}

macro_rules! nums_op {
    ($nums:expr, $op:tt) => {
        match $nums {
            Nums::Float(l, r) => Num::Float(l $op r),
            Nums::Int(l, r) => Num::Int(l $op r),
        }
    };
}

macro_rules! num_folder {
    ($name:ident, $op:tt, $default:expr) => {
        pub(in crate::interpreter) struct $name {
            current: Num,
        }

        impl NativeFolder for $name {
            fn start() -> Box<dyn NativeFolder> {
                Box::new(Self {
                    current: Num::Int($default),
                })
            }

            fn restart(&self) -> Box<dyn NativeFolder> {
                Self::start()
            }

            fn seed(&mut self, v: Val) -> Result<()> {
                self.current = v.borrow().cast_num(&v.borrow())?;
                Ok(())
            }

            fn accumulate(&mut self, value: &Val, _: &mut Interpreter) -> Result<()> {
                let v = value.borrow().cast_num(&value.borrow())?;
                self.current = nums_op!((self.current, v).into(), $op);

                Ok(())
            }

            fn int_accumulate(&mut self, value: i64, _: &mut Interpreter) -> Result<()> {
                self.current = nums_op!((self.current, Num::Int(value)).into(), $op);

                Ok(())
            }

            fn finish(&self, span: &Span) -> Result<Val> {
                Ok(Value::new_num(self.current, span))
            }

            fn kind(&self) -> &'static str {
                stringify!($name)
            }
        }
    };
}

num_folder! {TimesFolder, *, 1}
num_folder! {SumFolder, +, 0}
num_folder! {DifferenceFolder, -, 0}

pub(in crate::interpreter) struct PowerFolder {
    current: Num,
}

impl NativeFolder for PowerFolder {
    fn start() -> Box<dyn NativeFolder>
    where
        Self: Sized,
    {
        Box::new(Self {
            current: Num::Int(0),
        })
    }

    fn restart(&self) -> Box<dyn NativeFolder> {
        Self::start()
    }

    fn seed(&mut self, v: Val) -> Result<()> {
        self.current = v.borrow().cast_num(&v.borrow())?;
        Ok(())
    }

    fn accumulate(&mut self, value: &Val, _: &mut Interpreter) -> Result<()> {
        let v = value.borrow().cast_num(&value.borrow())?;
        self.current = power_nums(self.current, v);

        Ok(())
    }

    fn finish(&self, span: &Span) -> Result<Val> {
        Ok(Value::new_num(self.current, span))
    }

    fn kind(&self) -> &'static str {
        "PowerFolder"
    }
}

pub(in crate::interpreter) struct DivideFolder {
    current: f64,
}

impl NativeFolder for DivideFolder {
    fn start() -> Box<dyn NativeFolder>
    where
        Self: Sized,
    {
        Box::new(Self { current: 0. })
    }

    fn restart(&self) -> Box<dyn NativeFolder> {
        Self::start()
    }

    fn seed(&mut self, v: Val) -> Result<()> {
        self.current = v.borrow().cast_num(&*v.borrow())?.f64();
        Ok(())
    }

    fn accumulate(&mut self, value: &Val, _: &mut Interpreter) -> Result<()> {
        let v = value.borrow().cast_num(&*value.borrow())?;
        self.current /= v.f64();

        Ok(())
    }

    fn finish(&self, span: &Span) -> Result<Val> {
        Ok(Value::new_float(self.current, span))
    }

    fn kind(&self) -> &'static str {
        "DivideFolder"
    }
}

pub(in crate::interpreter) struct RedirectFolder {
    current: Val,
}

impl NativeFolder for RedirectFolder {
    fn start() -> Box<dyn NativeFolder>
    where
        Self: Sized,
    {
        Box::new(Self {
            current: Value::new_number(0, &*UNKNOWN_SPAN),
        })
    }

    fn restart(&self) -> Box<dyn NativeFolder> {
        Self::start()
    }

    fn seed(&mut self, v: Val) -> Result<()> {
        self.current = v;
        Ok(())
    }

    fn accumulate(&mut self, value: &Val, interpreter: &mut Interpreter) -> Result<()> {
        let v = value.borrow().cast_function(&*value.borrow())?;
        self.current = v.call(
            vec![self.current.clone()],
            HashMap::new(),
            interpreter,
            &UNKNOWN_SPAN,
        )?;

        Ok(())
    }

    fn finish(&self, span: &Span) -> Result<Val> {
        let v = self.current.clone();
        v.borrow_mut().swap_span(span);
        Ok(v)
    }

    fn kind(&self) -> &'static str {
        "RedirectFolder"
    }
}
