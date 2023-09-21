use std::{ops::Range, sync::Arc};

use logos::Logos;

use crate::{runner::FLOAT_PRECISION, span::SpannedValue};

#[derive(Debug, derive_more::Display, Logos, Clone)]
#[logos(skip r"[ \t\f]+")]
pub enum Token {
    #[token("as")]
    #[display(fmt = "as")]
    As,
    #[token("(")]
    #[display(fmt = "(")]
    LParen,
    #[token(")")]
    #[display(fmt = ")")]
    RParen,
    #[token("|>")]
    #[display(fmt = "|>")]
    Redirect,
    #[token("|")]
    #[display(fmt = "|")]
    LOr,
    #[token("&")]
    #[display(fmt = "&")]
    LAnd,
    #[token("^")]
    #[display(fmt = "^")]
    LXor,
    #[token("=")]
    #[display(fmt = "=")]
    Equal,
    #[token("+")]
    #[display(fmt = "+")]
    Plus,
    #[token("-")]
    #[display(fmt = "-")]
    Minus,
    #[token("*")]
    #[display(fmt = "*")]
    Times,
    #[token("**")]
    #[display(fmt = "**")]
    Power,
    #[token("/")]
    #[display(fmt = "/")]
    Divide,
    #[token("//")]
    #[display(fmt = "//")]
    IntDivide,
    #[token(">>")]
    #[display(fmt = ">>")]
    RShift,
    #[token("<<")]
    #[display(fmt = "<<")]
    LShift,
    #[token("%")]
    #[display(fmt = "%")]
    Percent,
    #[token("\\")]
    #[display(fmt = "\\")]
    Backslash,
    #[token(".")]
    #[display(fmt = ".")]
    Dot,
    #[token(":")]
    #[display(fmt = ":")]
    Colon,
    #[token(",")]
    #[display(fmt = ",")]
    Comma,
    #[token("_(")]
    #[display(fmt = "_(")]
    UnitParen,
    #[regex("[0-9][0-9_]*", |lex| rug::Integer::from_str_radix(lex.slice(), 10).unwrap(), priority = 2)]
    #[regex("0?x[0-9a-fA-F][0-9a-fA-F_]*", |lex|
        rug::Integer::from_str_radix(
            lex.slice().trim_start_matches("0x").trim_start_matches('x'),
            16,
        ).expect("regex should only match hexadecimal")
    )]
    #[regex("0?b[0-1][0-1_]*", |lex|
        rug::Integer::from_str_radix(
            lex.slice().trim_start_matches("0b").trim_start_matches('b'),
            2,
        ).expect("regex should only match binary")
    )]
    #[display(fmt = "<number:{}>", _0)]
    Number(rug::Integer),
    #[regex("[0-9]+([eE][-+]?[0-9]+)?", parse_float)]
    #[regex("\\.[0-9]+([eE][-+]?[0-9]+)?", parse_float)]
    #[regex("[0-9]+\\.[0-9]*([eE][-+]?[0-9]+)?", parse_float)]
    #[display(fmt = "<float:{}>", _0)]
    Float(rug::Float),
    #[regex("[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| Arc::from(lex.slice()))]
    #[display(fmt = "identifier({})", _0)]
    Ident(Arc<str>),
    #[regex("\\$[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| Arc::from(&lex.slice()[1..]))]
    #[display(fmt = "identifier({})", _0)]
    Binding(Arc<str>),
    #[regex("_[a-zA-Z]*", callback = |lex| Arc::from(&lex.slice()[1..]))]
    #[display(fmt = "unit({})", _0)]
    Unit(Arc<str>),
}

fn parse_float(s: &mut logos::Lexer<Token>) -> rug::Float {
    rug::Float::with_val(
        FLOAT_PRECISION.load(std::sync::atomic::Ordering::Relaxed),
        rug::Float::parse(s.slice()).unwrap(),
    )
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Variable(pub Vec<Arc<str>>);

impl std::fmt::Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "< ")?;
        for word in &self.0 {
            write!(f, "{} ", word)?;
        }
        write!(f, ">")
    }
}

pub enum Literal {
    Number(rug::Integer),
    Float(rug::Float),
    Atom(Arc<str>),
}

impl std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(arg0) => write!(f, "{}", arg0),
            Self::Float(arg0) => write!(f, "{}", arg0),
            Self::Atom(s) => write!(f, ":{}", s),
        }
    }
}

pub enum BinOpKind {
    Times,
    Divide,
    Sum,
    Diff,
    LeftShift,
    RightShift,
    LogicalOr,
    LogicalAnd,
    LogicalXor,
}

impl std::fmt::Debug for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Times => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Sum => write!(f, "+"),
            Self::Diff => write!(f, "-"),
            Self::LeftShift => write!(f, "<<"),
            Self::RightShift => write!(f, ">>"),
            Self::LogicalOr => write!(f, "|"),
            Self::LogicalAnd => write!(f, "&"),
            Self::LogicalXor => write!(f, "^"),
        }
    }
}

pub struct BinOp {
    pub lhs: Box<SpannedValue<Expr>>,
    pub kind: BinOpKind,
    pub rhs: Box<SpannedValue<Expr>>,
}

impl std::fmt::Debug for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}) {:?} ({:?})", **self.lhs, self.kind, **self.rhs)
    }
}

pub enum InputStatement {
    Expr(Expr),
    LastRedirect(Function),
    Command(SpannedValue<Arc<str>>, Option<SpannedValue<Expr>>),
}

impl std::fmt::Debug for InputStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(arg0) => write!(f, "{arg0:?}"),
            Self::LastRedirect(arg0) => write!(f, "|> ({arg0:?})"),
            Self::Command(name, value) => write!(f, ".{:?}={:?}", name, value),
        }
    }
}

pub struct Call {
    pub fun: SpannedValue<Function>,
    pub args: Vec<SpannedValue<Expr>>,
}

impl std::fmt::Debug for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?})(", self.fun)?;
        if !self.args.is_empty() {
            write!(f, "{:?}", self.args[0])?;
            for arg in self.args.iter().skip(1) {
                write!(f, ", {arg:?}")?;
            }
        }
        write!(f, ")")
    }
}

pub enum Function {
    Ref(SpannedValue<Variable>),
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Ref(a) => write!(f, "{:?}", **a),
        }
    }
}

pub enum UnaryOpKind {
    Minus,
    Plus,
}

impl std::fmt::Debug for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
        }
    }
}

pub struct UnaryOp {
    pub operand: Box<SpannedValue<Expr>>,
    pub kind: UnaryOpKind,
}

impl std::fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.kind, self.operand)
    }
}

pub enum Expr {
    Dimensioned(Box<SpannedValue<Expr>>, Vec<SpannedValue<(Arc<str>, i16)>>),
    Literal(SpannedValue<Literal>),
    Variable(SpannedValue<Variable>),
    Assign(SpannedValue<Variable>, Box<SpannedValue<Expr>>),
    BinOp(BinOp),
    Call(Call),
    UnaryOp(UnaryOp),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dimensioned(l, d) => write!(f, "{:?}_{:?}", **l, d),
            Self::Literal(arg0) => write!(f, "{:?}", **arg0),
            Self::Variable(arg0) => f.debug_tuple("&").field(&**arg0).finish(),
            Self::Assign(arg0, arg1) => write!(f, "{:?} = ({:?})", **arg0, ***arg1),
            Self::BinOp(arg0) => write!(f, "({:?})", arg0),
            Self::Call(arg0) => write!(f, "{arg0:?}"),
            Self::UnaryOp(arg0) => write!(f, "({arg0:?}"),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum UserParseError {
    #[error("Token error: {0}")]
    UnknownToken(#[from] UnknownToken),
    #[error("Number '{num}' is out of range (expected '{ty}')")]
    OutOfRange {
        ty: &'static str,
        num: rug::Integer,
        span: Range<usize>,
    },
}

#[derive(Debug, thiserror::Error)]
#[error("unknown token: '{token}'")]
pub struct UnknownToken {
    pub token: String,
    pub span: Range<usize>,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn lexer(input: &str) -> impl Iterator<Item = Spanned<Token, usize, UserParseError>> + '_ {
    Token::lexer(input)
        .spanned()
        .map(move |(token, span)| match token {
            Err(_) => Err(UnknownToken {
                token: input[span.clone()].to_owned(),
                span,
            }
            .into()),
            Ok(v) => Ok((span.start, v, span.end)),
        })
}
