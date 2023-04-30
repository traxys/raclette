use std::{ops::Range, sync::Arc};

use logos::Logos;

use crate::span::SpannedValue;

#[derive(Debug, derive_more::Display, Logos, Clone)]
pub enum Token {
    #[token("(")]
    #[display(fmt = "(")]
    LParen,
    #[token(")")]
    #[display(fmt = ")")]
    RParen,
    #[token("|>")]
    #[display(fmt = "|>")]
    Redirect,
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
    #[token("_(")]
    #[display(fmt = "_(")]
    UnitParen,
    #[regex("(-)?[0-9][0-9_]*", |lex| lex.slice().parse(), priority = 2)]
    #[regex("0?x[0-9a-fA-F][0-9a-fA-F_]*", |lex|
        i64::from_str_radix(
            lex.slice().trim_start_matches("0x").trim_start_matches('x'),
            16,
        )
    )]
    #[regex("0?b[0-1][0-1_]*", |lex|
        i64::from_str_radix(
            lex.slice().trim_start_matches("0b").trim_start_matches('b'),
            2,
        )
    )]
    #[display(fmt = "<number:{}>", _0)]
    Number(i64),
    #[regex("[-+]?[0-9]+([eE][-+]?[0-9]+)?", |lex| lex.slice().parse())]
    #[regex("[-+]?\\.[0-9]+([eE][-+]?[0-9]+)?", |lex| lex.slice().parse())]
    #[regex("[-+]?[0-9]+\\.[0-9]*([eE][-+]?[0-9]+)?", |lex| lex.slice().parse())]
    #[display(fmt = "<float:{}>", _0)]
    Float(f64),
    #[regex("[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| Arc::from(lex.slice()))]
    #[display(fmt = "identifier({})", _0)]
    Ident(Arc<str>),
    #[regex("\\$[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| Arc::from(&lex.slice()[1..]))]
    #[display(fmt = "identifier({})", _0)]
    Binding(Arc<str>),
    #[regex("_[a-zA-Z]*", callback = |lex| Arc::from(&lex.slice()[1..]))]
    #[display(fmt = "unit({})", _0)]
    Unit(Arc<str>),
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    #[display(fmt = "<error>")]
    Error,
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
    Number(i64),
    Float(f64),
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
}

impl std::fmt::Debug for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Times => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Sum => write!(f, "+"),
            Self::Diff => write!(f, "-"),
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

pub enum Expr {
    DimensionalLiteral(SpannedValue<Literal>, Vec<SpannedValue<(Arc<str>, i16)>>),
    Literal(SpannedValue<Literal>),
    Variable(SpannedValue<Variable>),
    Assign(SpannedValue<Variable>, Box<SpannedValue<Expr>>),
    BinOp(BinOp),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DimensionalLiteral(l, d) => write!(f, "{:?}_{:?}", **l, d),
            Self::Literal(arg0) => write!(f, "{:?}", **arg0),
            Self::Variable(arg0) => f.debug_tuple("&").field(&**arg0).finish(),
            Self::Assign(arg0, arg1) => write!(f, "{:?} = ({:?})", **arg0, ***arg1),
            Self::BinOp(arg0) => write!(f, "({:?})", arg0),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("unknown token: '{token}'")]
pub struct UnknownToken {
    pub token: String,
    pub span: Range<usize>,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn lexer(input: &str) -> impl Iterator<Item = Spanned<Token, usize, UnknownToken>> + '_ {
    Token::lexer(input)
        .spanned()
        .map(move |(token, span)| match token {
            Token::Error => Err(UnknownToken {
                token: input[span.clone()].to_owned(),
                span,
            }),
            v => Ok((span.start, v, span.end)),
        })
}
