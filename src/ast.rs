use std::{
    num::{ParseFloatError, ParseIntError},
    ops::Range,
    sync::Arc,
};

use logos::Logos;

use crate::span::SpannedValue;

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, Default)]
pub enum TokenError {
    #[default]
    #[error("Invalid token encountered")]
    InvalidToken,
    #[error("Integer could not be parsed")]
    ParseInt(#[from] ParseIntError),
    #[error("Float could not be parsed")]
    ParseFloat(#[from] ParseFloatError),
}

#[derive(Debug, derive_more::Display, Logos, Clone)]
#[logos(error = TokenError)]
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
    #[token("||")]
    #[display(fmt = "||")]
    LOr,
    #[token("&&")]
    #[display(fmt = "&&")]
    LAnd,
    #[token("|")]
    #[display(fmt = "|")]
    BOr,
    #[token("&")]
    #[display(fmt = "&")]
    BAnd,
    #[token("^")]
    #[display(fmt = "^")]
    BXor,
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
    #[token("'(")]
    #[token(",(")]
    #[display(fmt = "'(")]
    UnitParen,
    #[token("==")]
    #[display(fmt = "==")]
    LogicalEquals,
    #[token("!=")]
    #[display(fmt = "!=")]
    Different,
    #[token(">=")]
    #[display(fmt = ">=")]
    GreaterOrEqual,
    #[token(">")]
    #[display(fmt = ">")]
    Greater,
    #[token("<=")]
    #[display(fmt = "<=")]
    LesserOrEqual,
    #[token("<")]
    #[display(fmt = "<")]
    Lesser,
    #[regex("[0-9][_0-9]*", |lex| i128::from_str_radix(&lex.slice().chars().filter(|&c| c != '_').collect::<String>(), 10).map_err(TokenError::from), priority = 2)]
    #[regex("0?x[0-9a-fA-F][0-9a-fA-F_]*", |lex|
        i128::from_str_radix(
            &lex.slice().trim_start_matches("0x").trim_start_matches('x').chars().filter(|&c| c != '_').collect::<String>(),
            16,
        ).map_err(TokenError::from)
    )]
    #[regex("0?b[0-1][0-1_]*", |lex|
        i128::from_str_radix(
            &lex.slice().trim_start_matches("0b").trim_start_matches('b').chars().filter(|&c| c != '_').collect::<String>(),
            2,
        ).map_err(TokenError::from)
    )]
    #[display(fmt = "<number:{}>", _0)]
    Number(i128),
    #[regex("[0-9]+([eE][-+]?[0-9]+)?", |s| s.slice().parse(), priority = 1)]
    #[regex("\\.[0-9]+([eE][-+]?[0-9]+)?", |s| s.slice().parse())]
    #[regex("[0-9]+\\.[0-9]*([eE][-+]?[0-9]+)?", |s| s.slice().parse())]
    #[display(fmt = "<float:{}>", _0)]
    Float(f64),
    #[regex("[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| Arc::from(lex.slice()))]
    #[display(fmt = "identifier({})", _0)]
    Ident(Arc<str>),
    #[regex("\\$[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| Arc::from(&lex.slice()[1..]))]
    #[display(fmt = "identifier({})", _0)]
    Binding(Arc<str>),
    #[regex("('|,)[a-zA-Z]+", callback = |lex| Arc::from(&lex.slice()[1..]))]
    #[display(fmt = "unit({})", _0)]
    Unit(Arc<str>),
    #[token("true")]
    True,
    #[token("false")]
    False,
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
    Number(i128),
    Float(f64),
    Atom(Arc<str>),
    Bool(bool),
}

impl std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(arg0) => write!(f, "{}", arg0),
            Self::Float(arg0) => write!(f, "{}", arg0),
            Self::Atom(s) => write!(f, ":{}", s),
            Self::Bool(b) => write!(f, "{b}"),
        }
    }
}

pub enum BinOpKind {
    Times,
    Modulo,
    Divide,
    Sum,
    Diff,
    LeftShift,
    RightShift,
    LogicalOr,
    LogicalAnd,
    BinaryOr,
    BinaryAnd,
    BinaryXor,
    Greater,
    GreaterOrEqual,
    Lesser,
    LesserOrEqual,
    LogicalEquals,
    Different,
}

impl std::fmt::Debug for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Times => write!(f, "*"),
            Self::Modulo => write!(f, "%"),
            Self::Divide => write!(f, "/"),
            Self::Sum => write!(f, "+"),
            Self::Diff => write!(f, "-"),
            Self::LeftShift => write!(f, "<<"),
            Self::RightShift => write!(f, ">>"),
            Self::BinaryOr => write!(f, "|"),
            Self::BinaryAnd => write!(f, "&"),
            Self::BinaryXor => write!(f, "^"),
            Self::Greater => write!(f, ">"),
            Self::GreaterOrEqual => write!(f, ">="),
            Self::Lesser => write!(f, "<"),
            Self::LesserOrEqual => write!(f, "<="),
            Self::LogicalEquals => write!(f, "=="),
            Self::Different => write!(f, "!="),
            Self::LogicalOr => write!(f, "||"),
            Self::LogicalAnd => write!(f, "&&"),
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
    LastRedirect(SpannedValue<Function>),
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
    Dimensioned(Box<SpannedValue<Expr>>, Vec<SpannedValue<(Arc<str>, i64)>>),
    Literal(SpannedValue<Literal>),
    Variable(SpannedValue<Variable>),
    Assign(SpannedValue<Variable>, Box<SpannedValue<Expr>>),
    BinOp(BinOp),
    Call(SpannedValue<Call>),
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
    TokenError(#[from] SpannedTokenError),
    #[error("Number '{num}' is out of range (expected '{ty}')")]
    OutOfRange {
        ty: &'static str,
        num: i128,
        span: Range<usize>,
    },
}

#[derive(Debug, thiserror::Error)]
#[error("unknown token: '{token}'")]
pub struct SpannedTokenError {
    pub token: String,
    #[source]
    pub error: TokenError,
    pub span: Range<usize>,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn lexer(input: &str) -> impl Iterator<Item = Spanned<Token, usize, UserParseError>> + '_ {
    Token::lexer(input)
        .spanned()
        .map(move |(token, span)| match token {
            Err(error) => Err(SpannedTokenError {
                token: input[span.clone()].to_owned(),
                error,
                span,
            }
            .into()),
            Ok(v) => Ok((span.start, v, span.end)),
        })
}
