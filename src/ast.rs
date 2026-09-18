use std::{
    fmt::Debug,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
    rc::Rc,
};

use arcstr::ArcStr;
use logos::Logos;
use serde::{Deserialize, Serialize};

use crate::span::{MaybeNamed, SpannedValue};

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, Default)]
pub enum TokenError {
    #[default]
    #[error("Invalid token encountered")]
    InvalidToken,
    #[error("Integer could not be parsed")]
    ParseInt(#[from] ParseIntError),
    #[error("Float could not be parsed")]
    ParseFloat(#[from] ParseFloatError),
    #[error("Invalid escape sequence {0}")]
    InvalidEscape(char),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DecimalLiteral {
    pub integer: i128,
    pub decimal_count: u64,
    pub decimals: u64,
}

impl std::fmt::Display for DecimalLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}.{:0>width$}",
            self.integer,
            self.decimals,
            width = self.decimal_count as usize
        )
    }
}

fn parse_string(s: &str) -> Result<ArcStr, TokenError> {
    let mut inner = &s[1..s.len() - 1];
    let mut output = String::new();

    while let Some(index) = inner.find('\\') {
        output += &inner[..index];
        match inner[index + 1..].chars().next().unwrap() {
            'n' => output += "\n",
            'r' => output += "\r",
            't' => output += "\t",
            '\\' => output += "\\",
            '"' => output += "\"",
            c => return Err(TokenError::InvalidEscape(c)),
        }
        inner = &inner[index + 2..];
    }

    output += inner;
    Ok(output.into())
}

fn display_string(s: &str) -> String {
    let mut output = String::new();

    output.push('"');
    for c in s.chars() {
        match c {
            '\n' => output += "\\n",
            '\t' => output += "\\t",
            '\r' => output += "\\r",
            '"' => output += "\\\"",
            '\\' => output += "\\\\",
            c => output.push(c),
        }
    }
    output.push('"');

    output
}

#[derive(Debug, derive_more::Display, Logos, Clone)]
#[logos(error = TokenError)]
#[logos(skip r"[ \t\f]+")]
pub enum Token {
    #[token("as")]
    #[display("as")]
    As,
    #[token("(")]
    #[display("(")]
    LParen,
    #[token(")")]
    #[display(")")]
    RParen,
    #[token("|>")]
    #[display("|>")]
    Redirect,
    #[token("->")]
    #[display("->")]
    Arrow,
    #[token("||")]
    #[display("||")]
    LOr,
    #[token("&&")]
    #[display("&&")]
    LAnd,
    #[token("|")]
    #[display("|")]
    BOr,
    #[token("&")]
    #[display("&")]
    BAnd,
    #[token("^")]
    #[display("^")]
    BXor,
    #[token("=")]
    #[display("=")]
    Equal,
    #[token("+")]
    #[display("+")]
    Plus,
    #[token("-")]
    #[display("-")]
    Minus,
    #[token("*")]
    #[display("*")]
    Times,
    #[token("**")]
    #[display("**")]
    Power,
    #[token("/")]
    #[display("/")]
    Divide,
    #[token("//")]
    #[display("//")]
    IntDivide,
    #[token(">>")]
    #[display(">>")]
    RShift,
    #[token("<<")]
    #[display("<<")]
    LShift,
    #[token("%")]
    #[display("%")]
    Percent,
    #[token("\\")]
    #[display("\\")]
    Backslash,
    #[token(".")]
    #[display(".")]
    Dot,
    #[token(":")]
    #[display(":")]
    Colon,
    #[token(",")]
    #[display(",")]
    Comma,
    #[token("'(")]
    #[token(",(")]
    #[display("'(")]
    UnitParen,
    #[token("==")]
    #[display("==")]
    LogicalEquals,
    #[token("!=")]
    #[display("!=")]
    Different,
    #[token(">=")]
    #[display(">=")]
    GreaterOrEqual,
    #[token(">")]
    #[display(">")]
    Greater,
    #[token("<=")]
    #[display("<=")]
    LesserOrEqual,
    #[token("<")]
    #[display("<")]
    Lesser,
    #[regex("[0-9][_0-9]*", |lex| lex.slice().chars().filter(|&c| c != '_').collect::<String>().parse::<i128>().map_err(TokenError::from), priority = 2)]
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
    #[display("<number:{}>", _0)]
    Number(i128),
    #[regex("[0-9]+\\.[0-9]*", callback = |s| {
        let (int, decimals) = s.slice().split_once('.').unwrap();
        Ok::<_, TokenError>(
            DecimalLiteral {
                integer: match int.parse() {
                    Ok(n) => n,
                    Err(e) => return CallbackResult::Error(e.into()),
                },
                decimal_count: decimals.len() as u64,
                decimals: match decimals.parse() {
                    Ok(n) => n,
                    Err(e) => return CallbackResult::Error(e.into()),
                }
            }
        )
    })]
    #[display("<decimal:{}>", _0)]
    Decimal(DecimalLiteral),
    #[regex("[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| ArcStr::from(lex.slice()))]
    #[display("identifier({})", _0)]
    Ident(ArcStr),
    #[regex("\\$[a-zA-Z][a-zA-Z0-9_]*", callback = |lex| ArcStr::from(&lex.slice()[1..]))]
    #[display("identifier({})", _0)]
    Binding(ArcStr),
    #[regex("('|,)[a-zA-Z]+", callback = |lex| ArcStr::from(&lex.slice()[1..]))]
    #[display("unit({})", _0)]
    Unit(ArcStr),
    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\nrt/]|u[a-fA-F0-9]{4}))*""#, |lex| parse_string(lex.slice()))]
    #[display("{}", display_string(_0))]
    String(ArcStr),
    #[token("true")]
    True,
    #[token("false")]
    False,
}

#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize, PartialOrd, Ord)]
pub struct Variable(pub Vec<ArcStr>);

impl Variable {
    pub fn starts_with(&self, prefix: &Self) -> bool {
        let mut words = prefix.0.iter().peekable();
        let mut var = self.0.iter();

        while let Some(word) = words.next() {
            let Some(var_word) = var.next() else {
                return false;
            };

            match words.peek().is_some() {
                true => {
                    if word != var_word {
                        return false;
                    }
                }
                false => return var_word.starts_with(&**word),
            }
        }

        true
    }
}

impl Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "< ")?;
        for word in &self.0 {
            write!(f, "{} ", word)?;
        }
        write!(f, ">")
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for word in &self.0 {
            if !first {
                write!(f, " ")?;
            }
            first = false;
            write!(f, "{word}")?;
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub enum Literal {
    Number(i128),
    Decimal(DecimalLiteral),
    Atom(ArcStr),
    Bool(bool),
    String(ArcStr),
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(arg0) => write!(f, "{}", arg0),
            Self::Decimal(arg0) => write!(f, "{}", arg0),
            Self::Atom(s) => write!(f, ":{}", s),
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(c) => write!(f, "{c:?}"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Copy)]
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
    Power,
}

impl Debug for BinOpKind {
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
            Self::Power => write!(f, "**"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize")]
pub struct BinOp<Src = MaybeNamed> {
    pub lhs: Box<SpannedValue<Expr<Src>, Src>>,
    pub kind: BinOpKind,
    pub rhs: Box<SpannedValue<Expr<Src>, Src>>,
}

impl<Src: Clone> BinOp<Src> {
    pub fn map_source<New, F>(self, mut f: F) -> BinOp<New>
    where
        F: FnMut(Src) -> New,
    {
        BinOp {
            lhs: Box::new(self.lhs.map(|v| v.map_source(&mut f)).map_source(&mut f)),
            kind: self.kind,
            rhs: Box::new(self.rhs.map(|v| v.map_source(&mut f)).map_source(&mut f)),
        }
    }
}

impl<Src> Debug for BinOp<Src>
where
    Src: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}) {:?} ({:?})", **self.lhs, self.kind, **self.rhs)
    }
}

pub enum InputStatement {
    Expr(Expr),
    LastRedirect(SpannedValue<Expr>),
}

impl Debug for InputStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(arg0) => write!(f, "{arg0:?}"),
            Self::LastRedirect(arg0) => write!(f, "|> ({arg0:?})"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize")]
pub struct Call<Src = MaybeNamed> {
    pub fun: SpannedValue<Expr<Src>, Src>,
    pub args: Vec<SpannedValue<Expr<Src>, Src>>,
}

impl<Src: Clone> Call<Src> {
    pub fn map_source<New, F>(self, mut f: F) -> Call<New>
    where
        F: FnMut(Src) -> New,
    {
        Call {
            fun: self.fun.map(|v| v.map_source(&mut f)).map_source(&mut f),
            args: self
                .args
                .into_iter()
                .map(|v| v.map(|v| v.map_source(&mut f)).map_source(&mut f))
                .collect(),
        }
    }
}

impl<Src> Debug for Call<Src>
where
    Src: Debug,
{
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

#[derive(Serialize, Deserialize, Clone, Copy)]
pub enum UnaryOpKind {
    Minus,
    Plus,
}

impl Debug for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize")]
pub struct UnaryOp<Src = MaybeNamed> {
    pub operand: Box<SpannedValue<Expr<Src>, Src>>,
    pub kind: UnaryOpKind,
}

impl<Src: Clone> UnaryOp<Src> {
    pub fn map_source<New, F>(self, mut f: F) -> UnaryOp<New>
    where
        F: FnMut(Src) -> New,
    {
        UnaryOp {
            operand: Box::new(self.operand.map(|v| v.map_source(&mut f)).map_source(f)),
            kind: self.kind,
        }
    }
}

impl<Src> Debug for UnaryOp<Src>
where
    Src: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.kind, self.operand)
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize")]
pub struct DimensionedExpr<Src = MaybeNamed> {
    pub expr: SpannedValue<Expr<Src>, Src>,
    pub unit: Vec<SpannedValue<(ArcStr, i64), Src>>,
}

impl<Src: Clone> DimensionedExpr<Src> {
    pub fn map_source<New, F>(self, mut f: F) -> DimensionedExpr<New>
    where
        F: FnMut(Src) -> New,
    {
        DimensionedExpr {
            expr: self.expr.map(|v| v.map_source(&mut f)).map_source(&mut f),
            unit: self
                .unit
                .into_iter()
                .map(|v| v.map_source(&mut f))
                .collect(),
        }
    }
}

impl<Src> Debug for DimensionedExpr<Src>
where
    Src: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}_{:?}", *self.expr, self.unit)
    }
}

mod rc_slice_serde {
    use std::rc::Rc;

    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<S, T>(v: &Rc<[T]>, s: S) -> Result<S::Ok, S::Error>
    where
        T: Serialize,
        S: Serializer,
    {
        (*v).serialize(s)
    }

    pub fn deserialize<'de, D, T>(de: D) -> Result<Rc<[T]>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de>,
    {
        Ok(Vec::deserialize(de)?.into())
    }
}

mod rc_serde {
    use std::rc::Rc;

    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<S, T>(v: &Rc<T>, s: S) -> Result<S::Ok, S::Error>
    where
        T: Serialize,
        S: Serializer,
    {
        v.serialize(s)
    }

    pub fn deserialize<'de, D, T>(de: D) -> Result<Rc<T>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de>,
    {
        Ok(Rc::new(T::deserialize(de)?))
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize")]
pub struct Lambda<Src = MaybeNamed> {
    #[serde(with = "rc_slice_serde")]
    pub arguments: Rc<[SpannedValue<Variable, Src>]>,
    #[serde(with = "rc_serde")]
    pub body: Rc<SpannedValue<Expr<Src>, Src>>,
}

impl<Src: Clone> Lambda<Src> {
    pub fn map_source<New, F>(self, mut f: F) -> Lambda<New>
    where
        F: FnMut(Src) -> New,
    {
        Lambda {
            arguments: self
                .arguments
                .iter()
                .map(|v| v.clone().map_source(&mut f))
                .collect(),
            body: Rc::new(
                (*self.body)
                    .clone()
                    .map(|v| v.map_source(&mut f))
                    .map_source(f),
            ),
        }
    }
}

impl<Src> Debug for Lambda<Src>
where
    Src: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} -> {:?}", self.arguments, *self.body)
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "Src: for<'d> serde::de::Deserialize<'d> + Serialize")]
pub enum Expr<Src = MaybeNamed> {
    Dimensioned(Box<SpannedValue<DimensionedExpr<Src>, Src>>),
    Literal(SpannedValue<Literal, Src>),
    Variable(Vec<SpannedValue<Variable, Src>>),
    Assign(
        Vec<SpannedValue<Variable, Src>>,
        Box<SpannedValue<Expr<Src>, Src>>,
    ),
    BinOp(SpannedValue<BinOp<Src>, Src>),
    Call(Box<SpannedValue<Call<Src>, Src>>),
    UnaryOp(SpannedValue<UnaryOp<Src>, Src>),
    Lambda(Lambda<Src>),
}

impl<Src: Clone> Expr<Src> {
    pub fn map_source<New, F>(self, mut f: F) -> Expr<New>
    where
        F: FnMut(Src) -> New,
    {
        match self {
            Expr::Dimensioned(v) => {
                Expr::Dimensioned(Box::new(v.map(|v| v.map_source(&mut f)).map_source(f)))
            }
            Expr::Literal(v) => Expr::Literal(v.map_source(f)),
            Expr::Variable(v) => {
                Expr::Variable(v.into_iter().map(|v| v.map_source(&mut f)).collect())
            }
            Expr::Assign(a, b) => Expr::Assign(
                a.into_iter().map(|v| v.map_source(&mut f)).collect(),
                Box::new(b.map(|v| v.map_source(&mut f)).map_source(&mut f)),
            ),
            Expr::BinOp(v) => Expr::BinOp(v.map(|v| v.map_source(&mut f)).map_source(f)),
            Expr::Call(v) => Expr::Call(Box::new(v.map(|v| v.map_source(&mut f)).map_source(f))),
            Expr::UnaryOp(v) => Expr::UnaryOp(v.map(|v| v.map_source(&mut f)).map_source(f)),
            Expr::Lambda(lambda) => Expr::Lambda(lambda.map_source(f)),
        }
    }
}

impl<Src> Debug for Expr<Src>
where
    Src: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dimensioned(d) => d.fmt(f),
            Self::Lambda(l) => l.fmt(f),
            Self::Literal(arg0) => write!(f, "{:?}", **arg0),
            Self::Variable(arg0) => f.debug_tuple("&").field(&arg0).finish(),
            Self::Assign(arg0, arg1) => write!(f, "{:?} = ({:?})", arg0, ***arg1),
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
