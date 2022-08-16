use std::{collections::HashMap, ops::Range, rc::Rc};

use logos::Logos;

use crate::span::SpannedValue;

fn escape_string(input: &str) -> String {
    if !input.contains('\\') {
        input.into()
    } else {
        let mut val = Vec::new();
        let mut escape = false;
        for x in input.bytes() {
            if x == b'\\' {
                escape = true;
            } else if escape {
                match x {
                    b't' => val.push(b'\t'),
                    b'n' => val.push(b'\n'),
                    _ => {
                        val.push(b'\\');
                        val.push(x);
                    }
                }
                escape = false;
            } else {
                val.push(x);
            }
        }
        String::from_utf8(val).expect("should not produce garbage")
    }
}

#[derive(Logos, Debug, PartialEq, Clone, derive_more::Display)]
pub enum Token {
    #[regex("(-)?[0-9][0-9_]*", |lex| lex.slice().parse())]
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
    #[token("|")]
    #[display(fmt = "|")]
    Pipe,
    #[token("&")]
    #[display(fmt = "&")]
    And,
    #[token("|>")]
    #[display(fmt = "|>")]
    Redirect,
    #[token("\n")]
    #[display(fmt = r#"\n"#)]
    Newline,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", callback = |lex| lex.slice().to_string())]
    #[display(fmt = "identifier({})", _0)]
    Identifier(String),
    #[token("(")]
    #[display(fmt = "(")]
    LParen,
    #[token(")")]
    #[display(fmt = ")")]
    RParen,
    #[token("~")]
    #[display(fmt = "~")]
    Tilde,
    #[regex(r#""(\\[^\n]|[^"\n])*""#, |lex| {
        let s = lex.slice();
        escape_string(&s[1..s.len() - 1])
    })]
    #[display(fmt = "'{}'", _0)]
    String(String),
    #[token("{")]
    #[display(fmt = "{{")]
    LBrace,
    #[token("}")]
    #[display(fmt = "}}")]
    RBrace,
    #[token("[")]
    #[display(fmt = "[")]
    LBracket,
    #[token("]")]
    #[display(fmt = "]")]
    RBracket,
    #[token(".")]
    #[display(fmt = ".")]
    Dot,
    #[token("%")]
    #[display(fmt = "%")]
    Percent,
    #[token(",")]
    #[display(fmt = ",")]
    Comma,
    #[token("@")]
    #[display(fmt = "@")]
    At,
    #[token("=")]
    #[display(fmt = "=")]
    Equal,
    #[token("->")]
    #[display(fmt = "->")]
    Arrow,
    #[token("\\")]
    #[display(fmt = "\\")]
    Backslash,
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    #[display(fmt = "<error>")]
    Error,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(i64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Folder {
    Operator(BinaryOp),
    Args {
        func: Box<SpannedValue<Expr>>,
        def: Box<SpannedValue<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(SpannedValue<Literal>),
    Place(SpannedValue<Place>),
    Binary(BinaryOp, Box<SpannedValue<Expr>>, Box<SpannedValue<Expr>>),
    Map(Vec<(SpannedValue<Expr>, SpannedValue<Expr>)>),
    Array(Vec<SpannedValue<Expr>>),
    Tilde(Box<SpannedValue<Expr>>),
    Fold(SpannedValue<Folder>),
    Mapper(Box<SpannedValue<Expr>>),
    FuncDef {
        args: Vec<String>,
        ret: Box<SpannedValue<Expr>>,
    },
    NamedCall {
        func: Box<SpannedValue<Expr>>,
        named: HashMap<SpannedValue<Rc<str>>, SpannedValue<Expr>>,
    },
    Call {
        func: Box<SpannedValue<Expr>>,
        args: Vec<SpannedValue<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    BitwiseOr,
    BitwiseAnd,
    Redirect,
}

#[derive(Debug, Clone)]
pub enum Place {
    Ident(SpannedValue<String>),
    Deref(Box<SpannedValue<Expr>>, SpannedValue<Rc<String>>),
}

#[derive(Debug)]
pub enum Statement {
    Expr(SpannedValue<Expr>),
    Assign(SpannedValue<Place>, SpannedValue<Expr>),
}

#[derive(Debug, thiserror::Error)]
#[error("unknown token: {token}")]
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
