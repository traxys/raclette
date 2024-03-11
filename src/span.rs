use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use derivative::Derivative;
use miette::{NamedSource, SourceCode, SourceSpan};
//use once_cell::sync::Lazy;

#[derive(Clone, Debug)]
pub enum MaybeNamed {
    Named(Arc<NamedSource<String>>),
    Unamed(Arc<str>),
}

impl From<NamedSource<String>> for MaybeNamed {
    fn from(n: NamedSource<String>) -> Self {
        MaybeNamed::Named(Arc::new(n))
    }
}

impl From<&str> for MaybeNamed {
    fn from(s: &str) -> Self {
        MaybeNamed::Unamed(Arc::from(s))
    }
}

impl SourceCode for MaybeNamed {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        match self {
            MaybeNamed::Named(s) => s.read_span(span, context_lines_before, context_lines_after),
            MaybeNamed::Unamed(s) => {
                s.read_span(span, context_lines_before, context_lines_after)
            }
        }
    }
}

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialEq, Hash, Eq)]
pub struct SpannedValue<T> {
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub start: usize,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub end: usize,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub source: MaybeNamed,
    pub value: T,
}

pub type Span = SpannedValue<()>;

impl<T> From<SpannedValue<T>> for SourceSpan {
    fn from(t: SpannedValue<T>) -> Self {
        (&t).into()
    }
}

impl<T> From<&SpannedValue<T>> for SourceSpan {
    fn from(t: &SpannedValue<T>) -> Self {
        (t.start..t.end).into()
    }
}

/* pub static UNKNOWN_SPAN: Lazy<Span> = Lazy::new(|| Span {
    start: 1,
    end: 0,
    source: NamedSource::new("<unknown>", "").into(),
    value: (),
}); */

pub trait SpanningExt {
    fn spanned<U>(self, span: &SpannedValue<U>) -> SpannedValue<Self>
    where
        Self: Sized,
    {
        SpannedValue::with_span(self, span)
    }
}

impl<T> SpanningExt for T {}

impl<T> SpannedValue<T> {
    pub fn span(&self) -> Span {
        Span {
            source: self.source.clone(),
            start: self.start,
            end: self.end,
            value: (),
        }
    }

    fn with_span<U>(value: T, other: &SpannedValue<U>) -> Self {
        let other = other.span();
        Self::with_span_unit(value, &other)
    }

    fn with_span_unit(value: T, other: &Span) -> Self {
        Self {
            source: other.source.clone(),
            start: other.start,
            end: other.end,
            value,
        }
    }

    pub fn swap_span<U>(&mut self, span: &SpannedValue<U>)
    where
        Self: Sized,
    {
        self.start = span.span().start;
        self.end = span.span().end;
    }

    pub fn map<F, U>(self, f: F) -> SpannedValue<U>
    where
        F: FnOnce(T) -> U,
    {
        let SpannedValue {
            start,
            end,
            value,
            source,
        } = self;
        SpannedValue {
            source,
            value: f(value),
            start,
            end,
        }
    }

    pub fn wrap<F, U>(self, f: F) -> SpannedValue<U>
    where
        F: FnOnce(Self) -> U,
    {
        SpannedValue {
            source: self.source.clone(),
            start: self.start,
            end: self.end,
            value: f(self),
        }
    }
}

impl<T> SpannedValue<T> {}

impl<T> Deref for SpannedValue<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for SpannedValue<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}
