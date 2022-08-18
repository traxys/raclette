use std::ops::{Deref, DerefMut};

use arbitrary::Arbitrary;
use derivative::Derivative;
use gc::{Finalize, Trace};
use miette::SourceSpan;

#[derive(Debug, Clone, Trace, Finalize, Derivative, Eq)]
#[derivative(PartialEq, Hash)]
pub struct GcSpannedValue<T> {
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub start: usize,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub end: usize,
    pub value: T,
}

#[derive(Debug, Clone, Copy, Derivative, Eq, Arbitrary)]
#[derivative(PartialEq, Hash)]
pub struct SpannedValue<T> {
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub start: usize,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub end: usize,
    pub value: T,
}

#[derive(Debug, Clone, Trace, Finalize, Derivative, Eq)]
#[derivative(PartialEq, Hash)]
pub struct GenerationSpanned<T> {
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub generation: Option<u64>,
    pub span: GcSpannedValue<T>,
}

pub type Span = SpannedValue<()>;
pub type GenerationSpan = GenerationSpanned<()>;

impl<T> GenerationSpanned<T> {
    pub fn gen_span(&self) -> GenerationSpan {
        GenerationSpan {
            generation: self.generation,
            span: ().spanned_gc(self),
        }
    }

    pub fn source_span(&self, current_gen: u64) -> Option<SourceSpan> {
        if Some(current_gen) == self.generation {
            Some(SourceSpan::from(self.span.start..self.span.end))
        } else {
            None
        }
    }
}

pub const UNKNOWN_SPAN: &Span = &Span {
    start: 1,
    end: 0,
    value: (),
};

impl<T> From<SpannedValue<T>> for GcSpannedValue<T> {
    fn from(s: SpannedValue<T>) -> Self {
        let SpannedValue { start, end, value } = s;
        Self { start, end, value }
    }
}

impl<T> From<GcSpannedValue<T>> for SpannedValue<T>
where
    T: Default,
{
    fn from(mut v: GcSpannedValue<T>) -> Self {
        let value = std::mem::take(&mut v.value);
        Self {
            start: v.start,
            end: v.end,
            value,
        }
    }
}

pub trait Spanning<T> {
    fn span(&self) -> Span;
    fn with_span_unit(value: T, s: &Span) -> Self;

    fn with_span<U, S>(value: T, other: &S) -> Self
    where
        S: Spanning<U>,
        Self: Sized,
    {
        let other = other.span();
        Self::with_span_unit(value, &other)
    }

    fn with_generation(&self, generation: u64) -> GenerationSpan {
        GenerationSpan {
            generation: Some(generation),
            span: self.span().into(),
        }
    }

    /* fn update_span<U, S>(mut self, span: &S) -> Self
    where
        S: Spanning<U>,
        Self: Sized,
    {
        *self = Self::with_span(self, span);
    } */
}

pub trait SpanningExt {
    fn spanned<U, S>(self, span: &S) -> SpannedValue<Self>
    where
        Self: Sized,
        S: Spanning<U>,
    {
        SpannedValue::with_span(self, span)
    }

    fn spanned_gc<U, S>(self, span: &S) -> GcSpannedValue<Self>
    where
        Self: Sized,
        S: Spanning<U>,
    {
        GcSpannedValue::with_span(self, span)
    }

    fn spanned_gen<U, S>(self, span: &S, generation: u64) -> GenerationSpanned<Self>
    where
        Self: Sized,
        S: Spanning<U>,
    {
        let mut s = GenerationSpanned::with_span(self, span);
        s.generation = Some(generation);
        s
    }
}

impl<T> SpanningExt for T {}

impl<T> Spanning<T> for GcSpannedValue<T> {
    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.end,
            value: (),
        }
    }

    fn with_span_unit(value: T, other: &Span) -> Self {
        Self {
            start: other.start,
            end: other.end,
            value,
        }
    }
}

impl<T: Default> GcSpannedValue<T> {
    pub fn map<F, U>(mut self, f: F) -> GcSpannedValue<U>
    where
        F: FnOnce(T) -> U,
    {
        let value = std::mem::take(&mut self.value);

        GcSpannedValue {
            value: f(value),
            start: self.start,
            end: self.end,
        }
    }
}

impl<T> GcSpannedValue<T> {
    pub fn wrap<F, U>(self, f: F) -> GcSpannedValue<U>
    where
        F: FnOnce(Self) -> U,
    {
        GcSpannedValue {
            start: self.start,
            end: self.end,
            value: f(self),
        }
    }
}

impl<T> GcSpannedValue<T>
where
    T: Deref,
{
    pub fn as_deref(&self) -> GcSpannedValue<&T::Target> {
        GcSpannedValue {
            start: self.start,
            end: self.end,
            value: self.value.deref(),
        }
    }
}

impl<T> Spanning<T> for SpannedValue<T> {
    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.end,
            value: (),
        }
    }

    fn with_span_unit(value: T, other: &Span) -> Self {
        Self {
            start: other.start,
            end: other.end,
            value,
        }
    }
}

impl<T> Spanning<T> for GenerationSpanned<T> {
    fn span(&self) -> Span {
        self.span.span()
    }

    fn with_span_unit(value: T, s: &Span) -> Self {
        Self {
            generation: None,
            span: GcSpannedValue::with_span_unit(value, s),
        }
    }
}

impl<T> SpannedValue<T> {
    pub fn map<F, U>(self, f: F) -> SpannedValue<U>
    where
        F: FnOnce(T) -> U,
    {
        let SpannedValue { start, end, value } = self;
        SpannedValue {
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
            start: self.start,
            end: self.end,
            value: f(self),
        }
    }
}

impl<T> Deref for SpannedValue<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> Deref for GcSpannedValue<T> {
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

impl<T> DerefMut for GcSpannedValue<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Deref for GenerationSpanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.span.deref()
    }
}

impl<T> DerefMut for GenerationSpanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.span.deref_mut()
    }
}
