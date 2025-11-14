use std::{collections::HashMap, fmt::Display};

use either::Either;
use enum_map::{Enum, EnumMap};
use itertools::Itertools;
use once_cell::sync::Lazy;

use crate::{
    runner::{RunnerError, value::ValueMagnitude},
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Dimension {
    // Expressed in Bytes
    Byte,
    // Expressed in Meters
    Length,
    // Expressed in Seconds
    Time,
    // Expressed in Kilograms
    Mass,
}

impl Display for Dimension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Dimension::Byte => write!(f, "B"),
            Dimension::Length => write!(f, "m"),
            Dimension::Time => write!(f, "s"),
            Dimension::Mass => write!(f, "kg"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Unit {
    pub dimensions: EnumMap<Dimension, i64>,
}

impl Unit {
    pub fn dimensionless() -> Self {
        Self {
            dimensions: EnumMap::from_array([0; 4]),
        }
    }

    pub fn is_dimensionless(&self) -> bool {
        self.dimensions.values().all(|&v| v == 0)
    }

    pub fn pow(mut self, op_span: Span, exponent: i64) -> Result<Self, RunnerError> {
        for (_, dim) in self.dimensions.iter_mut() {
            match dim.checked_mul(exponent) {
                Some(val) => *dim = val,
                None => {
                    return Err(RunnerError::UnitOverflow {
                        location: (op_span.start..op_span.end).into(),
                        src: op_span.source,
                    });
                }
            };
        }

        Ok(self)
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl std::ops::Mul for Unit {
    type Output = Self;

    fn mul(mut self, rhs: Self) -> Self::Output {
        for (dim, e) in rhs.dimensions {
            self.dimensions[dim] += e;
        }

        self
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl std::ops::Div for Unit {
    type Output = Self;

    fn div(mut self, rhs: Self) -> Self::Output {
        for (dim, e) in rhs.dimensions {
            self.dimensions[dim] -= e;
        }

        self
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match KNOWN_UNITS.get(self) {
            Some(v) => write!(f, "{v}"),
            None if self.is_dimensionless() => write!(f, "scalar"),
            None => {
                write!(
                    f,
                    "{}",
                    self.dimensions
                        .iter()
                        .filter(|&(_, &p)| p != 0)
                        .map(|(u, &p)| if p != 1 {
                            format!("{u}{p}")
                        } else {
                            u.to_string()
                        })
                        .join(".")
                )
            }
        }
    }
}

pub enum ScaleType {
    Metric,
    TimeMetric,
    ShiftedMetric,
    Binary,
}

impl ScaleType {
    pub fn steps(&self) -> &'static [ScaleStep] {
        match self {
            ScaleType::Metric => &*METRIC_SCALE,
            ScaleType::TimeMetric => &*TIME_METRIC_SCALE,
            ScaleType::Binary => &*BINARY_SCALE,
            ScaleType::ShiftedMetric => &*SHIFTED_METRIC_SCALE,
        }
    }

    pub fn prefix(&self) -> impl Iterator<Item = (&'static str, &'static ValueMagnitude)> {
        let steps = self.steps();
        steps
            .iter()
            .filter_map(|s| match s.render {
                ScaleRender::Prefix(p) => Some(Either::Left(std::iter::once((p, &s.order)))),
                ScaleRender::EitherPrefix { main, alternative } => Some(Either::Right(
                    [(main, &s.order), (alternative, &s.order)].into_iter(),
                )),
                _ => None,
            })
            .flatten()
    }

    pub fn all_prefix() -> impl Iterator<Item = (&'static str, &'static ValueMagnitude)> {
        Self::Metric
            .prefix()
            .chain(Self::TimeMetric.prefix())
            .chain(Self::Binary.prefix())
            .unique_by(|(p, _)| *p)
            .sorted_by(|(pa, _), (pb, _)| pb.len().cmp(&pa.len()))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ScaleRender {
    Override(&'static str),
    Prefix(&'static str),
    EitherPrefix {
        main: &'static str,
        alternative: &'static str,
    },
    AsIs,
}

#[derive(Clone, Debug)]
pub struct ScaleStep {
    pub order: ValueMagnitude,
    pub render: ScaleRender,
}

fn div<A, B>(a: A, b: B) -> ValueMagnitude
where
    A: Into<ValueMagnitude>,
    B: Into<ValueMagnitude>,
{
    ValueMagnitude::div_ok(a.into(), b.into())
}

fn mul<A, B>(a: A, b: B) -> ValueMagnitude
where
    A: Into<ValueMagnitude>,
    B: Into<ValueMagnitude>,
{
    ValueMagnitude::mul_ok(a.into(), b.into())
}

fn inv(a: impl Into<ValueMagnitude>) -> ValueMagnitude {
    div(1, a)
}

static TIME_METRIC_SCALE: Lazy<Vec<ScaleStep>> = Lazy::new(|| {
    vec![
        ScaleStep {
            render: ScaleRender::Override("yr"),
            order: mul(mul(mul(60, 60), 24), div(36425, 100)),
        },
        ScaleStep {
            render: ScaleRender::Override("day"),
            order: mul(mul(60, 60), 24),
        },
        ScaleStep {
            render: ScaleRender::Override("hr"),
            order: mul(60, 60),
        },
        ScaleStep {
            render: ScaleRender::Override("min"),
            order: 60.into(),
        },
        ScaleStep {
            render: ScaleRender::AsIs,
            order: 1.into(),
        },
        ScaleStep {
            render: ScaleRender::Prefix("m"),
            order: inv(1_000),
        },
        ScaleStep {
            render: ScaleRender::Prefix("μ"),
            order: inv(1_000_000),
        },
        ScaleStep {
            render: ScaleRender::Prefix("u"),
            order: inv(1_000_000),
        },
        ScaleStep {
            render: ScaleRender::Prefix("n"),
            order: inv(1_000_000_000),
        },
        ScaleStep {
            render: ScaleRender::Prefix("p"),
            order: inv(1_000_000_000_000),
        },
    ]
});

static BINARY_SCALE: Lazy<Vec<ScaleStep>> = Lazy::new(|| {
    vec![
        ScaleStep {
            render: ScaleRender::Prefix("Ti"),
            order: mul(mul(mul(1024, 1024), 1024), 1024),
        },
        ScaleStep {
            render: ScaleRender::Prefix("Gi"),
            order: mul(mul(1024, 1024), 1024),
        },
        ScaleStep {
            render: ScaleRender::Prefix("Mi"),
            order: mul(1024, 1024),
        },
        ScaleStep {
            render: ScaleRender::Prefix("Ki"),
            order: 1024.into(),
        },
        ScaleStep {
            render: ScaleRender::AsIs,
            order: 1.into(),
        },
    ]
});

static METRIC_SCALE: Lazy<Vec<ScaleStep>> = Lazy::new(|| {
    vec![
        ScaleStep {
            render: ScaleRender::Prefix("E"),
            order: 1000000000000000000.into(),
        },
        ScaleStep {
            render: ScaleRender::Prefix("P"),
            order: 1000000000000000.into(),
        },
        ScaleStep {
            render: ScaleRender::Prefix("T"),
            order: 1000000000000.into(),
        },
        ScaleStep {
            render: ScaleRender::Prefix("G"),
            order: 1000000000.into(),
        },
        ScaleStep {
            render: ScaleRender::Prefix("M"),
            order: 1000000.into(),
        },
        ScaleStep {
            render: ScaleRender::Prefix("k"),
            order: 1000.into(),
        },
        ScaleStep {
            render: ScaleRender::AsIs,
            order: 1.into(),
        },
        ScaleStep {
            render: ScaleRender::Prefix("c"),
            order: inv(100),
        },
        ScaleStep {
            render: ScaleRender::Prefix("m"),
            order: inv(1_000),
        },
        ScaleStep {
            render: ScaleRender::EitherPrefix {
                main: "μ",
                alternative: "u",
            },
            order: inv(1_000_000),
        },
        ScaleStep {
            render: ScaleRender::Prefix("n"),
            order: inv(1_000_000_000),
        },
        ScaleStep {
            render: ScaleRender::Prefix("p"),
            order: inv(1_000_000_000_000),
        },
    ]
});

static SHIFTED_METRIC_SCALE: Lazy<Vec<ScaleStep>> = Lazy::new(|| {
    METRIC_SCALE
        .iter()
        .cloned()
        .map(|mut v| {
            v.order = div(v.order, 1000);
            v
        })
        .collect()
});

pub static BYTE_UNIT: Lazy<Unit> = Lazy::new(|| {
    let mut unit = Unit::dimensionless();
    unit.dimensions[Dimension::Byte] = 1;
    unit
});

pub static MASS_UNIT: Lazy<Unit> = Lazy::new(|| {
    let mut unit = Unit::dimensionless();
    unit.dimensions[Dimension::Mass] += 1;
    unit
});

pub static TIME_UNIT: Lazy<Unit> = Lazy::new(|| {
    let mut unit = Unit::dimensionless();
    unit.dimensions[Dimension::Time] += 1;
    unit
});

pub static KNOWN_UNITS: Lazy<HashMap<Unit, &'static str>> = Lazy::new(|| {
    use Dimension::*;

    let mut units = HashMap::new();

    units.insert(*BYTE_UNIT, "B");

    let mut len = Unit::dimensionless();
    len.dimensions[Length] += 1;
    units.insert(len, "m");

    let mut time = Unit::dimensionless();
    time.dimensions[Time] += 1;
    units.insert(time, "s");

    units.insert(*MASS_UNIT, "g");

    let mut freq = Unit::dimensionless();
    freq.dimensions[Time] -= 1;
    units.insert(freq, "Hz");

    let force = *MASS_UNIT * len / time / time;
    units.insert(force, "N");

    let pressure = force / len / len;
    units.insert(pressure, "Pa");

    let energy = force * len;
    units.insert(energy, "J");

    let power = energy / time;
    units.insert(power, "W");

    units
});
