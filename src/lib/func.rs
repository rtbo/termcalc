use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Category {
    General,
    PowerLog,
    Trigonometry,
}

impl Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Category::General => write!(f, "General"),
            Category::PowerLog => write!(f, "Power and Logarithms"),
            Category::Trigonometry => write!(f, "Trigonometry"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ArgCount {
    One,
    Two,
    Atleast(u32),
}

impl ArgCount {
    pub fn check(&self, count: usize) -> bool {
        match self {
            ArgCount::One => count == 1,
            ArgCount::Two => count == 2,
            ArgCount::Atleast(n) => count >= *n as usize,
        }
    }
}

impl Display for ArgCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // printed in error message
            ArgCount::One => write!(f, "one argument"),
            ArgCount::Two => write!(f, "two arguments"),
            ArgCount::Atleast(n) => {
                write!(f, "at least {n} argument{}", if *n > 1 { "s" } else { "" })
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Args {
    One(f64),
    Two(f64, f64),
    Dyn(Vec<f64>),
}

impl Args {
    pub fn first(&self) -> f64 {
        match self {
            Args::One(v) => *v,
            Args::Two(v, _) => *v,
            Args::Dyn(v) => v[0],
        }
    }

    pub fn second(&self) -> f64 {
        match self {
            Args::One(..) => unreachable!("Not enough arguments"),
            Args::Two(_, v) => *v,
            Args::Dyn(v) => v[1],
        }
    }

    pub fn all(&self) -> &[f64] {
        match self {
            Args::One(..) => unreachable!("No dynamic arguments"),
            Args::Two(..) => unreachable!("No dynamic arguments"),
            Args::Dyn(v) => v.as_slice(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Func {
    pub category: Category,
    pub name: String,
    pub help: String,
    pub arg_count: ArgCount,
    pub eval: fn(Args) -> f64,
}

pub fn all_funcs() -> Vec<Func> {
    vec![
        Func {
            category: Category::General,
            name: "floor".to_string(),
            help: "floor value to lower integer".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().floor(),
        },
        Func {
            category: Category::General,
            name: "round".to_string(),
            help: "round value to nearest integer".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().round(),
        },
        Func {
            category: Category::General,
            name: "ceil".to_string(),
            help: "ceil value to upper integer".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().ceil(),
        },
        Func {
            category: Category::General,
            name: "trunc".to_string(),
            help: "truncate value to its integer part (round towards zero)".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().trunc(),
        },
        Func {
            category: Category::General,
            name: "fract".to_string(),
            help: "truncate value to its fractional part".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().fract(),
        },
        Func {
            category: Category::General,
            name: "abs".to_string(),
            help: "compute absolute value".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().abs(),
        },
        Func {
            category: Category::General,
            name: "sign".to_string(),
            help: "value sign (1 or -1)".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().signum(),
        },
        Func {
            category: Category::General,
            name: "min".to_string(),
            help: "minimum of all arguments".to_string(),
            arg_count: ArgCount::Atleast(1),
            eval: |args| {
                let args = args.all();
                let mut min = args[0];
                for arg in args.iter().skip(1) {
                    min = min.min(*arg);
                }
                min
            },
        },
        Func {
            category: Category::General,
            name: "max".to_string(),
            help: "maximum of all arguments".to_string(),
            arg_count: ArgCount::Atleast(1),
            eval: |args| {
                let args = args.all();
                let mut max = args[0];
                for arg in args.iter().skip(1) {
                    max = max.max(*arg);
                }
                max
            },
        },
        Func {
            category: Category::PowerLog,
            name: "pow".to_string(),
            arg_count: ArgCount::Two,
            help: "first argument raised to the power the second argument".to_string(),
            eval: |args| args.first().powf(args.second()),
        },
        Func {
            category: Category::PowerLog,
            name: "sqrt".to_string(),
            help: "square root".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().sqrt(),
        },
        Func {
            category: Category::PowerLog,
            name: "cbrt".to_string(),
            help: "cubic root".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().cbrt(),
        },
        Func {
            category: Category::PowerLog,
            name: "exp".to_string(),
            help: "exponential function (exp(x) = pow(e, x))".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().exp(),
        },
        Func {
            category: Category::PowerLog,
            name: "log".to_string(),
            help: "log(b, x) is base b logarithm of x".to_string(),
            arg_count: ArgCount::Two,
            eval: |args| args.first().log(args.second()),
        },
        Func {
            category: Category::PowerLog,
            name: "ln".to_string(),
            help: "natural logarithm".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().ln(),
        },
        Func {
            category: Category::PowerLog,
            name: "log2".to_string(),
            help: "base 2 logarithm".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().log2(),
        },
        Func {
            category: Category::PowerLog,
            name: "log10".to_string(),
            help: "base 10 logarithm".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().log10(),
        },
        Func {
            category: Category::Trigonometry,
            name: "sin".to_string(),
            help: "sine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().sin(),
        },
        Func {
            category: Category::Trigonometry,
            name: "cos".to_string(),
            help: "cosine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().cos(),
        },
        Func {
            category: Category::Trigonometry,
            name: "tan".to_string(),
            help: "tangent function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().tan(),
        },
        Func {
            category: Category::Trigonometry,
            name: "csc".to_string(),
            help: "cosecante function (inverse of sine)".to_string(),
            arg_count: ArgCount::One,
            eval: |args| 1.0 / args.first().sin(),
        },
        Func {
            category: Category::Trigonometry,
            name: "sec".to_string(),
            help: "secante function (inverse of cosine)".to_string(),
            arg_count: ArgCount::One,
            eval: |args| 1.0 / args.first().cos(),
        },
        Func {
            category: Category::Trigonometry,
            name: "cot".to_string(),
            help: "cotangent function (inverse of tangent)".to_string(),
            arg_count: ArgCount::One,
            eval: |args| 1.0 / args.first().tan(),
        },
        Func {
            category: Category::Trigonometry,
            name: "asin".to_string(),
            help: "arc sine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().asin(),
        },
        Func {
            category: Category::Trigonometry,
            name: "acos".to_string(),
            help: "arc cosine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().acos(),
        },
        Func {
            category: Category::Trigonometry,
            name: "atan".to_string(),
            help: "arc tangent function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().atan(),
        },
        Func {
            category: Category::Trigonometry,
            name: "sinh".to_string(),
            help: "hyperbolic sine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().sinh(),
        },
        Func {
            category: Category::Trigonometry,
            name: "cosh".to_string(),
            help: "hyperbolic cosine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().cosh(),
        },
        Func {
            category: Category::Trigonometry,
            name: "tanh".to_string(),
            help: "hyperbolic tangent function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().tanh(),
        },
        Func {
            category: Category::Trigonometry,
            name: "asinh".to_string(),
            help: "hyperbolic arc sine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().asinh(),
        },
        Func {
            category: Category::Trigonometry,
            name: "acosh".to_string(),
            help: "hyperbolic arc cosine function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().acosh(),
        },
        Func {
            category: Category::Trigonometry,
            name: "atanh".to_string(),
            help: "hyperbolic arc tangent function".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().atanh(),
        },
        Func {
            category: Category::Trigonometry,
            name: "degs".to_string(),
            help: "convert radians to degrees".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().to_degrees(),
        },
        Func {
            category: Category::Trigonometry,
            name: "rads".to_string(),
            help: "convert degrees to radians".to_string(),
            arg_count: ArgCount::One,
            eval: |args| args.first().to_radians(),
        },
    ]
}
