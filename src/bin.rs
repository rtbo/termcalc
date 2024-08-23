use clap::Parser;
use std::fmt::Display;
use std::io::{self, BufRead, IsTerminal, Write};
use std::mem;
use std::process::ExitCode;
use tc::TermCalc;
use tc::{self, input::HasSpan};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AFTER_HELP: &str =
    "If [EVALS] if provided, passed arguments will be evaluated,
and program will exit unless --interactive is specified.

If [EVALS] is not provided, program will enter interactive mode
regardless of the --interactive switch.
    ";

/// A simple Terminal Calculator
#[derive(Parser, Debug)]
#[command(name = "tc", version = VERSION, about = "tc - a simple Terminal Calculator", after_long_help = AFTER_HELP)]
struct Args {
    /// Force to enter interactive mode
    #[arg(short = 'i', long = "interactive")]
    interactive: bool,

    /// Strip output of evaluations to minimum
    #[arg(short = 's', long = "strip")]
    strip: bool,

    /// Some evaluations
    evals: Vec<String>,
}

impl Args {
    fn need_prompt(&self) -> Result<bool, ArgError> {
        // If no evaluation is passed as argument, we enter interactive mode if we are connected to a terminal.
        // If there is an evaluation, we want to enter interactive mode only if --interactive is specified.
        // And if stdin is not connected to a terminal, we can't go interactive.
        //
        // Truth table is:
        //  has_eval    |   interactive     |   is_terminal |   result
        //  0           |   0               |   0           |   0
        //  0           |   0               |   1           |   need_prompt
        //  0           |   1               |   0           |   error
        //  0           |   1               |   1           |   need_prompt
        //  1           |   0               |   0           |   0
        //  1           |   0               |   1           |   0
        //  1           |   1               |   0           |   error
        //  1           |   1               |   1           |   need_prompt

        let is_terminal = io::stdin().is_terminal();

        if self.interactive && !is_terminal {
            return Err(ArgError::NeedTerminal);
        }

        Ok((self.evals.is_empty() && is_terminal) || self.interactive)
    }
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut driver = match Driver::new(args) {
        Ok(driver) => driver,
        Err(err) => {
            eprintln!("Error: {}", err);
            return ExitCode::FAILURE;
        }
    };

    driver.run()
}

struct Driver {
    tc: TermCalc,
    prompt: u32,
    interactive: bool,
    strip: bool,
    arg_evals: Vec<String>,
}

impl Driver {
    fn new(args: Args) -> Result<Driver, ArgError> {
        let interactive = args.need_prompt()?;
        Ok(Driver {
            tc: tc::TermCalc::new(),
            prompt: 1,
            interactive,
            strip: args.strip,
            arg_evals: args.evals,
        })
    }

    fn run(&mut self) -> ExitCode {
        let arg_evals = mem::take(&mut self.arg_evals);
        let num_arg_evals = arg_evals.len();
        if self.interactive {
            let lines = arg_evals
                .into_iter()
                .map(Result::Ok)
                .chain(io::stdin().lock().lines());
            self.line_loop(lines, num_arg_evals)
        } else {
            let lines = arg_evals.into_iter().map(Result::Ok);

            self.line_loop(lines, num_arg_evals)
        }
    }

    fn print_prompt(&self) {
        print!("{}> ", self.prompt);
        io::stdout().flush().unwrap();
    }

    fn line_loop<L>(&mut self, lines: L, num_arg_evals: usize) -> ExitCode
    where
        L: Iterator<Item = Result<String, io::Error>>,
    {
        if self.interactive {
            self.print_prompt();
        }

        let mut num_arg_evals = num_arg_evals;

        for line in lines {
            let line = match line {
                Ok(line) => line,
                Err(err) => {
                    eprintln!("IO Error: {}", err);
                    return ExitCode::FAILURE;
                }
            };

            if self.interactive {
                let ll = line.to_lowercase();
                if matches!(ll.as_str(), "exit" | "quit" | "q") {
                    break;
                }
            }

            if num_arg_evals > 0 && !self.strip {
                print!("{} = ", line);
            }

            match self.tc.eval_line(line.as_str()) {
                Ok(eval) => {
                    if self.strip || num_arg_evals > 0 {
                        println!("{}", eval.val);
                    } else {
                        println!("{} = {}", eval.sym, eval.val);
                    }
                    self.prompt += 1;
                }
                Err(err) => {
                    print_diagnostic(line.as_str(), &err);
                    if !self.interactive {
                        return ExitCode::FAILURE;
                    }
                }
            }

            if num_arg_evals > 0 {
                num_arg_evals -= 1;
            }

            if self.interactive {
                self.print_prompt();
            }
        }
        ExitCode::SUCCESS
    }
}

fn print_diagnostic(line: &str, err: &tc::Error) {
    let span = err.span();
    let msg = err.to_string();
    eprintln!("error: {}", msg);
    eprintln!("{line}");
    eprintln!(
        "{}{}",
        " ".repeat(span.0 as _),
        "^".repeat((span.1 - span.0) as _)
    );
}

#[derive(Debug)]
enum ArgError {
    NeedTerminal,
}

impl Display for ArgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgError::NeedTerminal => write!(f, "--interactive requires terminal"),
        }
    }
}

#[derive(Debug)]
enum Error {
    Tc(tc::Error),
    Io(io::Error),
    Arg(ArgError),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<tc::Error> for Error {
    fn from(e: tc::Error) -> Self {
        Error::Tc(e)
    }
}

impl From<ArgError> for Error {
    fn from(e: ArgError) -> Self {
        Error::Arg(e)
    }
}
