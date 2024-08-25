use clap::Parser;
use std::fmt::Display;
use std::io::{self, BufWriter, IsTerminal, Write};
use std::process::ExitCode;

use tc;

mod doc;
mod shell;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
enum ArgError {
    NotATerminal,
}

impl Display for ArgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgError::NotATerminal => write!(f, "--interactive requires terminal"),
        }
    }
}

#[derive(Debug)]
enum RunError {
    Io(io::Error),
    Tc(String, tc::Error),
}

impl From<io::Error> for RunError {
    fn from(err: io::Error) -> Self {
        RunError::Io(err)
    }
}

/// A simple Terminal Calculator
#[derive(Parser, Debug)]
#[command(
    name = "tc", 
    version = VERSION,
    about = "tc - a simple Terminal Calculator",
    after_long_help = doc::AFTER_HELP)]
struct Args {
    /// Force to enter interactive mode
    #[arg(short = 'i', long = "interactive")]
    interactive: bool,

    /// Strip output of evaluations to minimum
    #[arg(short = 's', long = "strip")]
    strip: bool,

    /// Print the list of supported functions
    #[arg(short = 'f', long = "functions")]
    functions: bool,

    /// Print the EBNF grammar reference
    #[arg(short = 'g', long = "grammar")]
    grammar: bool,

    /// Some evaluations
    evals: Vec<String>,
}

impl Args {
    fn run_shell(&self) -> Result<bool, ArgError> {
        // If no evaluation is passed as argument, we run the shell if we are connected to a terminal.
        // If there is an evaluation, we want to run the shell only if --interactive is specified.
        // And if some of stdio is redirected away from the terminal, we can't run the shell.
        //
        // Truth table is:
        //  has_eval    |   interactive     |   is_terminal |   run_shell
        //  0           |   0               |   0           |   0
        //  0           |   0               |   1           |   1
        //  0           |   1               |   0           |   error
        //  0           |   1               |   1           |   1
        //  1           |   0               |   0           |   0
        //  1           |   0               |   1           |   0
        //  1           |   1               |   0           |   error
        //  1           |   1               |   1           |   1

        let is_terminal =
            io::stdin().is_terminal() && io::stdout().is_terminal() && io::stderr().is_terminal();

        if self.interactive && !is_terminal {
            return Err(ArgError::NotATerminal);
        }

        Ok((self.evals.is_empty() && is_terminal) || self.interactive)
    }
}

fn main() -> ExitCode {
    let args = Args::parse();

    if args.functions {
        println!("TC FUNCTIONS");
        println!("============");
        if let Err(err) = print_functions() {
            eprintln!("IO Error: {}", err);
            return ExitCode::FAILURE;
        }
        return ExitCode::SUCCESS;
    }

    if args.grammar {
        println!("TC GRAMMAR");
        println!("==========");
        println!("");
        println!("{}", doc::GRAMMAR);
        return ExitCode::SUCCESS;
    }

    if args.strip && args.interactive {
        eprintln!("--strip cannot be used with --interactive");
        return ExitCode::FAILURE;
    }

    let driver = match Driver::new(args) {
        Ok(driver) => driver,
        Err(err) => {
            eprintln!("Error: {}", err);
            return ExitCode::FAILURE;
        }
    };

    match driver.run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(RunError::Io(err)) => {
            eprintln!("IO Error: {}", err);
            ExitCode::FAILURE
        }
        Err(RunError::Tc(line, err)) => {
            eprintln!("{line}");
            let _ = shell::print_diagnostic(&err, 0);
            ExitCode::FAILURE
        }
    }
}

struct Driver {
    tc: tc::TermCalc,
    interactive: bool,
    strip: bool,
    arg_evals: Vec<String>,
}

impl Driver {
    fn new(args: Args) -> Result<Driver, ArgError> {
        let interactive = args.run_shell()?;
        Ok(Driver {
            tc: tc::TermCalc::new(),
            interactive,
            strip: args.strip,
            arg_evals: args.evals,
        })
    }

    fn run(mut self) -> Result<(), RunError> {
        for expr in self.arg_evals.as_slice() {
            let eval = match self.tc.eval_line(expr.as_str()) {
                Ok(eval) => eval,
                Err(err) => {
                    return Err(RunError::Tc(expr.to_string(), err));
                }
            };
            if self.strip {
                println!("{}", eval.val);
            } else if eval.sym == "ans" {
                println!("{} = {}", expr, eval.val);
            } else {
                println!("{} = {}", eval.sym, eval.val);
            }
            io::stdout().flush()?;
        }

        if self.interactive {
            let mut sh = shell::Shell::new(self.tc);
            sh.main_loop()?;
        }

        Ok(())
    }
}

fn print_functions() -> io::Result<()> {
    let mut out = BufWriter::new(io::stdout().lock());
    doc::write_functions(&mut out)?;
    out.flush()
}
