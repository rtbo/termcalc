use clap::Parser;
use std::fmt::Display;
use std::io::{self, BufRead, IsTerminal, Write};
use std::process::ExitCode;
use tc::TermCalc;
use tc::{self, input::HasSpan};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AFTER_HELP: &str = "If [EVALS] if provided, passed arguments will be evaluated,
and program will exit unless --interactive is specified.

If [EVALS] is not provided, program will enter interactive mode
regardless of the --interactive switch.
    ";

const MANUAL: &str = include_str!("../doc/gen/tc.1.ansi");
const GRAMMAR: &str = include_str!("../doc/Grammar.ebnf");

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

    if args.functions {
        println!("TC FUNCTIONS");
        println!("============");
        print_functions();
        return ExitCode::SUCCESS;
    }

    if args.grammar {
        println!("TC GRAMMAR");
        println!("==========");
        println!("");
        println!("{}", GRAMMAR);
        return ExitCode::SUCCESS;
    }

    if args.strip && args.interactive {
        eprintln!("--strip cannot be used with --interactive");
        return ExitCode::FAILURE;
    }

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
        let mut errs = 0;
        for expr in self.arg_evals.as_slice() {
            let res = match self.tc.eval_line(expr.as_str()) {
                Ok(eval) if self.strip => {
                    println!("{}", eval.val);
                    io::stdout().flush()
                }
                Ok(eval) if eval.sym == "ans" => {
                    println!("{} = {}", expr, eval.val);
                    io::stdout().flush()
                }
                Ok(eval) => {
                    println!("{} = {}", eval.sym, eval.val);
                    io::stdout().flush()
                }
                Err(err) => {
                    errs += 1;
                    self.print_diagnostic(expr, &err)
                }
            };

            if let Err(err) = res {
                eprintln!("IO Error: {}", err);
                return ExitCode::FAILURE;
            }
        }
        if errs > 0 {
            return ExitCode::from(errs);
        }

        if self.interactive {
            self.interactive_loop()
        } else {
            ExitCode::SUCCESS
        }
    }

    fn print_prompt(&self) {
        use colored::{control, Colorize};

        if !io::stderr().is_terminal() {
            control::set_override(false);
        }

        print!("{} ", format!("{}>", self.prompt).dimmed());
        io::stdout().flush().unwrap();
    }

    fn interactive_loop(&mut self) -> ExitCode {
        self.print_prompt();

        for line in io::stdin().lock().lines() {
            let line = match line {
                Ok(line) => line,
                Err(err) => {
                    eprintln!("IO Error: {}", err);
                    return ExitCode::FAILURE;
                }
            };

            let ll = line.to_lowercase();
            match ll.as_str().trim() {
                "" => {
                    self.print_prompt();
                    continue;
                }
                "exit" | "quit" | "q" => break,
                "manual" | "man" => {
                    if let Err(err) = print_manual() {
                        eprintln!("IO Error: {}", err);
                        return ExitCode::FAILURE;
                    }
                    self.print_prompt();
                    continue;
                }
                "functions" => {
                    print_functions();
                    self.print_prompt();
                    continue;
                }
                "grammar" => {
                    println!("{}", GRAMMAR);
                    self.print_prompt();
                    continue;
                }
                _ => (),
            }

            let res = match self.tc.eval_line(line.as_str()) {
                Ok(eval) => {
                    self.prompt += 1;
                    println!("{} = {}", eval.sym, eval.val);
                    io::stdout().flush()
                }
                Err(err) => self.print_diagnostic(line.as_str(), &err),
            };

            if let Err(err) = res {
                eprintln!("IO Error: {}", err);
                return ExitCode::FAILURE;
            }

            self.print_prompt();
        }
        ExitCode::SUCCESS
    }

    fn print_diagnostic(&self, line: &str, err: &tc::Error) -> Result<(), io::Error> {
        use colored::{control, Colorize};

        let span = err.span();
        let msg = err.to_string();

        if !io::stderr().is_terminal() {
            control::set_override(false);
        }

        eprintln!("{}: {}", "error".red().bold(), msg);
        eprintln!("{line}");
        eprintln!(
            "{}{}",
            " ".repeat(span.0 as _),
            "^".repeat((span.1 - span.0) as _).red().bold()
        );

        control::unset_override();

        io::stderr().flush()
    }
}

fn print_functions() {
    use colored::{control, Colorize};
    use tc::func;

    if !io::stdout().is_terminal() {
        control::set_override(false);
    }

    let mut cat = None;

    let funcs = func::all_funcs();
    let max_len = funcs.iter().map(|f| f.name.len()).max().unwrap_or(0);

    for func in func::all_funcs() {
        if cat != Some(func.category) {
            let cat = func.category.to_string();
            println!("{}:", cat.bold().blue());
        }
        cat = Some(func.category);

        println!(
            "    {}{}: {}",
            func.name.bold(),
            " ".repeat(max_len - func.name.len()),
            func.help
        );
    }

    control::unset_override();
}

fn print_manual() -> io::Result<()> {
    use colored::control::SHOULD_COLORIZE;
    use strip_ansi_escapes::Writer;

    let color = io::stdout().is_terminal() && SHOULD_COLORIZE.should_colorize();
    if color {
        println!("{}", MANUAL);
        io::stdout().write_all(MANUAL.as_bytes())?;
    } else {
        let mut writer = Writer::new(io::stdout());
        writer.write_all(MANUAL.as_bytes())?;
    }
    io::stdout().flush()
}
