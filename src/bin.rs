use clap::Parser;
use std::fmt::Display;
use std::io::{self, BufRead, BufWriter, IsTerminal, Write};
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

    match driver.run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(RunError::Io(err)) => {
            eprintln!("IO Error: {}", err);
            ExitCode::FAILURE
        }
        Err(RunError::Tc(line, err)) => {
            let _ = print_diagnostic(&line, &err);
            ExitCode::FAILURE
        }
    }
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

    fn run(&mut self) -> Result<(), RunError> {
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
            self.interactive_loop()?;
        }

        Ok(())
    }

    fn print_prompt(&self) {
        use colored::{control, Colorize};

        if !io::stderr().is_terminal() {
            control::set_override(false);
        }

        print!("{} ", format!("{}>", self.prompt).dimmed());
        io::stdout().flush().unwrap();
    }

    fn interactive_loop(&mut self) -> io::Result<()> {
        self.print_prompt();

        for line in io::stdin().lock().lines() {
            let line = line?;

            let ll = line.to_lowercase();
            match ll.as_str().trim() {
                "" => {
                    self.print_prompt();
                    continue;
                }
                "exit" | "quit" | "q" => break,
                "manual" | "man" => {
                    page_manual()?;
                    self.print_prompt();
                    continue;
                }
                "functions" => {
                    page_functions()?;
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

            match self.tc.eval_line(line.as_str()) {
                Ok(eval) => {
                    self.prompt += 1;
                    println!("{} = {}", eval.sym, eval.val);
                    io::stdout().flush()?;
                }
                Err(err) => print_diagnostic(line.as_str(), &err)?,
            };

            self.print_prompt();
        }

        Ok(())
    }
}

fn print_diagnostic(line: &str, err: &tc::Error) -> Result<(), io::Error> {
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

fn print_functions() -> io::Result<()> {
    use colored::control::{self, SHOULD_COLORIZE};

    let color = io::stdout().is_terminal() && SHOULD_COLORIZE.should_colorize();
    control::set_override(color);

    let mut out = BufWriter::new(io::stdout().lock());
    let res = write_functions(&mut out);

    control::unset_override();

    res?;
    out.flush()
}

fn page_functions() -> io::Result<()> {
    use colored::control::{self, SHOULD_COLORIZE};

    let mut out = Vec::<u8>::new();

    let color = io::stdout().is_terminal() && SHOULD_COLORIZE.should_colorize();
    control::set_override(color);

    let res = write_functions(&mut out);

    control::unset_override();
    res?;

    let content = String::from_utf8(out).expect("functions page should be valid UTF-8");
    page_content("TC Functions".to_string(), content)
}

fn write_functions<W: Write>(out: &mut W) -> io::Result<()> {
    use colored::Colorize;
    use tc::func;

    let mut cat = None;

    let funcs = func::all_funcs();
    let max_len = funcs.iter().map(|f| f.name.len()).max().unwrap_or(0);

    for func in func::all_funcs() {
        if cat != Some(func.category) {
            let cat = func.category.to_string();
            writeln!(out, "{}:", cat.bold().blue())?;
        }
        cat = Some(func.category);

        writeln!(
            out,
            "    {}{}: {}",
            func.name.bold(),
            " ".repeat(max_len - func.name.len()),
            func.help
        )?;
    }

    Ok(())
}

fn page_manual() -> io::Result<()> {
    use colored::control::SHOULD_COLORIZE;

    let color = io::stdout().is_terminal() && SHOULD_COLORIZE.should_colorize();

    let content = if color {
        MANUAL.to_string()
    } else {
        strip_ansi_escapes::strip_str(MANUAL)
    };

    page_content("TC manual".to_string(), content)
}

fn page_content(title: String, content: String) -> io::Result<()> {
    use crossterm::event::KeyCode;
    use pager_rs::{Command, CommandList, CommandType, State, StatusBar};

    let status_bar = StatusBar::new(title);
    let mut state = State::new(
        content,
        status_bar,
        CommandList::combine(vec![
            CommandList::quit(),
            CommandList::navigation(),
            CommandList(vec![
                Command {
                    cmd: vec![CommandType::Key(KeyCode::Char('j'))],
                    desc: "Cursor down".to_string(),
                    func: &|state| state.down(),
                },
                Command {
                    cmd: vec![CommandType::Key(KeyCode::Char('k'))],
                    desc: "Cursor up".to_string(),
                    func: &|state| state.up(),
                },
            ]),
        ]),
    )?;
    state.show_line_numbers = false;

    pager_rs::init()?;
    pager_rs::run(&mut state)?;
    pager_rs::finish()?;
    Ok(())
}
