use std::fmt::Display;
use std::io::{self, BufRead, IsTerminal, Write};
use tc::TermCalc;
use tc::{self, input::HasSpan};

fn main() -> Result<(), Error> {
    let mut interactive = false;
    let mut strip = false;
    let mut eval = Vec::new();

    for arg in std::env::args().skip(1) {
        println!("arg: {}", arg);
        if arg == "--interactive" || arg == "-i" {
            interactive = true;
        } else if arg == "--strip" || arg == "-s" {
            strip = true;
        } else {
            eval.push(arg);
        }
    }

    let need_prompt = need_prompt(!eval.is_empty(), interactive, io::stdin().is_terminal())?;

    let mut driver = Driver {
        tc: tc::TermCalc::new(),
        prompt: 1,
        need_prompt,
        strip,
    };

    driver.do_loop(eval)
}

struct Driver {
    tc: TermCalc,
    prompt: u32,
    need_prompt: bool,
    strip: bool,
}

impl Driver {
    fn do_loop(&mut self, eval_lines: Vec<String>) -> Result<(), Error> {
        if self.need_prompt {
            print_prompt(self.prompt);
            let lines = eval_lines
                .into_iter()
                .map(Result::Ok)
                .chain(io::stdin().lock().lines());
            self.line_loop(lines)
        } else {
            let lines = eval_lines.into_iter().map(Result::Ok);
            self.line_loop(lines)
        }
    }

    fn line_loop<L>(&mut self, lines: L) -> Result<(), Error>
    where
        L: Iterator<Item = Result<String, io::Error>>,
    {
        for line in lines {
            let line = line?;
            if self.need_prompt {
                let ll = line.to_lowercase();
                if matches!(ll.as_str(), "exit" | "quit" | "q") {
                    break;
                }
            }
            self.do_line(&line)?;
        }
        Ok(())
    }

    fn do_line(&mut self, line: &str) -> Result<(), Error> {
        let eval = self.tc.eval_line(line);

        if let Err(err) = eval {
            print_diagnostic(line, &err);
            if self.need_prompt {
                print_prompt(self.prompt);
                return Ok(());
            } else {
                return Err(Error::Tc(err));
            }
        }

        let eval = eval.unwrap();
        if self.strip {
            println!("{}", eval.val);
        } else {
            println!("{} = {}", eval.sym, eval.val);
        }

        if self.need_prompt {
            self.prompt += 1;
            print_prompt(self.prompt);
        }
        Ok(())
    }
}

fn print_diagnostic(line: &str, err: &tc::Error) {
    let span = err.span();
    let msg = err.to_string();
    eprintln!("at {}: {}", span.0 + 1, msg);
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

fn print_prompt(prompt: u32) -> u32 {
    print!("{}> ", prompt);
    io::stdout().flush().unwrap();
    prompt / 10 + 2
}

fn need_prompt(has_eval: bool, interactive: bool, is_terminal: bool) -> Result<bool, ArgError> {
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

    if interactive && !is_terminal {
        return Err(ArgError::NeedTerminal);
    }

    Ok((!has_eval && is_terminal) || interactive)
}

#[test]
fn test_need_prompt() {
    // Truth table
    let tests = &[
        &[0, 0, 0, 0, 0],
        &[0, 0, 1, 1, 0],
        &[0, 1, 0, 0, 1],
        &[0, 1, 1, 1, 0],
        &[1, 0, 0, 0, 0],
        &[1, 0, 1, 0, 0],
        &[1, 1, 0, 0, 1],
        &[1, 1, 1, 1, 0],
    ];

    for test in tests {
        let has_eval = test[0] == 1;
        let interactive = test[1] == 1;
        let is_terminal = test[2] == 1;
        let result = need_prompt(has_eval, interactive, is_terminal);
        let error = result.is_err();
        let go_interactive = result.is_ok() && result.unwrap();
        assert_eq!(
            go_interactive,
            test[3] == 1,
            "{} {} {}",
            has_eval,
            interactive,
            is_terminal
        );
        assert_eq!(
            error,
            test[4] == 1,
            "{} {} {}",
            has_eval,
            interactive,
            is_terminal
        );
    }
}
