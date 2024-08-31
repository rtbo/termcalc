use std::io::{self, IsTerminal, Write};

use crossterm::{
    cursor,
    event::{self, KeyEventKind},
    queue,
    style::{self, Stylize},
    terminal,
};
use tc::input::HasSpan;

use crate::doc;

pub struct Shell {
    tc: tc::TermCalc,
    prompt: u32,
    expr_hist: Vec<String>,   // this one has also failed evalulations
    eval_hist: Vec<tc::Eval>, // only succesful evaluations
    size: (u16, u16),
    last_sym: Option<String>,
}

impl Shell {
    pub fn new(tc: tc::TermCalc) -> Self {
        Self {
            tc,
            prompt: 1,
            expr_hist: Vec::new(),
            eval_hist: Vec::new(),
            size: (0, 0),
        }
    }

    pub fn main_loop(&mut self) -> io::Result<()> {
        self.size = terminal::size()?;
        loop {
            let prompt_len = self.print_prompt();
            let pos = cursor::position()?;
            // on windows, position returns the correct line,
            // but column is 0 despite just printing the prompt
            // so we use prompt_len instead.
            let mut input = LineInput::new((prompt_len as u16, pos.1), self.expr_hist.clone());

            terminal::enable_raw_mode()?;

            loop {
                let cmd = self.handle_event(&mut input)?;
                input.render(&mut io::stdout())?;

                if cmd.breaks_input() {
                    terminal::disable_raw_mode()?;
                    println!();
                }

                match cmd {
                    Cmd::Loop => continue,
                    Cmd::Exit => return Ok(()),
                    Cmd::Empty => break,
                    Cmd::Eval(expr) => {
                        self.expr_hist.push(expr.clone());
                        match self.tc.eval_line(expr.as_str()) {
                            Ok(tc::Eval { sym, val }) => {
                                println!("{} = {}", sym, val);
                                self.eval_hist.push(tc::Eval { sym, val });
                                self.prompt += 1;
                                break;
                            }
                            Err(err) => {
                                print_diagnostic(&err, prompt_len)?;
                                break;
                            }
                        }
                    }
                    Cmd::Cmd(cmd) => match cmd.trim() {
                        "exit" | "quit" | "q" => return Ok(()),
                        "functions" => {
                            page_functions()?;
                            break;
                        }
                        "manual" | "man" => {
                            page_manual()?;
                            break;
                        }
                        "grammar" => {
                            println!("{}", doc::GRAMMAR);
                            break;
                        }
                        "copy" | "cp" => {
                            match self.eval_hist.last() {
                                None => eprintln!("Nothing to copy"),
                                Some(eval) => {
                                    Self::copy_to_clipboard(eval.val.to_string().as_str());
                                }
                            }
                            break;
                        }
                        cmd => {
                            if cmd.starts_with("copy ") || cmd.starts_with("cp ") {
                                self.handle_copy_arg(cmd);
                                break;
                            } else {
                                eprintln!(
                                    "{}: Unknown command: {}",
                                    "error".red().bold(),
                                    cmd.bold()
                                );
                                break;
                            }
                        }
                    },
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Cmd {
    Loop,
    Exit,
    Empty,
    Eval(String),
    Cmd(String),
}

impl Cmd {
    fn breaks_input(&self) -> bool {
        !matches!(self, Cmd::Loop)
    }
}

impl Shell {
    fn handle_event(&mut self, input: &mut LineInput) -> io::Result<Cmd> {
        let ev = event::read()?;
        match ev {
            event::Event::Resize(w, h) => self.size = (w, h),
            event::Event::Key(event::KeyEvent {
                code,
                modifiers,
                kind: KeyEventKind::Press,
                ..
            }) => match code {
                event::KeyCode::Left => {
                    input.left();
                }
                event::KeyCode::Right => {
                    input.right();
                }
                event::KeyCode::Up => {
                    input.up();
                }
                event::KeyCode::Down => {
                    input.down();
                }
                event::KeyCode::Backspace => {
                    input.backspace();
                }
                event::KeyCode::Char(c)
                    if c == 'c' && modifiers.contains(event::KeyModifiers::CONTROL) =>
                {
                    return Ok(Cmd::Exit);
                }
                event::KeyCode::Esc => {
                    return Ok(Cmd::Exit);
                }
                event::KeyCode::Char(c) => {
                    input.type_char(c);
                }
                event::KeyCode::Enter => {
                    if input.line().trim().is_empty() {
                        return Ok(Cmd::Empty);
                    } else if input.line().starts_with(":") {
                        return Ok(Cmd::Cmd(input.line()[1..].to_string()));
                    } else {
                        return Ok(Cmd::Eval(input.line().to_string()));
                    }
                }
                _ => {}
            },
            _ => {}
        }
        Ok(Cmd::Loop)
    }

    fn handle_copy_arg(&mut self, cmd: &str) {
        let arg = if cmd.starts_with("copy ") {
            &cmd[5..]
        } else if cmd.starts_with("cp ") {
            &cmd[3..]
        } else {
            unreachable!("expected copy command");
        };
        let arg = arg.trim();
        assert!(!arg.is_empty());
        if arg.chars().next().unwrap().is_ascii_digit() {
            let idx = match arg.parse::<usize>() {
                Ok(idx) => {
                    if idx == 0 {
                        eprintln!(
                            "{}: {} isn't a valid history entry",
                            "error".red().bold(),
                            "0".bold()
                        );
                        return;
                    }
                    idx - 1
                }
                Err(err) => {
                    eprintln!(
                        "{}: {arg} is not a valid number ({})",
                        "error".red().bold(),
                        err
                    );
                    return;
                }
            };
            self.copy_hist_to_clipboard(idx);
        } else {
            self.copy_sym_to_clipboard(arg);
        }
    }

    fn copy_sym_to_clipboard(&self, sym: &str) {
        let val = match self.tc.get_var(sym) {
            None => {
                eprintln!("{}: Unknown variable: {}", "error".red().bold(), sym.bold());
                return;
            }
            Some(val) => val,
        };
        Self::copy_to_clipboard(&format!("{val}"));
    }

    fn copy_hist_to_clipboard(&self, idx: usize) {
        match self.eval_hist.get(idx) {
            None => {
                eprintln!(
                    "{}: No such history entry: {}",
                    "error".red().bold(),
                    idx + 1
                );
            }
            Some(eval) => {
                Self::copy_to_clipboard(eval.val.to_string().as_str());
            }
        }
    }

    fn copy_to_clipboard(text: &str) {
        use arboard::Clipboard;

        let mut ctx = match Clipboard::new() {
            Ok(ctx) => ctx,
            Err(err) => {
                eprintln!(
                    "{}: {} ({err})",
                    "error".red().bold(),
                    "Clipboard not available".bold()
                );
                return;
            }
        };
        match ctx.set_text(text) {
            Ok(()) => {
                println!("Copied {}!", text.bold());
            }
            Err(err) => {
                eprintln!(
                    "{}: {} ({err})",
                    "error".red().bold(),
                    "Could not write to clipboard".bold()
                );
            }
        }
    }
}

struct LineInput {
    /// start position of the cursor
    pos: (u16, u16),
    /// horizontal position of the cursor
    hpos: usize,
    /// at which histoty entry is the cursor
    stack_idx: usize,
    /// the stack of history entries
    /// A new input start at the last position of the stack
    stack: Vec<String>,
}

impl LineInput {
    fn new(pos: (u16, u16), hist: Vec<String>) -> Self {
        let mut stack = hist;
        stack.push(String::new());
        Self {
            pos,
            hpos: 0,
            stack_idx: stack.len() - 1,
            stack,
        }
    }

    fn line(&self) -> &str {
        &self.stack[self.stack_idx]
    }

    fn type_char(&mut self, c: char) {
        self.stack[self.stack_idx].insert(self.hpos, c);
        self.hpos += 1;
    }

    fn backspace(&mut self) {
        if self.hpos > 0 {
            self.hpos -= 1;
            self.stack[self.stack_idx].remove(self.hpos);
        }
    }

    fn left(&mut self) {
        if self.hpos > 0 {
            self.hpos -= 1;
        }
    }

    fn right(&mut self) {
        if self.hpos < self.line().len() {
            self.hpos += 1;
        }
    }

    fn up(&mut self) {
        if self.stack_idx > 0 {
            self.stack_idx -= 1;
            self.hpos = self.line().len();
        }
    }

    fn down(&mut self) {
        if self.stack_idx < self.stack.len() - 1 {
            self.stack_idx += 1;
            self.hpos = self.line().len();
        }
    }

    fn render<W: io::Write>(&self, buf: &mut W) -> io::Result<()> {
        queue!(
            buf,
            cursor::MoveTo(self.pos.0, self.pos.1),
            terminal::Clear(terminal::ClearType::UntilNewLine),
        )?;
        let line = self.line();
        if line.starts_with(":") {
            queue!(
                buf,
                style::Print(":"),
                style::Print(line[1..].with(style::Color::Yellow)),
            )?;
        } else {
            queue!(buf, style::Print(line))?;
        }
        queue!(
            buf,
            cursor::MoveTo(self.pos.0 + self.hpos as u16, self.pos.1),
        )?;
        buf.flush()
    }
}

impl Shell {
    fn print_prompt(&self) -> u32 {
        let prompt = format!("{}>", self.prompt);
        let len = prompt.len() as u32;
        print!("{} ", prompt.dim());
        len + 1
    }
}

pub fn print_diagnostic(err: &tc::Error, indent: u32) -> io::Result<()> {
    let span = err.span();
    let space = " ".repeat((indent + span.0) as _);
    let marker = "^".repeat((span.1 - span.0) as _);
    let msg = err.to_string();

    if io::stderr().is_terminal() {
        eprintln!("{space}{}", marker.red().bold());
        eprintln!("{}: {}", "error".red().bold(), msg.bold());
    } else {
        eprintln!("{space}{marker}");
        eprintln!("error: {msg}");
    }

    io::stderr().flush()
}

fn env_bool(name: &str) -> bool {
    match std::env::var(name) {
        Ok(val) => val != "0",
        Err(_) => false,
    }
}

fn page_functions() -> io::Result<()> {
    let mut out = Vec::<u8>::new();

    doc::write_functions(&mut out, true)?;

    let content = String::from_utf8(out).expect("functions page should be valid UTF-8");
    page_content("TC Functions".to_string(), content)
}

fn page_manual() -> io::Result<()> {
    let content = if env_bool("NO_COLOR") {
        strip_ansi_escapes::strip_str(doc::MANUAL)
    } else {
        doc::MANUAL.to_string()
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
