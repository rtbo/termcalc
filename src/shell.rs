use std::io::{self, Write};

use crossterm::{
    cursor, event, queue,
    style::{self, Stylize},
    terminal,
};
use tc::input::HasSpan;

pub struct Shell {
    tc: tc::TermCalc,
    prompt: u32,
    hist: Vec<String>,
    size: (u16, u16),
}

impl Shell {
    pub fn new(tc: tc::TermCalc) -> Self {
        Self {
            tc,
            prompt: 1,
            hist: Vec::new(),
            size: (0, 0),
        }
    }

    pub fn main_loop(&mut self) -> io::Result<()> {
        self.size = terminal::size()?;
        let res = self.do_loop();
        res
    }
}

struct Input {
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

impl Input {
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

    fn typechar(&mut self, c: char) {
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
            queue!(buf,
                style::Print(":"),
                style::Print(line[1..].with(style::Color::Yellow)),
            )?;
        }
        queue!(
            buf,
            cursor::MoveTo(self.pos.0 + self.hpos as u16, self.pos.1),
        )?;
        buf.flush()
    }
}

enum Cmd {
    Loop,
    Exit,
    Eval(String),
    Cmd(String),
}

impl Cmd {
    fn ends_input(&self) -> bool {
        !matches!(self, Cmd::Loop)
    }
}

impl Shell {
    fn do_loop(&mut self) -> io::Result<()> {
        loop {
            self.print_prompt();

            let mut input = Input::new(cursor::position()?, self.hist.clone());

            terminal::enable_raw_mode()?;
            loop {
                let cmd = self.handle_event(&mut input)?;
                input.render(&mut io::stdout())?;

                if cmd.ends_input() {
                    terminal::disable_raw_mode()?;
                    println!("");
                }
                match cmd {
                    Cmd::Loop => continue,
                    Cmd::Exit => return Ok(()),
                    Cmd::Eval(expr) => {
                        self.hist.push(expr.clone());
                        match self.tc.eval_line(expr.as_str()) {
                            Ok(tc::Eval { sym, val }) => {
                                println!("{} = {}", sym, val);
                                self.prompt += 1;
                                break;
                            }
                            Err(err) => {
                                self.print_diagnostic(&expr, &err)?;
                                break;
                            }
                        }
                    }
                    Cmd::Cmd(cmd) => match cmd.as_str() {
                        "exit" | "quit" | "q" => return Ok(()),
                        _ => {
                            eprintln!("Unknown command: {}", cmd);
                            break;
                        }
                    },
                }
            }
        }
    }

    fn handle_event(&mut self, input: &mut Input) -> io::Result<Cmd> {
        let ev = event::read()?;
        match ev {
            event::Event::Resize(w, h) => self.size = (w, h),
            event::Event::Key(event::KeyEvent {
                code, modifiers, ..
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
                    input.typechar(c);
                }
                event::KeyCode::Enter => {
                    if input.line().starts_with(":") {
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
}

impl Shell {
    fn print_prompt(&self) {
        print!("{} ", format!("{}>", self.prompt).dim());
    }

    fn print_diagnostic(&self, expr: &str, err: &tc::Error) -> io::Result<()> {
        let span = err.span();
        let msg = err.to_string();

        eprintln!("{}: {}", "error".red().bold(), msg);
        eprintln!("{expr}");
        eprintln!(
            "{}{}",
            " ".repeat(span.0 as _),
            "^".repeat((span.1 - span.0) as _).red().bold()
        );

        io::stderr().flush()
    }
}
