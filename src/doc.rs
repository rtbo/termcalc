use std::io::{self, Write};

pub const MANUAL: &str = include_str!("../doc/gen/tc.1.ansi");
pub const GRAMMAR: &str = include_str!("../doc/Grammar.ebnf");
pub const AFTER_HELP: &str = "If [EVALS] if provided, passed arguments will be evaluated,
and program will exit unless --interactive is specified.

If [EVALS] is not provided, program will enter interactive mode
regardless of the --interactive switch.
";

pub fn write_functions<W: Write>(out: &mut W, style: bool) -> io::Result<()> {
    use crossterm::style::Stylize;
    use tc::func;

    let mut cat = None;

    let funcs = func::all_funcs();
    let max_len = funcs.iter().map(|f| f.name.len()).max().unwrap_or(0);
    for func in func::all_funcs() {
        if cat != Some(func.category) {
            if cat.is_some() {
                writeln!(out)?;
            }
            let cat = func.category.to_string();
            if style {
                writeln!(out, "{}:", cat.bold().blue())?;
            } else {
                writeln!(out, "{cat}:")?;
            }
        }
        cat = Some(func.category);

        let space = " ".repeat(max_len - func.name.len());
        if style {
            writeln!(out, "    {}{space}: {}", func.name.bold(), func.help)?;
        } else {
            writeln!(out, "    {}{space}: {}", func.name, func.help)?;
        }
    }

    Ok(())
}
