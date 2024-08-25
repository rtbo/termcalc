use std::io::{self, Write};

pub const MANUAL: &str = include_str!("../doc/gen/tc.1.ansi");
pub const GRAMMAR: &str = include_str!("../doc/Grammar.ebnf");
pub const AFTER_HELP: &str = "If [EVALS] if provided, passed arguments will be evaluated,
and program will exit unless --interactive is specified.

If [EVALS] is not provided, program will enter interactive mode
regardless of the --interactive switch.
";

pub fn write_functions<W: Write>(out: &mut W) -> io::Result<()> {
    use crossterm::style::Stylize;
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

        let name_len = func.name.len();

        writeln!(
            out,
            "    {}{}: {}",
            func.name.bold(),
            " ".repeat(max_len - name_len),
            func.help
        )?;
    }

    Ok(())
}
