% TC(1) User Manual
% Rémi THEBAULT
% August 24th, 2024

# NAME

**tc** - a simple Terminal Calculator

# SYNOPSYS

| **tc** [**-i**|**--interactive**]
|    [**-s**|**--strip**]
|    [**-f**|**--functions**]
|    [**-g**|**--grammar**]
|    [**-h**|**--help**]
|    [**-V**|**--version**]
|    [*EXPR*]...

# DESCRIPTION

**tc** is a simple Terminal Calculator. It can be provided expressions to
evaluate on the command line, or it can enter in interactive shell mode.

If expressions are provided on the command line, they will be evaluated
in order, and program will exit unless **--interactive**
is specified.

If no expression argument is provided, program will enter interactive
mode regardless of the **--interactive** switch.

The **--strip** option is only used for evaluation of expressions on the
command line. If provided, **tc** will print the bare result of evaluation
and exit. This is useful to parse **tc**'s output from scripts.

## Interactive shell commands

A set of special commands are available in the interactive shell:

* `quit`, `q`, `exit`: exit the program
* `functions`: page the list of supported functions
* `manual`, `man`: page this manual
* `grammar`: print the EBNF grammar reference

## Grammar

Whitespaces are ignored and comments can be entered,
starting from `#` and spanning until the end of the line.

Each evaluation consists of one line which is either an expression or an
assignment.  All expressions are evaluated in double precision floating point.

Expression grammar is what you would generally expect for ASCII arithmetic
expressions (see the *examples* hereunder for a quick tuto).

Assignment grammar is `variable = expression`.
Once a variable is assigned, it can be reused in latter expressions.
Variables `pi` and `e` are initialized at the start.
Expressions that are not assigned to a user variable, are assigned to
the special `ans` variable, which can also be reused in latter expressions
like any other variable.

The following operators are supported, listed in precedence order:

* `^`
* `+`, `-` (unary)
* `*`, `/`, `%`
* `+`, `-`

A set of mathematical functions are provided, such as `sin`, `sqrt`, or `ln`.
Trigonometric functions accept angles in radians only.
Functions `rads` and `degs` convert to degrees to radians and radians to degrees
respectively.

### Examples

* `4 + 3.5`
* `0.124 * (ans + 12) / 3`
* `4 * sin(2*pi/3)`
* `angle = asin(-0.15)`
* `tan(angle)`
* `e^(pi/3)`
* `exp(pi/3)` (same result as previous expression)
* `pow(e, pi/3)` (again same result)

# OPTIONS

**-i**, **--interactive**
:   Force to enter interactive mode

**-s**, **--strip**
:   Strip output of evaluations to minimum

**-f**, **--functions**
:   Print the list of supported functions

**-g**, **--grammar**
:   Print the EBNF grammar reference

**-h**, **--help**
:   Print help (see a summary with **-h**)

**-V**, **--version**
:   Print version
