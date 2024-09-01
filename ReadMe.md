# Terminal calculator

A simple command line calculator.

## Purpose

The binary name is `tc`.
If you are already in a terminal session and need a quick calculation,
typing `tc "2.5 + 3.73 / 4"` is actually faster than grabbing
your desktop calculator or starting and using a calculator app.

`tc` is also **a lot simpler** to use than `bc` or `dc`.

## Manual

The [manual](./doc/Manual.adoc) states quite everything you need to know about `tc`.
(and it'll take you about one minute to read)

## Examples

### Expressions on the command line

```sh
$ tc "1.2 + 3^2 + sqrt(12)"
1.2 + 3^2 + sqrt(12) = 13.664101615137753

$ tc "cos(pi/3)" "sin(pi/6)"
cos(pi/3) = 0.5000000000000001
sin(pi/6) = 0.49999999999999994

$ tc -s "12*3 + 18*2.5 - 3 * 2.3" # or --strip
74.1
```

### Interactive session

![an interactive session screenshot](./doc/interactive-session.png)

### Clear diagnostics

```sh
$ tc "1.4 + son(pi/6)" # typo
1.4 + son(pi/6)
      ^^^
error: Function `son` is unknown
```

## Grammar reference

See [Grammar.ebnf](./doc/Grammar.ebnf)
