
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reflow

![](https://img.shields.io/badge/status-experimental-red)

An experimental linter and formatter designed as a syntax pattern
matching DSL.

## Status

Right now this is entirely a proof of concept. The examples may break or
change as the package develops, and there are no guarantees that this
will ever mature into something stable.

## Goals

-   Make syntax styles easier to express, validate and apply
-   Apply style logic in a minimal traversal of the syntax tree.
-   Make composition of styles easy, built upon minimal rules

## Examples

``` r
library(reflow)
options(reflow.quiet = FALSE)

# define a set of linter rules
rs <- rules(
  "function header opening paren should not be preceded by whitespace" = rule(
    "function", ws(0), "expr"
  ),
  "function header closing paren should be followed by a space or newline" = rule(
    "function", "expr", either_token(ws(1), ws(nl = 1)), any_token()
  ),
  "parameter assignments should always be surrounded by single spaces" = rule(
    on = "eq_formals",
    ws(1), "=", ws(1)
  ),
  "multiline expressions should be indented by one level" = rule(
    "{",
    ws(nl = 1),
    indent(zero_or_more(any_token(), until = "}")),
    ws(nl = 1),
    "}"
  )
)

# match the rules against a block of code
match_style(rs, "
test <- function()  1L

test <- function (a = 1L, b  = 3){
   1 + 2L
  {
    1 + 2
  }
}
")
```

    <<text> 2:21>
    1▕
    2▕ test <- function()  1L
                           ^
    function header closing paren should be followed by a space or newline

    <<text> 4:18>
    2▕ test <- function()  1L
    3▕
    4▕ test <- function (a = 1L, b  = 3){
                        ^
    function header opening paren should not be preceded by whitespace

    <<text> 4:34>
    2▕ test <- function()  1L
    3▕
    4▕ test <- function (a = 1L, b  = 3){
                                        ^
    function header closing paren should be followed by a space or newline

    <<text> 4:30>
    2▕ test <- function()  1L
    3▕
    4▕ test <- function (a = 1L, b  = 3){
                                    ^
    parameter assignments should always be surrounded by single spaces

    <<text> 5:4>
    3▕
    4▕ test <- function (a = 1L, b  = 3){
    5▕    1 + 2L
          ^
    multiline expressions should be indented by one level
