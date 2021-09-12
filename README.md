
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
library(flow)

options(
  flow.debug = TRUE,  # print lint messages
  flow.quiet = FALSE
)

rs <- rules(
  "symbol_formals" = rule(
    on = "symbol_formals",
    pattern(
      "symbol_formals",
      zero_or_one(" = ", "expr"),
      either_token(
        rule(
          msg = "parameter-separating commas should always be followed by a space",
          ", "
        ),
        ")"
      )
    )
  ),

  "function_header" = rule(
    msg = "function headers should wrap to two indentation levels",
    on = "function",
    indent(newline = 0L, wrap = 2L,
      "function",
      "(",
      zero_or_more(any_token(), until = ")"),
      rule(") ", msg = "function headers should always be followed by a space")
    )
  )
)

match_style(rs, "
  x <- function(a=1,b  = 2){
    print(a + b)
  }
")
```

    <<text> 1:1>
    1▕   x <- function(a=1,b  = 2){ 
                        ^

    <<text> 1:1>
    1▕   x <- function(a=1,b  = 2){ 
                          ^

    <<text> 1:1>
    1▕   x <- function(a=1,b  = 2){ 
                              ^

    <<text> 1:1>
    1▕   x <- function(a=1,b  = 2){ 
                                  ^