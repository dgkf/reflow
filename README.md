
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

rs <- rules(
  "expr" = rule(
    msg = "multiline expressions should be indented by one level",
    "{",
    ws(nl = 1),
    indent(zero_or_more(any_token(), until = "}")),
    ws(nl = 1),
    "}"
  )
)

match_style(rs, "
test <- function() {
   1 + 2L
  {
     1 + 2
  }
}
")
```

    <<text> 3:4>
    1▕
    2▕ test <- function() {
    3▕    1 + 2L
          ^
    multiline expressions should be indented by one level

    <<text> 5:6>
    3▕    1 + 2L
    4▕   {
    5▕      1 + 2
            ^
    multiline expressions should be indented by one level
