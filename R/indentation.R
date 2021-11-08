#' @export
indentation <- function(newline = 0L, wrap = .spaces,
    .spaces = getOption("reflow.indent", 2L)) {

  s <- list(newline = newline, wrap = wrap)
  structure(s, class = c("indentation", class(s)))
}

indentation_apply_f <- function(f, e1, e2) {
  indentation(
    newline = f(e1$newline, e2$newline),
    wrap = e2$wrap
  )
}

#' @export
`+.indentation` <- function(e1, e2) {
  indentation_apply_f(`+`, e1, e2)
}

#' @export
`-.indentation` <- function(e1, e2) {
  indentation_apply_f(`-`, e1, e2)
}

format.indentation <- function(x, ...) {
  sprintf("indent[newlines:%d;wrap:%d]", x$newline, x$wrap)
}
