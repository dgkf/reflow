#' @export
indentation <- function(newline = 0L, wrap = .spaces,
    .spaces = getOption("reflow.indent", 2L)) {

  s <- list(newline = newline, wrap = wrap)
  structure(s, class = c("indentation", class(s)))
}

#' @export
prev_block_indentation <- function(xml_node) {
  xml_prev_line_node <- xml_first_node_of_first_expr_of_prev_line(xml_node)
  indentation(as.numeric(xml2::xml_attr(xml_prev_line_node, "col1")) - 1L)
}

#' @export
is.na.indentation <- function(ind, ...) {
  is.na(ind$newline) || is.na(ind$wrap)
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
