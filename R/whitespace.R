#' @export
whitespace <- function(spaces = 0L, newlines = 0L) {
  spaces <- as.numeric(spaces)
  newlines <- as.numeric(newlines)
  .pattern(list(), spaces = spaces, newlines = newlines, classes = "whitespace")
}


#' @export
ws <- function(s = NA, nl = NA) {
  whitespace(spaces = s, newlines = nl)
}


#' @export
is.na.whitespace <- function(ws, ...) {
  is.na(ws$spaces) && is.na(ws$newlines)
}


#' @export
is.na.whitespaced <- function(ws, ...) {
  is.na(ws$before) && is.na(ws$after)
}



#' A special whitespace class that fails on whitespace last
#'
#' A helpful lazy whitespace checker to prioritize syntactic checks over
#' stylistic checks.
#'
#' @export
whitespaced <- function(x, before = whitespace(NA, NA), after = whitespace(NA, NA)) {
  before_ws <- rule(
    msg = sprintf("`%s` should be preceeded by %s", x, format_human(before)),
    .pattern(x,
      before = before,
      after = whitespace(NA, NA),
      classes = "whitespaced"))

  after_ws <- rule(
    msg = sprintf("`%s` should be followed by %s", x, format_human(after)),
    .pattern(x,
      before = whitespace(NA, NA),
      after = after,
      classes = "whitespaced"))

  elems <- list()
  if (!is.na(before_ws$pattern)) elems <- append(elems, list(before_ws))
  if (!is.na(after_ws$pattern)) elems <- append(elems, list(after_ws))
  do.call(pattern, elems)
}

#' A constructor for coercing to whitespaced
#'
#' @export
as_whitespaced <- function(x, before, after, ...) {
  if (grepl("^\\s+|\\s+$", x)) {
    ws_before <- gsub("^(\\s*).*", "\\1", x)
    before <- if (nchar(ws_before)) {
      whitespace(
        newlines = nchar(gsub("[^\n]", "", ws_before)),
        spaces   = nchar(gsub("^(.*\n)?", "", ws_before)))
    } else {
      whitespace(NA, NA)
    }
    ws_after  <- gsub(".*(\\s*)?$", "\\1", x)
    after <- if (nchar(ws_after)) {
      whitespace(
        newlines = nchar(gsub("[^\n]", "", ws_after)),
        spaces   = nchar(gsub("^.*\n", "", ws_after)))
    } else {
      whitespace(NA, NA)
    }
    whitespaced(
      gsub("^\\s+|\\s+$", "", x),
      before = before,
      after = after)
  } else if (!missing(before) || !missing(after)) {
    NextMethod()
  } else {
    x
  }
}

#' @export
format.whitespaced <- function(x, ...) {
  paste(collapse = " ",
    c(if (!is.null(x$before)) format(x$before, ...),
      format_pattern_or_char(x$x, ...),
      if (!is.null(x$after)) format(x$after, ...)))
}



#' @export
newline <- function() whitespace(newlines = 1L)

#' @export
space <- function() whitespace(newlines = 0L, spaces = 1L)

#' @export
format.whitespace <- function(x, ...) {
  sprintf("ws[%s]",
    paste0(c(
      if (!is.na(x$newlines)) sprintf("%d\\n", x$newlines),
      if (!is.na(x$spaces)) sprintf("%d\\s", x$spaces)),
    collapse = ":"))
}

#' @export
format_human.whitespace <- function(x, ...) {
  paste(c(
    if (!is.na(x$newlines) && x$newlines > 0L)
      sprintf("%s newline%s",
        if (x$newlines == 1L) "a" else x$newlines,
        if (x$newlines != 1L) "s" else ""),
    if (!is.na(x$spaces))
      sprintf("%s space%s",
        if (x$spaces == 1L) "a" else x$spaces,
        if (x$spaces != 1L) "s" else "")),
    collapse = " and ")
}

#' @export
print.whitespace <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
`==.whitespace` <- function(e1, e2) {
  all(e1$spaces == e2$spaces, e1$newlines == e2$newlines, na.rm = TRUE)
}

#' @export
`<.whitespace` <- function(e1, e2) {
  isTRUE(e1$newlines < e2$newlines) || e1$spaces < e2$spaces
}

#' @export
`<=.whitespace` <- function(e1, e2) {
  isTRUE(e1$newlines <= e2$newlines) || e1$spaces <= e2$spaces
}

#' @export
`>.whitespace` <- function(e1, e2) {
  op_nl <- e1$newlines > e2$newlines
  isTRUE(op_nl) || is.na(op_nl) && e1$spaces > e2$spaces
}

#' @export
`>=.whitespace` <- function(e1, e2) {
  op_nl <- e1$newlines >= e2$newlines
  isTRUE(op_nl) || is.na(op_nl) && e1$spaces >= e2$spaces
}
