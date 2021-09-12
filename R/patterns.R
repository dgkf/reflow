#' @export
pattern <- function(..., classes = c()) {
  p <- list(...)
  is_char <- vapply(p, is.character, logical(1L))
  p[is_char] <- lapply(p[is_char], function(x) as_whitespaced(x))
  if (length(p) == 1L && (inherits(p[[1L]], "pattern") || inherits(p[[1L]], "character")))
    return(p[[1L]])
  .pattern(p, classes = classes)
}

#' @export
print.pattern <- function(x, ...) {
  cat(paste(collapse = "\n", format(x, ...)), "\n")
}

#' @export
format_human <- function(x, ...) {
  UseMethod("format_human")
}



.pattern <- function(x, ..., classes = c()) {
  obj <- list(x = x, ...)
  obj$x <- lapply(obj$x, function(x) {
    if (is.character(x)) as_whitespaced(x)
    else x
  })

  if (length(names(obj)) != length(obj))
    stop("All fields must be named")

  structure(obj, class = c(classes, "pattern", class(x)))
}



#' @export
expr <- function(...) {
  .pattern(list(...), classes = "expr")
}



#' @export
indent <- function(..., newline = 1L, wrap = 2L) {
  .pattern(list(...), newline = newline, wrap = wrap, classes = "indent")
}



#' @export
any_token <- function() {
  pattern(classes = "any_token")
}



#' @export
any_infix_token <- function() {
  either_token(
    "special",
    "op_plus",
    "op_minus",
    "op_star",
    "op_slash",
    "op_caret",
    "eq_assign",
    "left_assign",
    "right_assign"
  )
}



#' @export
either_token <- function(...) {
  pattern(..., classes = "either_token")
}



#' Will always match, returning same token
.empty_pattern <- function() pattern(classes = "empty_pattern")

#' Will never match
.never_pattern <- function() pattern(classes = "never_pattern")



#' @export
zero_or_more <- function(..., sep = .empty_pattern(), until = .never_pattern()) {
  .pattern(
    list(...),
    sep = pattern(sep),
    until = pattern(until),
    classes = "zero_or_more")
}

#' @export
zero_or_one <- function(...) {
  .pattern(list(...), classes = "zero_or_one")
}



#' @export
format.pattern <- function(x, ..., verbose = TRUE) {
  format_pattern_verbose(x, ...)
}

format_pattern_or_char <- function(x, ..., indent = 0L) {
  if (is.character(x)) paste0("\"", x, "\"")
  else if (length(x) == 1L && is.character(x[[1L]])) paste0("\"", x[[1L]], "\"")
  else format(x, ..., indent = indent)
}

format_pattern_verbose <- function(x, ..., indent = 0L, indent_nchar = 2L) {
  is_last <- if (is.numeric(indent)) {
    c(rep(FALSE, length.out = max(0, indent - 1L)), TRUE)
  } else {
    indent
  }

  gap  <- strrep(" ", indent_nchar)
  pipe <- paste0("\u2502", strrep(" ", indent_nchar - 1L))
  tee  <- paste0("\u251C", strrep(" ", indent_nchar - 1L))
  elb  <- paste0("\u2570", strrep(" ", indent_nchar - 1L))

  p <- if (is.character(x$x)) x$x else class(x)[[1L]]
  attrs <- x[!names(x) %in% "x"]
  attrs <- lapply(attrs, format_pattern_or_char)

  contents <- mapply(
    format_pattern_or_char,
    x$x,
    indent = lapply(seq_along(x$x) == length(x$x), append, x = is_last),
    MoreArgs = list(indent_nchar = indent_nchar),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    ...
  )

  paste_first <- function(x, pipe_first, pipe_else) {
    x[1L] <- sprintf("%s%s", pipe_first, x[1L])
    x[-1L] <- sprintf("%s%s", pipe_else, x[-1L])
    x
  }

  c(
    paste(p, paste0(sprintf("%s=%s", names(attrs), attrs), collapse = " ")),
    unlist(mapply(
      paste_first,
      contents,
      ifelse(seq_along(x$x) == length(x$x), elb, tee),
      ifelse(seq_along(x$x) == length(x$x), gap, pipe),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    ))
  )
}
