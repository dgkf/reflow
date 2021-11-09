#' @export
rules <- function(...) {
  rs <- list(...)
  rs <- mapply(
    function(x, n) {
      if (inherits(x, "rule")) {
        if (is.null(x$msg) && nchar(n)) x$msg <- n
        return(x)
      }
      rule(x, on = n)
    },
    rs,
    names(rs) %||% rep_len("", length(rs)),
    SIMPLIFY = FALSE)
  structure(rs, class = c("rules", class(rs)))
}

#' @export
rule <- function(x, ...) {
  UseMethod("rule")
}

#' @export
rule.default <- function(x, ..., on, msg) {
  p <- pattern(x, ...)
  if (missing(on)) on <- if (is.character(p)) p else p$x[[1L]]
  if (missing(msg)) msg <- NULL
  rule.pattern(p, on = on, msg = msg)
}

#' @export
rule.pattern <- function(x, ..., on = x$x[[1L]], msg = NULL) {
  obj <- list(pattern = x, on = on, msg = msg)
  structure(obj, class = c("rule", class(obj)))
}

#' @export
format.rule <- function(x, ...) {
  c(
    paste0("<rule> ", x$msg),
    "$on",
    paste0(collapse = "\n", "  ", format_pattern_or_char(x$on, ...)),
    "$pattern",
    paste0(collapse = "\n", "  ", format_pattern_or_char(x$pattern, ...))
  )
}

#' @export
print.rule <- function(x, ...) {
  cat(paste0(collapse = "\n", format(x, ...)), "\n")
}

#' @export
print.rules <- function(x, ...) {
  print(unclass(x), ...)
}
