#' @export
match_content <- function(style, xml_node, ...) {
  exprs <- content_tree_extract_xml(xml_node)
  code <- content_tree_extract_origin(xml_node)

  for (expr in exprs) {
    match_style(style, expr, code = code, ...)
  }

  invisible(xml_next_sibling(exprs[[length(exprs)]]))
}


#' @export
match_style <- function(style, xml_node, ...) {
  if (inherits(xml_node, "character"))
    xml_node <- content_tree(xml_node)
  if (inherits(xml_node, "content_tree"))
    return(match_content(style, xml_node, ...))
  UseMethod("match_style")
}


test_style <- function(..., debug) {
  withRestarts(withCallingHandlers({
      structure(TRUE,
        match = match_style(..., debug = FALSE),
        class = "test_style_result")
    },
    unmatchedRuleCondition = function(cond) {
      invokeRestart("unmatchedRuleRestart", cond)
    }),
    unmatchedRuleRestart = function(cond) {
      FALSE
    })
}


with_no_match_interrupt <- function(...) {
  withRestarts(withCallingHandlers(eval(...),
    uninterruptingUnmatchedRuleCondition = unmatchedRuleConditionHandler,
    interruptingUnmatchedRuleCondition = function(cond) {
      invokeRestart("unmatchedRuleRestart", cond)
    }),
    unmatchedRuleRestart = unmatchedRuleConditionHandler
  )
}


suppress_no_match_interrupt <- function(...) {
  withCallingHandlers(eval(...),
    interruptingUnmatchedRuleCondition = function(cond) {
      invokeRestart("continueUnmatchedRuleCondition", cond)
    })
}


get_test_match <- function(x, ...) {
  attr(x, "match")
}


#' @export
match_style.rules <- function(style, xml_node, ...) {
  for (xml_node in xml2::xml_find_all(xml_node, ".//*")) {
    for (rule in style) {
      # rules may apply at any depth, initialize with unknown indentation
      match_style(rule, xml_node, ind = indentation(newline = NA), ...)
    }
  }
  xml_node
}


#' @export
match_style.rule <- function(style, xml_node, ..., rule = style) {
  if (test_style(style$on, xml_node, ...))
    with_no_match_interrupt(match_style(style$pattern, xml_node, ..., rule = rule))
}


match_style.default <- function(style, xml_node, ...) {
  browser()
}


#' @export
match_style.list <- function(style, xml_node, ind = indentation(), ...) {
  # determine whether a subpattern will be caught by a future indentation check
  is_deepest_pattern <- function(i) {
    !inherits(i, "pattern") || length(i$x) == 0L
  }

  is_ignored_style_class <- function(i) {
    any(c("indent", "whitespace") %in% class(subpat))
  }

  for (subpat in style) {
    if (!is_ignored_style_class(subpat) && is_deepest_pattern(subpat))
      match_style(ind, xml_node, ind = ind, ...)
    xml_node <- match_style(subpat, xml_node, ind = ind, ...)
  }
  xml_node
}



#' @export
match_style.pattern <- function(style, xml_node, ...) {
  match_style(style$x, xml_node, ...)
}



#' @export
match_style.character <- function(style, xml_node, ...) {
  if (style %in% names(token_aliases)) {
    match_style(token_aliases[[style]], xml_node, ...)
  } else if (token(xml_node) == style) {
    xml_next_sibling(xml_node)
  } else {
    no_match(style, xml_node, ...)
  }
}




#' @export
match_style.whitespace <- function(style, xml_node, ...) {
  if (xml_whitespace_before(xml_node) == style) return(xml_node)
  no_match(style, xml_node, ...)
}



#' @export
match_style.whitespaced <- function(style, xml_node, ...) {
  match_style(style$x, xml_node, ...)
  match_style(style$before, xml_node, ...)
  match_style(style$after, xml_next_leaf(xml_node), ...)
}



#' @export
match_style.expr <- function(style, xml_node, ...) {
  if (token(xml_node) != "expr") return(NULL)

  # iterate over expression, matching style patterns
  outer_next_sibling <- xml_next_sibling(xml_node)
  xml_node <- xml2::xml_child(xml_node)

  and_any_tokens <- FALSE
  for (i in seq_along(style$x)) {
    subpat <- style$x[[i]]
    if (i == length(style$x) && identical(subpat, any_tokens)) {
      and_any_tokens <- TRUE
      break
    }
    xml_node <- match_style(subpat, xml_node, ...)
  }

  # test that the pattern fully exhausts the expressions
  finished_expr <- is.na(xml_node)
  if (!and_any_tokens && !isTRUE(finished_expr))
    no_match(style, xml_node, ...)

  outer_next_sibling
}



#' @export
match_style.singleline <- function(style, xml_node, ...) {
  startline <- xml_line1(xml_node)
  if (m <- test_style(style$x, xml_node, ...)) {
    xml_end_node <- get_test_match(m)
    endline <- xml_line2(xml_node)
    if (startline == endline) return(xml_end_node)
  }
  no_match(style, xml_node, ...)
}



#' @export
match_style.multiline <- function(style, xml_node, ...) {
  startline <- xml_line1(xml_node)
  if (m <- test_style(style$x, xml_node, ...)) {
    xml_end_node <- get_test_match(m)
    endline <- xml_line2(xml_preceding_sibling(xml_node))
    if (startline < endline) return(xml_end_node)
  }
  no_match(style, xml_node, ...)
}



#' @export
match_style.empty_pattern <- function(style, xml_node, ...) {
  xml_node
}



#' @export
match_style.never_pattern <- function(style, xml_node, ...) {
  no_match(style, xml_node, ...)
}



#' @export
match_style.indent <- function(style, xml_node, ind = indentation(), ...) {
  if (is.na(ind)) ind <- prev_block_indentation(xml_node)
  ind <- ind + indentation(newline = style$newline, wrap = style$wrap)
  NextMethod(ind = ind)
}



#' @export
match_style.indentation <- function(style, xml_node, ind = indentation(), ...) {
  ws <- xml_whitespace_before(xml_node)
  # cat(format(ws), format(ind), '\n')

  if (isTRUE(ws >= whitespace(newlines = 1L, spaces = 0L))) {
    node_indent <- as.numeric(xml2::xml_attr(xml_node, "col1")) - 1L
    if (isTRUE(node_indent != style$newline)) {
      suppress_no_match_interrupt(no_match(style, xml_node, ...))
    }
  }
  xml_node
}



#' @export
match_style.any_token <- function(style, xml_node, ...) {
  xml_next_sibling(xml_node)
}



#' @export
match_style.either_token <- function(style, xml_node, ...) {
  for (subtoken in style$x)
    if (m <- test_style(subtoken, xml_node, ...))
      return(get_test_match(m))
  no_match(style, xml_node, ...)
}



#' @export
match_style.zero_or_more <- function(style, xml_node, ...) {
  n <- 0L
  repeat {
    if (is.na(xml_node) || test_style(style$until, xml_node, ...))
      break

    # TODO: this assumes that this style will exhaust an expression
    if (n > 0L) xml_node <- match_style(style$sep, xml_node, ...)
    xml_node <- match_style(style$x, xml_node, ...)
    n <- n + 1L
  }
  xml_node
}



#' @export
match_style.zero_or_one <- function(style, xml_node, ...) {
  after_one_node <- match_style(style$x, xml_node, ...)
  if (is_matched(after_one_node)) after_one_node
  else xml_node
}
