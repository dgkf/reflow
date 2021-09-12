#' @export
match_content <- function(style, xml_node, ...) {
  exprs <- xml2::xml_find_all(xml_node, "/exprlist/*")
  code <- attr(xml_node, "origin")
  for (expr in exprs) {
    match_style(style, expr, code = code, ...)
  }
  invisible(xml_next_sibling(tail(exprs, 1L)))
}



#' @export
match_style <- function(style, xml_node, ...) {
  if (inherits(xml_node, "character"))
    xml_node <- content_tree(xml_node)
  if (inherits(xml_node, "content_tree"))
    return(match_content(style, xml_node, ...))
  UseMethod("match_style")
}


#' @export
match_style.rules <- function(style, xml_node, ...) {
  for (xml_node in xml2::xml_find_all(xml_node, "//*"))
    for (rule in style)
      match_style(rule, xml_node, ...)
  xml_node
}


#' @export
match_style.rule <- function(style, xml_node, ..., rule = style, must_match = TRUE) {
  if (is_matched(match_style(style$on, xml_node, ..., must_match = FALSE)))
    match_style(style$pattern, xml_node, ..., rule = rule, must_match = must_match)
}


match_style.default <- function(style, xml_node, ...) {
  browser()
}


#' @export
match_style.list <- function(style, xml_node, ind = indentation(), ..., must_match = TRUE) {
  for (subpat in style) {
    # match any indentation whitespace
    ws <- xml_whitespace_before(xml_node)
    if (ws >= whitespace(newlines = 1L, spaces = 0L))
      match_style(ind, xml_node, ind = ind, ..., must_match = must_match)

    # match style pattern
    next_xml_node <- match_style(subpat, xml_node, ind = ind, ..., must_match = must_match)
    if (is_unmatched(next_xml_node))
      return(no_match(subpat, xml_node, ..., must_match = must_match))
    xml_node <- next_xml_node

    # if must_match is NA, convert to TRUE after first match
    must_match <- is.na(must_match) || must_match
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
match_style.whitespaced <- function(style, xml_node, ..., must_match = TRUE) {
  # try to match inner node before whitespace
  next_xml_node <- match_style(style$x, xml_node, ..., must_match = must_match)
  if (is_matched(next_xml_node)) {
    must_match <- is.na(must_match) || must_match
    match_style(style$before, xml_node, ..., must_match = must_match)
    match_style(style$after, xml_next_sibling(xml_node), ..., must_match)
    return(next_xml_node)
  }

  no_match(style, xml_node, ..., must_match = must_match)
}



#' @export
match_style.expr <- function(style, xml_node, ...) {
  outer_next_sibling <- xml_next_sibling(xml_node)
  if (token(xml_node) != "expr") return(NULL)
  xml_node <- xml2::xml_child(xml_node)
  for (subpat in style$x) {
    next_xml_node <- match_style(subpat, xml_node, ...)
    if (is_unmatched(next_xml_node)) no_match(subpat, xml_node, ...)
    xml_node <- next_xml_node
  }
  finished_expr <- is.na(xml_node)
  if (isTRUE(finished_expr)) return(outer_next_sibling)
  no_match(style, xml_node, ...)
}



#' @export
match_style.empty_style <- function(style, xml_node, ...) {
  xml_node
}



#' @export
match_style.none_style <- function(style, xml_node, ...) {
  NULL
}



#' @export
match_style.indent <- function(style, xml_node, ind = indentation(), ...) {
  ind <- ind + indentation(newline = style$newline, wrap = style$wrap)
  NextMethod(ind = ind)
}



#' @export
match_style.indentation <- function(style, xml_node, ...) {
  col1 <- as.numeric(xml2::xml_attr(xml_node, "col1"))
  if (col1 == style$newline * 2L + 1L) xml_node
  else no_match(style, xml_node, ...)
}



#' @export
match_style.any_token <- function(style, xml_node, ...) {
  next_xml_node <- xml_next_sibling(xml_node)
  if (!is.na(next_xml_node)) return(next_xml_node)
  no_match(style, xml_node, ...)
}



#' @export
match_style.either_token <- function(style, xml_node, ..., must_match = TRUE) {
  for (subtoken in style$x) {
    next_xml_node <- match_style(subtoken, xml_node, ..., must_match = FALSE)
    if (is_matched(next_xml_node)) return(next_xml_node)
  }
  no_match(style, xml_node, ..., must_match = must_match)
}



#' @export
match_style.zero_or_more <- function(style, xml_node, ..., must_match = TRUE) {
  n <- 1L
  while (n > 0L) {
    if (is.na(xml_node) || is_matched(match_style(style$until, xml_node, ..., must_match = FALSE)))
      break
    if (n > 1L)
      xml_node <- match_style(style$sep, xml_node, ..., must_match = must_match)
    next_xml_node <- match_style(style$x, xml_node, ..., must_match = must_match)
    if (is_unmatched(next_xml_node)) no_match(style, xml_node, ..., must_match = must_match)
    xml_node <- next_xml_node
    n <- n + 1L
  }
  xml_node
}



#' @export
match_style.zero_or_one <- function(style, xml_node, ..., must_match = TRUE) {
  after_one_node <- match_style(style$x, xml_node, ..., must_match = NA)
  if (is_matched(after_one_node)) after_one_node
  else xml_node
}
