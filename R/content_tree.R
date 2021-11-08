#' @export
content_tree <- function(x, ...) {
  if (length(x) == 1L && file.exists(x)) {
    path <- x
    content <- readLines(path)
    content_xml <- lintr::get_source_expressions(path, ...)
    content_xml <- tail(content_xml$expressions, 1L)[[1L]]$full_xml_parsed_content
  } else if (is.character(x)) {
    content <- strsplit(x, "\n")[[1L]]
    parsed_content <- parse(text = x, keep.source = TRUE)
    content_xml <- lintr:::safe_parse_to_xml(parsed_content)
    path <- attr(parsed_content, "srcfile")$filename
  }

  # modify lintr (native) xml structure instead doing some post-processing to
  # make some parenthesized keyword expressions more consistent
  content_xml <- xml_restructure_function_headers(content_xml)

  structure(content_xml,
    origin = list(content = content, source_path = path),
    class = c("content_tree", class(content_xml))
  )
}

content_tree_extract_xml <- function(ct) {
  xml2::xml_find_all(ct, "/exprlist/*")
}

content_tree_extract_origin <- function(ct) {
  attr(ct, "origin")
}

#' @export
print.content_tree <- function(x, depth = NULL, spaces = 2L) {
  pipe <- "\u2502"
  tee  <- "\u251C"
  elb  <- "\u2570"

  path <- list()
  while (!is.na(x)) {
    if (!is.null(depth) && length(path) >= depth)
      return(invisible())

    sib <- xml2::xml_find_first(x, "following-sibling::*")
    paths <- vapply(tail(path, -1L), is.na, logical(1L))

    cat(
      paste0(sprintf("%s%s", ifelse(paths, " ", pipe), strrep(" ", spaces - 1L)), collapse = ""),
      if (length(path)) paste0(if (is.na(sib)) elb else tee, " "),
      format_content_tree_xml_node(x),
      "  ",
      "\n",
      sep = ""
    )

    if (xml2::xml_length(x)) {
      path <- append(path, list(if (is.na(sib)) NA else x))
      x <- xml2::xml_child(x)
    } else {
      x <- sib
      if (is.na(x)) {
        i <- Position(Negate(is.na), path, right = TRUE)
        if (is.na(i)) break
        x <- xml2::xml_find_first(path[[i]], "following-sibling::*")
        path <- path[-(i:length(path))]
      }
    }
  }
}

format_content_tree_xml_node <- function(x, .n = 1L, .spaces = 2L) {
  attrs <- xml2::xml_attrs(x)
  if (all(c("line1", "col1", "line2", "col2") %in% names(attrs))) {
    sprintf("%s [%s:%s/%s:%s]", token(x), attrs["line1"], attrs["col1"],
      attrs["line2"], attrs["col2"])
  } else {
    token(x)
  }
}
