xml_whitespace_before <- function(xml_node) {
  prev_node <- xml_previous(xml_node)
  prev_attr <- xml2::xml_attrs(prev_node)
  mode(prev_attr) <- "numeric"
  node_attr <- xml2::xml_attrs(xml_node)
  mode(node_attr) <- "numeric"
  n_newlines <- node_attr["line1"] - prev_attr["line2"]
  n_spaces  <- if (n_newlines) {
    node_attr["col1"] - 1L
  } else {
    node_attr["col1"] - prev_attr["col2"] - 1L
  }
  whitespace(spaces = n_spaces, newlines = n_newlines)
}

xml_previous <- function(x) {
  xml_prev <- xml2::xml_find_first(x, "preceding-sibling::*[1]")
  if (is.na(xml_prev)) xml2::xml_parent(x)
  else xml_prev
}

xml_next_sibling <- function(x) {
  xml2::xml_find_first(x, "following-sibling::*")
}
