xml_whitespace_before <- function(xml_node) {
  node_attr <- xml2::xml_attrs(xml_node)
  mode(node_attr) <- "numeric"
  prev_node <- xml_preceding_leaf(xml_node)

  # no tokens before current token
  if (is.na(prev_node)) {
    return(whitespace(
      spaces = node_attr["col1"] - 1L,
      newlines = node_attr["line1"]
    ))
  }

  prev_attr <- xml2::xml_attrs(prev_node)
  mode(prev_attr) <- "numeric"
  n_newlines <- node_attr["line1"] - prev_attr["line2"]
  n_spaces  <- if (n_newlines) {
    node_attr["col1"] - 1L
  } else {
    node_attr["col1"] - prev_attr["col2"] - 1L
  }

  whitespace(spaces = n_spaces, newlines = n_newlines)
}

xml_preceding_leaf <- function(x) {
  if (is.na(x)) return(x)
  node_attr <- xml2::xml_attrs(x)
  mode(node_attr) <- "numeric"
  # of all leafs that end before our start, take the last
  xpath <- sprintf("(//*[not(*) and @end < %d])[last()]", node_attr["start"])
  xml2::xml_find_first(x, xpath)
}

xml_next_leaf <- function(x) {
  if (is.na(x)) return(x)
  node_attr <- xml2::xml_attrs(x)
  mode(node_attr) <- "numeric"
  # of all leafs that end before our start, take the last
  xpath <- sprintf("(//*[not(*) and @end < %d])[last()]", node_attr["start"])
  xml2::xml_find_first(x, xpath)
}

xml_preceding_sibling <- function(x) {
  xml2::xml_find_first(x, "preceding-sibling::*[1]")
}

xml_next_sibling <- function(x) {
  xml2::xml_find_first(x, "following-sibling::*")
}
