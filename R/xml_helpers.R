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

xml_first_node_on_earlier_line <- function(x) {
  line <- as.numeric(xml2::xml_attr(x, "line1"))
  xpath <- sprintf("(//*[@line2 < %d])[last()]", line)
  last_prev_line_node <- xml2::xml_find_first(x, xpath)
  prev_line <- as.numeric(xml2::xml_attr(last_prev_line_node, "line2"))
  xpath <- sprintf("//*[@line1 = %d]", prev_line)
  xml2::xml_find_first(x, xpath)
}

xml_first_node_of_first_expr_of_prev_line <- function(x) {
  # find last node on a prior code line
  line <- as.numeric(xml2::xml_attr(x, "line1"))
  xpath <- sprintf("(//*[@line2 < %d])[last()]", line)
  last_prev_line_node <- xml2::xml_find_first(x, xpath)

  # find the start of the first expression on the previous code line
  prev_line <- as.numeric(xml2::xml_attr(last_prev_line_node, "line2"))
  xpath <- sprintf("//*[@line2 = %d]", prev_line)
  prev_block_start <- xml2::xml_find_first(x, xpath)

  # find the first node on the line where that expression begins
  prev_block_start_line <- as.numeric(xml2::xml_attr(prev_block_start, "line1"))
  xpath <- sprintf("//*[@line1 = %d]", prev_block_start_line)
  xml2::xml_find_first(x, xpath)
}

xml_line1 <- function(x) as.numeric(xml2::xml_attr(x, "line1"))
xml_col1 <- function(x) as.numeric(xml2::xml_attr(x, "col1"))
xml_line2 <- function(x) as.numeric(xml2::xml_attr(x, "line2"))
xml_col2 <- function(x) as.numeric(xml2::xml_attr(x, "col2"))

xml_restructure_function_headers <- function(x) {
  fns <- xml2::xml_find_all(x, "//FUNCTION")
  for (fn_node in fns) {
    par <- xml2::xml_parent(fn_node)
    header_node <- xml2::xml_add_child(par, .where = 1L, par)
    xml2::xml_remove(xml2::xml_child(header_node, 1L))
    xml2::xml_remove(xml2::xml_child(header_node, xml2::xml_length(header_node)))
    xml2::xml_remove(xml2::xml_children(par)[3:(xml2::xml_length(par)-1L)])
    header_node_first <- xml2::xml_child(header_node, 1L)
    header_node_last  <- xml2::xml_child(header_node, xml2::xml_length(header_node))
    xml2::xml_attr(header_node, "line1") <- xml2::xml_attr(header_node_first, "line1")
    xml2::xml_attr(header_node, "col1")  <- xml2::xml_attr(header_node_first, "col1")
    xml2::xml_attr(header_node, "line2") <- xml2::xml_attr(header_node_last,  "line2")
    xml2::xml_attr(header_node, "col2")  <- xml2::xml_attr(header_node_last,  "col2")
    xml2::xml_attr(header_node, "start") <- xml2::xml_attr(header_node_first, "start")
    xml2::xml_attr(header_node, "end")  <- xml2::xml_attr(header_node_first,  "end")
  }
  x
}
