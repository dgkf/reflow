no_match <- function(style, xml_node, ..., rule = NULL, code = NULL, must_match = TRUE) {
  if (isTRUE(must_match) && (isTRUE(f <- getOption("flow.debug", FALSE)) || is.function(f))) {
    if (!getOption("flow.quiet", TRUE)) {
      line1 <- as.numeric(xml2::xml_attr(xml_node, "line1"))
      col1 <- as.numeric(xml2::xml_attr(xml_node, "col1"))
      src <- basename(code$source_path)

      message(
        # "Style failed at: \n\n",
        sprintf("<%s %d:%d>\n", src, line1, col1),
        paste(collapse = "\n",
          sprintf(sprintf("%%%ds\u2595 %%s", linechars <- nchar(line1)),
            line_nums <- seq(from = max(1L, line1 - 2L), to = line1),
            code$content[line_nums])),
        "\n",
        paste0(strrep(" ", linechars + col1 + 1L), "^"),
        "\n",
        if (!is.null(rule)) rule$msg
        else paste0("expected: ", format(style)),
        "\n")
    }
    if (is.function(f)) f()
  }
  unmatched(xml_node)
}

unmatched <- function(x) {
  class(x) <- c("unmatched_style", class(x))
  x
}

is_unmatched <- function(x) {
  inherits(x, "unmatched_style")  # rad
}

is_matched <- function(x) {
  !is_unmatched(x) && !is.null(x)
}
