no_match <- function(style, xml_node, ..., rule = NULL, code = NULL,
  debug = getOption("reflow.debug", FALSE)) {

  msg <- rule$msg %||% paste0("expected: ", format(style))
  cond <- simpleCondition(msg)
  class(cond) <- c(
    "interruptingUnmatchedRuleCondition",
    "unmatchedRuleCondition",
    class(cond)
  )

  cond$style <- style
  cond$xml_node <- xml_node
  cond$rule <- rule
  cond$code <- code

  if (isTRUE(debug)) browser()
  else if (is.function(debug)) debug()

  withRestarts(signalCondition(cond),
    continueUnmatchedRuleCondition = function(cond) {
      class(cond) <- c(
        "uninterruptingUnmatchedRuleCondition",
        setdiff(class(cond), "interruptingUnmatchedRuleCondition")
      )
      signalCondition(cond)
    })

  invisible(xml_node)
}

unmatchedRuleConditionHandler <- function(cond, quiet = getOption("reflow.quiet", FALSE)) {
  if (!isTRUE(quiet)) {
    message(paste0(collapse = "\n", format(cond)))
  }
}

unmatched <- function(x) {
  class(x) <- c("unmatched_style", class(x))
  x
}

format.unmatchedRuleCondition <- function(cond, verbose = TRUE) {
  if (verbose) format_unmatched_verbose(cond)
  else format_unmatched_condensed(cond)
}

format_unmatched_verbose <- function(cond) {
  if (is.null(cond$code)) return(format_unmatched_safe(cond))

  line1 <- as.numeric(xml2::xml_attr(cond$xml_node, "line1"))
  col1 <- as.numeric(xml2::xml_attr(cond$xml_node, "col1"))
  src <- basename(cond$code$source_path %||% "unknown")

  c(
    # "Style failed at: \n\n",
    sprintf("<%s %d:%d>", src, line1, col1),
    sprintf(sprintf("%%%ds\u2595 %%s", linechars <- nchar(line1)),
      line_nums <- seq(from = max(1L, line1 - 2L), to = line1),
      cond$code$content[line_nums]),
    paste0(strrep(" ", linechars + col1 + 1L), "^"),
    if (!is.null(cond$rule)) cond$rule$msg
    else paste0("expected: ", format(cond$style)),
    ""
  )
}

format_unmatched_safe <- function(cond) {
  line1 <- as.numeric(xml2::xml_attr(cond$xml_node, "line1"))
  col1 <- as.numeric(xml2::xml_attr(cond$xml_node, "col1"))
  src <- basename(cond$code$source_path %||% "unknown")
  msg <- if (!is.null(cond$rule)) {
    cond$rule$msg
  } else {
    fmt <- format(cond$style)
    if (length(fmt) > 1L) fmt <- paste0(collapse = "\n", "  ", c("", fmt))
    paste0("expected: ", paste0(collapse = "\n", fmt))
  }

  sprintf("<%s %d:%d> %s", src, line1, col1, msg)
}

format_unmatched_condensed <- function(cond) {
  line1 <- as.numeric(xml2::xml_attr(cond$xml_node, "line1"))
  col1 <- as.numeric(xml2::xml_attr(cond$xml_node, "col1"))
  src <- basename(cond$code$source_path)
  msg <- if (!is.null(cond$rule)) cond$rule$msg
    else paste0("expected: ", format(cond$style))

  code_chrs <- 20L
  line <- cond$code$content[line1]
  line <- substring(line, m <- max(col1 - code_chrs %/% 2, 1), m + code_chrs)
  line <- paste0(line, strrep(" ", code_chrs - nchar(line)))

  sprintf("%s:%d:%d| %s # %s", src, line1, col1, line, msg)
}

is_unmatched <- function(x) {
  inherits(x, "unmatched_style")  # rad
}

is_matched <- function(x) {
  !is_unmatched(x) && !is.null(x)
}
