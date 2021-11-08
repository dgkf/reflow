aggregate_unmatched_rule_conds <- function(expr, envir = parent.frame()) {
  conds <- list()
  withRestarts(withCallingHandlers(
    eval(expr, envir = envir),
    uninterruptingUnmatchedRuleCondition = function(cond) {
      conds <<- append(conds, list(cond))
    },
    interruptingUnmatchedRuleCondition = function(cond) {
      invokeRestart("unmatchedRuleRestart", cond)
    }),
    unmatchedRuleRestart = function(cond) {
      conds <<- append(conds, list(cond))
    })
  conds
}

first_parsed_node <- function(str) {
  content_tree_extract_xml(content_tree(str))[[1L]]
}

strip_lpad_ws <- function(str) {
  str <- strsplit(gsub("^\n+|\n+\\s*$", "", str, perl = TRUE), "\n")[[1L]]
  nspaces <- min(nchar(gsub("(^\\s*).*$", "\\1", str)))
  paste0(collapse = "\n", substring(str, nspaces + 1L))
}

expect_lint <- function(expr, re = "", ..., n = 1L, perl = TRUE) {
  expr <- substitute(expr)
  input <- list(
    val = NULL,
    lab = deparse(expr),
    conds = aggregate_unmatched_rule_conds(expr, envir = parent.frame())
  )

  testthat::expect(
    length(input$conds) >= 1L &&
    (N <- sum(grepl(re, lapply(input$conds, conditionMessage), ..., perl = perl))) == n,
    if (length(input$conds) < 1L) {
      sprintf("%s did not produce an unmatchedRuleCondition.", input$lab)
    } else if (N == 0L) {
      sprintf("%s did not match expected unmatchedRuleConditions, produced:\n%s",
        input$lab,
        conditionMessage(input$conds[[1L]]))
    } else if (N != n) {
      sprintf("%s produced %d matching conditions. %d expected.",
        input$lab, N, n)
    }
  )

  invisible(input$conds)
}

expect_no_lint <- function(expr) {
  expr <- substitute(expr)
  input <- list(
    val = NULL,
    lab = deparse(expr),
    conds = aggregate_unmatched_rule_conds(expr, envir = parent.frame())
  )

  testthat::expect(
    length(input$conds) == 0L,
    if (length(input$conds) > 1L) {
      sprintf("%s produced multiple unmatchedRuleConditions.", input$lab)
    } else if (length(input$conds) == 1L) {
      sprintf("%s produced unmatchedRuleCondition:\n%s",
        input$lab,
        conditionMessage(input$conds[[1L]]))
    } else {
      "Expectation unreachable"
    }
  )

  invisible(input$conds)
}
