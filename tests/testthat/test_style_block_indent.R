test_that("block indentation: identifies malformed indents", {
  block_indent_rule <- rules(
    "code block" = rule(
      msg = "multiline code block bodies should be indented.",
      "{",
        indent(newline = 2L, zero_or_more(any_token(), until = "}")),
      "}"
    )
  )


  expect_no_lint(match_style(
    block_indent_rule,
    strip_lpad_ws("
    {
      1 + 2
    }
  ")))

  expect_lint(match_style(
    block_indent_rule,
    strip_lpad_ws("
    {
       1 + 2
    }
  ")))

  style <- expr("{", indent(zero_or_more(any_token(), until = "}")), "}")
  with_no_match_interrupt(match_style(
    style,
    strip_lpad_ws("
    {
       1 + 2
    }
  ")))

})
