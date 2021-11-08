test_that("match_style.indent: styling matching on block indentation", {
  # for testing indentation, need to use surface level generic, as NextMethod is
  # used to intercept indent styles and aggregate indentation state

  style <- indent(expr("num_const"), newline = 0L)
  expect_no_lint(match_style(style, first_parsed_node("1L")))

  style <- indent(expr("num_const"), newline = 0L)
  expect_lint(match_style(style, first_parsed_node(" 1L")))

  style <- indent(expr("num_const"), newline = 2L)
  expect_no_lint(match_style(style, first_parsed_node("\n  1L\n  2L")))

  style <- expr("{", indent(expr("num_const"), expr("num_const"), newline = 2L), "}")
  expect_no_lint(match_style(style, first_parsed_node("{\n  1L\n  2L\n}")))

  style <- expr("{", indent(expr("num_const"), expr("num_const"), newline = 2L), "}")
  expect_lint(match_style(style, first_parsed_node("{\n  1L\n   2L\n}")))

  style <- expr("{", indent(expr("num_const"), expr("num_const"), newline = 2L), "}")
  expect_lint(match_style(style, first_parsed_node("{\n   1L\n   2L\n}")), n = 2L)

  style <- expr("{", indent(expr("num_const"), indent(expr("num_const"), newline = 2L), newline = 2L), "}")
  expect_no_lint(match_style(style, first_parsed_node("{\n  1L\n    2L\n}")))

  # TODO:
  #  - test a failed indentation line, followed by a successful indentation line
})
