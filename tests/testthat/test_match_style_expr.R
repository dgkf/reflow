test_that("match_style.expr: styling matching on expressions", {
  style <- expr("{", "}")
  expect_no_lint(match_style.expr(style, first_parsed_node("{ }")))

  style <- expr("{", expr(expr("num_const"), "op_plus", expr("num_const")), "}")
  expect_no_lint(match_style.expr(style, first_parsed_node("{ 1 + 2 }")))

  style <- expr("{", expr("num_const"), "}")
  expect_lint(match_style.expr(style, first_parsed_node("{ 1 + 2 }")))
})
