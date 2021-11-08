test_that("match_style.list: styling matching on a series of style patterns", {
  style <- list(expr("num_const"), ";", expr("str_const"), ";", expr("num_const"))
  expect_no_lint(match_style.list(style, first_parsed_node("1L; 'a'; TRUE")))

  style <- list(expr("num_const"), ";", expr("str_const"))
  expect_lint(match_style.list(style, first_parsed_node("'a'; TRUE")))
})
