test_that("match_style.zero_or_more: styling matching on zero or more repeated patterns", {
  style <- zero_or_more(expr("num_const"), ";")
  expect_no_lint(match_style.zero_or_more(style, first_parsed_node("1L; 1L; 1L;")))

  style <- zero_or_more(expr("num_const"), ";")
  expect_lint(match_style.zero_or_more(style, first_parsed_node("1L; 1L; 1L + 2L;")))
})
