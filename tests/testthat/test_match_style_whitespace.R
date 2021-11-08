test_that("match_style.whitespace: styling matching on whitespace", {
  style <- whitespace(spaces = 2L, newlines = 1L)
  expect_no_lint(match_style.whitespace(style, first_parsed_node("  1L")))

  style <- whitespace(spaces = 1L, newlines = 1L)
  expect_lint(match_style.whitespace(style, first_parsed_node("  1L")))

  style <- whitespace(spaces = 2L, newlines = 2L)
  expect_no_lint(match_style.whitespace(style, first_parsed_node("  \n  1L")))

  style <- whitespace(spaces = 2L, newlines = 2L)
  expect_lint(match_style.whitespace(style, first_parsed_node("  1L")))
})
