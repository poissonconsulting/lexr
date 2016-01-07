context("input-lex-data")

test_that("returns lex_data object", {
  expect_is(input_lex_data(), "lex_data")
})
