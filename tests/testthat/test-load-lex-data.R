context("check-lex-data")

test_that("with qlexdatr", {
  data <- load_lex_data("qlexdatr")
  expect_is(check_lex_data(data), "list")
})
