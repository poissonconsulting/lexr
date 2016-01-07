context("check-lex-data")

test_that("qlexdatr passes checks", {
  data <- input_lex_data("qlexdatr")
  expect_identical(check_lex_data(data), data)
})
