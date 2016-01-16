context("make-detect-data")

test_that("qlexdatr passes checks", {
  data <- input_lex_data("qlexdatr")
  expect_is(make_detect_data(data), "detect_data")
})
