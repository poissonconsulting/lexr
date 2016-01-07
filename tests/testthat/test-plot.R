context("check-plot")

test_that("qlexdatr plots", {
  data <- input_lex_data("qlexdatr")
  expect_null(plot(data))
})
