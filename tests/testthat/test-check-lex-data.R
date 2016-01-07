context("check-lex-data")

test_that("qlexdatr passes checks", {
  data <- input_lex_data("qlexdatr")
  expect_identical(check_lex_data(data), data)
})

test_that("check deployment", {
  data <- input_lex_data("qlexdatr")
  data$deployment$ReceiverDateTimeOut[1] <- data$deployment$ReceiverDateTimeIn[1] - 1
  expect_error(check_lex_data(data), "receiver retrieved before deployed")
})
