context("check-lex-data")

test_that("qlexdatr passes checks", {
  data <- input_lex_data("qlexdatr")
  expect_is(check_lex_data(data), "lex_data")

  data$station$Station <- as.numeric(data$station$Station)
  expect_error(check_lex_data(data), "column Station in station must be of class 'factor'")
})

test_that("check deployment", {
  data <- input_lex_data("qlexdatr")
#  data$deployment$ReceiverDateTimeOut[1] <- data$deployment$ReceiverDateTimeIn[1] - 1
#  expect_error(check_lex_data(data), "receiver retrieved before deployed")
})
