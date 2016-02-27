context("klexdatr")

test_that("klexdatr", {
  require(klexdatr)
  require(dplyr)
  require(lubridate)

  lex <- input_lex_data("klexdatr")
  expect_is(check_lex_data(lex), "lex_data")

  capture <- filter(lex$capture, year(DateTimeCapture) == 2008)
  start_date <- as.Date("2008-01-01")
  end_date <- as.Date("2008-12-31")
  hourly_interval <- 24L # qlexdatr tests 6L
  detect <- make_detect_data(lex, capture, start_date, end_date, hourly_interval)
  expect_is(check_detect_data(detect), "detect_data")
  expect_identical(get_difftime(detect), make_difftime(num = 60 * 60 * hourly_interval, units = "hours"))
  expect_is(check_detect_data(detect), "detect_data")
  analysis <- make_analysis_data(detect, make_difftime(60 * 60 * 24 * 7 * 4))
  expect_is(check_analysis_data(analysis), "analysis_data")
})
