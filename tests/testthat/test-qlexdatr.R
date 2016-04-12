context("qlexdatr")

test_that("qlexdatr", {
  require(qlexdatr)
  require(dplyr)
  require(lubridate)

  lex <- input_lex_data("qlexdatr")
  expect_is(check_lex_data(lex), "lex_data")

  capture <- filter(lex$capture, year(DateTimeCapture) == 2014)
  recapture <- lex$recapture
  recapture$Released <- !recapture$Public
  start_date <- as.Date("2014-01-01")
  end_date <- as.Date("2014-12-31")
  hourly_interval <- 6L # klexdatr tests 24L
  lex <- filter_lex_data(lex, capture = capture, recapture = recapture)

  detect <- make_detect_data(lex, capture = capture, recapture = recapture, start_date = start_date, end_date = end_date, hourly_interval = hourly_interval)

  expect_identical(get_difftime(detect), lubridate::make_difftime(num = 60 * 60 * hourly_interval, units = "hours"))
  expect_is(check_detect_data(detect), "detect_data")
  analysis <- make_analysis_data(detect, interval_period = lubridate::make_difftime(60 * 60 * 24 * 7))
  expect_is(check_analysis_data(analysis), "analysis_data")
  data <- as.data.frame(analysis)
  expect_is(data, "data.frame")
})
