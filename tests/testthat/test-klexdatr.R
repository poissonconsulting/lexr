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
  detect <- make_detect_data(lex, capture = capture, start_date = start_date,
                             end_date = end_date, recovery_days = 30L)
  expect_is(check_detect_data(detect), "detect_data")
  expect_identical(get_difftime(detect), lubridate::make_difftime(num = 60 * 60 * 24, units = "days"))
  expect_is(check_detect_data(detect), "detect_data")
  analysis <- make_analysis_data(detect, interval_period = lubridate::make_difftime(60 * 60 * 24 * 7 * 4), growth = growth_vb, k = 0.234)
  expect_is(check_analysis_data(analysis), "analysis_data")
  data <- as.data.frame(analysis)
  expect_is(data, "data.frame")
})
