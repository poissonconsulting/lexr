context("lexr")

test_that("lexr", {
  require(klexdatr)
  require(dplyr)
  require(lubridate)

  klex <- input_lex_data("klexdatr")

  capture <- filter(klex$capture, year(DateTimeCapture) == 2008)
  start_date <- as.Date("2008-01-01")
  end_date <- as.Date("2008-12-31")
  hourly_interval <- 6L
  detect <- make_detect_data(klex, capture, start_date, end_date, hourly_interval)
  hourly_interval <- 24L
  detect <- make_detect_data(klex, capture, start_date, end_date, hourly_interval)
})
