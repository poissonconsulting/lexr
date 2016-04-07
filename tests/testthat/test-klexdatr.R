context("klexdatr")

test_that("klexdatr", {
  require(klexdatr)
  require(dplyr)
  require(lubridate)

  lex <- input_lex_data("klexdatr")
  expect_is(check_lex_data(lex), "lex_data")
  expect_true(is.lex_data(lex))

  capture <- filter(lex$capture, year(DateTimeCapture) == 2008)
  start_date <- as.Date("2008-01-01")
  end_date <- as.Date("2008-12-31")
  sections <- list(
    "S02" = "S02", "S03" = "S03",
    "S04" = "S04", "S01" = c("S01", "S05", "S06"), "S07" = "S07",
                   "S08" = "S08",
                   "S09" = "S09",
                   "S10" = "S10",
                   "S11" = "S11",
                   "S12" = "S12",
                   "S13" = "S13",
                   "S14" = "S14",
                   "S15" = "S15",
                   "S16" = "S16",
                   "S17" = "S17",
                   "S18" = "S18",
                   "S23" = "S23",
                   "S24" = "S24",
                   "S21" = "S21",
                   "S22" = "S22",
                   "S25" = "S25",
                   "S26" = "S26",
                   "S27" = "S27",
                   "S28" = "S28",
                   "S29" = "S29",
                   "S30" = "S30",
                   "S31" = "S31",
                   "S32" = "S32",
                   "S33" = "S33")
  lex <- filter_lex_data(lex, capture = capture, sections = sections)
  detect <- make_detect_data(lex, start_date = start_date,
                             end_date = end_date, recovery_days = 30L)
  expect_is(check_detect_data(detect), "detect_data")
  expect_identical(get_difftime(detect), lubridate::make_difftime(num = 60 * 60 * 24, units = "days"))
  expect_is(check_detect_data(detect), "detect_data")
  expect_true(is.detect_data(detect))
  analysis <- make_analysis_data(detect, interval_period = lubridate::make_difftime(60 * 60 * 24 * 7 * 4), growth = growth_vb, k = 0.234)
  expect_is(check_analysis_data(analysis), "analysis_data")
  expect_true(is.analysis_data(analysis))
  data <- as.data.frame(analysis)
  expect_is(data, "data.frame")
})
