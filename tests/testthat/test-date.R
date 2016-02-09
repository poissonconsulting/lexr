context("date")

test_that("date", {
  time <- as.POSIXct("2000-01-01 00:00:01")
  expect_identical(lexr::date(time), as.Date("2000-01-01"))
  times <- as.POSIXct(c("2000-01-01 00:00:01", "2000-02-01 00:00:01"))
  expect_identical(lexr::date(times), as.Date(c("2000-01-01", "2000-02-01")))
})
