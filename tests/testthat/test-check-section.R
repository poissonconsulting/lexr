context("check-section")

test_that("section passes", {
  expect_true(check_section(qlexdatr::section))
})

test_that("informative error messages", {

  expect_error(check_section(1), "section is not a data frame")

  section <- qlexdatr::section
  section <- section[FALSE,]
  expect_error(check_section(data.frame()), "section must contain at least one row of data")

  section <- qlexdatr::section
  section$SectionY <- NULL
  expect_error(check_section(section), "section must include columns Section, SectionArea, SectionX and SectionY")

  section <- qlexdatr::section
  section$SectionArea %<>% round() %>% as.integer()
  expect_error(check_section(section), "section columns Section, SectionArea, SectionX and SectionY must have modes integer, double, double and double, respectively")

  section <- qlexdatr::section
  is.na(section$SectionX) <- TRUE
  expect_error(check_section(section), "section cannot include missing values in Section, SectionArea, SectionX or SectionY")

  section <- qlexdatr::section
  section$SectionArea <- 0
  expect_error(check_section(section), "section areas must be positive numbers")
})
