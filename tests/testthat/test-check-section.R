context("check-section")

test_that("section passes", {
  expect_true(check_section(qlexdatr::section))
})

test_that("informative error messages", {
  section <- qlexdatr::section
  section$SectionY <- NULL
  expect_error(check_section(section), "section must include columns Section, SectionArea, SectionX and SectionY")

  section <- qlexdatr::section
  is.na(section$SectionX) <- TRUE
  expect_error(check_section(section), "section cannot include missing values in Section, SectionArea, SectionX or SectionY")
})
