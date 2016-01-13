context("input-lex-data")

test_that("returns lex_data object", {
  expect_is(input_lex_data(), "lex_data")
  expect_identical(names(input_lex_data()), c("section", "station", "receiver", "deployment", "capture",
       "recapture", "detection", "depth"))
})
