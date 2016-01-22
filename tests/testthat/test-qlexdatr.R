context("qlexdatr")

test_that("qlexdatr", {
   require(qlexdatr)
   lex <- input_lex_data("qlexdatr")
   expect_is(check_lex_data(lex), "lex_data")
})
