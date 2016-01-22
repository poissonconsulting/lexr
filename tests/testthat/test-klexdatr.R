context("klexdatr")

test_that("klexdatr", {
   require(klexdatr)
   lex <- input_lex_data("klexdatr")
   expect_is(check_lex_data(lex), "lex_data")
})
