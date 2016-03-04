context("growth")

test_that("growth_no", {
  expect_identical(growth_no(0, -1), 0)
  expect_identical(growth_no(2000, -1), 2000)
})

test_that("growth_vb", {
  expect_identical(growth_vb(500, 0), 500)
  expect_identical(growth_vb(0, -1), 0)
  expect_equal(growth_vb(growth_vb(500, 1.33),-1.33), 500)
  expect_identical(growth_vb(2000, 1), 2000)
})
