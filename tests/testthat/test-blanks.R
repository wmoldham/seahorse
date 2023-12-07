# test-blanks.R

test_that("blanks accessor works", {
  expect_snapshot(blanks(sea))
})
