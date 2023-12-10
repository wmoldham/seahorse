#test-analysis.R

test_that("levels works", {
  expect_snapshot(levels(sea, blanks = TRUE, outliers = TRUE))
  expect_snapshot(levels(sea, blanks = TRUE, outliers = FALSE))
  expect_snapshot(levels(sea, blanks = FALSE, outliers = FALSE))
})
