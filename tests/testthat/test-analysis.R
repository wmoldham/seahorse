#test-analysis.R

test_that("levels works", {
  expect_snapshot(levels(sea, blanks = TRUE, outliers = TRUE))
  expect_snapshot(levels(sea, blanks = TRUE, outliers = FALSE))
  expect_snapshot(levels(sea, blanks = FALSE, outliers = FALSE))
})

test_that("rates works", {
  expect_snapshot(rates(sea, blanks = TRUE, outliers = TRUE, normalize = TRUE))
  expect_snapshot(rates(sea, blanks = FALSE, outliers = TRUE, normalize = TRUE))
  expect_snapshot(rates(sea, blanks = TRUE, outliers = FALSE, normalize = TRUE))
  expect_snapshot(rates(sea, blanks = TRUE, outliers = TRUE, normalize = FALSE))
})
