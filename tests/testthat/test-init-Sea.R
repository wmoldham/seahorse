# test-initialize.R

test_that("valid object from file only", {
  expect_s4_class(Seahorse(path), "Seahorse")
})

test_that("path works", {
  expect_error(Seahorse("test.xlsx"))
})

test_that("ccf and bf formatted correctly", {
  expect_error(Seahorse(path, bf = -1), "positive")
  expect_error(Seahorse(path, cf = -1), "positive")
})


