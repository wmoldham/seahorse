# test-initialize.R

real_path <- system.file("extdata/test_1.xlsx", package = "seahorse", mustWork = TRUE)
fake_path <- "test.xlsx"

test_that("valid object from file only", {
  expect_s4_class(Seahorse(real_path), "Seahorse")
})

test_that("path works", {
  expect_error(Seahorse(fake_path))
})
