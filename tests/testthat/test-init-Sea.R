# test-initialize.R

test_that("valid object from file only", {
  expect_s4_class(Seahorse(path), "Seahorse")
})

test_that("path works", {
  expect_error(Seahorse("test.xlsx"))
})


