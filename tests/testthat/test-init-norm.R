# init-norm.R

test_that("norm formatted correctly", {
  expect_error(Seahorse(path, norm = list(well = "A01")), "'value")
  expect_error(Seahorse(path, norm = list(value = 1)), "'well'")
  expect_error(Seahorse(path, norm = list(well = "A1", value = 1)), "'A01'")
})

test_that("unit works", {
  expect_s4_class(Seahorse(path, unit = "cell"), "Seahorse")
})
