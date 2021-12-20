# test-wells.R

test_that("init_wells works", {
  expect_error(init_wells(list()), regexp = "'well'", class = "error_bad_format")
  expect_error(init_wells(list(well = "A01")), regexp = "'type'", class = "error_bad_format")
  expect_error(init_wells(list(type = "background")), regexp = "'well'", class = "error_bad_format")
  expect_error(init_wells(list(well = "A01", type = "x")), regexp = "'type'", class = "error_bad_format")
  expect_s3_class(init_wells(list(well = "A01", type = "sample")), "tbl")
  expect_s3_class(init_wells(data.frame(well = "A01", type = "sample")), "tbl")
  expect_s3_class(init_wells(tibble::tibble(well = "A01", type = "sample")), "tbl")
  x <- init_wells(list(well = "A01", type = "sample", treat = "control", oxygen = "normoxia"))
  expect_true("group" %in% names(x))
  expect_true(x$group[[1]] == "control_normoxia")
  expect_s3_class(x$group, "factor")
})

test_that("wells formatted correctly", {
  expect_error(Seahorse(path, wells = list(well = "A01")), class = "error_bad_format")
  expect_error(Seahorse(path, wells = list(type = "sample")), class = "error_bad_format")
  expect_error(Seahorse(path, wells = list(well = "1", type = "sample")), regexp = "pattern")
  expect_error(Seahorse(path, wells = list(well = "A01", type = "sample")), regexp = "one row")
  x <- stringr::str_c(rep(LETTERS[1:4], each = 6), sprintf("%02d", 1:6))
  expect_error(Seahorse(path, wells = list(well = c(x, "A01"), type = "blank")), regexp = "Duplicate")
  expect_error(Seahorse(path, wells = list(well = x, type = "background")), class = "error_bad_format")
  # expect_identical(wells(x)[["type"]], rep("sample", length(wells(x)[["type"]])))
})
