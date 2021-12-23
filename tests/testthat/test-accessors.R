test_that("bf works", {
  expect_error(bf(b) <- -1, "positive")
  expect_equal(bf(`bf<-`(b, NA_real_)), NA_real_)
  expect_equal(bf(`bf<-`(b, 1)), 1)
})

test_that("blanks works", {
  x <- c("A01", "B04", "C03", "D06")
  y <- blanks(b)
  expect_equal(blanks(b), list(OCR = x, ECAR = x))
  expect_warning(blanks(b) <- list(OCR = x, ECAR = x), "unchanged")
  expect_error(blanks(b) <- list(), "Format new blanks")
  expect_error(blanks(b) <- list(a = x, b = x), "Format new blanks")
  expect_error(blanks(b) <- list(OCR = "A1", ECAR = x), regexp = "pattern 'A01'")
  blanks(b) <- list(OCR = NA_character_, ECAR = NA_character_)
  expect_equal(a@env$OCR, b@env$OCR)
  expect_equal(a@env$ECAR, b@env$ECAR)
  blanks(b) <- y
})

test_that("cells works", {
  expect_equal(cells(b), tibble::as_tibble(good_cells))
  expect_error(cells(b) <- list(well = "A01"), "'value")
  expect_error(cells(b) <- list(value = 1), "'well'")
  expect_error(cells(b) <- list(well = "A1", value = 1), "'A01'")
})

test_that("cf works", {
  expect_error(cf(b) <- -1, "positive")
  expect_equal(cf(`cf<-`(b, NA_real_)), NA_real_)
  expect_equal(cf(`cf<-`(b, 1)), 1)
})

test_that("levels works", {
  expect_s3_class(levels(b), "tbl")
  expect_true(all(c("well", "type", "group", "measurement", "stage", "time", "sensor", "value") %in% names(levels(b))))
})

test_that("rates works", {
  expect_s3_class(rates(b), "tbl")
  expect_true(all(c("well", "type", "group", "measurement", "stage", "rate", "value") %in% names(rates(b))))
  x <- dplyr::filter(rates(`bf<-`(b, NA_real_)), rate == "PER")
  expect_equal(nrow(x), 0)
})

test_that("stages works", {
  expect_equal(stages(b), tibble::as_tibble(good_stages))
  expect_error(stages(b) <- list(stage = rep("basal", 12)), "measurement")
  expect_error(stages(b) <- list(measurement = 1:11, stage = rep("basal", 11)), "entry for each measurement")
  expect_error(stages(b) <- list(measurement = 1:12, stage = rep("basal", 12), well = rep("A01", 12)), "entry for each well")
})

test_that("units works", {
  expect_equal(units(`units<-`(b, "cells")), "cells")
})

test_that("wells works", {
  expect_equal(wells(b), init_wells(good_wells))
  expect_error(wells(b) <- list(well = "A01"), class = "error_bad_format")
  expect_error(wells(b) <- list(type = "sample"), class = "error_bad_format")
  expect_error(wells(b) <- list(well = "1", type = "sample"), regexp = "pattern")
  expect_error(wells(b) <- list(well = "A01", type = "sample"), regexp = "one row")
  x <- stringr::str_c(rep(LETTERS[1:4], each = 6), sprintf("%02d", 1:6))
  expect_error(wells(b) <- list(well = c(x, "A01"), type = "blank"), regexp = "Duplicate")
  expect_error(wells(b) <- list(well = x, type = "background"), class = "error_bad_format")
  expect_true("group" %in% names(wells(b)))
})
