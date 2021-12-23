test_that("bf works", {
  expect_error(bf(t2) <- -1, "positive")
  expect_equal(bf(`bf<-`(t2, NA_real_)), NA_real_)
  expect_equal(bf(`bf<-`(t2, 1)), 1)
})

test_that("blanks works", {
  x <- c("A01", "B04", "C03", "D06")
  y <- blanks(t2)
  expect_equal(blanks(t2), list(OCR = x, ECAR = x))
  expect_warning(blanks(t2) <- list(OCR = x, ECAR = x), "unchanged")
  expect_error(blanks(t2) <- list(), "Format new blanks")
  expect_error(blanks(t2) <- list(t1 = x, t2 = x), "Format new blanks")
  expect_error(blanks(t2) <- list(OCR = "A1", ECAR = x), regexp = "pattern 'A01'")
  blanks(t2) <- list(OCR = NA_character_, ECAR = NA_character_)
  expect_equal(t1@env$OCR, t2@env$OCR)
  expect_equal(t1@env$ECAR, t2@env$ECAR)
  blanks(t2) <- y
})

test_that("cells works", {
  expect_equal(cells(t2), tibble::as_tibble(good_cells))
  expect_error(cells(t2) <- list(well = "A01"), "'value")
  expect_error(cells(t2) <- list(value = 1), "'well'")
  expect_error(cells(t2) <- list(well = "A1", value = 1), "'A01'")
})

test_that("cf works", {
  expect_error(cf(t2) <- -1, "positive")
  expect_equal(cf(`cf<-`(t2, NA_real_)), NA_real_)
  expect_equal(cf(`cf<-`(t2, 1)), 1)
})

test_that("levels works", {
  expect_s3_class(levels(t2), "tbl")
  expect_true(all(c("well", "type", "group", "measurement", "stage", "time", "sensor", "value") %in% names(levels(t2))))
})

test_that("rates works", {
  expect_s3_class(rates(t2), "tbl")
  expect_true(all(c("well", "type", "group", "measurement", "stage", "rate", "value") %in% names(rates(t2))))
  x <- dplyr::filter(rates(`bf<-`(t2, NA_real_)), rate == "PER")
  expect_equal(nrow(x), 0)
})

test_that("stages works", {
  expect_equal(stages(t2), tibble::as_tibble(good_stages))
  expect_error(stages(t2) <- list(stage = rep("basal", 12)), "measurement")
  expect_error(stages(t2) <- list(measurement = 1:11, stage = rep("basal", 11)), "entry for each measurement")
  expect_error(stages(t2) <- list(measurement = 1:12, stage = rep("basal", 12), well = rep("A01", 12)), "entry for each well")
})

test_that("units works", {
  expect_equal(units(`units<-`(t2, "cells")), "cells")
})

test_that("wells works", {
  expect_equal(wells(t2), init_wells(good_wells))
  expect_error(wells(t2) <- list(well = "A01"), class = "error_bad_format")
  expect_error(wells(t2) <- list(type = "sample"), class = "error_bad_format")
  expect_error(wells(t2) <- list(well = "1", type = "sample"), regexp = "pattern")
  expect_error(wells(t2) <- list(well = "A01", type = "sample"), regexp = "one row")
  x <- stringr::str_c(rep(LETTERS[1:4], each = 6), sprintf("%02d", 1:6))
  expect_error(wells(t2) <- list(well = c(x, "A01"), type = "blank"), regexp = "Duplicate")
  expect_error(wells(t2) <- list(well = x, type = "background"), class = "error_bad_format")
  expect_true("group" %in% names(wells(t2)))
})
