# test-blanks.R

test_that("blanks accessor works", {
  expect_snapshot(blanks(sea))
})

test_that("blanks assignment format", {
  expect_warning(
    `blanks<-`(sea, "remove", value = 1),
    "Replacement values provided but ignored"
  )
  expect_error(
    `blanks<-`(sea, "add", value = 1),
    "'value' must be a list or data.frame"
  )
  expect_error(
    `blanks<-`(sea, "add", value = data.frame(a = 1, b = 1)),
    "Blanks data.frames must contain 'rate' and 'well' columns"
  )
  expect_error(
    `blanks<-`(sea, "add", value = data.frame(rate = 1, well = 1, c = 1)),
    "Blanks data.frames must contain 'rate' and 'well' columns"
  )
  expect_error(
    `blanks<-`(sea, "add", value = data.frame(rate = "x", well = 1)),
    "Blanks data.frame rate column must contain only 'OCR' or 'ECAR'"
  )
  expect_error(
    `blanks<-`(sea, "add", value = data.frame(rate = "OCR", well = 1)),
    "Blanks values must match the pattern 'A01'"
  )
  expect_error(
    `blanks<-`(sea, "add", value = list(a = 1, b = 1)),
    "Blanks list vectors must be named 'OCR' or 'ECAR'"
  )
  expect_error(
    `blanks<-`(sea, "add", value = list(OCR = 1)),
    "Blanks values must match the pattern 'A01'"
  )
})

test_that("blanks assignment", {
  rate1 <- factor(character(), levels = c("OCR", "ECAR"))
  blanks(sea, "remove") <- NA
  expect_identical(
    blanks(sea),
    tibble::tibble(rate = rate1, well = character())
  )
  expect_identical(
    blanks(`blanks<-`(sea, "reset")),
    init_blanks(sea@wells)
  )
  df1 <-
    tibble::tibble(
      rate = factor("OCR", levels = c("OCR", "ECAR")),
      well = "A02"
    )
  expect_identical(blanks(`blanks<-`(sea, "replace", value = df1)), df1)
  blanks(sea, "add") <- df1
  expect_identical(blanks(sea), df1)
  expect_identical(
    blanks(`blanks<-`(sea, "subtract", value = df1)),
    tibble::tibble(rate = rate1, well = character())
  )
})

test_that("blanks assignment errors", {
  rate1 <- c("OCR", "OCR")
  well1 <- c("A01", "A02")
  expect_message(
    `blanks<-`(sea, "add", value = tibble::tibble(rate = rate1, well = well1)),
    "These wells are currently blanks:"
  )
  expect_message(
    `blanks<-`(sea, "subtract", value = tibble::tibble(rate = rate1, well = well1)),
    "These wells are not currently blanks:"
  )
  expect_warning(`blanks<-`(sea, "reset"), "Blanks unchanged")
})

test_that("blanks assignment triggers recalculation", {
  sea1 <- `blanks<-`(sea, "remove")
  sea2 <- `blanks<-`(sea1, "replace", value = list(OCR = "A01"))
  expect_equal(nrow(dplyr::intersect(sea1@OCR, sea2@OCR)), 0)
  expect_equal(sea1@ECAR, sea2@ECAR)
  sea3 <- `blanks<-`(sea1, "replace", value = list(ECAR = "A01"))
  expect_equal(nrow(dplyr::intersect(sea1@ECAR, sea3@ECAR)), 0)
  expect_equal(sea1@OCR, sea3@OCR)
})

