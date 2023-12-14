# test-out.R

test_that("outliers accessor works", {
  expect_snapshot(outliers(sea))
})

test_that("outliers assignment format", {
  expect_warning(
    `outliers<-`(sea, "remove", value = 1),
    "Replacement values provided but ignored"
  ) |>
    expect_message("Moving these outlier wells to blanks")
  expect_error(
    `outliers<-`(sea, "add", value = 1),
    "'value' must be a data.frame"
  )
  expect_error(
    `outliers<-`(sea, "add", value = data.frame(a = 1, b = 1)),
    "Outliers data.frame column names must be 'rate' and 'well'"
  )
  expect_error(
    `outliers<-`(sea, "add", value = data.frame(rate = 1, well = 1, c = 1)),
    "Outliers data.frame column names must be 'rate' and 'well'"
  )
  expect_error(
    `outliers<-`(sea, "add", value = data.frame(rate = "x", well = 1)),
    "Outliers data.frame rate column must contain only 'OCR', 'ECAR', or 'PER'"
  )
  expect_error(
    `outliers<-`(sea, "add", value = data.frame(rate = "OCR", well = 1)),
    "Outliers well values must match the pattern 'A01'"
  )
})

test_that("outlier assignment", {
  suppressMessages(
    outliers(sea, "replace") <- tibble::tibble(rate = "OCR", well = "A01")
  )
  expect_snapshot(outliers(sea))
  suppressMessages(
    outliers(sea, "remove") <- NA
  )
  expect_snapshot(outliers(sea))
  outliers(sea, "add") <- tibble::tibble(rate = "OCR", well = "A02")
  expect_snapshot(outliers(sea))
  outliers(sea, "subtract") <- tibble::tibble(rate = "OCR", well = "A02")
  expect_snapshot(outliers(sea))
  suppressMessages(
    outliers(sea, "replace") <- tibble::tibble(rate = "OCR", well = "A01")
  )
  expect_snapshot(outliers(sea))
  suppressMessages(
    outliers(sea, "reset") <- NA
  )
  expect_snapshot(outliers(sea))
})

test_that("outlier assignment errors", {
  suppressMessages(
    outliers(sea, "replace") <- tibble::tibble(rate = "OCR", well = "A01")
  )
  expect_message(
    `outliers<-`(sea, "add", value = tibble::tibble(rate = "OCR", well = "A01")),
    "These values are already outliers:"
  ) |>
    expect_warning("Outliers unchanged")
  expect_message(
    `outliers<-`(sea, "subtract", value = tibble::tibble(rate = "OCR", well = "A02")),
    "These values are not currently outliers:"
  ) |>
    expect_warning("Outliers unchanged")
  expect_warning(
    `outliers<-`(sea, "add", value = tibble::tibble(rate = "OCR", well = "A01")),
    "Outliers unchanged"
  ) |>
    expect_message("These values are already outliers")
})

test_that("blanks handled", {
  value1 <- tibble::tibble(rate = "OCR", well = "A01")
  expect_message(
    `outliers<-`(sea, "add", value = value1),
    "Moving these blank wells to outliers"
  )
  suppressMessages(
    outliers(sea, "add") <- value1
  )
  expect_message(
    `outliers<-`(sea, "subtract", value = value1),
    "Moving these outlier wells to blanks"
  )
})


test_that("findOut works", {
  expect_snapshot(findOut(sea))
})
