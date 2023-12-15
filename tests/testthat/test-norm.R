# test-norm.R

local_csv <- function(x, path) {
  readr::write_csv(x, path)
  withr::defer(unlink(path), envir = parent.frame())
  invisible(path)
}

test_that("format correct", {
  row <- LETTERS[1:4]
  good_df <- tibble::tibble(row = row, x01 = 1:4)
  path <- local_csv(good_df, "good.csv")
  expect_snapshot(format_cells(path))

  bad_df1 <- tibble::tibble(x = row, x01 = 1:4)
  path <- local_csv(bad_df1, "bad_df1.csv")
  expect_error(
    format_cells(path),
    "Input data file missing a column named 'row'"
  )

  bad_df2 <- tibble::tibble(row = row, x = 1:4)
  path <- local_csv(bad_df2, "bad_df2.csv")
  expect_error(
    format_cells(path),
    "Input data column headers must be formatted as 'x01'"
  )

  bad_df3 <- tibble::tibble(row = tolower(row), x01 = 1:4)
  path <- local_csv(bad_df3, "bad_df3.csv")
  expect_error(
    format_cells(path),
    "Input data row names must contain capital letters"
  )
})
