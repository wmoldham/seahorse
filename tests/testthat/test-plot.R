# test-plot.R

test_that("cells works", {
  vdiffr::expect_doppelganger(
    "cells",
    plot(harold, "cells")
  )
})

test_that("levels works", {
  vdiffr::expect_doppelganger(
    "levels-by-group",
    plot(harold, "levels", group = TRUE)
  )
  vdiffr::expect_doppelganger(
    "levels-by-well",
    plot(harold, "levels", group = FALSE)
  )
  vdiffr::expect_doppelganger(
    "levels-no-blanks",
    plot(harold, "levels", group = FALSE, blanks = FALSE)
  )
  vdiffr::expect_doppelganger(
    "levels-no-outliers",
    plot(harold, "levels", group = FALSE, outliers = FALSE)
  )
})

test_that("seahorse rates works", {
  vdiffr::expect_doppelganger(
    "sea-rates-norm-by-group",
    plot(harold, "rates", normalize = TRUE, group = TRUE)
  )
  vdiffr::expect_doppelganger(
    "sea-rates-not-norm-by-group",
    plot(harold, "rates", normalize = FALSE, group = TRUE)
  )
  vdiffr::expect_doppelganger(
    "sea-rates-norm-by-well",
    plot(harold, "rates", normalize = FALSE, group = FALSE)
  )
  vdiffr::expect_doppelganger(
    "sea-rates-norm-by-well",
    plot(harold, "rates", normalize = FALSE, group = FALSE)
  )
  vdiffr::expect_doppelganger(
    "sea-rates-no-blanks",
    plot(harold, "rates", blanks = FALSE, group = FALSE)
  )
  vdiffr::expect_doppelganger(
    "sea-rates-no-outliers",
    plot(harold, "rates", outliers = FALSE, group = FALSE)
  )
})
