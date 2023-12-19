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

test_that("herd rates works", {
  vdiffr::expect_doppelganger(
    "herd-rates-by-group",
    plot(herd, "rates", group = TRUE)
  )
  vdiffr::expect_doppelganger(
    "herd-rates-by-experiment",
    plot(herd, "rates", group = FALSE)
  )
})

test_that("summary plot works", {
  vdiffr::expect_doppelganger(
    "sea-summary",
    plot(harold, "summary")
  )
  vdiffr::expect_doppelganger(
    "herd-summary",
    plot(herd, "summary")
  )
})

test_that("mst plot works", {
  vdiffr::expect_doppelganger(
    "sea-mst",
    plot(harold, "mst")
  )
  vdiffr::expect_doppelganger(
    "herd-mst",
    plot(herd, "mst")
  )
})

test_that("gst plot works", {
  vdiffr::expect_doppelganger(
    "sea-gst",
    plot(harold, "gst")
  )
  vdiffr::expect_doppelganger(
    "herd-gst",
    plot(herd, "gst")
  )
})

test_that("atp plot works", {
  vdiffr::expect_doppelganger(
    "sea-atp-xy",
    plot(harold, "atp", type = "scatter")
  )
  vdiffr::expect_doppelganger(
    "sea-atp-bar",
    plot(harold, "atp", type = "bar")
  )
  vdiffr::expect_doppelganger(
    "herd-atp-xy",
    plot(herd, "atp", type = "scatter")
  )
  vdiffr::expect_doppelganger(
    "herd-atp-bar",
    plot(herd, "atp", type = "bar")
  )
})
