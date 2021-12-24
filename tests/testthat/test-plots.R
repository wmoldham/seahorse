test_that("levels works", {
  vdiffr::expect_doppelganger("levels-by-group", plot(t2, "levels", by = "group"))
  vdiffr::expect_doppelganger("levels-by-well", plot(t2, "levels", by = "well"))
})

test_that("rates works", {
  vdiffr::expect_doppelganger("rates-normalized-by-group", plot(t2, "rates", normalize = TRUE, by = "group"))
  vdiffr::expect_doppelganger("rates-not-normalized-by-group", plot(t2, "rates", normalize = FALSE, by = "group"))
  vdiffr::expect_doppelganger("rates-normalized-by-well", plot(t2, "rates", normalize = TRUE, by = "well"))
  vdiffr::expect_doppelganger("rates-not-normalized-by-well", plot(t2, "rates", normalize = FALSE, by = "well"))
})
