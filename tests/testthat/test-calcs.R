test_that("levels work", {
  x <- t2@raw
  O2_inst <- dplyr::filter(x, sensor == "O2")
  O2 <- t2@env$O2_level
  O2_test <- dplyr::inner_join(O2, O2_inst, by = c("measurement", "well", "time"))
  expect_equal(O2_test$O2, O2_test$inst)
  pH_inst <- dplyr::filter(x, sensor == "pH")
  pH <- t2@env$pH_level
  pH_test <- dplyr::inner_join(pH, pH_inst, by = c("measurement", "well", "time"))
  expect_equal(pH_test$pH, pH_test$inst, tolerance = 1e-3)
})

test_that("rates work", {
  x <-
    readxl::read_excel(path, sheet = "Rate") %>%
    dplyr::filter(Group != "Background") %>%
    dplyr::select(
      measurement = Measurement,
      well = Well,
      OCR_inst = OCR,
      ECAR_inst = ECAR
    ) %>%
    dplyr::left_join(t2@env$OCR, by = c("measurement", "well")) %>%
    dplyr::left_join(t2@env$ECAR, by = c("measurement", "well"))
  expect_equal(x$OCR_inst, x$OCR, tolerance = 5e-3)
  expect_equal(x$ECAR_inst, x$ECAR, tolerance = 5e-2)
})
