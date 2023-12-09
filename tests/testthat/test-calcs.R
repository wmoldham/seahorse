# test-calcs.R

test_that("levels", {
  suppressMessages(outliers(sea, "remove") <- NA)
  O2_inst <- dplyr::filter(sea@raw, sensor == "O2")
  O2 <- sea@O2
  O2_test <- dplyr::inner_join(O2, O2_inst, by = c("measurement", "well", "time"))
  expect_equal(O2_test$O2, O2_test$inst)

  pH_inst <- dplyr::filter(sea@raw, sensor == "pH")
  pH <- sea@pH
  pH_test <- dplyr::inner_join(pH, pH_inst, by = c("measurement", "well", "time"))
  expect_equal(pH_test$pH, pH_test$inst, tolerance = 1e-3)
})

test_that("rates", {
  suppressMessages(outliers(sea, "remove") <- NA)
  rates <-
    readxl::read_excel(.path, sheet = "Rate") |>
    dplyr::filter(Group != "Background") |>
    dplyr::select(
      measurement = Measurement,
      well = Well,
      OCR_inst = OCR,
      ECAR_inst = ECAR
    ) |>
    dplyr::left_join(sea@OCR, by = c("measurement", "well")) |>
    dplyr::left_join(sea@ECAR, by = c("measurement", "well"))
  expect_equal(rates$OCR_inst, rates$OCR, tolerance = 5e-3)
  expect_equal(rates$ECAR_inst, rates$ECAR, tolerance = 5e-2)
})

