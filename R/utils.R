# utils.R

get_cell <- function(path, sheet, cell)
{
  readxl::read_excel(
    path = path,
    sheet = sheet,
    range = cell,
    col_names = FALSE
  ) %>%
    dplyr::pull() %>%
    suppressMessages()
}

get_rng <- function(path, range)
{
  readxl::read_excel(
    path = path,
    sheet = "Calibration",
    range = range,
    col_names = TRUE
  ) %>%
    dplyr::rename(row = .data$...1) %>%
    tidyr::pivot_longer(-row, names_to = "column", values_to = "ref") %>%
    dplyr::mutate(
      well = stringr::str_c(.data$row, stringr::str_pad(.data$column, 2, "left", "0"))
    ) %>%
    dplyr::select(.data$well, .data$ref) %>%
    tibble::deframe() %>%
    suppressMessages()
}

init_config <- function(path)
{
  config <- list()
  config$f0 <- get_cell(path, "Assay Configuration", "B61")
  config$Ksv <- get_cell(path, "Assay Configuration", "B58")
  config$O20 <- (config$f0/get_cell(path, "Calibration", "B4") - 1) / config$Ksv
  config$Kac <- 1 / get_cell(path, "Assay Configuration", "B63")
  config$Kaw <- get_cell(path, "Assay Configuration", "B64")
  config$Kw <- 1 / get_cell(path, "Assay Configuration", "B65")
  config$Kc <- 1 / get_cell(path, "Assay Configuration", "B66")
  config$Kp <- 1 / get_cell(path, "Assay Configuration", "B67")
  config$Vc <- get_cell(path, "Assay Configuration", "B62")
  config$pH_em <- get_rng(path, "P16:V20")
  config$O2_ref <- get_rng(path, "B34:H38")
  config$pH_ref <- get_rng(path, "P34:V38")
  config$O2_tar <- get_cell(path, "Calibration", "B4")
  config$pH_tar <- get_cell(path, "Calibration", "P4")
  config$vol <- get_cell(path, "Assay Configuration", "B77")
  config$Kvol <- get_cell(path, "Assay Configuration", "B78")
  config
}

init_raw <- function(path, config)
{
  df <-
    readxl::read_excel(
      path = path,
      sheet = "Raw"
    ) %>%
    dplyr::select(
      measurement = .data$Measurement,
      well = .data$Well,
      time = .data$TimeStamp,
      well_temp = .data$`Well Temperature`,
      env_temp = .data$`Env. Temperature`,
      O2_valid = .data$`O2 is Valid`,
      O2_inst = .data$`O2 (mmHg)`,
      O2_light = .data$`O2 Light Emission`,
      O2_dark = .data$`O2 Dark Emission`,
      O2_ref_light = .data$`O2 Ref Light`,
      O2_ref_dark = .data$`O2 Ref Dark`,
      O2_em = .data$`O2 Corrected Em.`,
      pH_valid = .data$`pH Is Valid`,
      pH_inst = .data$`pH`,
      pH_light = .data$`pH Light`,
      pH_dark = .data$`pH Dark`,
      pH_ref_light = .data$`pH Ref Light`,
      pH_ref_dark = .data$`pH Ref Dark`,
      pH_em = .data$`pH Corrected Em.`
    ) %>%
    dplyr::mutate(
      time = lubridate::hms(.data$time),
      time = lubridate::seconds(.data$time),
      time = as.numeric(.data$time - .data$time[[1]])
    ) %>%
    dplyr::left_join(
      tibble::enframe(config$O2_ref, name = "well", value = "O2_ref"),
      by = "well"
    ) %>%
    dplyr::left_join(
      tibble::enframe(config$pH_ref, name = "well", value = "pH_ref"),
      by = "well"
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("(O2|pH)_"),
      names_to = c("name", ".value"),
      names_pattern = "(O2|pH)_(.*)"
    ) %>%
    dplyr::mutate(
      fluor = (.data$light - .data$dark) / (.data$ref_light - .data$ref_dark) * .data$ref,
      name = dplyr::if_else(.data$name == "O2", "OCR", "ECAR")
    ) %>%
    dplyr::arrange(.data$name) %>%
    suppressMessages()

  if (!all(dplyr::near(df$fluor, df$em))) {
    rlang::abort(
      error = "error_bad_calculation",
      message = "Calculated fluorescence emission doesn't match instrument values"
    )
  }
  df
}
