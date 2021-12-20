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
  config$o20 <- (config$f0/get_cell(path, "Calibration", "B4") - 1) / config$Ksv
  config$Kac <- 1 / get_cell(path, "Assay Configuration", "B63")
  config$Kaw <- get_cell(path, "Assay Configuration", "B64")
  config$Kw <- 1 / get_cell(path, "Assay Configuration", "B65")
  config$Kc <- 1 / get_cell(path, "Assay Configuration", "B66")
  config$Kp <- 1 / get_cell(path, "Assay Configuration", "B67")
  config$Vc <- get_cell(path, "Assay Configuration", "B62")
  config$ph_em <- get_rng(path, "P16:V20")
  config$o2_ref <- get_rng(path, "B34:H38")
  config$ph_ref <- get_rng(path, "P34:V38")
  config$o2_tar <- get_cell(path, "Calibration", "B4")
  config$ph_tar <- get_cell(path, "Calibration", "P4")
  config$vol <- get_cell(path, "Assay Configuration", "B77")
  config$Kvol <- get_cell(path, "Assay Configuration", "B78")
  config
}
