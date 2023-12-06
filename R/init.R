# init.R

get_cell <- function(path, sheet, cell) {
  readxl::read_excel(
    path = path,
    sheet = sheet,
    range = cell,
    col_names = FALSE
  ) |>
    dplyr::pull() |>
    suppressMessages()  # column name repair
}


get_rng <- function(path, range) {
  readxl::read_excel(
    path = path,
    sheet = "Calibration",
    range = range,
    col_names = TRUE
  ) |>
    dplyr::rename(row = "...1") |>
    tidyr::pivot_longer(
      -"row",
      names_to = "column",
      values_to = "ref"
    ) |>
    dplyr::mutate(
      well = stringr::str_c(
        .data$row,
        stringr::str_pad(.data$column, 2, "left", "0")
      )
    ) |>
    dplyr::select("well", "ref") |>
    tibble::deframe() |>
    suppressMessages()  # column name repair
}


init_config <- function(path) {
  s1 <- "Assay Configuration"
  s2 <- "Calibration"

  config <- list()
  config$f0 <- get_cell(path, s1, "B61")
  config$Ksv <- get_cell(path, s1, "B58")
  config$O20 <- (config$f0 / get_cell(path, s2, "B4") - 1) / config$Ksv
  config$Kac <- 1 / get_cell(path, s1, "B63")
  config$Kaw <- get_cell(path, s1, "B64")
  config$Kw <- 1 / get_cell(path, s1, "B65")
  config$Kc <- 1 / get_cell(path, s1, "B66")
  config$Kp <- 1 / get_cell(path, s1, "B67")
  config$Vc <- get_cell(path, s1, "B62")
  config$pH_em <- get_rng(path, "P16:V20")
  config$O2_ref <- get_rng(path, "B34:H38")
  config$pH_ref <- get_rng(path, "P34:V38")
  config$O2_tar <- get_cell(path, s2, "B4")
  config$pH_tar <- get_cell(path, s2, "P4")
  config$vol <- get_cell(path, s1, "B77")
  config$Kvol <- get_cell(path, s1, "B78")
  config
}


init_raw <- function(path, config) {
  df <-
    readxl::read_excel(
      path,
      sheet = "Raw"
    ) |>
    dplyr::select(
      measurement  = "Measurement",
      group        = "Group",
      well         = "Well",
      time         = "TimeStamp",
      well_temp    = "Well Temperature",
      env_temp     = "Env. Temperature",
      O2_valid     = "O2 is Valid",
      O2_inst      = "O2 (mmHg)",
      O2_light     = "O2 Light Emission",
      O2_dark      = "O2 Dark Emission",
      O2_ref_light = "O2 Ref Light",
      O2_ref_dark  = "O2 Ref Dark",
      O2_em        = "O2 Corrected Em.",
      O2_inst      = "O2 (mmHg)",
      pH_valid     = "pH Is Valid",
      pH_inst      = "pH",
      pH_light     = "pH Light",
      pH_dark      = "pH Dark",
      pH_ref_light = "pH Ref Light",
      pH_ref_dark  = "pH Ref Dark",
      pH_em        = "pH Corrected Em."
    ) |>
    dplyr::mutate(
      time = lubridate::hms(.data$time),
      time = as.numeric(.data$time - .data$time[[1]])
    ) |>
    dplyr::left_join(
      tibble::enframe(config$O2_ref, name = "well", value = "O2_ref"),
      by = "well"
    ) |>
    dplyr::left_join(
      tibble::enframe(config$pH_ref, name = "well", value = "pH_ref"),
      by = "well"
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("(O2|pH)_"),
      names_to = c("sensor", ".value"),
      names_pattern = "(O2|pH)_(.*)"
    ) |>
    dplyr::mutate(
      fluor =
        (.data$light - .data$dark) /
        (.data$ref_light - .data$ref_dark) *
        .data$ref
    ) |>
    dplyr::arrange(.data$sensor)

  if (!all(dplyr::near(df$fluor, df$em))) {
    rlang::abort(
      message = "Calculated emission values don't match instrument values"
    )
  }
  df
}


init_wells <- function(wells, x = NULL) {
  if (length(wells) == 0) {
    out <-
      x@raw |>
      dplyr::select("well", "group") |>
      dplyr::distinct() |>
      dplyr::mutate(
        group = replace(.data$group, .data$group == "Background", "blank"),
        group = factor(.data$group),
        group = forcats::fct_relevel(.data$group, "blank", after = Inf),
        type = ifelse(.data$group == "blank", "blank", "sample")
      ) |>
      dplyr::relocate("group", .after = tidyselect::last_col())
    return(out)
  }

  # check format
  if ("well" %nin% names(wells)) {
    rlang::abort("Wells must contain a column named 'well'")
  }
  if ("type" %nin% names(wells)) {
    rlang::abort("Wells must contain a column named 'type'")
  }
  if (!all(wells$type %in% c("blank", "sample", "hypoxia"))) {
    rlang::abort(
      "Wells column 'type' must contain only: 'blank', 'sample', 'hypoxia'"
    )
  }
  if (!all(stringr::str_detect(wells$well, "^[A-Z]\\d{2}$"))) {
    rlang::abort("Wells column 'well' must match the pattern 'A01'")
  }

  df <- tibble::as_tibble(wells)

  #add group column
  if ("group" %in% names(df)) {
    df
  } else if (length(setdiff(names(df), c("well", "type"))) == 0) {
    df <- dplyr::mutate(df, group = .data$type)
  } else {
    df <- tidyr::unite(df, "group", -c("well", "type"), remove = FALSE)
  }

  df |>
    dplyr::mutate(group = factor(.data$group)) |>
    {\(x) if ("blank" %in% x$group) {
      dplyr::mutate(
        x,
        group = forcats::fct_relevel(.data$group, "blank", after = Inf)
      )
    } else {
      x
    }}() |>
    dplyr::relocate("group", .after = tidyselect::last_col())
}


init_stages <- function(stages, x = NULL) {
  if (length(stages) == 0) {
    out <-
      x@raw |>
      dplyr::select("measurement") |>
      dplyr::distinct() |>
      dplyr::mutate(stage = "basal") |>
      tidyr::crossing(well = x@wells$well)
    return(out)
  }

  # check format
  if ("stage" %nin% names(stages)) {
    rlang::abort("Stages must contain a column named 'stage'")
  }
  if ("measurement" %nin% names(stages)) {
    rlang::abort("Stages must contain a column named 'measurement'")
  }

  out <- tibble::as_tibble(stages)
  if ("well" %nin% names(out) & !is.null(x)) {
    tidyr::crossing(out, well = x@wells$well)
  } else {
    out
  }
}
