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


get_rng <- function(path, sheet, range) {
  readxl::read_excel(
    path = path,
    sheet = sheet,
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
  config$pH_em <- get_rng(path, s2, "P16:V20")
  config$O2_ref <- get_rng(path, s2, "B34:H38")
  config$pH_ref <- get_rng(path, s2, "P34:V38")
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


extract_wells <- function(path) {
  readxl::read_excel(
    path = path,
    sheet = "Assay Configuration",
    range = "B11:H15",
    col_names = TRUE
  ) |>
    dplyr::rename(row = "...1") |>
    tidyr::pivot_longer(
      -"row",
      names_to = "col",
      values_to = "group"
    ) |>
    dplyr::mutate(
      col = stringr::str_pad(.data$col, 2, "left", "0"),
      well = stringr::str_c(.data$row, .data$col),
      type = ifelse(.data$group == "Background", "blank", "sample"),
      group = stringr::str_replace(.data$group, "Background", "blank")
    ) |>
    dplyr::select("well", "type", "group") |>
    init_wells() |>
    suppressMessages()
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


extract_stages <- function(path) {
  df <-
    readxl::read_excel(
      path = path,
      sheet = "Assay Configuration",
      range = "C42:G48",
      col_names = TRUE
    )

  stage <-
    df |>
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to = "stage",
      values_to = "value"
    ) |>
    dplyr::filter(stringr::str_detect(.data$value, "Cycles")) |>
    dplyr::mutate(
      value = as.integer(stringr::str_extract(.data$value, "\\d+"))
    ) |>
    purrr::pmap(\(stage, value) unlist(rep(stage, value))) |>
    unlist() |>
    factor(levels = names(df)) |>
    forcats::fct_drop()

  measurement <- 1:length(stage)

  list(measurement = measurement, stage = stage)
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


init_cells <- function(cells, x = NULL) {
  if (length(cells) == 0) {
    out <-
      x@raw |>
      dplyr::select("well") |>
      dplyr::distinct() |>
      dplyr::mutate(value = 1)
    return(out)
  }

  #check format
  if ("well" %nin% names(cells)) {
    rlang::abort("Cells must contain a column named 'well'")
  }
  if (!all(stringr::str_detect(cells$well, "^[A-Z]\\d{2}$"))) {
    rlang::abort("Cells column 'well' must match the pattern 'A01'")
  }
  if ("value" %nin% names(cells)) {
    rlang::abort("Cells must contain a column named 'value'")
  }
  tibble::as_tibble(cells)
}


init_blanks <- function(wells) {
  wells |>
    dplyr::filter(.data$type == "blank") |>
    dplyr::select("well") |>
    tidyr::expand_grid(rate = c("OCR", "ECAR")) |>
    dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "ECAR"))) |>
    dplyr::select("rate", "well") |>
    dplyr::arrange(.data$rate, .data$well)
}


find_blank_outliers <- function(x) {
  dplyr::left_join(x@stages, x@wells, by = "well") |>
    dplyr::filter(.data$type == "blank") |>
    dplyr::left_join(x@OCR, by = c("well", "measurement")) |>
    dplyr::left_join(x@ECAR, by = c("well", "measurement")) |>
    tidyr::pivot_longer(
      tidyselect::any_of(c("OCR", "ECAR")),
      names_to = "rate",
      values_to = "value"
    ) |>
    dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "ECAR"))) |>
    dplyr::arrange(.data$rate, .data$measurement, .data$well) |>
    dplyr::group_by(.data$rate) |>
    tidyr::nest() |>
    dplyr::mutate(
      model = purrr::map(
        .data$data,
        \(x) MASS::rlm(value ~ factor(measurement), data = x, maxit = 100)
      ),
      fit = purrr::map(.data$model, stats::fitted),
      res = purrr::map(.data$model, stats::residuals),
      se = purrr::map(.data$res, \(x) x ^ 2)
    ) |>
    tidyr::unnest(c("data", "fit", "res", "se")) |>
    dplyr::group_by(.data$rate, .data$well) |>
    dplyr::summarise(mse = mean(.data$se, na.rm = TRUE)) |>
    dplyr::mutate(outlier = msd(.data$mse, n = 2)) |>
    dplyr::filter(.data$outlier) |>
    dplyr::select("rate", "well")
}
