# analysis.R


# levels ------------------------------------------------------------------

setGeneric(
  "levels",
  function(x, ...) standardGeneric("levels"),
  useAsDefault = function(x, ...) base::levels(x)
) |>
  suppressMessages()

setMethod("levels", "Seahorse", function(x, blanks = TRUE, outliers = FALSE) {
  df <-
    dplyr::left_join(x@stages, x@wells, by = c("well")) |>
    dplyr::left_join(x@O2, by = c("well", "measurement")) |>
    dplyr::left_join(x@pH, by = c("well", "measurement", "time")) |>
    tidyr::pivot_longer(
      cols = c("O2", "pH"),
      names_to = "sensor",
      values_to = "value"
    ) |>
    dplyr::mutate(sensor = factor(.data$sensor, levels = c("O2", "pH"))) |>
    dplyr::arrange(.data$sensor)

  if (!blanks) {
    blanks_df <-
      x@blanks |>
      dplyr::mutate(
        sensor = ifelse(.data$rate == "OCR", "O2", "pH"),
        sensor = factor(.data$sensor, levels = c("O2", "pH"))
        )
    df <-
      dplyr::anti_join(df, blanks_df, by = c("sensor", "well"))
  }

  if (!outliers) {
    outliers_df <-
      x@outliers |>
      dplyr::filter(.data$outlier) |>
      dplyr::mutate(
        sensor = ifelse(.data$rate == "OCR", "O2", "pH"),
        sensor = factor(.data$sensor, levels = c("O2", "pH"))
      )

    df <- dplyr::anti_join(df, outliers_df, by = c("sensor", "measurement", "well"))
  }

  df
})


# rates -------------------------------------------------------------------

setGeneric("rates", function(x, ...) standardGeneric("rates"))

setMethod(
  "rates",
  "Seahorse",
  function(x, blanks = TRUE, outliers = FALSE, normalize = TRUE) {
    df <-
      dplyr::left_join(x@stages, x@wells, by = c("well")) |>
      dplyr::left_join(x@OCR, by = c("well", "measurement")) |>
      dplyr::left_join(x@ECAR, by = c("well", "measurement")) |>
      tidyr::pivot_longer(
        tidyselect::any_of(c("OCR", "ECAR")),
        names_to = "rate",
        values_to = "value"
      )

    if (normalize) {
      df <-
        dplyr::left_join(df, x@cells, by = "well") |>
        dplyr::mutate(value = .data$value.x / .data$value.y ) |>
        dplyr::select(-c("value.x", "value.y"))
    }

    if (!blanks) {
      df <- dplyr::anti_join(df, x@blanks, by = c("rate", "well"))
    }

    if (!outliers) {
      outliers_df <-
        x@outliers |>
        dplyr::filter(.data$outlier)
      df <- dplyr::anti_join(df, outliers_df, by = c("rate", "measurement", "well"))
    }

    if (!is.na(x@bf)) {
      df <-
        df |>
        dplyr::mutate(
          value = dplyr::if_else(
            .data$rate == "ECAR",
            .data$value * x@bf * x@config$vol * x@config$Kvol,
            .data$value
          ),
          rate = dplyr::if_else(.data$rate == "ECAR", "PER", .data$rate)
        )
    }

    df |>
      dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "PER", "ECAR"))) |>
      dplyr::arrange(.data$rate)
  })
