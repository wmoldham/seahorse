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
