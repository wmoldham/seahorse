# analysis.R

#' @include Seahorse-class.R
NULL


# document ----------------------------------------------------------------

#' @title Accessors for analysis output
#'
#' @name analysis
#' @aliases analysis seahorse_levels rates seahorse_summary mst gst atp
#'     Seahorse_analysis
#'
#' @description These functions access calculated values from `Seahorse` or
#'     `Herd` objects. Collecting `Seahorse` objects into a `Herd` returns one
#'     average value for each group from each `Seahorse` experiment. For
#'     example, `summary(Seahorse)` returns summarized values for each stage
#'     from each well, while `summary(Herd)` returns summarized values for each
#'     stage from each group. Note that `summary` functions automatically
#'     exclude outliers.
#'
#' @param x An **R** object. Currently there are methods for
#'     [seahorse::Seahorse] and [seahorse::Herd-class] objects.
#' @param ... Additional arguments passed to other functions.
#' @param object An **R** object.
#' @param blanks Include blanks in output?
#' @param outliers Include outliers in output?
NULL


# levels ------------------------------------------------------------------

#' @describeIn analysis The O2 and pH measurements used to calculate oxygen
#'     consumption and extracellular acidification rates, respectively.
setGeneric(
  "levels",
  function(x, ...) standardGeneric("levels"),
  useAsDefault = function(x, ...) base::levels(x)
)

#' @rdname analysis
#' @export
#' @examples
#' levels(sheldon)
#' levels(sheldon, blanks = FALSE, outliers = FALSE)
#'
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
      dplyr::mutate(
        sensor = ifelse(.data$rate == "OCR", "O2", "pH"),
        sensor = factor(.data$sensor, levels = c("O2", "pH"))
      )

    df <- dplyr::anti_join(df, outliers_df, by = c("sensor", "well"))
  }

  df
})


# rates -------------------------------------------------------------------

#' @describeIn analysis The oxygen consumption and extracellular acidification
#'     or proton efflux rates.
setGeneric("rates", function(x, ...) standardGeneric("rates"))

#' @rdname analysis
#' @param normalize Divide values by normalization factor?
#' @export
#' @examples
#' rates(sheldon)
#' rates(sheldon, blanks = FALSE, outliers = FALSE)
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
      df <- dplyr::anti_join(df, x@outliers, by = c("rate", "well"))
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
