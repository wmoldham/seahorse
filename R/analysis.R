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


# analyze -----------------------------------------------------------------

setGeneric("analyze", function(x) standardGeneric("analyze"))

#' @return `analyze()` returns a new `Seahorse` object with calculated
#'     parameters for mitochondrial stress, glycolytic stress, and ATP
#'     production rate experiments. These data are normalized if a normalization
#'     factor is provided.
#'
#' @noRd
#' @examples
#' analyze(sheldon)
setMethod("analyze", "Seahorse", function(x) {
  x@summary <- summarize_rates(x)
  x@mst <- summarize_mst(x)
  x@gst <- summarize_gst(x)
  x@atp <- summarize_atp(x)
  x
})

summarize_rates <- function(x) {
  rates(x, blanks = FALSE, outliers = FALSE, normalize = TRUE) |>
    dplyr::group_by(.data$well, .data$group, .data$stage, .data$rate) |>
    dplyr::mutate(
      summary = dplyr::case_when(
        .data$stage == "basal" ~ .data$value[which.max(.data$measurement)],
        .data$rate == "OCR" & .data$stage == "oligo" ~ min(.data$value),
        .data$rate != "OCR" & .data$stage == "oligo" ~ max(.data$value),
        .data$stage == "fccp" ~ max(.data$value),
        .data$rate == "OCR" & .data$stage == "rot/ama" ~ min(.data$value),
        .data$rate != "OCR" & .data$stage == "rot/ama" ~ max(.data$value),
        TRUE ~ .data$value[which.max(.data$measurement)]
      )
    ) |>
    dplyr::select(-c("type", "measurement", "value")) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::arrange(.data$rate, .data$group, .data$stage) |>
    dplyr::rename(value = "summary")
}


summarize_mst <- function(x) {
  stage <- x@stages
  required_stages <- c("basal", "oligo", "fccp", "rot/ama")
  missing_stages <- setdiff(required_stages, stage$stage)

  if (length(missing_stages) > 0) {
    rlang::inform(
      glue::glue(
        "A mito stress test requires basal, oligo, fccp, and rot/ama stages.\n",
        "These stages are missing: ",
        "{glue::glue_collapse(missing_stages, sep = ', ', last = ', and ')}."
      )
    )
    return(list())
  }

  x@summary |>
    dplyr::filter(.data$rate == "OCR") |>
    dplyr::group_by(.data$well, .data$group) |>
    dplyr::mutate(value = .data$value - .data$value[.data$stage == "rot/ama"]) |>
    dplyr::summarise(
      src = .data$value[.data$stage == "fccp"] /
        .data$value[.data$stage == "basal"],
      coupling = 1 - .data$value[.data$stage == "oligo"] /
        .data$value[.data$stage == "basal"]
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      c("src", "coupling"),
      names_to = "rate",
      values_to = "value"
    ) |>
    dplyr::arrange(.data$rate, .data$group)
}


summarize_gst <- function(x) {
  stage <- x@stages
  required_stages <- c("basal", "oligo")
  missing_stages <- setdiff(required_stages, stage$stage)

  if (length(missing_stages) > 0) {
    rlang::inform(
      glue::glue(
        "A glyco stress test requires basal and oligo stages.\n",
        "These stages are missing: ",
        "{glue::glue_collapse(missing_stages, sep = ' and ')}."
      )
    )
    return(list())
  }

  x@summary |>
    dplyr::filter(.data$rate != "OCR") |>
    dplyr::group_by(.data$well, .data$group) |>
    dplyr::summarise(
      gst = .data$value[.data$stage == "oligo"] /
        .data$value[.data$stage == "basal"]
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$group) |>
    tidyr::pivot_longer(
      "gst",
      names_to = "rate",
      values_to = "value"
    )
}


summarize_atp <- function(x) {
  missing <- vector("character")
  if (is.na(x@cf)) {
    missing <- c(missing, "cf")
  }
  if (is.na(x@bf)) {
    missing <- c(missing, "bf")
  }
  stage <- x@stages
  required_stages <- c("basal", "oligo", "rot/ama")
  missing_stages <- setdiff(required_stages, stage$stage)
  if (length(missing_stages) > 0) {
    missing <- c(missing_stages, missing)
  }

  if (length(missing) > 0) {
    rlang::inform(
      glue::glue(
        "ATP rate calculations require basal, oligo, and rot/ama stages;\n",
        "a carbon dioxide correction factor (cf);\n",
        "and a medium buffer factor (bf).\n",
        "These items are missing: ",
        "{glue::glue_collapse(missing, sep = ', ', last = ', and ')}."
      )
    )
    return(list())
  }

  df <- x@summary

  mito <-
    df |>
    dplyr::filter(.data$rate == "OCR") |>
    dplyr::group_by(.data$well, .data$group) |>
    dplyr::summarise(
      ATP_mito = (.data$value[.data$stage == "basal"] - .data$value[.data$stage == "oligo"]) * 2 * 2.75
    )

  df |>
    tidyr::pivot_wider(names_from = "rate", values_from = "value") |>
    dplyr::group_by(.data$well) |>
    dplyr::mutate(
      mito_OCR = .data$OCR - .data$OCR[.data$stage == "rot/ama"],
      mito_PER = .data$mito_OCR * x@cf,
      ATP_glyco = .data$PER - .data$mito_PER
    ) |>
    dplyr::left_join(mito, by = c("well", "group")) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$stage == "basal") |>
    dplyr::select("well", "group", "ATP_glyco", "ATP_mito") |>
    tidyr::pivot_longer(
      c("ATP_glyco", "ATP_mito"),
      names_to = "rate",
      values_to = "value",
      names_prefix = "ATP_"
    ) |>
    dplyr::mutate(rate = factor(.data$rate, levels = c("mito", "glyco"))) |>
    dplyr::arrange(.data$rate, .data$group, .data$well)
}
