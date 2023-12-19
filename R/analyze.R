# analyze.R

#' @include Seahorse-class.R
NULL


# document ----------------------------------------------------------------

#' @title Analysis functions for Seahorse rate data
#'
#' @name analyze
#' @aliases analyze summarize_rates summarize_mst summarize_gst summarize_atp
#'
#' @description These functions operate on a `rates` data.frame from a `Seahorse`
#'     object to return summary values from various experimental designs
#'     including mitochondrial stress tests, glycolytic stress tests, and ATP
#'     production rates. These analyses are automatically performed during the
#'     creation of a `Seahorse` object, but these functions are exposed to the
#'     user to facilitate finer-grained analyses.
#'
#' @param x A data.frame returned by `rates()`.
#' @param bf The buffer factor of the assay medium.
#' @param cf The carbon dioxide correction factor.
#' @param remove_outliers Should outliers based on median absolutely deviation
#'     be removed?
NULL


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
  df <- rates(x, blanks = FALSE, outliers = TRUE, normalize = TRUE)
  x@summary <- summarize_rates(df, remove_outliers = TRUE)
  x@mst <- summarize_mst(df)
  x@gst <- summarize_gst(df)
  x@atp <- summarize_atp(df, x@bf, x@cf)
  x
})

#' @describeIn analyze Calculates summary values for each interval of a
#'     `Seahorse` object. For `basal`, the last measurement in the interval is
#'     returned. For `oligo` and `rot/ama`, the lowest OCR and highest ECAR
#'     values are returned. For `fccp`, the highest OCR and ECAR values are
#'     returned. For any other interval, the value of the last measurement in
#'     the interval is returned.
#' @export
summarize_rates <- function(x, remove_outliers = FALSE) {
  out <-
    x |>
    dplyr::group_by(.data$well, .data$group, .data$stage, .data$rate) |>
    dplyr::mutate(
      summary = dplyr::case_when(
        .data$stage == "basal" ~ .data$value[which.max(.data$measurement)],
        # .data$rate == "OCR" & .data$stage == "oligo" ~ min(.data$value),
        .data$rate != "OCR" & .data$stage == "oligo" ~ max(.data$value),
        .data$stage == "fccp" ~ max(.data$value),
        # .data$rate == "OCR" & .data$stage == "rot/ama" ~ min(.data$value),
        .data$rate != "OCR" & .data$stage == "rot/ama" ~ max(.data$value),
        TRUE ~ .data$value[which.max(.data$measurement)]
      )
    ) |>
    dplyr::select(-c("type", "measurement", "value")) |>
    dplyr::distinct()

  if (remove_outliers) {
    out <- remove_outliers(out, "summary", c("rate", "group", "stage"))
  }

  out |>
    dplyr::arrange(.data$rate, .data$group, .data$stage) |>
    dplyr::rename(value = "summary") |>
    dplyr::ungroup()
}


#' @describeIn analyze Calculates the spare respiratory capacity (`src`) and
#'     coupling efficiency (`coupling`) from a mitochondrial stress test. This
#'     test requires stages labeled `basal`, `oligo`, `fccp`, and `rot/ama`.
#'     Both values are expressed as the fold change relative to the basal OCR
#'     after subtracting non-mitochondrial OCR.
#' @export
summarize_mst <- function(x, remove_outliers = TRUE) {
  df <- summarize_rates(x)
  required_stages <- c("basal", "oligo", "fccp", "rot/ama")
  missing_stages <- setdiff(required_stages, df$stage)

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

  out <-
    df |>
    dplyr::filter(.data$rate == "OCR") |>
    dplyr::group_by(.data$group, .data$well) |>
    dplyr::mutate(
      value = .data$value - .data$value[.data$stage == "rot/ama"]
    ) |>
    dplyr::summarise(
      src = .data$value[.data$stage == "fccp"] /
        .data$value[.data$stage == "basal"],
      coupling = 1 - .data$value[.data$stage == "oligo"] /
        .data$value[.data$stage == "basal"]
    ) |>
    tidyr::pivot_longer(
      c("src", "coupling"),
      names_to = "rate",
      values_to = "value"
    )

  if (remove_outliers) {
    out <- remove_outliers(out, "value", c("rate", "group"))
  }

  out |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$rate, .data$group)
}


#' @describeIn analyze Calculates the results of a glycolytic stress test. This
#'     test requires stages labeled `basal` and `oligo`. The `gst` value is the
#'     fold increase in ECAR or PER following oligomycin treatment.
#' @export
summarize_gst <- function(x, remove_outliers = TRUE) {
  df <- summarize_rates(x)
  required_stages <- c("basal", "oligo")
  missing_stages <- setdiff(required_stages, df$stage)

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

  out <-
    df |>
    dplyr::filter(.data$rate != "OCR") |>
    dplyr::group_by(.data$group, .data$well) |>
    dplyr::summarise(
      gst = .data$value[.data$stage == "oligo"] /
        .data$value[.data$stage == "basal"]
    ) |>
    dplyr::arrange(.data$group) |>
    tidyr::pivot_longer(
      "gst",
      names_to = "rate",
      values_to = "value"
    )

  if (remove_outliers) {
    out <- remove_outliers(out, "value", c("group"))
  }

  out |>
    dplyr::ungroup()
}

#' @describeIn analyze Calculates the mitochondrial and glycolytic ATP
#'     production rates. This test requires stages labeled `basal`, `oligo`, and
#'     `rot/ama`. In addition, the assay medium buffer factor (`bf`) and carbon
#'     dioxide correction factor (`cf`) must also be provided.
#' @export
summarize_atp <- function(x, bf, cf, remove_outliers = TRUE) {
  missing <- vector("character")
  missing <- vector("character")
  if (is.na(bf)) {
    missing <- c(missing, "bf")
  }
  if (is.na(cf)) {
    missing <- c(missing, "cf")
  }
  df <- summarize_rates(x)
  required_stages <- c("basal", "oligo", "rot/ama")
  missing_stages <- setdiff(required_stages, df$stage)
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

  mito <-
    df |>
    dplyr::filter(.data$rate == "OCR") |>
    dplyr::group_by(.data$group, .data$well) |>
    dplyr::summarise(
      ATP_mito = (.data$value[.data$stage == "basal"] -
                    .data$value[.data$stage == "oligo"]) * 2 * 2.75
    )

  out <-
    df |>
    tidyr::pivot_wider(names_from = "rate", values_from = "value") |>
    dplyr::group_by(.data$well) |>
    dplyr::mutate(
      mito_OCR = .data$OCR - .data$OCR[.data$stage == "rot/ama"],
      mito_PER = .data$mito_OCR * cf,
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
    dplyr::mutate(rate = factor(.data$rate, levels = c("mito", "glyco")))

  if (remove_outliers) {
    out <- remove_outliers(out, "value", c("rate", "group"))
  }

  out |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$rate, .data$group, .data$well)
}
