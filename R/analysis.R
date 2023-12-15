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
      blanks <- FALSE
      df <-
        dplyr::left_join(df, x@cells, by = "well") |>
        dplyr::mutate(value = .data$value.x / .data$value.y ) |>
        dplyr::select(-c("value.x", "value.y"))
    }

    if (!blanks) {
      df <- dplyr::anti_join(df, x@blanks, by = c("rate", "well"))
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

    if (!outliers) {
      df <- dplyr::anti_join(df, x@outliers, by = c("rate", "well"))
    }

    df |>
      dplyr::mutate(
        rate = factor(
          .data$rate,
          levels = c("OCR", "PER", "ECAR")
        )
      ) |>
      dplyr::arrange(.data$rate)
  })


# summary -----------------------------------------------------------------

#' @describeIn analysis For `basal`, measurements, the last value of the stage
#'     is returned. For `oligo` and `rot/ama` OCR measurements, the minimum
#'     value of the stage is returned. For the ECAR/PER measurements, the
#'     maximum value of the stage is returned. For all other stages, the last
#'     value in the stage is returned for both OCR and ECAR.
setGeneric("summary", function(object) standardGeneric("summary"))

#' @rdname analysis
#' @export
#' @examples
#' summary(sheldon)
setMethod("summary", "Seahorse", function(object) object@summary)

#' @rdname analysis
#' @export
#' @examples
#' summary(herd)
#'
setMethod("summary", "Herd", function(object) object@summary)


# mst ---------------------------------------------------------------------

#' @describeIn analysis Values calculated from a mitochondrial stress assay.
#'     The spare respiratory capacity (`src`) is the ratio of FCCP / basal OCR
#'     after subtracting non-mitochondrial OCR (*i.e.,* OCR following rotenone
#'     and antimycin A treatment). The `coupling` fraction is the proportion of
#'     basal OCR dedicated to ATP production.
setGeneric("mst", function(x) standardGeneric("mst"))

#' @rdname analysis
#' @export
setMethod("mst", "Seahorse", function(x) {
  out <- x@mst
  if (length(out) == 0) {
    rlang::warn("No mito stress results available, run analyze()")
    return(invisible(list()))
  }
  out
})

#' @rdname analysis
#' @export
setMethod("mst", "Herd", function(x) {
  out <- x@mst
  if (length(out) == 0) {
    rlang::warn(
      "No mito stress results available, analyze() component experiments"
    )
    return(invisible(list()))
  }
  out
})


# gst ---------------------------------------------------------------------

#' @describeIn analysis The fold increase in ECAR or PER following oligomycin.
setGeneric("gst", function(x) standardGeneric("gst"))

#' @rdname analysis
#' @export
setMethod("gst", "Seahorse", function(x) {
  out <- x@gst
  if (length(out) == 0) {
    rlang::warn("No glyco stress results available, run analyze()")
    return(invisible(list()))
  }
  out
})

#' @rdname analysis
#' @export
setMethod("gst", "Herd", function(x) {
  out <- x@gst
  if (length(out) == 0) {
    rlang::warn(
      "No glyco stress results available, analyze() component experiments"
    )
    return(invisible(list()))
  }
  out
})


# atp ---------------------------------------------------------------------

#' @describeIn analysis The ATP production rates from oxidative phosphorylation
#'     (`mito`) and glycolysis (`glyco`).
setGeneric("atp", function(x, ...) standardGeneric("atp"))

#' @rdname analysis
#' @param format Three output options are available. For `table`, ATP production
#'     rates are returned in table format. For `scatter`, the data are formatted
#'     for an *x*-*y* scatter plot. For `bar`, the data return as a list of
#'     summarized data and points for a stacked bar plot.
#' @export
setMethod("atp", "Seahorse", function(x, format = c("table", "scatter", "bar")) {
  format <- rlang::arg_match(format)
  get_atp(x, format)
})

#' @rdname analysis
#' @export
setMethod("atp", "Herd", function(x, format = c("table", "scatter", "bar")) {
  format <- rlang::arg_match(format)
  get_atp(x, format)
})

get_atp <- function(x, format) {
  if (length(x@atp) == 0) {
    rlang::warn("No ATP production rates available, run analyze()")
    return(invisible(list()))
  }

  switch(
    format,
    table = {
      out <- x@atp
      out
    },
    scatter = {
      out <-
        x@atp |>
        tidyr::pivot_wider(
          names_from = "rate",
          values_from = "value"
        ) |>
        dplyr::group_by(.data$group) |>
        dplyr::summarise(
          mito = ggplot2::mean_se(.data$mito),
          glyco = ggplot2::mean_se(.data$glyco)
        )
      out
    },

    bar = {
      means <-
        x@atp |>
        tidyr::pivot_wider(
          names_from = "rate",
          values_from = "value"
        ) |>
        dplyr::group_by(.data$group) |>
        dplyr::summarize(
          mito = ggplot2::mean_se(.data$mito),
          glyco = ggplot2::mean_se(.data$glyco)
        ) |>
        tidyr::unnest_wider(
          c("mito", "glyco"),
          names_sep = "_"
        ) |>
        dplyr::mutate(
          dplyr::across(tidyselect::contains("glyco_ym"), \(x) .data$mito_y + x)
        ) |>
        tidyr::pivot_longer(
          -"group",
          names_to = c("rate", "measure"),
          names_sep = "_"
        ) |>
        tidyr::pivot_wider(names_from = "measure", values_from = "value")

      pts <-
        x@atp |>
        dplyr::left_join(
          means |>
            dplyr::filter(.data$rate == "mito") |>
            dplyr::select("group", "rate", "y"),
          by = "group"
        ) |>
        dplyr::mutate(
          value = ifelse(
            .data$rate.x == .data$rate.y,
            .data$value,
            .data$value + .data$y
          )
        )

      out <- list(means = means, pts = pts)
      out
    }
  )
  out
}

