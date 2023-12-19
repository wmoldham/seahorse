# Herd-class.R


# class -------------------------------------------------------------------

setClass(
  "Herd",
  slots = c(
    experiments = "character",
    rates =       "list",
    outliers =    "data.frame",
    summary =     "list",
    mst =         "list",
    gst =         "list",
    atp =         "list",
    units =       "character"
  )
)


# constructor -------------------------------------------------------------

#' Herd class constructor
#'
#' `Herd` is an S4 container for summarized data from multiple `Seahorse`
#' experiments. These `Seahorse` experiments should be completely analyzed
#' prior to creating a `Herd` object, including data normalization and outlier
#' identification. The combined data automatically exclude blank and outlier
#' wells from individual experiments. As the data are aggregated, outlying
#' points are excluded based on median absolute deviation and removed prior to
#' calculating the average for an experiment.
#'
#' @param ... A list of `Seahorse` objects.
#'
#' @export
#' @rdname Herd-class
#' @aliases Herd Herd-class
#'
#' @examples
#' Herd(harold, horsea, sheldon, storm)
Herd <- function(...) {
  methods::new("Herd", ...)
}


# initialize --------------------------------------------------------------

setMethod("initialize", "Herd", function(.Object, ...) {
  members <- unlist(list(...))
  if (!all(purrr::map(members, methods::is) == "Seahorse")) {
    rlang::abort(
      "A Herd must contain only Seahorse objects"
    )
  }

  .Object@experiments <-
    purrr::map_chr(
      members,
      methods::slot,
      name = "filename"
    )
  names(members) <- .Object@experiments

  .Object@rates <-
    map_condense(
      members,
      \(x) rates(x, blanks = FALSE, outliers = FALSE, normalize = TRUE),
    )

  .Object@outliers <-
    tibble::tibble(
      rate = factor(),
      experiment = character(),
      group = character()
    )
  .Object@summary <- map_condense(members, \(x) summary(x))
  suppressWarnings({
    .Object@mst <- map_condense(members, mst)
    .Object@gst <- map_condense(members, gst)
    .Object@atp <- map_condense(members, atp)
  })
  .Object@units <- purrr::map(members, units)[[1]]
  .Object
})

condense <- function(df, rm_outliers = TRUE) {
  if (length(df) == 0) {
    return(invisible(list()))
  }
  df |>
    dplyr::select(-tidyselect::any_of("type")) |>
    dplyr::group_by(dplyr::across(-c("well", "value"))) |>
    dplyr::summarise(value = mean(.data$value, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$rate, .data$experiment)
}

map_condense <- function(ls, fn, ...) {
  purrr::map_dfr(ls, fn, .id = "experiment") |>
    condense(...)
}


# show --------------------------------------------------------------------

setMethod("show", "Herd", function(object) {
  cat("\n")
  cat("Herd of Seahorse experiments\n")

  cat("Files ----------------------\n")
  purrr::map(
    object@experiments,
    \(x) cat("-", x, "\n")
  )

  cat("Outliers -------------------\n")
  if (nrow(object@outliers) == 0) {
    cat("- None\n")
  } else {
    print(object@outliers)
  }

  cat("Analyses -------------------\n")
  analyses <- list()
  if (length(object@mst) != 0) {
    analyses <- c("- Mitochondrial stress\n")
  }
  if (length(object@gst) != 0) {
    analyses <- c(analyses, "- Glycolysis stress\n")
  }
  if (length(object@atp) != 0) {
    analyses <- c(analyses, "- ATP production\n")
  }
  if (length(analyses) == 0) {
    cat("- None\n")
  } else {
    purrr::map(analyses, cat)
  }
  cat("\n")
})

