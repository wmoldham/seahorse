# outliers.R

#' @include Seahorse-class.R
NULL

# document ----------------------------------------------------------------

#' Outliers accessors for `seahorse` S4 objects
#'
#' Outlier wells may be excluded from analyses. They are identified using the
#' `findOut` method.
#'
#' @param x A `Seahorse` or `Herd` object.
#' @param action Indicates how replacement values modify the existing values:
#'
#' | **action** | &nbsp;&nbsp;&nbsp; **description** |
#' |------------|-----------------------------------|
#' | remove   | &nbsp;&nbsp;&nbsp; set all values to `NA` |
#' | reset   | &nbsp;&nbsp;&nbsp; assign outliers based on `findOut` |
#' | replace | &nbsp;&nbsp;&nbsp; completely replace `outliers` with new values |
#' | add | &nbsp;&nbsp;&nbsp; add new outlier wells |
#' | subtract | &nbsp;&nbsp;&nbsp; remove outlier wells |
#'
#' @param value A data frame with two columns named `rate` and `well`. The
#'     `rate` columns should contain either `OCR` or `ECAR`. The `well` column
#'     identifies the outlier wells formatted as "A01". For the "reset" and
#'     "remove" actions, the assigned value is ignored, but use `NA`.
#' @name outliers
#' @aliases outliers outliers<-
#' @seealso [findOut]
#'
NULL

setGeneric("outliers", function(x) standardGeneric("outliers"))
setGeneric("outliers<-", function(x, ..., value = NA) standardGeneric("outliers<-"))

# getter ------------------------------------------------------------------

#' @export
#' @rdname outliers
#' @returns `outliers(x)` returns a data frame of wells identified as outliers
#'     for OCR and ECAR calculations and plots.
setMethod("outliers", "Seahorse", function(x) {
  cat(print_wells(x@outliers), sep = "\n")
  invisible(x@outliers)
})


# setter ------------------------------------------------------------------

#' @export
#' @rdname outliers
#' @examples
#' outliers(sheldon)
#' outliers(sheldon, "replace") <- findOut(sheldon)
#' outliers(sheldon)
#' outliers(sheldon, "reset") <- NA
#' outliers(sheldon)
#'
setMethod("outliers<-", "Seahorse", function(
    x,
    action = c("remove", "reset", "replace", "add", "subtract"),
    value = NA
) {
  old_values <- x@outliers
  action <- rlang::arg_match(action)

  # checks
  input_matters <- all(action %in% c("replace", "add", "subtract"))
  if (!input_matters) {
    if(!is.na(value)) {
      rlang::warn("Replacement values provided but ignored")
    }
  } else {
    if (!any(class(value) == "data.frame")) {
      rlang::abort("'value' must be a data.frame")
    }
    if (!all(names(value) %in% c("rate", "well"))) {
      rlang::abort(
        "Outliers data.frame column names must be 'rate' and 'well'"
      )
    }
    if (all(value$rate %nin% c("OCR", "ECAR", "PER"))) {
      rlang::abort(
        "Outliers data.frame rate column must contain only 'OCR', 'ECAR', or 'PER'"
      )
    }
    wells <- value$well
    if (!all(stringr::str_detect(wells, "^[A-Z]\\d{2}$") | is.na(wells))) {
      rlang::abort("Outliers well values must match the pattern 'A01'")
    }
    value <-
      tibble::tibble(value) |>
      dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "ECAR", "PER")))
  }

  # generate new values
  switch(
    action,
    remove = {
      new_values <- tibble::tibble(rate = factor(), well = character())
    },
    reset = {
      new_values <- findOut(x)
    },
    replace = {
      new_values <- value
    },
    add = {
      overlap <- dplyr::intersect(old_values, value)
      if (nrow(overlap) > 0) {
        rlang::inform(
          c(
            "These values are already outliers:\n",
            print_df(overlap), "\n"
          )
        )
      }
      new_values <-
        dplyr::bind_rows(old_values, value) |>
        dplyr::distinct()
    },
    subtract = {
      overlap <- dplyr::intersect(old_values, value)
      if (nrow(overlap) == 0) {
        rlang::inform(
          c(
            "These values are not currently outliers:\n",
            print_df(overlap), "\n"
          )
        )
      }
      new_values <-
        dplyr::anti_join(old_values, value, by = c("rate", "well")) |>
        dplyr::distinct()
    }
  )
  x@outliers <-
    new_values |>
    dplyr::arrange(.data$rate, .data$well)

  # check differences
  if (dplyr::setequal(old_values, new_values)) {
    rlang::warn("Outliers unchanged")
  }
  added <- dplyr::setdiff(new_values, old_values)
  removed <- dplyr::setdiff(old_values, new_values)

  # blanks to outliers
  blanks_to_outliers <- dplyr::intersect(added, x@blanks)
  if (nrow(blanks_to_outliers) > 0) {
    rlang::inform(
      c(
        "\nMoving these blank wells to outliers:",
        print_wells(blanks_to_outliers), "\n"
      )
    )
    x <-
      `blanks<-`(x, "subtract", value = blanks_to_outliers) |>
      suppressWarnings() |>
      suppressMessages()
  }

  # outliers to blanks
  original_blanks <- init_blanks(x@wells)
  outliers_to_blanks <- dplyr::intersect(removed, original_blanks)
  if (nrow(outliers_to_blanks) > 0) {
    rlang::inform(
      c(
        "\nMoving these outlier wells to blanks:",
        print_wells(outliers_to_blanks), "\n"
      )
    )
    x <-
      `blanks<-`(x, "add", value = outliers_to_blanks) |>
      suppressWarnings() |>
      suppressMessages()
  }
  # update analysis
  if (!dplyr::setequal(old_values, new_values)) {
    suppressMessages({
      x <- analyze(x)
    })
  }
  x
})


# findOut -----------------------------------------------------------------

#' Remove outlying samples
#'
#' Generic function for removing outliers.
#'
#' @param x An **R** object. Currently there are methods for
#'     [seahorse::Seahorse] and [seahorse::Herd-class] objects.
#' @param ... Reserved for future development
#' @returns Returns a data frame of outlying samples.
setGeneric("findOut", function(x, ...) standardGeneric("findOut"))


#' @export
#' @param blanks Should `findOut` look for outliers in blank samples?
#' @param outliers Should `findOut` include current outliers when looking for
#'     new outliers?
#' @describeIn findOut This method compares OCR and ECAR rates within
#'     experimental groups to identify outlying wells. The function returns a
#'     data frame identifying outlying wells. These wells can then be excluded
#'     from downstream analyses by assigning to the `Seahorse` object using
#'     `outliers<-`.
#' @examples
#' outliers(sheldon, "replace") <- findOut(sheldon)
setMethod("findOut", "Seahorse", function(x, blanks = TRUE, outliers = FALSE, ...) {
  df <-
    rates(x, blanks = TRUE, outliers = FALSE, normalize = TRUE) |>
    dplyr::mutate(
      group = forcats::fct_drop(.data$group)
    )

  if (length(unique(df$stage)) == 1) {
    fo <- stats::formula(value ~ measurement)
  } else {
    fo <- stats::formula(value ~ measurement * stage)
  }

  df |>
    dplyr::group_by(.data$rate, .data$type, .data$group) |>
    tidyr::nest() |>
    dplyr::mutate(
      model = purrr::map(
        .data$data,
        \(x) MASS::rlm(fo, data = x, maxit = 100)
      ),
      fit = purrr::map(.data$model, stats::fitted),
      res = purrr::map(.data$model, stats::residuals),
      se = purrr::map(.data$res, \(x) x ^ 2)
    ) |>
    dplyr::select(-"model") |>
    tidyr::unnest(c("data", "fit", "res", "se")) |>
    dplyr::group_by(.data$well, .add = TRUE) |>
    dplyr::summarise(
      mse = mean(.data$se, na.rm = TRUE)
    ) |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(outlier = msd(.data$mse, n = 4)) |>
    dplyr::filter(.data$outlier) |>
    dplyr::ungroup() |>
    dplyr::select("rate", "well")
})
