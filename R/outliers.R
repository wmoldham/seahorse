# outliers.R

setGeneric("outliers", function(x) standardGeneric("outliers"))
setGeneric("outliers<-", function(x, ..., value = NA) standardGeneric("outliers<-"))

setMethod("outliers", "Seahorse", function(x) x@outliers)

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
    if (!all(names(value) %in% c("rate", "well", "measurement"))) {
      rlang::abort(
        "Outliers data.frame column names must be 'rate', 'well', and 'measurement'"
      )
    }
    if (all(value$rate %nin% c("OCR", "ECAR"))) {
      rlang::abort(
        "Outliers data.frame rate column must contain only 'OCR' or 'ECAR'"
      )
    }
    wells <- value$well
    if (!all(stringr::str_detect(wells, "^[A-Z]\\d{2}$") | is.na(wells))) {
      rlang::abort("Outliers well values must match the pattern 'A01'")
    }
    value <-
      tibble::tibble(value) |>
      dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "ECAR")))
    nms <- names(value)
  }

  # generate new values
  switch(
    action,
    remove = {
      new_values <- dplyr::mutate(x@outliers, outlier = FALSE)
    },
    reset = {
      new_values <- init_outliers(x)
    },
    replace = {
      old_values_reset <-
        x@outliers |>
        dplyr::mutate(outlier = FALSE)
      replacements <-
        dplyr::left_join(value, old_values_reset, by = nms) |>
        dplyr::mutate(outlier = TRUE)
      new_values <-
        dplyr::anti_join(old_values_reset, value, by = nms) |>
        dplyr::bind_rows(replacements)
    },
    add = {
      old_values <- x@outliers
      replacements <-
        dplyr::left_join(value, old_values, by = nms) |>
        dplyr::mutate(outlier = TRUE)

      overlap <-
        dplyr::intersect(old_values, replacements)
      if (nrow(overlap) > 0 ) {
        rlang::inform(
          c(
            "These wells are currently outliers:\n\n",
            print_df(overlap), "\n"
          )
        )
      }
      new_values <-
        dplyr::anti_join(old_values, value, by = nms) |>
        dplyr::bind_rows(replacements) |>
        dplyr::distinct()
    },
    subtract = {
      old_values <- x@outliers

      replacements <-
        dplyr::left_join(value, old_values, by = nms) |>
        dplyr::mutate(outlier = FALSE)

      overlap <- dplyr::intersect(replacements, old_values)
      if (nrow(overlap) > 0) {
        rlang::inform(
          c(
            "These wells are not currently outliers:\n\n",
            print_df(overlap), "\n"
          )
        )
      }
      new_values <-
        dplyr::anti_join(old_values, replacements, by = nms) |>
        dplyr::bind_rows(replacements) |>
        dplyr::distinct()
    }
  )
  x@outliers <-
    new_values |>
    dplyr::arrange(.data$rate, .data$measurement, .data$well)

  # check differences
  if (dplyr::setequal(old_values, new_values)) {
    rlang::warn("Outliers unchanged")
  }
  changed <- dplyr::setdiff(new_values, old_values)
  added <- dplyr::filter(changed, .data$outlier)  # false to true
  removed <- dplyr::filter(changed, !.data$outlier)  # true to false

  x
})
