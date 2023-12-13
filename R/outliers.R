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
    if (!all(names(value) %in% c("rate", "well"))) {
      rlang::abort(
        "Outliers data.frame column names must be 'rate' and 'well'"
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
  blanks_to_outliers <- dplyr::intersect(added, blanks(x))
  if (nrow(blanks_to_outliers) > 0) {
    rlang::inform(
      c(
        "Moving these blank wells to outliers:\n",
        print_df(blanks_to_outliers), "\n"
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
        "Moving these outlier wells to blanks:\n",
        print_df(outliers_to_blanks), "\n"
      )
    )
    x <-
      `blanks<-`(x, "add", value = outliers_to_blanks) |>
      suppressWarnings() |>
      suppressMessages()
  }

  x
})


setGeneric("findOut", function(x, ...) standardGeneric("findOut"))

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
