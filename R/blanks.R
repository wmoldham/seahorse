# blanks.R

setGeneric("blanks", function(x) standardGeneric("blanks"))
setGeneric("blanks<-", function(x, ..., value = NA) standardGeneric("blanks<-"))

setMethod("blanks", "Seahorse", function(x) {
  cat(print_wells(x@blanks), sep = "\n")
  invisible(x@blanks)
})



setMethod("blanks<-", "Seahorse", function(
    x,
    action = c("remove", "reset", "replace", "add", "subtract"),
    value = NA
) {
  old_values <- x@blanks
  action <- rlang::arg_match(action)

  # checks
  input_matters <- all(action %in% c("replace", "add", "subtract"))
  if (!input_matters) {
    if(!is.na(value)) {
      rlang::warn("Replacement values provided but ignored")
    }
  } else {
    if (!any(class(value) %in% c("list", "data.frame"))) {
      rlang::abort("'value' must be a list or data.frame")
    }

    if ("data.frame" %in% class(value)) {
      if (!all(names(value) %in% c("rate", "well"))) {
        rlang::abort(
          "Blanks data.frames must contain 'rate' and 'well' columns"
        )
      }
      if (all(value$rate %nin% c("OCR", "ECAR"))) {
        rlang::abort(
          "Blanks data.frame rate column must contain only 'OCR' or 'ECAR'"
        )
      }
      wells <- value$well
      if (!all(stringr::str_detect(wells, "^[A-Z]\\d{2}$") | is.na(wells))) {
        rlang::abort("Blanks values must match the pattern 'A01'")
      }
      value <-
        tibble::tibble(value) |>
        dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "ECAR")))

    } else if ("list" %in% class(value)) {
      if (!all(names(value) %in% c("OCR", "ECAR"))) {
        rlang::abort("Blanks list vectors must be named 'OCR' or 'ECAR'")
      }
      wells <- unlist(value)
      if (!all(stringr::str_detect(wells, "^[A-Z]\\d{2}$") | is.na(wells))) {
        rlang::abort("Blanks values must match the pattern 'A01'")
      }
      value <-
        tibble::enframe(value, name = "rate", value = "well") |>
        tidyr::unnest(c("well")) |>
        dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "ECAR")))
    }
  }

  # generate new values
  switch(
    action,
    remove = {
      new_values <- tibble::tibble(rate = character(), well = character())
    },
    reset = {
      new_values <- init_blanks(x@wells)
    },
    replace = {
      new_values <- value
    },
    add = {
      overlap <- dplyr::intersect(old_values, value)
      if (nrow(overlap) > 0 ) {
        rlang::inform(
          c(
            "These wells are currently blanks:\n",
            print_df(overlap), "\n"
          )
        )
      }
      new_values <-
        dplyr::bind_rows(old_values, value) |>
        dplyr::distinct()
    },
    subtract = {
      overlap <- dplyr::setdiff(value, old_values)
      if (nrow(overlap) > 0) {
        rlang::inform(
          c(
            "These wells are not currently blanks:\n",
            print_df(overlap), "\n"
          )
        )
      }
      new_values <-
        dplyr::anti_join(old_values, value, by = c("rate", "well"))
    }
  )

  x@blanks <-
    new_values |>
    dplyr::mutate(rate = factor(.data$rate, levels = c("OCR", "ECAR"))) |>
    dplyr::arrange(.data$rate, .data$well)

  # check differences
  if (dplyr::setequal(old_values, new_values)) {
    rlang::warn("Blanks unchanged")
  }
  added <- dplyr::setdiff(new_values, old_values)
  removed <- dplyr::setdiff(old_values, new_values)

  # blanks in outliers
  blanks_in_outliers <- dplyr::intersect(added, x@outliers)
  if (nrow(blanks_in_outliers) > 0) {
    rlang::inform(
      c(
        "\nMoving these outlier values to blanks:",
        print_wells(blanks_in_outliers), "\n"
      )
    )
    x <-
      `outliers<-`(x, "subtract", value = added) |>
      suppressWarnings() |>
      suppressMessages()
  }

  # move blank to outliers
  if (nrow(removed) > 0) {
    rlang::inform(
      c(
        "\nMoving these blank wells to outliers:",
        print_wells(removed), "\n"
      )
    )
    x <-
      `outliers<-`(x, "add", value = removed) |>
      suppressWarnings() |>
      suppressMessages()
  }

  # update calculations
  ocr_changed <- any(c(added$rate, removed$rate) %in% "OCR")
  ecar_changed <- any(c(added$rate, removed$rate) %in% "ECAR")
  if (ocr_changed) {
    x@O2 <- level_O2(x@raw, x@config, x@blanks)
    x@OCR <- rate_O2(x@O2, x@config)
  }
  if (ecar_changed) {
    x@ECAR <- rate_pH(x@pH, x@blanks)
  }

  x
})
