# utils.R

`%nin%` <- function(x, table) {
  !match(x, table, nomatch = 0L) > 0L
}


print_df <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

print_wells <- function(x) {
  x |>
    dplyr::group_by(.data$rate) |>
    dplyr::summarise(z = list(.data$well)) |>
    tibble::deframe() |>
    purrr::imap_chr(
      \(x, nm) if (length(x) > 0 ) {
        glue::glue(
          "- {ifelse(nm != 'ECAR', paste0(nm, ' '), nm)}:  ",
          "{glue::glue_collapse(x, sep = ' ')}\n"
        )
      }
    )
}

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

msd <- function(x, n = 3) {
  abs(x - stats::median(x)) / stats::mad(x) > n
}

remove_outliers <- function(x, y, groups, n = 3) {
  x |>
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(groups))
    ) |>
    dplyr::mutate(outliers = msd(.data[[y]], n)) |>
    dplyr::filter(!.data$outliers) |>
    dplyr::select(-"outliers")
}
