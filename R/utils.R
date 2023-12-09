# utils.R

`%nin%` <- function(x, table) {
  !match(x, table, nomatch = 0L) > 0L
}


print_df <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}


msd <- function(x, n = 3) {
  abs(x - stats::median(x)) / stats::mad(x) > n
}


summarise_to_well <- function(x, measurements) {
  x |>
    dplyr::group_by(.data$rate, .data$well) |>
    dplyr::summarise(total = all(measurements %in% .data$measurement)) |>
    dplyr::filter(.data$total) |>
    dplyr::select(-"total")
}
