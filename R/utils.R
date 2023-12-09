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
