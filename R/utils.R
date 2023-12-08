# utils.R

`%nin%` <- function(x, table) {
  !match(x, table, nomatch = 0L) > 0L
}


print_df <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}
