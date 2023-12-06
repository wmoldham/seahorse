# format_cells.R

#' Import table of normalization values
#'
#' This function will read in a `.csv` file of normalization data.
#'
#' @details
#' The file should be organized the same as the Seahorse microplate. The header
#' row must be `row` followed by the column number formatted as `x01`. The `row`
#' column should be the letter identification for each row:
#'
#' | row | &nbsp;x01 | &nbsp;x02 | &nbsp;x03 | &nbsp;x04 | &nbsp;x05 |&nbsp; x06 |
#' |-----|----------:|----------:|----------:|----------:|----------:|----------:|
#' | A   | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... |
#' | B   | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... |
#' | C   | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... |
#' | D   | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... | &nbsp;... |
#'
#' Example data may also be found in the `extdata` folder of the package source.
#' The average blank value will be subtracted from the sample values.
#'
#' @param path Path to the data file
#' @param blanks A character vector of blank well IDs. If no blanks were
#'     included in the experiment, set to `NA`.
#'
#' @return A `tbl_df` of normalization data
#' @export
#'
#' @examples
#' path <- system.file("extdata/counts_1.csv", package = "seahorse", mustWork = TRUE)
#' format_cells(path, blanks = c("A01", "B04", "C03", "D06"))
#'
format_cells <- function(path, blanks = c("A01", "B04", "C03", "D06")) {
  x <- readr::read_csv(path, show_col_types = FALSE)

  # check format
  if ("row" %nin% names(x)) {
    rlang::abort("Input data file missing a column named 'row'")
  }
  idx <- which(names(x) == "row")
  if (!all(stringr::str_detect(names(x)[-idx], "x\\d{2}"))) {
    rlang::abort("Input data column headers must be formatted as 'x01'")
  }

  if (!all(dplyr::pull(x, idx) %in% LETTERS)) {
    rlang::abort("Input data row names must contain capital letters")
  }

  x |>
    tidyr::pivot_longer(
      -"row",
      names_to = "col",
      values_to = "value"
    ) |>
    dplyr::mutate(col = stringr::str_replace(.data$col, "x", "")) |>
    tidyr::unite("well", "row", "col", sep = "") |>
    {if (!all(is.na(blanks))) {
      \(x) dplyr::mutate(x, value = .data$value - mean(.data$value[which(.data$well %in% blanks)]))
    } else {
      \(x) x
    }}()
}
