# utils.R

get_cell <- function(path, sheet, cell)
{
  readxl::read_excel(
    path = path,
    sheet = sheet,
    range = cell,
    col_names = FALSE
  ) %>%
    dplyr::pull() %>%
    suppressMessages()
}
