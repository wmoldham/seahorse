# report.R

write_report <- function(
    output_path = "~/Desktop/scratch/",
    report_name = paste0(Sys.Date(), "_seahorse-report"),
    title = "Seahorse Analysis Report",
    author = "William Oldham",
    wells = seahorse::wells_ex,
    stages = seahorse::stages_mst,
    cells_files = list.files(system.file("extdata", package = "seahorse", mustWork = TRUE), "counts", full.names = TRUE),
    data_files = list.files(system.file("extdata", package = "seahorse", mustWork = TRUE), "raw", full.names = TRUE),
    units = "cells",
    bf = 2.4,
    cf = 0.41
) {
  if (!dir.exists(output_path)) dir.create(output_path)

  template <-
    system.file(
      "template/template.Rmd",
      package = "seahorse",
      mustWork = TRUE
    ) |>
    readr::read_file()

  actual <-
    template |>
    stringr::str_replace("::title::", title) |>
    stringr::str_replace("::author::", author)

  rmd_name <- paste0(output_path, report_name, ".rmd")
  readr::write_file(actual, rmd_name)

  wells <- data.frame(wells)
  stages <- data.frame(stages)

  rmarkdown::render(
    input = rmd_name,
    params = list(
      wells = wells,
      stages = stages,
      cells = cells_files,
      data = data_files,
      units = units,
      bf = bf,
      cf = cf
    )
  )
}
