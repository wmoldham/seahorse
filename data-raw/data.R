# data.R

devtools::load_all()


# data --------------------------------------------------------------------

paths <-
  list.files(
    system.file("extdata", package = "seahorse", mustWork = TRUE),
    pattern = ".xlsx",
    full.names = TRUE
  )

counts <-
  list.files(
    system.file("extdata", package = "seahorse", mustWork = TRUE),
    pattern = ".csv",
    full.names = TRUE
  )


# design ------------------------------------------------------------------

wells_24 <-
  stringr::str_c(
    rep(LETTERS[1:4], each = 6),
    rep(sprintf("%02d", 1:6), 4)
  )

group <- rep(LETTERS[1:4], each = 6)
group[c(1, 10, 15, 24)] <- "blank"
type <- ifelse(group == "blank", "blank", "sample")

wells_ex <- list(well = wells_24, type = type, group = group)

measurement <- 1:12
stages <- c("basal", "oligo", "fccp", "rot/ama")
stage <- rep(stages, each = 3)
stage <- factor(stage, levels = stages)
stages_mst <- list(measurement = measurement, stage = stage)

usethis::use_data(wells_24, stages_mst, wells_ex, overwrite = TRUE)


# normalization -----------------------------------------------------------

cells <- purrr::map(counts, format_cells)


# import ------------------------------------------------------------------

names <- c("harold", "horsea", "sheldon", "storm")

sea <-
  purrr::map2(
    paths,
    cells,
    \(x, y) Seahorse(
      path = x,
      wells = wells_ex,
      stages = stages_mst,
      cells = y,
      bf = 2.4,
      cf = 0.41,
      units = "cell"
    )
  ) |>
  rlang::set_names(names)


# output ------------------------------------------------------------------

purrr::iwalk(
  sea,
  \(x, nm) {
    assign(nm, x)
    do.call("use_data", list(as.name(nm), overwrite = TRUE))
  }
)
