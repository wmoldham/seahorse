# Seahorse-class.R


# class -------------------------------------------------------------------

setClass(
  "Seahorse",
  slots = c(
    path =     "character",
    filename = "character",
    time =     "POSIXct",
    config =   "list",
    raw =      "list",
    wells =    "list",
    stages =   "list",
    cells =    "list",
    units =    "character",
    bf =       "numeric",
    cf =       "numeric",
    blanks =   "list",
    O2 =       "list",
    OCR =      "list",
    pH =       "list",
    ECAR =     "list",
    outliers = "list"
  )
)


# constructor -------------------------------------------------------------

Seahorse <- function(
    path,
    wells =  list(),
    stages = list(),
    cells =  list(),
    units =  NA_character_,
    bf =     NA_real_,
    cf =     NA_real_
) {
  methods::new(
    "Seahorse",
    path =   path,
    wells =  wells,
    stages = stages,
    cells =  cells,
    units =  units,
    bf =     bf,
    cf =     cf
  )
}


# initialize --------------------------------------------------------------

setMethod("initialize", "Seahorse", function(
    .Object,
    path,
    wells,
    stages,
    cells,
    units,
    bf,
    cf
) {
  .Object@path <- path
  .Object@filename <- sub("\\.xlsx", "", basename(path))
  .Object@time <- lubridate::mdy_hms(get_cell(path, "Operation Log", "D2"))
  .Object@config <- init_config(path)
  .Object@raw <- init_raw(path, .Object@config)
  .Object@wells <- init_wells(wells, .Object)
  .Object@stages <- init_stages(stages, .Object)
  .Object@cells <- init_cells(cells, .Object)
  .Object@units <- units
  .Object@bf <- bf
  .Object@cf <- cf
  .Object@blanks <- init_blanks(.Object@wells)
  methods::validObject(.Object)

  .Object@O2 <- level_O2(.Object@raw, .Object@config, .Object@blanks)
  .Object@OCR <- rate_O2(.Object@O2, .Object@config)
  .Object@pH <- level_pH(.Object@raw, .Object@config)
  .Object@ECAR <- rate_pH(.Object@pH, .Object@blanks)
  .Object@outliers <- init_outliers(.Object)
  outliers(.Object, "replace") <- find_blank_outliers(.Object)

  .Object
})


# validate ----------------------------------------------------------------

methods::setValidity("Seahorse", function(object) {
  msg <- NULL

  raw <- object@raw

  # wells
  wells <- object@wells
  if ("well" %nin% colnames(wells)) {
    msg <- c(msg, "Wells must contain a column named 'well'")
  }
  if ("type" %nin% colnames(wells)) {
    msg <- c(msg, "Wells must contain a column named 'type'")
  }
  if ("group" %nin% colnames(wells)) {
    msg <- c(msg, "Wells must contain a column named 'group'")
  }
  if (!all(stringr::str_detect(wells[["well"]], "^[A-Z]\\d{2}$"))) {
    msg <- c(msg, "Wells column 'well' must match the pattern 'A01'")
  }
  if (!all(raw[["well"]] %in% wells[["well"]])) {
    msg <- c(msg, "Wells column 'well' must contain one row for each sample well")
  }
  if (!all(wells[["well"]] %in% raw[["well"]])) {
    msg <- c(msg, "Wells column 'well' contains rows not in the experiment")
  }
  if (any(duplicated(wells[["well"]]))) {
    msg <- c(msg, "Wells column 'well' contains duplicates")
  }
  if (!all(wells[["type"]] %in% c("blank", "sample", "hypoxia"))) {
    msg <- c(msg, "Wells column 'type' must contain only: 'blank', 'sample', 'hypoxia'")
  }

  # stages
  stages <- object@stages
  if ("measurement" %nin% colnames(stages)) {
    msg <- c(msg, "Stages must contain a column named 'measurement'")
  }
  if ("stage" %nin% colnames(stages)) {
    msg <- c(msg, "Stages must contain a column named 'stage'")
  }
  if (!(all(unique(raw[["measurement"]]) %in% stages[["measurement"]]))) {
    msg <- c(msg, "Each measurement must have a stage")
  }
  if (!(all(unique(stages[["measurement"]]) %in% raw[["measurement"]]))) {
    msg <- c(msg, "Stages 'measurement' has values missing from the experiment")
  }
  if (!(all(unique(raw[["well"]]) %in% stages[["well"]]))) {
    msg <- c(msg, "Stages must contain an entry for each well")
  }
  if (!(all(unique(stages[["well"]]) %in% raw[["well"]]))) {
    msg <- c(msg, "Stages 'well' has values missing from the experiment")
  }

  # cells
  cells <- object@cells
  if ("well" %nin% colnames(cells)) {
    msg <- c(msg, "Cells must contain a column named 'well'")
  }
  if ("value" %nin% colnames(cells)) {
    msg <- c(msg, "Cells must contain a column named 'value'")
  }
  if (!all(stringr::str_detect(cells[["well"]], "^[A-Z]\\d{2}$"))) {
    msg <- c(msg, "Cells column 'well' must match the pattern 'A01'")
  }
  if (!all(raw[["well"]] %in% cells[["well"]])) {
    msg <- c(msg, "Cells must contain an entry for each well")
  }
  if (!all(cells[["well"]] %in% raw[["well"]])) {
    msg <- c(msg, "Cells 'well' has values missing from the experiment")
  }
  if (any(duplicated(cells[["well"]]))) {
    msg <- c(msg, "Cells column 'well' contains duplicates")
  }

  # bf format
  bf <- object@bf
  if (!is.na(bf) && bf <= 0) {
    msg <- c(msg, "Buffer factor must be positive")
  }

  # cf format
  cf <- object@cf
  if (!is.na(cf) && cf <= 0) {
    msg <- c(msg, "CO2 correction factor must be positive")
  }

  if (is.null(msg)) TRUE else msg
})


# show --------------------------------------------------------------------

setMethod("show", "Seahorse", function(object) {
  cat("\n")
  cat("Seahorse Experiment\n")
  cat("File:   ", object@filename, "\n")
  cat("Time:   ", as.character(object@time), "\n")
  # cat("Blanks ----------------------\n")
  # cat("- OCR:  ", object@blanks$OCR, "\n")
  # cat("- ECAR: ", object@blanks$ECAR, "\n")
  # cat("Outliers --------------------\n")
  # cat("- OCR:  ", object@outliers$OCR, "\n")
  # cat("- ECAR: ", object@outliers$ECAR, "\n")
  # cat("Analyses --------------------\n")
  # analyses <- list()
  # if (length(object@mst) != 0) {
  #   analyses <- c("- Mitochondrial stress\n")
  # }
  # if (length(object@gst) != 0) {
  #   analyses <- c(analyses, "- Glycolysis stress\n")
  # }
  # if (length(object@atp) != 0) {
  #   analyses <- c(analyses, "- ATP production\n")
  # }
  # if (length(analyses) == 0) {
  #   cat("- None\n")
  # } else {
  #   purrr::map(analyses, cat)
  # }
  cat("\n")
})
