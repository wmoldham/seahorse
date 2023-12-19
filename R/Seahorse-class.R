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
    outliers = "list",
    summary =  "list",
    mst =      "list",
    gst =      "list",
    atp =      "list"
  )
)


# constructor -------------------------------------------------------------

#' Seahorse class constructor
#'
#' `Seahorse` is an S4 container used to store the calibration values and raw
#' data from a single Seahorse assay; to calculate oxygen consumption,
#' extracellular acidification, and proton efflux rates from the raw data; and
#' to facilitate outlier detection, data analysis, and visualization.
#'
#' @param path Character vector for the Seahorse `xlsx` file path.
#' @param wells An optional list or data frame containing vectors of
#'     experimental descriptors for each of the sample wells. At minimum,
#'     `wells` must contain character vectors named `well` and `type`. `well`
#'     must contain a unique entry for each well in the experimental plate.
#'     `type` must be `sample` or `blank` for each well. Users should consider
#'     a `group` column that labels unique experimental groups. It is probably
#'     worthwhile to convert this column to a factor based on the preferred
#'     ordering of experimental groups. Additional descriptors may be included
#'     (*e.g.*, `treatment`, `time`, `condition`). If a `group` vector was not
#'     provided, one will be generated from `type` or a combination of the
#'     remaining descriptors.
#' @param stages An optional list or data frame containing vectors of
#'     experimental descriptors for each of the measurement stages. `stages`
#'     must contain character vectors named `measurement` and `stage`.
#'     `measurement` must contain a unique entry for each measurement in the
#'     experiment. `stage` describes the experimental interval (*e.g.*, for a
#'     typical mitochondrial stress assay, `stage` would be `basal`, `oligo`,
#'     `fccp`, or `rot/ama`). A `well` column may also be included if the
#'     injections differ among treatment wells.
#' @param cells An optional list or data frame containing vectors of
#'     normalization data for an experiment. If provided, `cells` must contain
#'     a character vector named `well` and a numeric vector named `value`.
#'     `well` must contain a unique entry for each well in the experimental
#'     plate. `value` contains the normalization factor (*e.g.*, cell number or
#'     protein content).
#' @param units An optional character vector describing the units of the
#'     normalization factor provided in the `value`column of `cells`. This will
#'     be used to annotate graphs.
#' @param bf An optional length-one numeric vector containing the buffer factor
#'     of the medium. This value is used to calculate the proton efflux rate
#'     (PER) from the extracellular acidification rate (ECAR).
#' @param cf An optional length-one numeric vector describing the carbon dioxide
#'     correction factor for the cells in the assay. This parameter is used to
#'     separate the mitochondrial and glycolytic contributions to the proton
#'     efflux rate.
#'
#' @details During creation of the Seahore object, raw fluorescence readings
#'     are converted to O2 and pH measurements from which oxygen consumptions
#'     and extracellular acidification rates are calculated. Outlying wells are
#'     identified and removed.
#'
#' @seealso [helpers], [format_cells], [accessors], [blanks], [outliers], [plot]
#'
#' @export
#'
#' @examples
#' path <- system.file("extdata/raw_1.xlsx", package = "seahorse", mustWork = TRUE)
#' wells <- wells_ex
#' stages <- stages_mst
#' cells <- list(well = wells_24, value = 20000)
#' Seahorse(path, wells, stages, cells, units = "cell", bf = 2.4, cf = 0.41)
#'
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
  .Object@outliers <- tibble::tibble(rate = factor(), well = character())
  out <- findOut(.Object)
  if (nrow(out) != 0) {
    outliers(.Object, "add") <- out
  }
  suppressMessages({
    .Object <- analyze(.Object)
  })
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
  cat("Seahorse Experiment ---------\n")
  cat("- File: ", object@filename, "\n")
  cat("- Time: ", as.character(object@time), "\n")
  cat("Blanks ----------------------\n")
  cat(print_wells(object@blanks), sep = "\n")
  cat("Outliers --------------------\n")
  cat(print_wells(object@outliers), sep = "\n")
  cat("Analyses --------------------\n")
  analyses <- list()
  if (length(object@mst) != 0) {
    analyses <- c(analyses, "- Mitochondrial stress\n")
  }
  if (length(object@gst) != 0) {
    analyses <- c(analyses, "- Glycolysis stress\n")
  }
  if (length(object@atp) != 0) {
    analyses <- c(analyses, "- ATP production\n")
  }
  if (length(analyses) == 0) {
    cat("- None\n")
  } else {
    purrr::map(analyses, cat)
  }
  cat("\n")
})
