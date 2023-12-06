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
    wells =    "list"
  )
)


# constructor -------------------------------------------------------------

Seahorse <- function(
    path,
    wells = list()
) {
  methods::new(
    "Seahorse",
    path = path,
    wells = wells
  )
}


# initialize --------------------------------------------------------------

setMethod("initialize", "Seahorse", function(
    .Object,
    path,
    wells
) {
  .Object@path <- path
  .Object@filename <- sub("\\.xlsx", "", basename(path))
  .Object@time <- lubridate::mdy_hms(get_cell(path, "Operation Log", "D2"))
  .Object@config <- init_config(path)
  .Object@raw <- init_raw(path, .Object@config)
  .Object@wells <- init_wells(wells, .Object)
  methods::validObject(.Object)
  .Object
}
)


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

  if (is.null(msg)) TRUE else msg
})

