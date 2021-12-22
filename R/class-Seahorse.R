# class-Seahorse.R

setClass(
  "Seahorse",
  slots = c(
    path = "character",
    filename = "character",
    time = "POSIXct",
    config = "list",
    raw = "list",
    wells = "list",
    stages = "list",
    cells = "list",
    unit = "character",
    bf = "numeric",
    cf = "numeric",
    env = "environment"
  ),
  prototype = list(
    path = NA_character_,
    filename = NA_character_,
    time = NA_real_,
    config = list(),
    raw = list(),
    wells = list(),
    stages = list(),
    cells = list(),
    unit = NA_character_,
    bf = NA_real_,
    cf = NA_real_
  )
)

#' @export
Seahorse <- function(
  path,
  wells = list(),
  stages = list(),
  cells = list(),
  unit = NA_character_,
  bf = NA_real_,
  cf = NA_real_
){
  methods::new(
    "Seahorse",
    path = path,
    wells = wells,
    stages = stages,
    cells = cells,
    unit = unit,
    bf = bf,
    cf = cf
  )
}

setMethod(
  "initialize",
  "Seahorse",
  function(.Object, path, wells, stages, cells, unit, bf, cf)
  {
    .Object@path <- path
    .Object@filename <- sub("\\.xlsx", "", basename(path))
    .Object@time <- lubridate::mdy_hms(get_cell(path, "Operation Log", "D2"))
    .Object@config <- init_config(path)
    .Object@raw <- init_raw(path, .Object@config)
    .Object@env <- new.env(parent = emptyenv())

    if (length(wells) == 0) {
      well_list <- unique(.Object@raw[["well"]])
      .Object@wells <-
        tibble::tibble(
          well = well_list,
          type = rep("sample", length(well_list)),
          group = factor(.data$type)
        )
      .Object@env$blanks <- list(OCR = NA_character_, ECAR = NA_character_)
    } else {
      .Object@wells <- init_wells(wells)
      .Object@env$blanks <- init_blanks(.Object@wells)
    }

    if (length(stages) == 0) {
      measure_list <- unique(.Object@raw[["measurement"]])
      .Object@stages <-
        tibble::tibble(
          measurement = measure_list,
          stage = rep("basal", length(measure_list))
        )
    } else {
      .Object@stages <- tibble::as_tibble(stages)
    }

    if (length(cells) == 0) {
      .Object@cells<- tibble::tibble(well = unique(.Object@raw[["well"]]), value = 1)
    } else {
      .Object@cells <- init_cells(cells)
    }
    .Object@unit <- unit

    .Object@bf <- bf
    .Object@cf <- cf

    .Object@env$O2_level <- level_O2(.Object@raw, .Object@config, .Object@env$blanks)
    .Object@env$OCR <- rate_O2(.Object@env$O2_level, .Object@config)

    .Object@env$pH_level <- level_pH(.Object@raw, .Object@config)
    .Object@env$ECAR <- rate_pH(.Object@env$pH_level, .Object@env$blanks)

    methods::validObject(.Object)
    .Object
  }
)

methods::setValidity(
  "Seahorse",
  function(object)
  {
    msg <- NULL

    path <- object@path
    wells <- object@wells
    blanks <- object@env$blanks
    raw <- object@raw
    stages <- object@stages
    cells <- object@cells
    bf <- object@bf
    cf <- object@cf

    # path
    if (!file.exists(path)) {
      msg <- "File does not exist"
    }

    # wells
    if (length(wells) != 0) {
      if (!("well" %in% colnames(wells))) {
        msg <- c(msg, "Wells must contain a column named 'well'")
      }

      if (!("type" %in% colnames(wells))) {
        msg <- c(msg, "Wells must contain a column named 'type'")
      }

      if (!("group" %in% colnames(wells))) {
        msg <- c(msg, "Wells must contain a column named 'group'")
      }

      if (!all(stringr::str_detect(wells[["well"]], "^[A-Z]\\d{2}$"))) {
        msg <- c(msg, "Wells column 'well' must match the pattern 'A01'")
      }

      if (!all(raw[["well"]] %in% wells[["well"]])) {
        msg <- c(msg, "Wells column must contain one row for each well")
      }

      if (any(duplicated(wells[["well"]]))) {
        msg <- c(msg, "Duplicate well identifications identified")
      }

      if (!all(wells[["type"]] %in% c("blank", "sample", "hypoxia"))) {
        msg <- c(msg, "Wells column 'type' must contain only: 'blank', 'sample', 'hypoxia'")
      }
    }

    # blanks
    if (length(blanks) != 2) {
      msg <- c(msg, "Blanks must be a list of two vectors of well IDs")
    }
    if (!all(names(blanks) %in% c("OCR", "ECAR"))) {
      msg <- c(msg, "Names of blank lists must be 'OCR' and 'ECAR'")
    }
    if (!(all(is.na(blanks)))) {
      if (!all(is.na(unlist(blanks)) | stringr::str_detect(unlist(blanks), "^[A-Z]\\d{2}$"))) {
        msg <- c(msg, "Blanks must match the pattern 'A01'")
      }
    }

    # stages
    if (length(stages) != 0) {
      if (!("measurement" %in% colnames(stages))) {
        msg <- c(msg, "Stages must contain a column named 'measurement'")
      }
      if (!("stage" %in% colnames(stages))) {
        msg <- c(msg, "Stages must contain a column named 'stage'")
      }
      if (!(all(unique(raw[["measurement"]]) %in% stages[["measurement"]]))) {
        msg <- c(msg, "Stages must contain an entry for each measurement")
      }
      if (any(colnames(stages) == "well")){
        if (!(all(unique(raw[["well"]]) %in% stages[["well"]]))) {
          msg <- c(msg, "Stages must contain an entry for each well or for no wells")
        }
      }
    }

    # cells
    if (length(cells) != 0) {
      if (!("well" %in% colnames(cells))) {
        msg <- c(msg, "Norm must contain a column named 'well'")
      }

      if (!("value" %in% colnames(cells))) {
        msg <- c(msg, "Norm must contain a column named 'value'")
      }

      if (!all(stringr::str_detect(cells[["well"]], "^[A-Z]\\d{2}$"))) {
        msg <- c(msg, "Norm column 'well' must match the pattern 'A01'")
      }
    }

    # bf format
    if (!is.na(bf) && bf < 0) {
      msg <- c(msg, "Buffer factor must be positive")
    }

    # cf format
    if (!is.na(cf) && cf < 0) {
      msg <- c(msg, "CO2 correction factor must be positive")
    }

    if (is.null(msg)) TRUE else msg
  }
)



#' @param x A `Seahorse` object
#' @describeIn Seahorse-class Getter for 'wells' slot
#' @export
setMethod("wells", "Seahorse", function(x) x@wells)

#' @describeIn Seahorse-class Setter for 'wells' slot
#' @param x A `Seahorse` object
#' @param value Replacement value
#' @export
setMethod("wells<-", "Seahorse", function(x, value) {
  value <- init_wells(value)
  x@wells <- value
  methods::validObject(x)
  x
})

#' @param x A `Seahorse` object
#' @describeIn Seahorse-class Getter for 'stages' slot
#' @export
setMethod("stages", "Seahorse", function(x) x@stages)

#' @describeIn Seahorse-class Setter for 'stages' slot
#' @param x A `Seahorse` object
#' @param value Replacement value
#' @export
setMethod("stages<-", "Seahorse", function(x, value) {
  if(!is.data.frame(value)) value <- tibble::as_tibble(value)
  x@stages <- value
  methods::validObject(x)
  x
})

#' @param x A `Seahorse` object
#' @describeIn Seahorse-class Getter for 'cells' slot
#' @export
setMethod("cells", "Seahorse", function(x) x@cells)

#' @describeIn Seahorse-class Setter for 'cells' slot
#' @param x A `Seahorse` object
#' @param value Replacement value
#' @export
setMethod("cells<-", "Seahorse", function(x, value) {
  x@cells <- value
  methods::validObject(x)
  x
})

#' @param x A `Seahorse` object
#' @describeIn Seahorse-class Getter for 'unit' slot
#' @export
setMethod("unit", "Seahorse", function(x) x@unit)

#' @describeIn Seahorse-class Setter for 'unit' slot
#' @param x A `Seahorse` object
#' @param value Replacement value
#' @export
setMethod("unit<-", "Seahorse", function(x, value) {
  x@unit <- value
  methods::validObject(x)
  x
})

#' @param x A `Seahorse` object
#' @describeIn Seahorse-class Getter for 'bf' slot
#' @export
setMethod("bf", "Seahorse", function(x) x@bf)

#' @describeIn Seahorse-class Setter for 'bf' slot
#' @param x A `Seahorse` object
#' @param value Replacement value
#' @export
setMethod("bf<-", "Seahorse", function(x, value) {
  x@bf <- value
  methods::validObject(x)
  x
})

#' @param x A `Seahorse` object
#' @describeIn Seahorse-class Getter for 'cf' slot
#' @export
setMethod("cf", "Seahorse", function(x) x@cf)

#' @describeIn Seahorse-class Setter for 'cf' slot
#' @param x A `Seahorse` object
#' @param value Replacement value
#' @export
setMethod("cf<-", "Seahorse", function(x, value) {
  x@cf <- value
  methods::validObject(x)
  x
})
