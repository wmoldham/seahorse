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
    norm = "list",
    unit = "character"
  ),
  prototype = list(
    path = NA_character_,
    filename = NA_character_,
    time = NA_real_,
    config = list(),
    raw = list(),
    wells = list(),
    stages = list(),
    norm = list(),
    unit = NA_character_
  )
)

#' @export
Seahorse <- function(
  path,
  wells = list(),
  stages = list(),
  norm = list(),
  unit = NA_character_
){
  methods::new(
    "Seahorse",
    path = path,
    wells = wells,
    stages = stages,
    norm = norm,
    unit = unit
  )
}

setMethod(
  "initialize",
  "Seahorse",
  function(.Object, path, wells, stages, norm, unit)
  {
    .Object@path <- path
    .Object@filename <- sub("\\.xlsx", "", basename(path))
    .Object@time <- lubridate::mdy_hms(get_cell(path, "Operation Log", "D2"))
    .Object@config <- init_config(path)
    .Object@raw <- init_raw(path, .Object@config)

    if (length(wells) == 0) {
      well_list <- unique(.Object@raw[["well"]])
      .Object@wells <-
        tibble::tibble(
          well = well_list,
          type = rep("sample", length(well_list)),
          group = factor(type)
        )
    } else {
      .Object@wells <- init_wells(wells)
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

    if (length(norm) == 0) {
      .Object@norm<- tibble::tibble(well = unique(.Object@raw[["well"]]), value = 1)
    } else {
      .Object@norm <- init_norm(norm)
    }
    .Object@unit <- unit

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
    raw <- object@raw
    stages <- object@stages
    norm <- object@norm

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

    # norm
    if (length(norm) != 0) {
      if (!("well" %in% colnames(norm))) {
        msg <- c(msg, "Norm must contain a column named 'well'")
      }

      if (!("value" %in% colnames(norm))) {
        msg <- c(msg, "Norm must contain a column named 'value'")
      }

      if (!all(stringr::str_detect(norm[["well"]], "^[A-Z]\\d{2}$"))) {
        msg <- c(msg, "Norm column 'well' must match the pattern 'A01'")
      }
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

#' @describeIn Seahorse-class Setter for 'spans' slot
#' @param x A `Seahorse` object
#' @param value Replacement value
#' @export
setMethod("stages<-", "Seahorse", function(x, value) {
  if(!is.data.frame(value)) value <- tibble::as_tibble(value)
  x@stages <- value
  methods::validObject(x)
  x
})
