# AllClasses.R

setClass(
  "Seahorse",
  slots = c(
    path = "character",
    filename = "character",
    time = "POSIXct",
    config = "list",
    raw = "list",
    wells = "list"
  ),
  prototype = list(
    path = NA_character_,
    filename = NA_character_,
    time = NA_real_,
    config = list(),
    raw = list(),
    wells = list()
  )
)

#' @export
Seahorse <- function(
  path,
  wells = list()
){
  methods::new(
    "Seahorse",
    path = path,
    wells = wells
  )
}

methods::setValidity(
  "Seahorse",
  function(object)
  {
    msg <- NULL

    path <- object@path
    wells <- object@wells
    raw <- object@raw

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

    if (is.null(msg)) TRUE else msg
  }
)

setMethod(
  "initialize",
  "Seahorse",
  function(.Object, path, wells)
  {
    .Object@path <- path
    .Object@filename <- sub("\\.xlsx", "", basename(path))
    .Object@time <- lubridate::mdy_hms(get_cell(path, "Operation Log", "D2"))
    .Object@config <- init_config(path)
    .Object@raw <- init_raw(path, .Object@config)


    if (length(wells) == 0)
    {
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

    methods::validObject(.Object)
    .Object
  }
)
