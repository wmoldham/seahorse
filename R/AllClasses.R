# AllClasses.R

setClass(
  "Seahorse",
  slots = c(
    path = "character",
    filename = "character",
    time = "POSIXct",
    config = "list"
  ),
  prototype = list(
    path = NA_character_,
    filename = NA_character_,
    time = NA_real_,
    config = list()
  )
)

#' @export
Seahorse <- function(
  path
){
  methods::new(
    "Seahorse",
    path = path
  )
}

methods::setValidity(
  "Seahorse",
  function(object)
  {
    msg <- NULL

    path <- object@path

    # path
    if (!file.exists(path)) {
      msg <- "File does not exist"
    }
    if (is.null(msg)) TRUE else msg
  }
)

setMethod(
  "initialize",
  "Seahorse",
  function(.Object, path)
  {
    .Object@path <- path
    .Object@filename <- sub("\\.xlsx", "", basename(path))
    .Object@time <- lubridate::mdy_hms(get_cell(path, "Operation Log", "D2"))
    .Object@config <- init_config(path)
    methods::validObject(.Object)
    .Object
  }
)
