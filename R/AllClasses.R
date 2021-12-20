# AllClasses.R

setClass(
  "Seahorse",
  slots = c(
    path = "character",
    filename = "character"
  ),
  prototype = list(
    path = NA_character_
    # filename = NA_character_
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
    # .Object@filename <- stringr::str_extract(basename(path), ".*(?=.xlsx)")
    methods::validObject(.Object)
    .Object
  }
)
