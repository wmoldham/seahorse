# Seahorse-class.R


# class -------------------------------------------------------------------

setClass(
  "Seahorse",
  slots = c(
    path =     "character",
    filename = "character",
    time =     "POSIXct",
    config =   "list",
    raw =      "list"
  )
)


# constructor -------------------------------------------------------------

Seahorse <- function(
    path
) {
  methods::new(
    "Seahorse",
    path = path
  )
}


# initialize --------------------------------------------------------------

setMethod(
  "initialize",
  "Seahorse",
  function(
    .Object,
    path
  ) {
    .Object@path <- path
    .Object@filename <- sub("\\.xlsx", "", basename(path))
    .Object@time <- lubridate::mdy_hms(get_cell(path, "Operation Log", "D2"))
    .Object@config <- init_config(path)
    .Object@raw <- init_raw(path, .Object@config)
    methods::validObject(.Object)
    .Object
  }
)
