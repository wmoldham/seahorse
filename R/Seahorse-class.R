# Seahorse-class.R


# class -------------------------------------------------------------------

setClass(
  "Seahorse",
  slots = c(
    path = "character",
    filename = "character"
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
    methods::validObject(.Object)
    .Object
  }
)
