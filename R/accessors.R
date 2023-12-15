# accessors.R

#' @include Seahorse-class.R
NULL


# document ----------------------------------------------------------------

#' Accessors for `seahorse` S4 objects
#'
#' These functions describe how to access and replace values in [Seahorse] and
#' [Herd] objects.
#'
#' @param x A `Seahorse` or `Herd` object.
#' @param value A replacement value.
#'
#' @details Format of `value` for the class replacement methods:
#' @name accessors
#' @aliases bf bf<- cells cells<- cf cf<- filename stages stages<- units
#'     units<- wells wells<-
NULL


# bf ----------------------------------------------------------------------

setGeneric("bf", function(x) standardGeneric("bf"))
setGeneric("bf<-", function(x, value) standardGeneric("bf<-"))

#' @export
#' @rdname accessors
#' @returns `bf(x)` returns the medium buffer factor.
setMethod("bf", "Seahorse", function(x) x@bf)

#' @export
#' @rdname accessors
#' @details \describe{\item{`bf:`}{A length-one numeric vector containing the
#'     buffer factor of the medium. This value is used to calculate the proton
#'     efflux rate (PER) from the extracellular acidification rate (ECAR).}}
setMethod("bf<-", "Seahorse", function(x, value) {
  old <- x@bf
  if (identical(old, value)) {
    rlang::warn(
      "Buffer factor unchanged "
    )
    return(x)
  }
  x@bf <- value
  methods::validObject(x)
  suppressMessages({
    x <- analyze(x)
  })
  x
})


# cells -------------------------------------------------------------------

setGeneric("cells", function(x) standardGeneric("cells"))
setGeneric("cells<-", function(x, value) standardGeneric("cells<-"))

#' @export
#' @rdname accessors
#' @returns `cells(x)` returns the normalization values used to adjust the rate
#'     measurements.
setMethod("cells", "Seahorse", function(x) x@cells)

#' @export
#' @rdname accessors
#' @details \describe{\item{`cells:`}{A list or data frame containing normalization
#'     data for an experiment. `cells` must contain a character vector named `well`
#'     and a numeric vector named `value`. `well` must contain a unique entry for
#'     each well in the experimental plate. `value` contains the normalization
#'     factor (*e.g.*, cell number or protein content).}}
setMethod("cells<-", "Seahorse", function(x, value) {
  value <- init_cells(value, x)
  x@cells <- value
  methods::validObject(x)
  suppressMessages({
    x <- analyze(x)
  })
  x
})


# cdcf --------------------------------------------------------------------

setGeneric("cf", function(x) standardGeneric("cf"))
setGeneric("cf<-", function(x, value) standardGeneric("cf<-"))

#' @export
#' @rdname accessors
#' @returns `cf(x)` returns the carbon dioxide correction factor.
setMethod("cf", "Seahorse", function(x) x@cf)

#' @export
#' @rdname accessors
#' @details \describe{\item{`cf:`}{A length-one numeric vector describing the CO2
#'     correction factor for the cells in the assay. This parameter is used to
#'     separate the mitochondrial and glycolytic contributions to the proton
#'     efflux rate.}}
setMethod("cf<-", "Seahorse", function(x, value) {
  old <- x@cf
  if (identical(old, value)) {
    rlang::warn(
      "Carbon dioxide correction factor unchanged "
    )
    return(x)
  }
  x@cf <- value
  methods::validObject(x)
  suppressMessages({
    x <- analyze(x)
  })
  x
})


# filename ----------------------------------------------------------------

setGeneric("filename", function(x) standardGeneric("filename"))

#' @export
#' @rdname accessors
#' @returns `filename(x)` returns the base filename for the `Seahorse`
#'     experiment.
setMethod("filename", "Seahorse", function(x) x@filename)


# stages ------------------------------------------------------------------

setGeneric("stages", function(x) standardGeneric("stages"))
setGeneric("stages<-", function(x, value) standardGeneric("stages<-"))

#' @export
#' @rdname accessors
#' @returns `stages(x)` returns the experimental design details associated with
#'     each stage.
setMethod("stages", "Seahorse", function(x) x@stages)

#' @export
#' @rdname accessors
#' @details \describe{\item{`stages:`}{A list or data frame containing vectors
#'     of experimental descriptors for each of the measurement stages. At
#'     minimum, `stages` must contain character vectors named `measurement` and
#'     `stage`. `measurement` must contain a unique entry for each measurement
#'     in the experiment. `stage` describes the experimental interval (*e.g.*,
#'     for a typical mitochondrial stress assay, `stage` would be `basal`,
#'     `oligo`, `fccp`, `rot/ama`). A `well` column may also be included if the
#'     injections differ across the plate.}}
setMethod("stages<-", "Seahorse", function(x, value) {
  value <- init_stages(value, x)
  x@stages <- value
  methods::validObject(x)
  suppressMessages({
    x <- analyze(x)
  })
  x
})


# units -------------------------------------------------------------------

setGeneric("units", function(x) standardGeneric("units"))
setGeneric("units<-", function(x, value) standardGeneric("units<-"))

#' @export
#' @rdname accessors
#' @returns `units(x)` returns the character vector describing the normalization
#'     units.
setMethod("units", "Seahorse", function(x) x@units)

#' @export
#' @rdname accessors
#' @details \describe{\item{`units:`}{A character vector describing the units of
#'     the normalization factor provided in the `value` column of `cells`. This
#'     will be used to annotate graphs.}}
setMethod("units<-", "Seahorse", function(x, value) {
  x@units <- value
  methods::validObject(x)
  x
})


# wells -------------------------------------------------------------------

setGeneric("wells", function(x) standardGeneric("wells"))
setGeneric("wells<-", function(x, value) standardGeneric("wells<-"))

#' @export
#' @rdname accessors
#' @returns `wells(x)` returns the experimental design details associated with
#'     each well.
setMethod("wells", "Seahorse", function(x) x@wells)

#' @export
#' @rdname accessors
#' @details \describe{\item{`wells:`}{A list or data frame containing vectors
#'     of experimental descriptors for each of the sample wells. `wells` must
#'     contain character vectors named `well` and `type`. `well` must contain
#'     a unique entry for each well in the experimental plate. `type` must be
#'     `sample` or `blank` for each well. Users should consider a `group` column
#'     that labels unique experimental groups. Additional descriptors may be
#'     included (*e.g.*, `treatment`, `time`, `condition`, *etc.*). The
#'     experimental data provided will be converted to a data frame. If a
#'     `group` vector was not provided, one will be generated from `type` or a
#'     combination of the remaining descriptors. Assigning an empty list with
#'     `wells<-` will restore the assignment based on the raw data file.}}
setMethod("wells<-", "Seahorse", function(x, value) {
  value <- init_wells(value, x)
  x@wells <- value
  methods::validObject(x)
  suppressMessages({
    x <- analyze(x)
  })
  x
})
