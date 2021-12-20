# AllMethods.R

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
