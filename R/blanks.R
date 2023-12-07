# blanks.R

setGeneric("blanks", function(x) standardGeneric("blanks"))
setGeneric("blanks<-", function(x, ..., value) standardGeneric("blanks<-"))

setMethod("blanks", "Seahorse", function(x) x@blanks)

