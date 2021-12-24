# environments.R

# Do level and rate information need to be added to environment?

setClass(
  "Person",
  slots = c(
    name = "character",
    age = "numeric",
    bin = "character"
  ),
  prototype = list(
    name = NA_character_,
    age = NA_real_,
    bin = NA_character_
  )
)

Person <- function(name, age)
{
  methods::new("Person", name = name, age = age)
}

bin_age <- function(age) {
  dplyr::case_when(
    age < 1 ~ "infant",
    age < 3 ~ "toddler",
    age < 13 ~ "child",
    age < 18 ~ "teenager",
    age < 65 ~ "adult",
    TRUE ~ "senior"
  )
}

setMethod(
  "initialize",
  "Person",
  function(.Object, name, age) {
    .Object@name <- name
    .Object@age <- age
    .Object@bin <- bin_age(age)
    .Object
  }
)

setMethod(
  "show",
  "Person",
  function(object) {
    cat(object@name, "\n")
    cat("Age:   ", object@age, "\n")
    cat("Class: ", object@bin)
  }
)

setGeneric("age<-", function(x, value) standardGeneric("age<-"))
setMethod("age<-", "Person", function(x, value) {
  x@age <- value
  x@bin <- bin_age(value)
  x
})

describe <- function(x) {
  cat(x@name, "is a", x@age, "yo", x@bin, "\n")
}

x <- Person("Will", 12)
describe(x)
age(x) <- 37
describe(x)

# This suggests that rates and levels do not need to be in an environment...
