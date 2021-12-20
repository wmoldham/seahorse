library(testthat)
library(seahorse)

path <- system.file("extdata/test_1.xlsx", package = "seahorse", mustWork = TRUE)

test_check("seahorse")
