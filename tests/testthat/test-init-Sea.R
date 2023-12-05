#test-init-Sea.R

test_that("Seahorse from path only", {
  expect_s4_class(Seahorse(.path), "Seahorse")
})
