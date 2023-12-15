# test-access.R


# bf ----------------------------------------------------------------------

test_that("bf accessor works", {
  expect_equal(bf(harold), 2.4)
})

test_that("bf assignment works", {
  expect_error(bf(harold) <- -1, "Buffer factor must be positive")
  expect_error(bf(harold) <- 0, "Buffer factor must be positive")
  expect_equal(bf(`bf<-`(harold, NA_real_)), NA_real_)
  expect_equal(bf(`bf<-`(harold, 1)), 1)
})


# cells -------------------------------------------------------------------

test_that("cells accessor works", {
  expect_snapshot(cells(harold))
})

test_that("cells assignment works", {
  expect_snapshot(cells(`cells<-`(harold, list())))
})


# cf ----------------------------------------------------------------------

test_that("cf accessor works", {
  expect_equal(cf(harold), 0.41)
})

test_that("cf assignment works", {
  expect_error(cf(harold) <- -1, "CO2 correction factor must be positive")
  expect_error(cf(harold) <- 0, "CO2 correction factor must be positive")
  expect_equal(cf(`cf<-`(harold, NA_real_)), NA_real_)
  expect_equal(cf(`cf<-`(harold, 1)), 1)
})


# stages ------------------------------------------------------------------

test_that("stages accessor works", {
  expect_snapshot(stages(harold))
})

test_that("stages assignment works", {
  expect_snapshot(stages(`stages<-`(harold, list())))
})


# units -------------------------------------------------------------------

test_that("units assignment works", {
  expect_equal(units(`units<-`(harold, "x")), "x")
})


# wells -------------------------------------------------------------------

test_that("wells accessor works", {
  expect_equal(wells(harold), init_wells(wells_ex))
})

test_that("wells assignment works", {
  expect_snapshot(wells(`wells<-`(harold, list())))
})
