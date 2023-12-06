#test-init-Sea.R

test_that("Seahorse from path only", {
  expect_s4_class(Seahorse(.path), "Seahorse")
})

test_that("init_wells works", {
  x <- Seahorse(.path)
  expect_snapshot(init_wells(wells = list(), x = x))
  expect_true("group" %in% names(Seahorse(.path)@wells))
  expect_error(
    init_wells(wells = list(type = "sample")),
    "Wells must contain a column named 'well'"
  )
  expect_error(
    init_wells(wells = list(well = "A01")),
    "Wells must contain a column named 'type'"
  )
  expect_error(
    init_wells(wells = list(well = "A01", type = "x")),
    "Wells column 'type' must contain only: 'blank', 'sample', 'hypoxia'"
  )
  expect_error(
    init_wells(wells = list(well = "A1", type = "sample")),
    "Wells column 'well' must match the pattern 'A01'"
  )
  expect_snapshot(
    init_wells(wells = list(well = "A01", type = "sample", group = "A"))
  )
  expect_error(
    Seahorse(.path, wells = list(well = "A01", type = "sample")),
    "Wells column 'well' must contain one row for each sample well"
  )
  expect_error(
    Seahorse(.path, wells = list(well = c(wells_24, "A01"), type = "blank")),
    "Wells column 'well' contains duplicates"
  )
  expect_error(
    Seahorse(.path, wells = list(well = c(wells_24, "E01"), type = "blank")),
    "Wells column 'well' contains rows not in the experiment"
  )
})
