#test-init-Sea.R

suppressMessages(
  x <- Seahorse(.path)
)

test_that("Seahorse from path only", {
  expect_s4_class(x, "Seahorse")
})

test_that("init_wells works", {
  expect_snapshot(init_wells(wells = list(), x = sea))
  expect_true("group" %in% names(x@wells))
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

test_that("init_stages works", {
  expect_snapshot(init_stages(stages = list(), x = sea))
  expect_true("well" %in% names(x@stages))
  expect_error(
    init_stages(stages = list(measurement = 1)),
    "Stages must contain a column named 'stage'"
  )
  expect_error(
    init_stages(stages = list(stage = 1)),
    "Stages must contain a column named 'measurement'"
  )
  expect_error(
    Seahorse(.path, stages = list(measurement = 1:11, stage = "basal")),
    "Each measurement must have a stage"
  )
  expect_error(
    Seahorse(.path, stages = list(measurement = 1:13, stage = "basal")),
    "Stages 'measurement' has values missing from the experiment"
  )
  l <- list(measurement = 1:12, stage = "basal")
  expect_error(
    Seahorse(.path, stages = c(l, list(well = rep("A01", 12)))),
    "Stages must contain an entry for each well"
  )
  expect_error(
    Seahorse(.path, stages = c(l, list(well = rep("E01", 12)))),
    "Stages 'well' has values missing from the experiment"
  )
})

test_that("init_cells works", {
  expect_error(
    init_cells(cells = list(value = 1)),
    "Cells must contain a column named 'well'"
  )
  expect_error(
    init_cells(cells = list(well = "A01")),
    "Cells must contain a column named 'value'"
  )
  expect_error(
    init_cells(cells = list(well = 1, value = 1)),
    "Cells column 'well' must match the pattern 'A01'"
  )
  expect_error(
    Seahorse(.path, cells = list(well = "A01", value = 1)),
    "Cells must contain an entry for each well"
  )
  expect_error(
    Seahorse(.path, cells = list(well = c(wells_24, "E01"), value = 1)),
    "Cells 'well' has values missing from the experiment"
  )
  expect_error(
    Seahorse(.path, cells = list(well = c(wells_24, "A01"), value = 1)),
    "Cells column 'well' contains duplicates"
  )
})

test_that("units argument to Seahorse works", {
  expect_error(Seahorse(.path, units = 1), "numeric")
})

test_that("bf formatted correctly", {
  expect_error(Seahorse(.path, bf = -1), "Buffer factor must be positive")
})

test_that("cf formatted correctly", {
  expect_error(
    Seahorse(.path, cf = -1),
    "CO2 correction factor must be positive"
  )
})

test_that("blanks formatted correctly", {
  expect_snapshot(sea@blanks)
})

test_that("outliers formatted correctly", {
  expect_snapshot(sea@outliers)
})
