# init-stages.R

test_that("stages formatted correctly", {
  expect_error(Seahorse(path, stages = list(measurement = 1:12)), "stage")
  expect_error(Seahorse(path, stages = list(stage = rep("basal", 12))), "measurement")
  expect_error(Seahorse(path, stages = list(measurement = 1:11, stage = rep("basal", 11))), "entry for each measurement")
  expect_error(Seahorse(path, stages = list(measurement = 1:12, stage = rep("basal", 12), well = rep("A01", 12))), "entry for each well")
})
