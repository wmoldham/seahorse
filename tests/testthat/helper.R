# helper.R

path <- system.file("inst/extdata/test_1.xlsx", package = "seahorse", mustWork = TRUE)

well <- stringr::str_c(rep(LETTERS[1:4], each = 6), sprintf("%02d", 1:6))
type <- c("blank", rep("sample", 8), "blank", rep("sample", 4), "blank", rep("sample", 8), "blank")
group <- c(NA_character_, rep("A", 4), rep("B", 4), NA_character_, rep("C", 4), NA_character_, rep("D", 4), rep("E", 4), NA_character_)
good_wells <- list(well = well, type = type, group = group)

measurement <- 1:12
stage <- rep(LETTERS[1:4], each = 3)
good_stages <- list(measurement = measurement, stage = stage)

good_cells <- list(well = well, value = 2)

a <- Seahorse(path)
b <- Seahorse(path, good_wells, good_stages, good_cells, bf = 2.4, cf = 0.41)
