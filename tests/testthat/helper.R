# helper.R

.path <- system.file("extdata/raw_1.xlsx", package = "seahorse", mustWork = TRUE)

suppressMessages(
  sea <- Seahorse(.path, stages = stages_mst, bf = 2.4, cf = 0.41)
)
