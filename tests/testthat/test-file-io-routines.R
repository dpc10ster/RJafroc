context("File I/O routines")
tmp <- tempfile()
fileName <- system.file("extdata", "includedRocData.xlsx", 
                        package = "RJafroc", mustWork = TRUE)
expect_known_output(
  DfReadDataFile(fileName), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile()
fileName <- system.file("extdata", "includedRocData.csv", 
                        package = "RJafroc", mustWork = TRUE)
expect_known_output(
  DfReadDataFile(fileName, format = "MRMC"), 
  tmp, print = TRUE, update = TRUE)

