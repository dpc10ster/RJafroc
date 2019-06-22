context("Significance testing: StSignificanceTestingCadVsRadiologists")


test_that("StSignificanceTestingCadVsRadiologists", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "singleModality"), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "dualModality"),
    tmp, print = TRUE, update = TRUE)

  expect_error(
    StSignificanceTestingCadVsRadiologists(dataset09, FOM = "PCL"))
  
  expect_error(
    StSignificanceTesting(datasetCadLroc, FOM = "wAFROC", option = "RRFC"))
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", option = "RRFC"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "Wilcoxon", method = "singleModality"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "Wilcoxon", method = "dualModality"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "PCL", method = "singleModality"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "PCL", method = "dualModality"), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "ALROC", method = "singleModality"), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "ALROC", method = "dualModality"), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (
      datasetCadLroc, FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (
      datasetCadLroc, FOM = "PCL", option = "RRRC", method = "dualModality", FPFValue = 0.05), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (
      datasetCadLroc, FOM = "PCL", option = "RRFC", method = "singleModality", FPFValue = 0.05), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (
      datasetCadLroc, FOM = "PCL", option = "RRFC", method = "dualModality", FPFValue = 0.05), 
    tmp, print = TRUE, update = TRUE)
  
  datasetCadLroc7 <- DfExtractDataset(datasetCadLroc, rdrs = seq(1:7))
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (
      datasetCadLroc7, FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05), 
    tmp, print = TRUE, update = TRUE)
})

