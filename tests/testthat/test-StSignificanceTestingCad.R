context("Significance testing: StSignificanceTestingCadVsRadiologists")


test_that("StSignificanceTestingCadVsRadiologists", {
  
  skip_on_travis()
  skip_on_cran()
  
  dataset_arr <- list(dataset09, datasetCadLroc, dataset01, dataset02)
  dataset_arr_str <- c("dataset09", "datasetCadLroc", "dataset01", "dataset02")
  fom_arr <- c("Wilcoxon", "PCL", "ALROC")
  method_arr <- c("singleModality", "dualModality")
  
  for (d in 1:length(dataset_arr)) {
    dataset <- dataset_arr[[d]]
    for (i in 1:length(fom_arr)) {
      for (j in 1:length(method_arr)) {
        
        if ((dataset$dataType == "ROC") && (fom_arr[i] != "Wilcoxon")){
          
          expect_error(StSignificanceTestingCadVsRadiologists (dataset, FOM = fom_arr[i], method = method_arr[j]))
          
        } else if ((dataset$dataType == "ROC") && (fom_arr[i] %in% c("PCL", "ALROC"))) {
          
          expect_error(StSignificanceTestingCadVsRadiologists (dataset, FOM = fom_arr[i], method = method_arr[j]))
          
        } else if (((dataset$dataType == "LROC") && (fom_arr[i] %in% c("Wilcoxon", "ALROC", "PCL")))) {
          
          fn <- paste0(test_path(), "/goodValues/SigTestCad/", dataset_arr_str[d], method_arr[j], fom_arr[i])
          if (!file.exists(fn)) {
            ret <- StSignificanceTestingCadVsRadiologists (dataset, FOM = fom_arr[i], method = method_arr[j])
            saveRDS(ret, file = fn)
          }  
          ret <- readRDS(fn)
          expect_equal(StSignificanceTestingCadVsRadiologists (dataset, FOM = fom_arr[i], method = method_arr[j]), ret)
          # end of test
          
        } else if ((dataset$dataType == "ROC") && (fom_arr[i] != "Wilcoxon")) {
          
          expect_error(StSignificanceTestingCadVsRadiologists (dataset, FOM = fom_arr[i], method = method_arr[j]))
          # end of test
          
        } else if (dataset$dataType == "FROC") {
          # DPC: FROC unimplemented at this time 6/30/19 
          expect_error(StSignificanceTestingCadVsRadiologists (dataset, FOM = fom_arr[i], method = method_arr[j]))
          # end of test
          
        }
        
      }
    }
  }
  
  
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "singleModality"), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "singleModality"), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "dualModality"),
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "dualModality"),
  #   tmp, print = TRUE, update = TRUE)
  # 
  # expect_error(
  #   StSignificanceTestingCadVsRadiologists(dataset09, FOM = "PCL"))
  # 
  # expect_error(
  #   StSignificanceTesting(datasetCadLroc, FOM = "wAFROC", option = "RRFC"))
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", option = "RRFC"),
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", option = "RRFC"),
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "Wilcoxon", method = "singleModality"),
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "Wilcoxon", method = "singleModality"),
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "Wilcoxon", method = "dualModality"),
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "Wilcoxon", method = "dualModality"),
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "PCL", method = "singleModality"),
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "PCL", method = "singleModality"),
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "PCL", method = "dualModality"), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "PCL", method = "dualModality"), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "ALROC", method = "singleModality"), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "ALROC", method = "singleModality"), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "ALROC", method = "dualModality"), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (datasetCadLroc, FOM = "ALROC", method = "dualModality"), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRRC", method = "dualModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRRC", method = "dualModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRFC", method = "singleModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRFC", method = "singleModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRFC", method = "dualModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc, FOM = "PCL", option = "RRFC", method = "dualModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE)
  # 
  # datasetCadLroc7 <- DfExtractDataset(datasetCadLroc, rdrs = seq(1:7))
  # tmp <- tempfile()
  # expect_warning(expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc7, FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE),
  #   "Creating reference output")
  # 
  # expect_known_output(
  #   StSignificanceTestingCadVsRadiologists (
  #     datasetCadLroc7, FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05), 
  #   tmp, print = TRUE, update = TRUE)
  
})

