#############################################################################
# if the two datastructures are IDENTICAL, dont need all this
# the way to get them identical is to re-generate the goodValue files
# CompareLists <- function(x1, x2, d = 0, f = 0, m = 0)
# {
#   for (t in 1:length(x1)) {
#     y1 <- unlist(x1[[t]])
#     y2 <- unlist(x2[[t]])
#     # y1 <- unlist(x1[[t]]); attributes(y1) <- NULL; y1 <- y1[!is.na(y1)]
#     # y2 <- unlist(x2[[t]]); attributes(y2) <- NULL; y2 <- y2[!is.na(y2)]
#     # x11 <- x1[[t]][q]; attributes(x11) <- NULL
#     # x22 <- x2[[t]][q]; attributes(x22) <- NULL
#     expect_equal(y1, y2,
#                  info =
#                    paste0 ("Dataset = ",d,", FOM = ",
#                            f,", method = ",m, ", t = ", t))
#   }
# }
#############################################################################


context("StSignificanceTesting-datasetFROCSp-wAFROC")
test_that("StSignificanceTesting-datasetFROCSp-wAFROC", {

  dataset <- datasetFROCSp

  fn <- paste0(test_path(), "/goodValues361/SigTest/datasetFROCSp-wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- StSignificanceTesting(datasetFROCSp, FOM = "wAFROC", method = "ORH")
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- StSignificanceTesting(datasetFROCSp, FOM = "wAFROC", method = "ORH")

  expect_equal(x1,x2)

  expect_error(StSignificanceTesting(datasetFROCSp, FOM = "wAFROC", method = "DBMH"))
  expect_error(StSignificanceTesting(datasetFROCSp, FOM = "wAFROC", method = "ORH", covEstMethod = "bootstrap"))
  expect_error(StSignificanceTesting(datasetFROCSp, FOM = "wAFROC", method = "ORH", covEstMethod = "DeLong"))

})



context("SignificanceTestingAllCombinations")
test_that("SignificanceTestingAllCombinations", {
  
  ####################################################################################  
  skip_on_cran()
  ####################################################################################
  # skip_on_os("mac")
  # This was fixed 3/7/20 by deleting all goodValue files!!
  # cannot for the life of me figure out why this fails in R CMD check and devtools::check() but not in devtools::test()
  # started working again 2/14/20, go figure
  # Not so fast; failed at line 83; 2/17/20; reinstated skip_on_os("mac")"
  # 3/7/20: finally figured out error was in StDBMHAnalysis.R, in the way I was building up
  # the dataframe for anovaY; see fix around line 33
  # removed skip_on_os("mac") 3/7/20
  ####################################################################################
  
  # dataset = an ROC and an FROC dataset; dataset02, dataset05
  # FOM = "Wilcoxon", "HrAuc"
  # method = "DBMH", "ORH"
  dataset_arr <- list(dataset02, dataset05) # deparse(substitute(dataset02)) does not work below
  dataset_arr_str <- c("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc") #, "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBMH", "ORH")
  # dataset_arr <- list(dataset02) # deparse(substitute(dataset02)) does not work below
  # dataset_arr_str <- c("dataset02")
  # FOM_arr <- c("Wilcoxon") #, "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBMH")
  
  for (d in 1:length(dataset_arr)) {
    for (f in 1:length(FOM_arr)) {
      for (m in 1:length(method_arr)) {
        dataset <- dataset_arr[[d]]
        if ((dataset$dataType == "ROC") && (FOM_arr[f] != "Wilcoxon")) {
          
          # for ROC data, only Wilcoxon FOM is allowed
          expect_error(StSignificanceTesting(dataset, FOM = FOM_arr[f], method = method_arr[m]))
          
        } else if ((dataset$dataType == "FROC") && (FOM_arr[f] == "Wilcoxon")) {
          
          # for FROC data, Wilcoxon FOM is NOT allowed
          expect_error(StSignificanceTesting(dataset, FOM = FOM_arr[f], method = method_arr[m]))
          
        } else {
          
          fn <- paste0(test_path(), "/goodValues361/SigTest/", dataset_arr_str[d], FOM_arr[f], method_arr[m], ".rds")
          if (!file.exists(fn)) {
            warning(paste0("File not found - generating new ",fn))
            x1 <- StSignificanceTesting(dataset, FOM = FOM_arr[f], method = method_arr[m])
            saveRDS(x1, file = fn)
          }
          
          x1 <- readRDS(fn)
          x2 <- StSignificanceTesting(dataset, FOM = FOM_arr[f],method = method_arr[m])

          # CompareLists(x1,x2, d, f, m)
          expect_equal(x1,x2)  # this fails on R CMD check, hence reverted to line above
          # expect_equal(x1$anovaY[-1],x2$anovaY[-1])  # this fails on R CMD check, hence reverted to line above
          # show_failure(expect_equal(x1,x2))  # this fails on R CMD check, hence reverted to line above
          # cat("d = ", d, ", f = ", f, ", m = ", m, "\n")
        }
      }
    }
  }
  
})



context("StSignificanceTestingSingleFixedFactor")
test_that("StSignificanceTestingSingleFixedFactor", {

  fn <- paste0(test_path(), "/goodValues361/SigTest/SingleFixedFactor_02_1_14", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon")
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon")

  # CompareLists(x1,x2)
  expect_equal(x1,x2)

})



context("StSignificanceTestingSingleFixedFactor")
test_that("StSignificanceTestingSingleFixedFactor", {

  skip_on_os("windows")

  fn <- paste0(test_path(), "/goodValues361/SigTest/SingleFixedFactor_05_1_14", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4), FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4), FOM = "wAFROC")

  # CompareLists(x1,x2)
  expect_equal(x1,x2)

})



test_that("StSignificanceTestingSingleFixedFactor", {

  fn <- paste0(test_path(), "/goodValues361/SigTest/SingleFixedFactor_05_12_4", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4), FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4), FOM = "wAFROC")

  # CompareLists(x1,x2)
  expect_equal(x1,x2)

})

############################################################################# 
#
# TODO: fix travis developer failure on this test as per saved log
# Probably need to set attributes explicitly
# Replacing expect_equal with expect_equivalent may fix this: DID NOT WORK
# Temporary fix: just comment out the test
# 7/12/19: added this back; passed on new version of R 3.6.1
# did not work; commented out again 7/12/19
# following code now works on osx 3/7/20
# but not on travis old release
context("StSignificanceTestingCrossedModalities")
test_that("StSignificanceTestingCrossedModalities", {

  # crossedFileName <- system.file(
  #   "extdata", "CrossedModalitiesData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/SigTest/CrossedModalities", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    goodValues <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
    saveRDS(goodValues, file = fn)
  }

  goodValues <- readRDS(fn)
  currentValues <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
  expect_equal(goodValues, currentValues)
  # following klutzy code can be simplified to above line by simply deleting the goodValues file
  # expect_equal(length(goodValues), length(currentValues))
  #
  # for (f in 1: length(goodValues)){
  #   x <- as.vector(unlist(goodValues[[f]]))
  #   y <- as.vector(unlist(currentValues[[f]]))
  #   if (f %in% c(9,14)) {x <- as.numeric(x[-(1:4)]);y <- as.numeric(y[-(1:4)])}
  #   if (f %in% c(15)) {
  #     x <- as.numeric(unlist(goodValues[[f]][-c(1,2,5)]))
  #     y <- as.numeric(unlist(currentValues[[f]][-c(1,2,5)]))
  #   }
  #   if (f %in% c(16)) {
  #     x <- as.numeric(unlist(goodValues[[f]][-c(1)]))
  #     y <- as.numeric(unlist(currentValues[[f]][-c(1)]))
  #   }
  #   if (f %in% c(21)) {
  #     x <- as.numeric(unlist(goodValues[[f]][-1]))
  #     y <- as.numeric(unlist(currentValues[[f]][-1]))
  #   }
  #   expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  # }

})

