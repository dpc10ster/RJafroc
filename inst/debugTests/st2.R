library(testthat)
library(RJafroc)
rm(list = ls())
#############################################################################
# if the two datastructures are IDENTICAL, dont need all this
# the way to get them identical is to re-generate the goodValue files
CompareLists <- function(x1, x2, d = 0, f = 0, m = 0)
{
  for (t in 1:length(x1)) {
    y1 <- unlist(x1[[t]]); attributes(y1) <- NULL; y1 <- y1[!is.na(y1)]
    y2 <- unlist(x2[[t]]); attributes(y2) <- NULL; y2 <- y2[!is.na(y2)]
    # x11 <- x1[[t]][q]; attributes(x11) <- NULL
    # x22 <- x2[[t]][q]; attributes(x22) <- NULL
    expect_equal(y1, y2,
                 info =
                   paste0 ("Dataset = ",d,", FOM = ",
                           f,", method = ",m, ", t = ", t))
  }
}
#############################################################################
# dataset = an ROC and an FROC dataset; dataset02, dataset05
# FOM = "Wilcoxon", "HrAuc"
# method = "DBMH", "ORH"
dataset_arr <- list(dataset02, dataset05) # deparse(substitute(dataset02)) does not work below
dataset_arr_str <- c("dataset02", "dataset05")
FOM_arr <- c("Wilcoxon", "HrAuc") #, "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
method_arr <- c("DBMH", "ORH")

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
        
        cat("d = ", d, ", f = ", f, ", m = ", m, "\n")
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
        
      }
    }
  }
}


