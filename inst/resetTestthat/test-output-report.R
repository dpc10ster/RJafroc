contextStr <- "UtilOutputReport text format"
context(contextStr)
test_that(contextStr, {
  
# analysisOption not needed as output file produces all three analysisOption DPC 6/30/19
# Excel tests not yet implemented  
  skip_on_cran()
  
  dataset_arr <- list(dataset02, dataset05) # first is ROC second is FROC dataset object
  dataset_arr_str <- c("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc") #, "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBM", "OR")
  format_arr <- c("txt")
  ## cycle through all representative datasets, FOMs, methods 
  for (d in 1:length(dataset_arr)) {
    dataset <- dataset_arr[[d]]
    for (i in 1:length(FOM_arr)) {
      for (j in 1:length(method_arr)) {
        for (f in 1:length(format_arr)) {
          if ((dataset$descriptions$type == "ROC") && (FOM_arr[i] != "Wilcoxon")) {
            
            # for ROC data, only Wilcoxon FOM is allowed
            expect_error(UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileExt = format_arr[f], overWrite = TRUE))
            
          } else if ((dataset$descriptions$type == "FROC") && (FOM_arr[i] == "Wilcoxon")) {
            
            # for FROC data, Wilcoxon FOM is NOT allowed
            expect_error(UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileExt = format_arr[f], overWrite = TRUE))
            
          } else {
            
            fn <- paste0(test_path(), "/goodValues361/OutputReport/",dataset_arr_str[d], 
                         FOM_arr[i], method_arr[j]) # either format; base name only !! dpc 7/28/19
            fnwithExt <- paste0(fn, ".", format_arr[f])
            if (!file.exists(fnwithExt)) {
              UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileBaseName = fn, ReportFileExt = format_arr[f], overWrite = TRUE)
            }
            
            fn2 <- paste0(test_path(), "/goodValues361/OutputReport/",dataset_arr_str[d], 
                          FOM_arr[i], method_arr[j], "Temp")
            UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileBaseName = fn2, ReportFileExt = format_arr[f], overWrite = TRUE)
            fnwithExt <- paste0(fn, ".", format_arr[f])
            fn2withExt <- paste0(fn2, ".", format_arr[f])
            xx <- file(fnwithExt, open = "rt")
            xx1 <- readLines(xx);xx1 <- xx1[20:length(xx1)] # skip date related stuff which will change
            yy <- file(fn2withExt, open = "rt")
            yy1 <- readLines(yy);yy1 <- yy1[20:length(yy1)] # skip date related stuff which will change
            close(xx);close(yy)
            expect_equivalent(xx1, yy1) # works!
            unlink(fn2withExt)
            # end of test
            # 
          }  
        }  
      }
    }
  }
  
})

