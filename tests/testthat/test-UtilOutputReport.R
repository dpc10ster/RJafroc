# 7/11/20: this does not test the actual output produced by UtilOutputReport
# It just tests that the signficance testing routines run without errors
# See next test commented out that represents failed attempt at comparing actual outputs for text files only
contextStr <- "UtilOutputReport: datasets02, 03 and 04"
context(contextStr)
test_that(contextStr, {
  methodStr <- c("DBM", "OR")
  datasetStr <- c("dataset04", "dataset02", "dataset03")
  fomStr <- c("Wilcoxon", "wAFROC")
  for (f in 1:length(fomStr)) {
    for (d in 1:length(datasetStr)) {
      for (m in 1:length(methodStr)) {
        FOM <- fomStr[f]
        method <- methodStr[m]
        dataset <- get(datasetStr[d])
        if (dataset$descriptions$type == "ROC" && (FOM != "Wilcoxon")) next 
        if (dataset$descriptions$type == "FROC" && (FOM != "wAFROC")) next 
        fn <- paste0(test_path(), "/goodValues361/UtilOutputReport/", datasetStr[d], "-", methodStr[m], "-", fomStr[f], "rds")
        if (!file.exists(fn)) {
          warning(paste0("File not found - generating new ",fn))
          x <- UtilOutputReport(dataset, FOM = FOM, method = method)
          saveRDS(x, fn)
        }
        x <- readRDS(fn)
        y <- UtilOutputReport(dataset, FOM = FOM, method = method)
        expect_equal(x, y)
      }
    }
  }
})

# 7/11/20: following code passes when run using Run Tests but fails when run using
# devtools::test()
# therefore commented out code comparing actual outputs and revereted to basic test that
# UtilOutputReport runs without errors

# test_that("this does not work in R CMD check", {
  # methodStr <- c("DBM", "OR")
  # datasetStr <- c("dataset04", "dataset02", "dataset03")
  # fomStr <- c("Wilcoxon", "wAFROC")
  # for (f in 1:length(fomStr)) {
  #   for (d in 1:length(datasetStr)) {
  #     for (m in 1:length(methodStr)) {
  #       FOM <- fomStr[f]
  #       method <- methodStr[m]
  #       dataset <- get(datasetStr[d])
  #       if (dataset$descriptions$type == "ROC" && (FOM != "Wilcoxon")) next 
  #       if (dataset$descriptions$type == "FROC" && (FOM != "wAFROC")) next 
  #       fn <- paste0(test_path(), "/goodValues361/UtilOutputReport/", datasetStr[d], "-", methodStr[m], "-", fomStr[f])
  #       fnwithExt <- paste0(fn, ".", "txt")
  #       if (!file.exists(fnwithExt)) {
  #         warning(paste0("File not found - generating new ",fn))
  #         ret <- UtilOutputReport(dataset, ReportFileBaseName = fn, overWrite = TRUE, FOM = FOM, method = method)
  #       }
  #       fn2 <- paste0(test_path(), "/goodValues361/UtilOutputReport/", datasetStr[d], "-", methodStr[m], "-", fomStr[f], "-Temp")
  #       fn2withExt <- paste0(fn2, ".", "txt")
  #       ret <- UtilOutputReport(dataset, ReportFileBaseName = fn2, overWrite = TRUE, FOM = FOM, method = method)
  #       xx <- file(fnwithExt, open = "rt")
  #       xx1 <- readLines(xx);xx1 <- xx1[15:length(xx1)] # skip date related stuff which will change
  #       yy <- file(fn2withExt, open = "rt")
  #       yy1 <- readLines(yy);yy1 <- yy1[15:length(yy1)] # skip date related stuff which will change
  #       close(xx);close(yy)
  #       expect_equivalent(xx1, yy1)
  #       unlink(fn2withExt)
  #     }
  #   }
  # }
# })


