library(RJafroc)

methodStr <- c("DBM", "OR")
datasetStr <- c("dataset04", "dataset02", "dataset03")
fomStr <- c("Wilcoxon", "wAFROC")

for (f in 1:length(fomStr)) {
  for (d in 1:length(datasetStr)) {
    for (m in 1:length(methodStr)) {
      cat("f = ", f, ", d = ", d, ", m = ", m, "\n")
      FOM <- fomStr[f]
      method <- methodStr[m]
      dataset <- get(datasetStr[d])
      if (dataset$descriptions$type == "ROC" && (FOM != "Wilcoxon"))
        next
      if (dataset$descriptions$type == "FROC" && (FOM != "wAFROC"))
        next
      fn <-
        paste0(
          test_path(),
          "/goodValues361/UtilOutputReport/",
          datasetStr[d],
          "-",
          methodStr[m],
          "-",
          fomStr[f],
          "rds"
        )
      if (!file.exists(fn)) {
        warning(paste0("File not found - generating new ", fn))
        x <- UtilOutputReport(dataset, FOM = FOM, method = method)
        saveRDS(x, fn)
      }
      x <- readRDS(fn)
      y <- UtilOutputReport(dataset, FOM = FOM, method = method)
      expect_equal(x, y)
    }
  }
}

