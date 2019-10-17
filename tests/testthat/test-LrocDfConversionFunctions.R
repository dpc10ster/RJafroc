context("LROC related Df conversion functions")

test_that("SimulateLrocFromFrocDataset", {
  
  dsLroc <- SimulateLrocFromFrocDataset(dataset05)
  ret1 <- UtilFigureOfMerit(dataset05, FOM = "HrAuc")
  ret2 <- UtilFigureOfMerit(dsLroc, FOM = "Wilcoxon")

  for (i in 1:length(ret1))
  {
    val1 <- ret1[[i]]
    val2 <- ret2[[i]]
    expect_equal(val1, val2)
  }

  frocDataset <- SimulateFrocFromLrocDataset(datasetCadLroc)
  ret1 <- UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon")
  ret2 <- UtilFigureOfMerit(frocDataset, FOM = "HrAuc")

  for (i in 1:length(ret1))
  {
    val1 <- ret1[[i]]
    val2 <- ret2[[i]]
    expect_equal(val1, val2)
  }

  rocDataset <- DfDatasetLroc2Roc(datasetCadLroc)
  ret1 <- UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon")
  ret2 <- UtilFigureOfMerit(rocDataset, FOM = "Wilcoxon")    
  
  for (i in 1:length(ret1))
  {
    val1 <- ret1[[i]]
    val2 <- ret2[[i]]
    expect_equal(val1, val2)  
  }
  
})

