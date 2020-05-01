context("LROC related Df conversion functions")

test_that("DfFroc2Lroc", {
  
  dsLroc <- DfFroc2Lroc(dataset05)
  ret1 <- t(UtilFigureOfMerit(dataset05, FOM = "HrAuc"))
  ret2 <- t(UtilFigureOfMerit(dsLroc, FOM = "Wilcoxon"))

  for (i in 1:length(ret1))
  {
    val1 <- ret1[[i]]
    val2 <- ret2[[i]]
    expect_equal(val1, val2)
  }

  frocDataset <- DfLroc2Froc(datasetCadLroc)
  ret1 <- t(UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon"))
  ret2 <- t(UtilFigureOfMerit(frocDataset, FOM = "HrAuc"))

  for (i in 1:length(ret1))
  {
    val1 <- ret1[[i]]
    val2 <- ret2[[i]]
    expect_equal(val1, val2)
  }

  rocDataset <- DfLroc2Roc(datasetCadLroc)
  ret1 <- t(UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon"))
  ret2 <- t(UtilFigureOfMerit(rocDataset, FOM = "Wilcoxon"))    
  
  for (i in 1:length(ret1))
  {
    val1 <- ret1[[i]]
    val2 <- ret2[[i]]
    expect_equal(val1, val2)  
  }
  
})

