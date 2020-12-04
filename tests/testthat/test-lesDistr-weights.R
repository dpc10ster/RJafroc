context("utils:UtilLesionWeightsDistr")
test_that("UtilLesionWeightsDistr", {
  
  maxLL <- 4
  relWeights <- c(0.2, 0.4, 0.1, 0.3)
 
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionWeightsDistr1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesionWeightsDistr (maxLL, relWeights) 
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilLesionWeightsDistr (maxLL, relWeights), ret)
  # end of test
  
})




context("utils:UtilLesionWeightsDistr")
test_that("UtilLesionWeightsDistr", {
  
  relWeights <- c(0.1, 0.8, 0.1)
  maxLL <- length(relWeights)
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionWeightsDistr2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesionWeightsDistr(maxLL, relWeights) 
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilLesionWeightsDistr(maxLL, c(0.1, 0.8, 0.1)), ret)
  # end of test
  
})




context("utils:UtilLesionDistr and UtilLesionWeightsDistr")
test_that("UtilLesionDistr and UtilLesionWeightsDistr", {
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesionWeightsDistr (dataset02), ret)
  
  ret <- array(c(1, 1), dim = c(1,2)) 
  expect_equal(UtilLesionDistr (dataset02), ret)
  
  ret <- array(c(c(1, 2), c(1, 0.5), c(-Inf, 0.5)), dim = c(2,3)) 
  expect_equal(UtilLesionWeightsDistr (dataset01), ret)
  
  ret <- array(c(c(1, 2), c(0.93258427, 0.06741573)), dim = c(2,2)) 
  expect_equal(UtilLesionDistr (dataset01), ret)
  
  ret <- array(c(c(1, 2, 3), c(1, 0.5, 0.333333), c(-Inf, 0.5, 0.333333), c(-Inf, -Inf, 0.333333)), dim = c(3,4)) 
  expect_equal(UtilLesionWeightsDistr (dataset05), ret, tolerance = 1e-6, scale = 1)
  
  ret <- array(c(c(1, 2, 3), c(0.78723404, 0.17021277, 0.04255319)), dim = c(3,2)) 
  expect_equal(UtilLesionDistr (dataset05), ret)
  
  ret <- array(c(1, 1), dim = c(1,2))
  expect_equal(UtilLesionDistr (datasetCadLroc), ret)
  
  ret <- array(c(c(1, 2, 3, 4), c(0.125, 0.375, 0.4, 0.1)), dim = c(4,2))
  expect_equal(UtilLesionDistr (datasetROI), ret)
  
})


