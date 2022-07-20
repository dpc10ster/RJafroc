context("utils:UtilLesionWeightsMatrixLesDistr")
test_that("UtilLesionWeightsMatrixLesDistr", {
  
  lesDistr <- c(0.1, 0.2, 0.3, 0.4)
  relWeights <- c(0.2, 0.4, 0.1, 0.3)
 
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionWeightsDistr1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesionWeightsMatrixLesDistr (lesDistr, relWeights) 
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilLesionWeightsMatrixLesDistr (lesDistr, relWeights), ret)

  lesDistr <- c(0.1, 0.5, 0.0, 0.4)
  relWeights <- c(0.3, 0.4, 0.0, 0.3)
  
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionWeightsDistr2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesionWeightsMatrixLesDistr (lesDistr, relWeights) 
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilLesionWeightsMatrixLesDistr (lesDistr, relWeights), ret)
  
  # end of test
  
})




context("utils:UtilLesionWeightsMatrixLesDistr")
test_that("UtilLesionWeightsMatrixLesDistr", {
  
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionWeightsMatrixDataset3", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesionWeightsMatrixLesDistr(c(0.8, 0.15, 0.05), c(0.1, 0.8, 0.1)) 
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilLesionWeightsMatrixLesDistr(c(0.8, 0.15, 0.05), c(0.1, 0.8, 0.1)), ret)
  # end of test
  
})




context("utils:UtilLesionDistrVector")
test_that("UtilLesionDistrVector", {

  ret <- c(0.78723404, 0.17021277, 0.04255319) 
  expect_equal(UtilLesionDistrVector (dataset05), ret)
  
  ret <- c(0.93258427, 0.06741573)
  expect_equal(UtilLesionDistrVector (dataset01), ret)
  
  expect_equal(UtilLesionDistrVector (datasetCadLroc), 1)
  
  expect_equal(UtilLesionDistrVector (datasetROI), c(0.125, 0.375, 0.4, 0.1))
  
})


context("utils:UtilLesionWeightsMatrixDataset")
test_that("UtilLesionWeightsMatrixDataset", {
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesionWeightsMatrixDataset (dataset02), ret)
  
  ret <- array(c(c(1, 2), c(1, 0.5), c(-Inf, 0.5)), dim = c(2,3)) 
  expect_equal(UtilLesionWeightsMatrixDataset (dataset01), ret)
  
  ret <- array(c(c(1, 2, 3), c(1, 0.5, 0.333333), c(-Inf, 0.5, 0.333333), c(-Inf, -Inf, 0.333333)), dim = c(3,4)) 
  expect_equal(UtilLesionWeightsMatrixDataset (dataset05), ret, tolerance = 1e-6, scale = 1)
  
  ret <- array(c(c(1, 2, 3), c(1, 0.8222222, 0.78723404), c(-Inf, 0.1777778, 0.1702128), c(-Inf, -Inf, 0.04255319)), dim = c(3,4)) 
  expect_equal(UtilLesionWeightsMatrixDataset(dataset05, relWeights = c(0.78723404, 0.17021277, 0.04255319)), ret, tolerance = 1e-6, scale = 1)
  
})

context("utils:UtilLesionWeightsMatrixLesDistr")
test_that("UtilLesionWeightsMatrixLesDistr", {
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesionWeightsMatrixLesDistr (1, 1), ret)
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesionWeightsMatrixLesDistr (1, 0), ret)
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesionWeightsMatrixLesDistr (1, 1), ret)
  
  expect_error(UtilLesionWeightsMatrixLesDistr (1, 1.5))
  
  ret <- array(c(c(1, 2, 3, 4), c(1, 0.25, 0.125, 0.1), c(-Inf, 0.75, 0.375, 0.3), c(-Inf, -Inf, 0.5,  0.4), c(-Inf, -Inf, -Inf,  0.2)), dim = c(4,5)) 
  expect_equal(UtilLesionWeightsMatrixLesDistr (c(0.1, 0.3, 0.4, 0.2), c(0.1, 0.3, 0.4, 0.2)), ret)
  
  # test if ((sum(lesDistr != 1)) || (sum(relWeights) != 1)) stop("lesDistr and relWeights arrays must each sum to unity")
  expect_error(UtilLesionWeightsMatrixLesDistr (c(0.1, 0.3, 0.41, 0.2), c(0.1, 0.3, 0.4, 0.2)))
  expect_error(UtilLesionWeightsMatrixLesDistr (c(0.1, 0.3, 0.41, 0.2), c(0.1, 0.305, 0.4, 0.2)))
  expect_error(UtilLesionWeightsMatrixLesDistr (c(0.1, 0.3, 0.4, 0.2), c(0.1, 0.305, 0.4, 0.2)))
  
  # third lesion has zero weight; i.e., dataset does not have any dis. cases with 3 lesions
  ret <- array(c(c(1, 2, 4), c(1, 0.5714286, 0.5), c(-Inf, 0.4285714, 0.3750000), c(-Inf, -Inf, 0.125)), dim = c(3,4)) 
  expect_equal(UtilLesionWeightsMatrixLesDistr (c(0.1, 0.7, 0.0, 0.2), c(0.4, 0.3, 0.2, 0.1)), ret, tolerance = 1e-6, scale = 1)
  
  
  ret <- array(c(c(1, 2, 3, 4), c(1, 0.333333, 0.2857143, 0.2), c(-Inf, 0.6666667, 0.5714286, 0.4000000), c(-Inf, -Inf, 0.1428571,  0.1000000), c(-Inf, -Inf, -Inf,  0.3000000)), dim = c(4,5)) 
  expect_equal(UtilLesionWeightsMatrixLesDistr (c(0.6, 0.2, 0.1, 0.1), c(0.2, 0.4, 0.1, 0.3)), ret, tolerance = 1e-6, scale = 1)
  
  ret <- array(c(c(1, 2, 4), c(1, 0.5714286, 0.5), c(-Inf, 0.4285714, 0.3750000), c(-Inf, -Inf, 0.125)), dim = c(3,4)) 
  expect_equal(UtilLesionWeightsMatrixLesDistr (c(0.1, 0.7, 0.0, 0.2), c(0.4, 0.3, 0.2, 0.1)), ret, tolerance = 1e-6, scale = 1)
  
})

