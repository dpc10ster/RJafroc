context("UtilLesWghtsLD")
test_that("UtilLesWghtsLD", {
  
  expect_equivalent(UtilLesDistr(c(0.1, 0.2, 0.3, 0.4))$Freq,c(0.1, 0.2, 0.3, 0.4))
  
  expect_equivalent(UtilLesDistr(dataset02)$Freq, 1)
  
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionDistrVectorDataset11", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesDistr(dataset11)$Freq 
    saveRDS(ret, file = fn)
  }
  ret <- readRDS(fn)
  expect_equal(UtilLesDistr(dataset11)$Freq, ret)
  
  
  ret <- c(0.78723404, 0.17021277, 0.04255319) 
  expect_equivalent(UtilLesDistr(dataset05)$Freq, ret)
  
  ret <- c(0.93258427, 0.06741573)
  expect_equivalent(UtilLesDistr(dataset01)$Freq, ret)
  
  expect_equivalent(UtilLesDistr(datasetCadLroc)$Freq, 1)
  
  expect_equivalent(UtilLesDistr(datasetROI)$Freq, c(0.125, 0.375, 0.4, 0.1))
  
})


context("UtilLesWghtsDS")
test_that("UtilLesWghtsDS", {
  
  ret <- array(1, dim = c(1,2)) 
  expect_equivalent(UtilLesWghtsDS(dataset02), ret)
  
  ret <- array(c(c(1, 2), c(1, 0.5), c(-Inf, 0.5)), dim = c(2,3)) 
  expect_equal(UtilLesWghtsDS (dataset01), ret)
  
  ret <- array(c(c(1, 2, 3), c(1, 0.5, 0.333333), c(-Inf, 0.5, 0.333333), c(-Inf, -Inf, 0.333333)), dim = c(3,4)) 
  expect_equal(UtilLesWghtsDS (dataset05), ret, tolerance = 1e-6, scale = 1)
  
  ret <- array(c(c(1, 2, 3), c(1, 0.8222222, 0.78723404), c(-Inf, 0.1777778, 0.1702128), c(-Inf, -Inf, 0.04255319)), dim = c(3,4)) 
  expect_equal(UtilLesWghtsDS(dataset05, relWeights = c(0.78723404, 0.17021277, 0.04255319)), ret, tolerance = 1e-6, scale = 1)
  
})

context("UtilLesWghtsLD")
test_that("UtilLesWghtsLD", {
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesWghtsLD (UtilLesDistr(1), 1), ret)
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesWghtsLD (UtilLesDistr(1), 0), ret)
  
  ret <- array(1, dim = c(1,2)) 
  expect_equal(UtilLesWghtsLD (UtilLesDistr(1), 1), ret)
  
  lesDistr <- c(0.6, 0.2, 0.1, 0.1)
  relWeights <- c(0.2, 0.4, 0.1, 0.3)
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionWeightsDistr1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesWghtsLD (UtilLesDistr(lesDistr), relWeights) 
    saveRDS(ret, file = fn)
  }
  ret <- readRDS(fn)
  expect_equal(UtilLesWghtsLD (UtilLesDistr(lesDistr), relWeights), ret)
  
  lesDistr <- c(0.1, 0.7, 0.0, 0.2)
  relWeights <- c(0.4, 0.3, 0.2, 0.1)
  fn <- paste0(test_path(), "/goodValues361/Utils/UtilLesionWeightsDistr2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilLesWghtsLD (UtilLesDistr(lesDistr), relWeights) 
    saveRDS(ret, file = fn)
  }
  ret <- readRDS(fn)
  expect_equal(UtilLesWghtsLD (UtilLesDistr(lesDistr), relWeights), ret)
  expect_equal(UtilLesWghtsLD (lesDistr, relWeights), ret)
  
  ret <- array(c(c(1, 2, 3, 4), c(1, 0.25, 0.125, 0.1), c(-Inf, 0.75, 0.375, 0.3), c(-Inf, -Inf, 0.5,  0.4), c(-Inf, -Inf, -Inf,  0.2)), dim = c(4,5)) 
  expect_equal(UtilLesWghtsLD (UtilLesDistr(c(0.1, 0.3, 0.4, 0.2)), c(0.1, 0.3, 0.4, 0.2)), ret)
  
  #expect_error(UtilLesWghtsLD (UtilLesDistr(c(0.1, 0.3, 0.41, 0.2)), c(0.1, 0.3, 0.4, 0.2)))
  
  expect_error(UtilLesWghtsLD (UtilLesDistr(c(0.1, 0.3, 0.41, 0.2)), c(0.1, 0.305, 0.4, 0.2)))
  
  expect_error(UtilLesWghtsLD (UtilLesDistr(c(0.1, 0.3, 0.4, 0.2)), c(0.1, 0.305, 0.4, 0.2)))
  
  ret <- array(c(c(1, 2, 3, 4), c(1, 0.333333, 0.2857143, 0.2), c(-Inf, 0.6666667, 0.5714286, 0.4000000), c(-Inf, -Inf, 0.1428571,  0.1000000), c(-Inf, -Inf, -Inf,  0.3000000)), dim = c(4,5)) 
  expect_equal(UtilLesWghtsLD (UtilLesDistr(c(0.6, 0.2, 0.1, 0.1)), c(0.2, 0.4, 0.1, 0.3)), ret, tolerance = 1e-6, scale = 1)
  

})

