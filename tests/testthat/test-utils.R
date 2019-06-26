test_that("UtilIntrinsic2Physical", {
  mu <- 2;lambda <- 20;nu <- 1.1512925 
  # goodValues <- UtilIntrinsic2PhysicalRSM(mu, lambda, nu)
  # save(goodValues, file = "goodValues/UtilIntrinsic2PhysicalRSM")
  
  load("goodValues/UtilIntrinsic2PhysicalRSM")
  currentValues <- UtilIntrinsic2PhysicalRSM(mu, lambda, nu)
  expect_equal(goodValues, currentValues)
})


test_that("UtilAucBinormal", {
  a <- 2;b <- 0.7
  x <- 0.9493375
  expect_equal(UtilAucBinormal(a,b), x, tolerance = 1e-6, scale = x)
})

test_that("UtilAucCBM", {
  mu <- 2;alpha <- 0.8
  x <- 0.8370803
  expect_equal(UtilAucCBM(mu,alpha), x, tolerance = 1e-6, scale = x)
})

test_that("UtilAucPROPROC", {
  c1 <- .2;da <- 1.5
  x <- 0.8558655
  expect_equal(UtilAucPROPROC(c1,da), x, tolerance = 1e-6, scale = x)
})

test_that("UtilAucsRSM", {
  mu <- 1;lambdaP <- 1;nuP <- 1
  lesDistr <- rbind(c(1, 0.9), c(2, 0.1)) 
  x <- 0.8470065
  expect_equal(
    UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucROC, x, tolerance = 1e-6, scale = x)
  x <- 0.8071583
  expect_equal(
    UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucAFROC, x, tolerance = 1e-6, scale = x)
})


test_that("UtilPseudoValues", {
  
  dataset <- dataset05
  # "Wilcoxon" will generate error, skipping "SongA1" and "SongA2"
  FOM_arr <- c("AFROC", "AFROC1", "wAFROC", "wAFROC1", "MaxNLF", "MaxLLF", "MaxNLFAllCases", 
               "ExpTrnsfmSp", "HrSp", "HrSe")
  
  for (i in 1:length(FOM_arr)) {
    FOM <- FOM_arr[i]
    
    tmp <- tempfile()
    expect_warning(expect_known_output(
      UtilPseudoValues(dataset, FOM = FOM), 
      tmp, print = TRUE, update = TRUE),
      "Creating reference output")
    
    expect_known_output(
      UtilPseudoValues(dataset05, FOM = FOM), 
      tmp, print = TRUE, update = TRUE)
    
  }
  
})


test_that("UtilMeanSquaresDBMH", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilMeanSquares(dataset02, FOM = "Wilcoxon"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    UtilMeanSquares(dataset02, FOM = "Wilcoxon"), 
    tmp, print = TRUE, update = TRUE)
  
})


test_that("UtilLesionDistribution", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilLesionWeights (UtilLesionDistribution(dataset01)), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    UtilLesionWeights (UtilLesionDistribution(dataset01)), 
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilLesionWeights (UtilLesionDistribution(dataset05)), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    UtilLesionWeights (UtilLesionDistribution(dataset05)), 
    tmp, print = TRUE, update = TRUE)
  
})





