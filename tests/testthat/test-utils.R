test_that("UtilIntrinsic2Physical", {
  mu <- 2;lambda <- 20;nu <- 1.1512925

  fn <- paste0(test_path(), "/goodValues/Utils/Intrinsic2PhysicalRSM")
  if (!file.exists(fn)) {
    ret <- UtilIntrinsic2PhysicalRSM(mu, lambda, nu)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilIntrinsic2PhysicalRSM(mu, lambda, nu), ret)
  # end of test

})


test_that("UtilAucBinormal", {
  a <- 2;b <- 0.7

  fn <- paste0(test_path(), "/goodValues/Utils/AucBinormal")
  if (!file.exists(fn)) {
    ret <- UtilAucBinormal(a,b)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilAucBinormal(a,b), ret)
  # end of test

})

test_that("UtilAucCBM", {
  mu <- 2;alpha <- 0.8

  fn <- paste0(test_path(), "/goodValues/Utils/AucCbm")
  if (!file.exists(fn)) {
    ret <- UtilAucCBM(mu,alpha)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilAucCBM(mu,alpha), ret)
  # end of test

})

test_that("UtilAucPROPROC", {
  c1 <- .2;da <- 1.5

  fn <- paste0(test_path(), "/goodValues/Utils/AucPROPROC")
  if (!file.exists(fn)) {
    ret <- UtilAucPROPROC(c1,da)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilAucPROPROC(c1,da), ret)
  # end of test

})

test_that("UtilAucsRSM", {
  mu <- 1;lambdaP <- 1;nuP <- 1
  lesDistr <- rbind(c(1, 0.9), c(2, 0.1))

    fn <- paste0(test_path(), "/goodValues/Utils/AucRSM")
    if (!file.exists(fn)) {
      ret <- UtilAucsRSM(mu, lambdaP, nuP, lesDistr)
      saveRDS(ret, file = fn)
    }

    ret <- readRDS(fn)
    expect_equal(UtilAucsRSM(mu, lambdaP, nuP, lesDistr), ret)
    # end of test

})


test_that("UtilPseudoValues", {

  dataset <- dataset05
  # "Wilcoxon" will generate error, skipping "SongA1" and "SongA2"
  FOM_arr <- c("AFROC", "AFROC1", "wAFROC", "wAFROC1", "MaxNLF", "MaxLLF", "MaxNLFAllCases",
               "ExpTrnsfmSp", "HrSp", "HrSe")

  for (i in 1:length(FOM_arr)) {

    fn <- paste0(test_path(), "/goodValues/Utils/PseudoValues", "-", FOM_arr[i])
    if (!file.exists(fn)) {
      ret <- UtilPseudoValues(dataset, FOM = FOM_arr[i])
      saveRDS(ret, file = fn)
    }

    ret <- readRDS(fn)
    expect_equal(UtilPseudoValues(dataset, FOM = FOM_arr[i]), ret)
    # end of test

  }

})


test_that("UtilMeanSquaresDBMH", {

  fn <- paste0(test_path(), "/goodValues/Utils/PseudoValues", "-", "Wilcoxon")
  if (!file.exists(fn)) {
    ret <- UtilPseudoValues(dataset02, FOM = "Wilcoxon")
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilPseudoValues(dataset02, FOM = "Wilcoxon"), ret)
  # end of test

})



test_that("UtilLesionDistribution", {

    fn <- paste0(test_path(), "/goodValues/Utils/LesionWeights01")
    if (!file.exists(fn)) {
      ret <- UtilLesionWeights (UtilLesionDistribution(dataset01))
      saveRDS(ret, file = fn)
    }

    ret <- readRDS(fn)
    expect_equal(UtilLesionWeights (UtilLesionDistribution(dataset01)), ret)
    # end of test

    fn <- paste0(test_path(), "/goodValues/Utils/LesionWeights05")
    if (!file.exists(fn)) {
      ret <- UtilLesionWeights (UtilLesionDistribution(dataset05))
      saveRDS(ret, file = fn)
    }

    ret <- readRDS(fn)
    expect_equal(UtilLesionWeights (UtilLesionDistribution(dataset05)), ret)
    # end of test

})


