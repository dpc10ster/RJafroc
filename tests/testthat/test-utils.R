test_that("UtilIntrinsic2Physical", {
  mu <- 2;lambda <- 20;nu <- 1.1512925

  fn <- paste0(test_path(), "/goodValues361/Utils/Intrinsic2PhysicalRSM", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilIntrinsic2PhysicalRSM(mu, lambda, nu)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilIntrinsic2PhysicalRSM(mu, lambda, nu), ret)
  # end of test

})



test_that("UtilAucBinormal", {
  a <- 2;b <- 0.7

  fn <- paste0(test_path(), "/goodValues361/Utils/AucBinormal", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAucBinormal(a,b)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilAucBinormal(a,b), ret)
  # end of test

})



test_that("UtilAucCBM", {
  mu <- 2;alpha <- 0.8

  fn <- paste0(test_path(), "/goodValues361/Utils/AucCbm", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAucCBM(mu,alpha)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(UtilAucCBM(mu,alpha), ret)
  # end of test

})



test_that("UtilAucPROPROC", {
  c1 <- .2;da <- 1.5

  fn <- paste0(test_path(), "/goodValues361/Utils/AucPROPROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
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

    fn <- paste0(test_path(), "/goodValues361/Utils/AucRSM", ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
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

    fn <- paste0(test_path(), "/goodValues361/Utils/PseudoValues", "-", FOM_arr[i], ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      ret <- UtilPseudoValues(dataset, FOM = FOM_arr[i])
      saveRDS(ret, file = fn)
    }

    ret <- readRDS(fn)
    expect_equal(UtilPseudoValues(dataset, FOM = FOM_arr[i]), ret,
      info = paste0("FOM = ",FOM_arr[i])
    )
    # end of test

  }

})



test_that("UtilMeanSquaresDBMH", {

  fn <- paste0(test_path(), "/goodValues361/Utils/PseudoValues", "-", "Wilcoxon", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilPseudoValues(dataset02, FOM = "Wilcoxon")
    saveRDS(ret, file = fn)
  }

  ret1 <- readRDS(fn)
  ret2 <- UtilPseudoValues(dataset02, FOM = "Wilcoxon")
  expect_equal(ret2, ret1)
  # end of test

})



test_that("UtilLesionDistribution", {

    fn <- paste0(test_path(), "/goodValues361/Utils/LesionWeights01", ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      ret <- UtilLesionWeightsDistr (dataset01)
      saveRDS(ret, file = fn)
    }

    ret <- readRDS(fn)
    expect_equal(UtilLesionWeightsDistr (dataset01), ret)
    # end of test

    fn <- paste0(test_path(), "/goodValues361/Utils/LesionWeights05", ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      ret <- UtilLesionWeightsDistr (dataset05)
      saveRDS(ret, file = fn)
    }

    ret <- readRDS(fn)
    expect_equal(UtilLesionWeightsDistr (dataset05), ret)
    # end of test

})


