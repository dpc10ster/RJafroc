# TODO::DPC add sample size tests for an FROC dataset !!!DPC!!!

context("SsSampleSizeKGivenJ VarComp Input")

test_that("SsSampleSizeKGivenJ VarComp Input", {
  
  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "DBMH")
  a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")$varComp
  ret2 <- SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "DBMH", 
                      list(varYTR = a$varTR, varYTC = a$varTC, varYEps = a$varErr))
  
  expect_equal(ret1, ret2)
  
  
  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "ORH")
  a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$varComp
  KStar <- length(dataset02$NL[1,1,,1])
  ret2 <- SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "ORH", 
                      list(KStar = KStar, varTR = a$varTR, cov1 = a$cov1, cov2 = a$cov2, 
                           cov3 = a$cov3, varEps = a$var))
  expect_equal(ret1, ret2)
  
})


test_that("SsPowerGivenJK VarComp Input", {
  
  ret1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 251, method = "DBMH")
  a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")$varComp
  ret2 <- SsPowerGivenJK(dataset = NULL, J = 6, K = 251, effectSize = 0.05, method = "DBMH", 
                 list(varYTR = a$varTR, varYTC = a$varTC, varYEps = a$varErr))
  
  expect_equal(ret1, ret2)
  
  
  ret1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 251, method = "ORH")
  a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$varComp
  KStar <- length(dataset02$NL[1,1,,1])
  ret2 <- SsPowerGivenJK(dataset = NULL, effectSize = 0.05, J = 6, K = 251, method = "ORH", 
                         list(KStar = KStar, varTR = a$varTR, cov1 = a$cov1, cov2 = a$cov2, 
                              cov3 = a$cov3, varEps = a$var))
  
  expect_equal(ret1, ret2)
  
})

