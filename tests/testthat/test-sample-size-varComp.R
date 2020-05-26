# TODO::DPC add sample size tests for an FROC dataset !!!DPC!!!

context("SsSampleSizeKGivenJ VarComp Input")

test_that("SsSampleSizeKGivenJ VarComp Input", {
  
  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "DBMH")
  a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")
  ret2 <- SsSampleSizeKGivenJ(dataset = NULL, 
                              J = 6, effectSize = 0.05, method = "DBMH", 
                              list(varYTR = a$VarCom["VarTR",1], 
                                   varYTC = a$VarCom["VarTC",1], 
                                   varYEps = a$VarCom["VarErr",1]))
  
  expect_equal(ret1, ret2)
  
  
  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "ORH")
  a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")
  KStar <- length(dataset02$NL[1,1,,1])
  ret2 <- SsSampleSizeKGivenJ(dataset = NULL, 
                              J = 6, 
                              effectSize = 0.05, 
                              method = "ORH", 
                              list(KStar = KStar, 
                                   VarTR = a$VarCom["VarTR",1], 
                                   Cov1 = a$VarCom["Cov1",1], 
                                   Cov2 = a$VarCom["Cov2",1], 
                                   Cov3 = a$VarCom["Cov3",1], 
                                   Var  = a$VarCom["Var",1]))
  expect_equal(ret1, ret2)
  
})


test_that("SsPowerGivenJK VarComp Input", {
  
  ret1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 251, method = "DBMH")
  a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")
  ret2 <- SsPowerGivenJK(dataset = NULL, 
                         J = 6, 
                         K = 251, 
                         effectSize = 0.05, 
                         method = "DBMH", 
                         list(varYTR = a$VarCom["VarTR",1], 
                              varYTC = a$VarCom["VarTC",1], 
                              varYEps = a$VarCom["VarErr",1]))
  
  expect_equal(ret1, ret2)
  
  
  ret1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", 
                         effectSize = 0.05, J = 6, K = 251, method = "ORH")
  a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")
  KStar <- length(dataset02$NL[1,1,,1])
  ret2 <- SsPowerGivenJK(dataset = NULL, 
                         effectSize = 0.05, 
                         J = 6, 
                         K = 251, 
                         method = "ORH", 
                         list(KStar = KStar, 
                              VarTR = a$VarCom["VarTR",1], 
                              Cov1 = a$VarCom["Cov1",1], 
                              Cov2 = a$VarCom["Cov2",1], 
                              Cov3 = a$VarCom["Cov3",1], 
                              Var  = a$VarCom["Var",1]))
  
  expect_equal(ret1, ret2)
  
})

