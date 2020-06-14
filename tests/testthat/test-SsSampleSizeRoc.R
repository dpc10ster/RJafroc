context("SsSampleSizeKGivenJ DBM & OR VarComp Input")

test_that("SsSampleSizeKGivenJ DBM & OR VarComp Input", {
  
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
  KStar <- length(dataset02$ratings$NL[1,1,,1])
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


test_that("SsPowerGivenJK DBM & OR VarComp Input", {
  
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
  KStar <- length(dataset02$ratings$NL[1,1,,1])
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


context("SsPowerGivenJKDbmVarComp & SsPowerGivenJKOrVarComp")
test_that("SsPowerGivenJKDbmVarComp & SsPowerGivenJKOrVarComp", {
  
  dataset <- dataset02 ## the pilot study
  KStar <- length(dataset$ratings$NL[1,1,,1])
  VarCom <- StSignificanceTesting(dataset, FOM = "Wilcoxon", 
                                  method = "ORH", analysisOption = "RRRC")$ANOVA$VarCom
  VarTR <- VarCom["VarTR",1]
  Cov1 <- VarCom["Cov1",1]
  Cov2 <- VarCom["Cov2",1]
  Cov3 <- VarCom["Cov3",1]
  Var <- VarCom["Var",1]
  ret1 <- SsPowerGivenJKOrVarComp (J = 5, K = 100, KStar = KStar,  
                                   effectSize = 0.05, VarTR, Cov1, Cov2, Cov3, Var, analysisOption = "RRRC")
  
  
  VarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH", 
                                  analysisOption = "RRRC")$ANOVA$VarCom
  varYTR <- VarCom["VarTR",1]
  varYTC <- VarCom["VarTC",1]
  varYEps <- VarCom["VarErr",1]
  ret2 <- SsPowerGivenJKDbmVarComp (J = 5, K = 100, effectSize = 0.05, varYTR, 
                                    varYTC, varYEps, analysisOption = "RRRC")
  
  expect_equal(ret1, ret2)
})

context("Sample Size ROC")

test_that("SsPowerGivenJK:DBMH: expected values are obtained for dataset02", 
          expect_equivalent(SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 6, K = 111, analysisOption = "RRRC")$powerRRRC, 
                            0.5526116, tolerance = 5e-8))

test_that("SsPowerGivenJK:ORH: expected values are obtained for dataset02", 
          expect_equivalent(SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 6, K = 111, analysisOption = "RRRC", method = "ORH")$powerRRRC, 
                            0.5526116, tolerance = 5e-8))


test_that("SsSampleSizeKGivenJ:DBMH: expected values are obtained for dataset02", 
          expect_equal(SsSampleSizeKGivenJ(dataset02, J = 6, FOM = "Wilcoxon", analysisOption = "RRRC")$KRRRC, 
                            251))

test_that("SsSampleSizeKGivenJ:ORH: expected values are obtained for dataset02", 
          expect_equal(SsSampleSizeKGivenJ(dataset02, J = 6, FOM = "Wilcoxon", analysisOption = "RRRC", method = "ORH")$KRRRC, 
                       251))

skip_on_cran()

x <- SsPowerTable(dataset02, FOM = "Wilcoxon")
y <- x$powerTableRRRC
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numReaders[4]),6))
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numCases[4]),251))
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$power[4]),0.801))


x <- SsPowerTable(dataset02, FOM = "Wilcoxon", method = "ORH")
y <- x$powerTableRRRC
test_that("SsPowerTable:ORH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numReaders[4]),6))
test_that("SsPowerTable:ORH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numCases[4]),251))
test_that("SsPowerTable:ORH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$power[4]),0.801))

