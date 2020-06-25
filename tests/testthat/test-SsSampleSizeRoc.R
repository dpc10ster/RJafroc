context("SsPowerGivenJK OR VarComp Input")

test_that("Following tests added after updating Ss routines from Hillis 2011 and 2018 papers", {
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset02-RRRC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "ORH", effectSize = -0.043800322, J = 10, K = 165, analysisOption = "RRRC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "ORH", effectSize = -0.043800322, J = 10, K = 165, analysisOption = "RRRC")
  expect_equal(x1,x2)
  expect_equivalent(x1$powerRRRC, 0.80541995, tolerance = 1e-8) 
  
  x3 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 10, K = 165, method = "DBMH", LegacyCode = TRUE)  
  expect_equal(x1,x3)
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset02-FRRC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "ORH", effectSize = -0.043800322, J = 10, K = 131, analysisOption = "FRRC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "ORH", effectSize = -0.043800322, J = 10, K = 131, analysisOption = "FRRC")
  
  expect_equal(x1,x2)
  expect_equivalent(x1$powerFRRC, 0.80091813, tolerance = 1e-8)  
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset02-FRFC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "ORH", effectSize = -0.043800322, J = 10, K = 53, analysisOption = "RRFC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "ORH", effectSize = -0.043800322, J = 10, K = 53, analysisOption = "RRFC")
  
  expect_equal(x1,x2)
  expect_equivalent(x1$powerRRFC, 0.80496663, tolerance = 1e-8)  
  
})


context("SsSampleSizeKGivenJ DBM & OR VarComp Input")
test_that("SsSampleSizeKGivenJ DBM & OR VarComp Input", {

  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "DBMH")
  a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")
  ret2 <- SsSampleSizeKGivenJ(NULL,LegacyCode = TRUE,
                              J = 6, effectSize = 0.05, method = "DBMH",
                              list(VarTR = a$VarCom["VarTR",1],
                                   VarTC = a$VarCom["VarTC",1],
                                   VarErr = a$VarCom["VarErr",1]))

  expect_equal(ret1, ret2)


  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "ORH")
  a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")
  KStar <- length(dataset02$ratings$NL[1,1,,1])
  ret2 <- SsSampleSizeKGivenJ(NULL,
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
  ret2 <- SsPowerGivenJK(NULL,
                         J = 6,
                         K = 251,
                         effectSize = 0.05,
                         method = "DBMH", LegacyCode = TRUE,
                         list(VarTR = a$VarCom["VarTR",1],
                              VarTC = a$VarCom["VarTC",1],
                              VarErr = a$VarCom["VarErr",1]))

  expect_equal(ret1, ret2)


  ret1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon",
                         effectSize = 0.05, J = 6, K = 251, method = "ORH")
  a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")
  KStar <- length(dataset02$ratings$NL[1,1,,1])
  ret2 <- SsPowerGivenJK(NULL,
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


context("SsPowerGivenJKDbmVarCom & SsPowerGivenJKOrVarCom")
test_that("SsPowerGivenJKDbmVarCom & SsPowerGivenJKOrVarCom", {

  dataset <- dataset02
  KStar <- length(dataset$ratings$NL[1,1,,1])
  stORH <- StSignificanceTesting(dataset, FOM = "Wilcoxon",
                                  method = "ORH", analysisOption = "RRRC")
  VarTR <- stORH$ANOVA$VarCom["VarTR",1]
  Cov1 <- stORH$ANOVA$VarCom["Cov1",1]
  Cov2 <- stORH$ANOVA$VarCom["Cov2",1]
  Cov3 <- stORH$ANOVA$VarCom["Cov3",1]
  Var <- stORH$ANOVA$VarCom["Var",1]

  ret1 <- SsPowerGivenJKOrVarCom (J = 5, K = 100, KStar = KStar,
                                   effectSize = 0.05, VarTR, Cov1, Cov2, Cov3, Var, analysisOption = "RRRC")


  VarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH",
                                  analysisOption = "RRRC")$ANOVA$VarCom
  VarTR <- VarCom["VarTR",1]
  VarTC <- VarCom["VarTC",1]
  VarErr <- VarCom["VarErr",1]
  ret2 <- SsPowerGivenJKDbmVarCom (J = 5, K = 100, effectSize = 0.05, VarTR,
                                    VarTC, VarErr, analysisOption = "RRRC")

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
          expect_equal(as.numeric(y$numReaders[7]),6))
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numCases[7]),251))
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$power[7]),0.801))


