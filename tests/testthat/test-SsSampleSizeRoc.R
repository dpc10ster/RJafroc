contextStr <- "SsPowerGivenJK DBM/OR VarComp Input Franken data: after updating Ss routines from Hillis 2011 and 2018 papers"
context(contextStr)
test_that(contextStr, {
  
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset03-RRRC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsSampleSizeKGivenJ(dataset03, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "OR")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsSampleSizeKGivenJ(dataset03, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "OR")
  expect_equal(x1,x2)
  expect_equivalent(x1$powerRRRC, 0.8004469, tolerance = 1e-8) 
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset03-RRRC-LegacyCode", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsPowerGivenJK(dataset03, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 121, method = "DBM", LegacyCode = TRUE)
    saveRDS(x1, file = fn)
    
  }
  
  x1 <- readRDS(fn)
  x2 <- SsPowerGivenJK(dataset03, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 121, method = "DBM", LegacyCode = TRUE)
  expect_equal(x1,x2)
  expect_equivalent(x1$powerRRRC, 0.78574588, tolerance = 1e-8) # Legacy code gives less power
  
})


contextStr <- "SsPowerGivenJK OR VarComp Input VanDyke data: after updating Ss routines from Hillis 2011 and 2018 papers"
context(contextStr)
test_that(contextStr, {
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset02-RRRC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "OR", effectSize = -0.043800322, J = 10, K = 165)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "OR", effectSize = -0.043800322, J = 10, K = 165)
  expect_equal(x1,x2)
  expect_equivalent(x1$powerRRRC, 0.80541995, tolerance = 1e-8) 
  
  x3 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 10, K = 165, method = "DBM", LegacyCode = TRUE)  
  expect_equal(x1,x3)
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset02-FRRC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "OR", effectSize = -0.043800322, J = 10, K = 131, analysisOption = "FRRC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "OR", effectSize = -0.043800322, J = 10, K = 131, analysisOption = "FRRC")
  
  expect_equal(x1,x2)
  expect_equivalent(x1$powerFRRC, 0.80091813, tolerance = 1e-8)  
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/dataset02-FRFC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "OR", effectSize = -0.043800322, J = 10, K = 53, analysisOption = "RRFC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", method = "OR", effectSize = -0.043800322, J = 10, K = 53, analysisOption = "RRFC")
  
  expect_equal(x1,x2)
  expect_equivalent(x1$powerRRFC, 0.80496663, tolerance = 1e-8)  
  
})


contextStr <- "SsSampleSizeKGivenJ DBM & OR VarComp Input"
context(contextStr)
test_that(contextStr, {
  
  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "DBM")
  a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")
  ret2 <- SsSampleSizeKGivenJ(NULL,LegacyCode = TRUE,
                              J = 6, effectSize = 0.05, method = "DBM",
                              list(VarTR = a$VarCom["VarTR",1],
                                   VarTC = a$VarCom["VarTC",1],
                                   VarErr = a$VarCom["VarErr",1]))
  
  expect_equal(ret1, ret2)
  
  
  ret1 <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "OR")
  a <- UtilORVarComponentsFactorial(dataset02, FOM = "Wilcoxon")
  KStar <- length(dataset02$ratings$NL[1,1,,1])
  ret2 <- SsSampleSizeKGivenJ(NULL,
                              J = 6,
                              effectSize = 0.05,
                              method = "OR",
                              list(KStar = KStar,
                                   VarTR = a$VarCom["VarTR",1],
                                   Cov1 = a$VarCom["Cov1",1],
                                   Cov2 = a$VarCom["Cov2",1],
                                   Cov3 = a$VarCom["Cov3",1],
                                   Var  = a$VarCom["Var",1]))
  expect_equal(ret1, ret2)
  
})


contextStr <- "SsPowerGivenJK DBM & OR VarComp Input"
context(contextStr)
test_that(contextStr, {
  
  ret1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 251, method = "DBM")
  a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")
  ret2 <- SsPowerGivenJK(NULL,
                         J = 6,
                         K = 251,
                         effectSize = 0.05,
                         method = "DBM", LegacyCode = TRUE,
                         list(VarTR = a$VarCom["VarTR",1],
                              VarTC = a$VarCom["VarTC",1],
                              VarErr = a$VarCom["VarErr",1]))
  
  expect_equal(ret1, ret2)
  
  
  ret1 <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon",
                         effectSize = 0.05, J = 6, K = 251, method = "OR")
  a <- UtilORVarComponentsFactorial(dataset02, FOM = "Wilcoxon")
  KStar <- length(dataset02$ratings$NL[1,1,,1])
  ret2 <- SsPowerGivenJK(NULL,
                         effectSize = 0.05,
                         J = 6,
                         K = 251,
                         method = "OR",
                         list(KStar = KStar,
                              VarTR = a$VarCom["VarTR",1],
                              Cov1 = a$VarCom["Cov1",1],
                              Cov2 = a$VarCom["Cov2",1],
                              Cov3 = a$VarCom["Cov3",1],
                              Var  = a$VarCom["Var",1]))
  
  expect_equal(ret1, ret2)
  
})


contextStr <- "SsPowerGivenJKDbmVarCom & SsPowerGivenJKOrVarCom"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset02
  KStar <- length(dataset$ratings$NL[1,1,,1])
  stOR <- StSignificanceTesting(dataset, FOM = "Wilcoxon",
                                method = "OR")
  VarTR <- stOR$ANOVA$VarCom["VarTR",1]
  Cov1 <- stOR$ANOVA$VarCom["Cov1",1]
  Cov2 <- stOR$ANOVA$VarCom["Cov2",1]
  Cov3 <- stOR$ANOVA$VarCom["Cov3",1]
  Var <- stOR$ANOVA$VarCom["Var",1]
  
  ret1 <- SsPowerGivenJKOrVarCom (J = 5, K = 100, KStar = KStar,
                                  effectSize = 0.05, VarTR, Cov1, Cov2, Cov3, Var)
  
  
  VarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBM",
                                  analysisOption = "RRRC")$ANOVA$VarCom
  VarTR <- VarCom["VarTR",1]
  VarTC <- VarCom["VarTC",1]
  VarErr <- VarCom["VarErr",1]
  ret2 <- SsPowerGivenJKDbmVarCom (J = 5, K = 100, effectSize = 0.05, VarTR, VarTC, VarErr)
  
  expect_equal(ret1, ret2)
})

contextStr <- "SsPowerGivenJK:DBM: expected values are obtained for dataset02"
context(contextStr)
test_that(contextStr, {
  
  expect_equivalent(SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 6, K = 111, method = "DBM")$powerRRRC,
                    0.5526116, tolerance = 5e-8)
  
})


contextStr <- "SsPowerGivenJK:OR: expected values are obtained for dataset02"
context(contextStr)
test_that(contextStr, {
  expect_equivalent(SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 6, K = 111, method = "OR")$powerRRRC,
                    0.5526116, tolerance = 5e-8)
  
})

contextStr <- "SsSampleSizeKGivenJ:DBM: expected values are obtained for dataset02"
context(contextStr)
test_that(contextStr, {
  
  expect_equal(SsSampleSizeKGivenJ(dataset02, J = 6, FOM = "Wilcoxon", method = "DBM")$KRRRC,
               251)
})


contextStr <- "SsSampleSizeKGivenJ:OR: expected values are obtained for dataset02"
context(contextStr)
test_that(contextStr, {
  expect_equal(SsSampleSizeKGivenJ(dataset02, J = 6, FOM = "Wilcoxon", method = "OR")$KRRRC,
               251)
})

skip_on_cran()

x <- SsPowerTable(dataset02, FOM = "Wilcoxon")
y <- x$powerTableRRRC

contextStr <- "SsPowerTable:DBM: expected values are obtained for dataset02"
context(contextStr)
test_that(contextStr, {
  expect_equal(as.numeric(y$numReaders[7]),6)
  
})


contextStr <- "SsPowerTable:DBM: expected values are obtained for dataset02"
context(contextStr)
test_that(contextStr, {
  expect_equal(as.numeric(y$numCases[7]),251)
  
})


contextStr <- "SsPowerTable:DBM: expected values are obtained for dataset02"
context(contextStr)
test_that(contextStr, {
  expect_equal(as.numeric(y$power[7]),0.801)
  
})



