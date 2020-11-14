context("Df2RJafrocDataset")
test_that("Df2RJafrocDataset", {
  
  # a small ROC dataset
  set.seed(1)
  z1 <- rnorm(5);z2 <- rnorm(7)*1.5 + 2
  
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/SmallRocDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- Df2RJafrocDataset(z1, z2)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(Df2RJafrocDataset(z1, z2), ds)
  # end of test
  
  ## Input as counts tables
  K1t <- c(30, 19, 8, 2, 1)
  K2t <- c(5,  6, 5, 12, 22)
  dataset <- Df2RJafrocDataset(K1t, K2t, InputIsCountsTable = TRUE)
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/CountsTableInput", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- Df2RJafrocDataset(K1t, K2t, InputIsCountsTable = TRUE)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(Df2RJafrocDataset(K1t, K2t, InputIsCountsTable = TRUE), ds)
  # end of test
  
  
  # a larger MRMC ROC dataset
  set.seed(1)
  I <- 2;J <- 3
  K1 <- 25;K2 <- 35
  z1 <- array(dim = c(I, J, K1))
  z2 <- array(dim = c(I, J, K2))
  mu <- 2;sigma <- 1.5
  for (i in 1:I) {
    for (j in 1:J) {
      z1[i,j,1:K1] <- rnorm(K1)
      z2[i,j,] <- rnorm(K2) * sigma + mu
    }
  }
  
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/largerMRMCDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- Df2RJafrocDataset(z1, z2)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(Df2RJafrocDataset(z1, z2), ds)
  # end of test
  #
})



context("SimulateFrocDataset")
test_that("SimulateFrocDataset", {
  
  # a FROC dataset
  set.seed(1)
  I <- 2;J <- 3
  K1 <- 25;K2 <- 35
  mu <- 1;nuP <- 0.8;lambdaP <- 1;zeta1 <- 0
  lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
  nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
  Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
  z1 <- array(-Inf,dim = c(I,J,K1+K2,40))
  z2 <- array(-Inf,dim = c(I,J,K2,40))
  dimNL <- array(dim=c(I,J,2))
  dimLL <- array(dim=c(I,J,2))
  for (i in 1:I) {
    for (j in 1:J) {
      frocDataRaw <- SimulateFrocDataset(
        mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, perCase = Lk2, seed = 1)
      dimNL[i,j,] <- dim(drop(frocDataRaw$ratings$NL))
      dimLL[i,j,] <- dim(drop(frocDataRaw$ratings$LL))
      z1[i,j,,1:dimNL[i,j,2]] <- drop(frocDataRaw$ratings$NL) # drop the excess location indices
      z2[i,j,,1:dimLL[i,j,2]] <- drop(frocDataRaw$ratings$LL)
    }
  }
  z1 <- z1[,,,1:max(dimNL[,,2])]
  z2 <- z2[,,,1:max(dimLL[,,2])]
  
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/FrocDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- Df2RJafrocDataset(z1, z2, perCase = Lk2) # an FROC dataset
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(Df2RJafrocDataset(z1, z2, perCase = Lk2), ds) # an FROC dataset
  # end of test
  
})



