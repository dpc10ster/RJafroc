## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
fed <- system.file("extdata", "FrocData.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x1 <- DfReadDataFile(fed, newExcelFileFormat = FALSE)
str(x1$NL)
str(x1$LL)
t1 <- x1$truthTableStr
str(t1)

## -----------------------------------------------------------------------------
sum(is.na(t1[,1,1:100,1]))
sum(is.na(t1[,,1:100,1]))
sum(is.na(t1[,1,101:200,1]))
sum(is.na(t1[,1,101:200,2]))
sum(is.na(t1[,1,101:200,3]))
sum(is.na(t1[,1,101:200,4]))

## -----------------------------------------------------------------------------
t1[,1,26:100,1] <- NA;t1[,1,126:200,] <- NA
t1[,2,1:25,1] <- NA;t1[,2,51:100,1] <- NA;t1[,2,101:125,] <- NA;t1[,2,151:200,] <- NA
t1[,3,1:50,1] <- NA;t1[,3,76:100,1] <- NA;t1[,3,101:150,] <- NA;t1[,3,176:200,] <- NA
t1[,4,1:75,1] <- NA;t1[,4,101:175,] <- NA

## -----------------------------------------------------------------------------
fedsp <- system.file("extdata", "toyFiles/FROC/FrocDataSp.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x2 <- DfReadDataFile(fedsp, newExcelFileFormat = TRUE)

## -----------------------------------------------------------------------------
t2 <- x2$truthTableStr
testthat::expect_equal(t1, t2)

## -----------------------------------------------------------------------------
testthat::expect_equal(x1$NL[,1,1:25,1], x2$NL[,1,1:25,1])
testthat::expect_equal(x1$NL[,2,26:50,1], x2$NL[,2,26:50,1])
testthat::expect_equal(x1$NL[,3,51:75,1], x2$NL[,3,51:75,1])
testthat::expect_equal(x1$NL[,4,76:100,1], x2$NL[,4,76:100,1])

## -----------------------------------------------------------------------------
testthat::expect_equal(x1$NL[,1,101:125,], x2$NL[,1,101:125,])
testthat::expect_equal(x1$NL[,2,126:150,], x2$NL[,2,126:150,])
testthat::expect_equal(x1$NL[,3,151:175,], x2$NL[,3,151:175,])
testthat::expect_equal(x1$NL[,4,176:200,], x2$NL[,4,176:200,])

## -----------------------------------------------------------------------------
testthat::expect_equal(x1$LL[,1,1:25,], x2$LL[,1,1:25,])
testthat::expect_equal(x1$LL[,2,26:50,], x2$LL[,2,26:50,])
testthat::expect_equal(x1$LL[,3,51:75,], x2$LL[,3,51:75,])
testthat::expect_equal(x1$LL[,4,76:100,], x2$LL[,4,76:100,])

