## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
frocCr <- system.file("extdata", "toyFiles/FROC/frocCr.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr, newExcelFileFormat = TRUE)
str(x)

## -----------------------------------------------------------------------------
x$lesions$IDs

## -----------------------------------------------------------------------------
x$lesions$weights

## -----------------------------------------------------------------------------
x <- dataset11
str(x)

## -----------------------------------------------------------------------------
x$lesions$perCase

## -----------------------------------------------------------------------------
for (el in 1:max(x$lesions$perCase)) cat(
  "abnormal cases with", el, "lesions = ", 
  length(which(x$lesions$perCase == el)), "\n")

## -----------------------------------------------------------------------------
for (el in 1:max(x$lesions$perCase)) cat("fraction of abnormal cases with", el, "lesions = ", 
                                              length(which(x$lesions$perCase == el))/length(x$LL[1,1,,1]), "\n")

## -----------------------------------------------------------------------------
lesDistr <- UtilLesionDistr(x)
lesDistr

## -----------------------------------------------------------------------------
sum(UtilLesionDistr(x)[,2])

## -----------------------------------------------------------------------------
lesWghtDistr <- UtilLesionWeightsDistr(x)
cat("dim(lesDistr) =", dim(lesDistr),"\n")
cat("dim(lesWghtDistr) =", dim(lesWghtDistr),"\n")
cat("lesWghtDistr = \n\n")
lesWghtDistr

