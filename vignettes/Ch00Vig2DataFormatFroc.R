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
x$lesionID

## -----------------------------------------------------------------------------
x$lesionWeight

## -----------------------------------------------------------------------------
x <- dataset11
str(x)

## -----------------------------------------------------------------------------
x$lesionVector

## -----------------------------------------------------------------------------
for (el in 1:max(x$lesionVector)) cat(
  "abnormal cases with", el, "lesions = ", 
  length(which(x$lesionVector == el)), "\n")

## -----------------------------------------------------------------------------
for (el in 1:max(x$lesionVector)) cat("fraction of abnormal cases with", el, "lesions = ", 
                                              length(which(x$lesionVector == el))/length(x$LL[1,1,,1]), "\n")

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

