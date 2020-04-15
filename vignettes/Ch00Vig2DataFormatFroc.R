## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 1: Truth worksheet for file inst/extdata/toyFiles/FROC/frocCr.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/frocCrTruth.png")

## -----------------------------------------------------------------------------
frocCr <- system.file("extdata", "toyFiles/FROC/frocCr.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr, newExcelFileFormat = TRUE)
str(x)

## -----------------------------------------------------------------------------
x$lesionID

## -----------------------------------------------------------------------------
x$lesionWeight

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 2: FP/NL worksheet for file inst/extdata/toyFiles/FROC/frocCr.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/frocCrNL.png")

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 3: TP/LL worksheet for file inst/extdata/toyFiles/FROC/frocCr.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/frocCrLL.png")

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

