#' Compare three proper-ROC curve fitting models 
#' 
#' @description Applies the Radiological Search Model (RSM) and the 
#'    Contaminated Binormal Model (CBM) ROC-curve fitting methods to 14 
#'    datasets and compares the fits to Proper ROC (PROPROC) 
#'    fits obtained using Windows software downloaded from the Univ. of 
#'    Iowa ROC website ca. June 2017.
#' 
#' @usage ExampleCompare3ProperRocFits(startIndx = 1, endIndx = 14, 
#'    showPlot = FALSE, saveProprocLrcFile = FALSE, reAnalyze = FALSE) 
#' 
#' @param startIndx An integer in the range 1 to 14.
#' @param endIndx An integer in the range 1 to 14, greater than or equal 
#'    to \code{startIndx}.
#' @param showPlot If \code{TRUE} the three plots are shown along with 95 
#'    percent confidence intervals on the lowest and uppermost operating 
#'    points. The default is \code{FALSE}.
#' @param saveProprocLrcFile If \code{TRUE} the binned datasets are saved 
#'    for subsequent analysis using other ROC software, e.g., Windows 
#'    DBM-MRMC. The default is \code{FALSE}.
#' @param reAnalyze If \code{TRUE} the data is reanalyzed. The default 
#'    is \code{FALSE} in which case the previously saved results are used.
#' 
#' 
#' @return The returned value \code{allResults} is a \code{list} containing all
#'    results from the three parametric model fits. See details. 
#' 
#' 
#' @details allResults is a list-array with length equal to 
#'    (\code{endIndx} - \code{startIndx} + 1), where each element of the list-array 
#'    consists of 10 elements, see above. For example, \code{allResults[[1]]} 
#'    corresponds to 
#'    the dataset corresponding to \code{startIndx}. \code{allResults[[2]]} 
#'    corresponds to the dataset corresponding to \code{startIndx+1}, etc.
#' A specific member, e.g., \code{allResults[[1]]}, has the following structure:   
#' \itemize{
#' \item{\code{retRsm}}{ The RSM parameters following the output structure of \link{FitRsmRoc}}
#' \item{\code{retCbm}}{ The CBM parameters following the output structure of \link{FitCbmRoc}}
#' \item{\code{lesDistr}}{ The lesion distribution matrix}
#' \item{\code{c1}}{ The \code{c}-parameter of PROPROC}
#' \item{\code{da}}{ The \code{d_sub_a} parameter of PROPROC}
#' \item{\code{aucProp}}{ The \code{PROPROC} AUC}
#' \item{\code{I}}{ The number of modalities}
#' \item{\code{J}}{ The number of readers}
#' \item{\code{K1}}{ The number of non-diseased cases}
#' \item{\code{K2}}{ The number of diseased cases}
#' }
#' 
#' 
#' The PROPROC parameters were obtained by running Windows software OR 
#'    DBM-MRMC 2.50 (Sept. 04, 2014, Build 4) with \strong{PROPROC} and 
#'    \strong{area} selected. The \code{RSM} and \code{CBM} fits are implemented
#'    in this package. The corresponding returned objects contain all relevant 
#'    parameters. Chapter 18 of the author's book has further details. 
#'    If \code{saveProprocLrcFile} 
#'    is \code{TRUE}, the \code{.lrc} files will be written to the \code{File-Panes}
#'    directory, \strong{overwriting} any existing files with the same names.
#' 
#' ##  DPC notes on updating the results 2/17/18
#' ##  First run PROPROC on all datasets
#' ##  1. ret14 <- ExampleCompare3ProperRocFits(saveProprocLrcFile = TRUE) 
#' ##     this generates 14 .lrc files in RJafroc
#' ##  2. Move these files to VmWareShared folder
#' ##  3. Start VmWare and Windows 8
#' ##  4. Start OR DBM MRMC, select .lrc file, select PROPROC AUC and RUN ALL
#' ##  5. Repeat for each dataset
#' ##  6. Move 2 files (ending with .lroc and PROPROC area pooled.csv) from 
#' ##     VmWareShared to RJafroc/inst/MRMCRuns to appropriate subdirectories.
#' ##  7. Remove spaces in names of all "proproc area pooled.csv" files  
#' ##  8. ret14 <- ExampleCompare3ProperRocFits(reAnalyze = TRUE) 
#' ##     this generates new results files in RJafroc/inst/ANALYZED/RSM6
#' ##
#'  
#' @examples
#' \dontrun{
#' ExampleCompare3ProperRocFits(1,1)$allResults
#' }
#'
#' 
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' Metz CE, Pan X 1999 Proper Binormal ROC Curves: Theory and Maximum-Likelihood Estimation. 
#' J Math Psychol. \bold{43},(1):1--33.
#' 
#' Dorfman DD, Berbaum KS, 2000 A contaminated binormal model for ROC data: Part II. A formal model 
#' Acad Radiol \bold{7}, 427--437.
#'  
#' @export
#' 
ExampleCompare3ProperRocFits <- function(startIndx = 1, endIndx = 14, 
                                         showPlot = FALSE, saveProprocLrcFile = FALSE, reAnalyze = FALSE)
{
  options(warn = 2) # warnings as errors
  fileNames <-  c("TONY", "VD", "FR", 
                  "FED", "JT", "MAG", 
                  "OPT", "PEN", "NICO",
                  "RUS", "DOB1", "DOB2", 
                  "DOB3", "FZR")
  if (!(startIndx %in% seq(1,14) && endIndx %in% seq(1,14))) stop("illegal values for startIndx and/ or endIndx")
  AllBinnedDatasets <- as.list(array(dim = endIndx - startIndx + 1))
  count <- 0
  for (f in startIndx:endIndx) {
    fileName <- fileNames[f]
    theData <- get(sprintf("dataset%02d", f)) # the datasets already exist as R objects
    lesDistr <- UtilLesionDistribution(theData) # RSM ROC fitting needs to know lesDistr
    
    # convert to HR ROC data; and remove negative infinities
    rocData <- DfFroc2Roc(theData)
    
    if (saveProprocLrcFile) {
      DfSaveDataFile(rocData, 
                     fileName = paste0(fileName,".lrc"), format = "MRMC")
    }
    I <- length(rocData$modalityID);J <- length(rocData$readerID)
    K <- dim(rocData$NL)[3];K2 <- dim(rocData$LL)[3];K1 <- K - K2
    
    ## retrieve PROPROC parameters
    csvFileName <- paste0(fileName, "proprocareapooled.csv") # runs on July 29, 2017
    sysCsvFileName <- system.file(paste0("MRMCRuns/",fileName), csvFileName, package = "RJafroc", mustWork = TRUE)
    if (!file.exists(sysCsvFileName)) stop("need to run Windows PROPROC for this dataset using VMware Fusion")
    proprocRet <- read.csv(sysCsvFileName)
    c1 <- matrix(data = proprocRet$c, nrow = length(unique(proprocRet$T)), 
                 ncol = length(unique(proprocRet$R)), byrow = TRUE)
    da <- matrix(data = proprocRet$d_a, nrow = length(unique(proprocRet$T)), 
                 ncol = length(unique(proprocRet$R)), byrow = TRUE)
    
    retFileName <- paste0("allResults", fileName) 
    sysAnalFileName <- system.file("ANALYZED/RSM6", retFileName, package = "RJafroc", mustWork = TRUE)
    if (fileName %in% c("JT", "NICO", "DOB1", "DOB3")){
      binnedRocData <- DfBinDataset(rocData, desiredNumBins = 5, opChType = "ROC") # new function
    }else{
      binnedRocData <- rocData
    }
    
    cat(fileName,	" i, j, mu, lambdaP,	nuP, c,	da,	alpha, muCbm,	AUC-RSM, AUC-PROPROC, AUC-CBM, chisq, p-value,  df\n")
    if (reAnalyze || !file.exists(sysAnalFileName)){
      allResults <- list()
      AllResIndx <- 0
      for (i in 1:I){
        for (j in 1:J){
          #if (!(f == 2 && i == 2 && j == 2)) next ## investigating warnings
          AllResIndx <- AllResIndx + 1
          retCbm <- FitCbmRoc(binnedRocData, trt = i, rdr = j)
          retRsm <- FitRsmRoc(binnedRocData, trt = i, rdr = j, lesDistr = lesDistr) # fit to RSM, need lesDistr matrix
          retCbm1 <- retCbm[-10] # saving plots genrerates Note in R CMD CHK; file size too large
          retRsm1 <- retRsm[-11] #   do:
          aucProproc <- UtilAucPROPROC(c1[i,j], da[i,j])
          allResults[[AllResIndx]] <- list(retRsm = retRsm1, retCbm = retCbm1, lesDistr = lesDistr, 
                                           c1 = c1[i, j], da = da[i, j], aucProp = aucProproc, 
                                           I = I, J = J, K1 = K1, K2 = K2)
          x <- allResults[[AllResIndx]]
          if (showPlot) {
            lesDistr <- x$lesDistr
            empOp <- UtilBinCountsOpPts(binnedRocData, trt = i, rdr = j)
            fpf <- empOp$fpf; tpf <- empOp$tpf
            compPlot <- gpfPlotRsmPropCbm(
              which(fileNames == fileName), x$retRsm$mu, x$retRsm$lambdaP, x$retRsm$nuP, 
              lesDistr, c1[i, j], da[i, j],
              x$retCbm$mu, x$retCbm$alpha,
              fpf, tpf, i, j, K1, K2, c(1, length(fpf)))
            print(compPlot)
          }
          # follows same format as RSM Vs. Others.xlsx
          cat(fileName, i, j, x$retRsm$mu, x$retRsm$lambdaP, x$retRsm$nuP, 
              c1[i,j], da[i,j], 
              x$retCbm$alpha, x$retCbm$mu,
              x$retRsm$AUC, x$aucProp, x$retCbm$AUC, 
              x$retRsm$ChisqrFitStats[[1]], x$retRsm$ChisqrFitStats[[2]], 
              x$retRsm$ChisqrFitStats[[3]],"\n")
          next
        }
      }
      # safety comments
      ##sysSavFileName <- paste0("/Users/Dev/RJafroc/inst/ANALYZED/RSM6/", retFileName)
      ##save(allResults, file = sysSavFileName)
    } else {
      load(sysAnalFileName)
      AllResIndx <- 0
      #cat(fileName,	" i, j, mu, lambdaP,	nuP, c,	da,	alpha, muCbm,	AUC-RSM, AUC-PROPROC, AUC-CBM, chisq, p-value,  df\n")
      for (i in 1:I){
        for (j in 1:J){
          AllResIndx <- AllResIndx + 1
          x <- allResults[[AllResIndx]]
           if (showPlot) {
            empOp <- UtilBinCountsOpPts(binnedRocData, trt = i, rdr = j)
            fpf <- empOp$fpf; tpf <- empOp$tpf
            compPlot <- gpfPlotRsmPropCbm(
              which(fileNames == fileName), x$retRsm$mu, x$retRsm$lambdaP, x$retRsm$nuP, 
              lesDistr, c1[i, j], da[i, j],
              x$retCbm$mu, x$retCbm$alpha,
              fpf, tpf, i, j, K1, K2, c(1, length(fpf)))
            print(compPlot)
          }
          # follows same format as RSM Vs. Others.xlsx
          cat(fileName, i, j, x$retRsm$mu, x$retRsm$lambdaP, x$retRsm$nuP, 
              c1[i,j], da[i,j], 
              x$retCbm$alpha, x$retCbm$mu,
              x$retRsm$AUC, x$aucProp, x$retCbm$AUC, 
              x$retRsm$ChisqrFitStats[[1]], x$retRsm$ChisqrFitStats[[2]], 
              x$retRsm$ChisqrFitStats[[3]],"\n")
          next
        }
      }
      count <- count + 1
      AllBinnedDatasets[[count]] <- binnedRocData
      cat("\n\n\n")
    }
  }
  return(list(
    allResults = allResults,
    AllBinnedDatasets = AllBinnedDatasets
  ))
}



gpfPlotRsmPropCbm <- function(fileName, mu, lambdaP, nuP, lesDistr, c1, da, 
                              muCbm, alpha, fpf, tpf, i, j, K1, K2, ciIndx) {
  
  retProproc <- gpfPropRocOperatingCharacteristic(c1,da)
  FPFProp <- c(1, retProproc$FPF);TPFProp <- c(1, retProproc$TPF) # make sure it goes to upper-right corner
  plotProp <- data.frame(FPF = FPFProp, TPF = TPFProp, Model = "PROP")
  retRsm <- gpfRsmOperatingCharacteristic (mu, lambdaP, nuP, lesDistr = lesDistr)
  FPFRsm <- retRsm$FPF;TPFRsm <- retRsm$TPF
  plotRsm <- data.frame(FPF = FPFRsm, TPF = TPFRsm, Model = "RSM")
  dashedRsm <- data.frame(FPF = c(FPFRsm[1], 1), TPF = c(TPFRsm[1], 1), Model = "RSM")
  
  retCbm <- gpfCbmOperatingCharacteristic (muCbm, alpha)
  FPFCbm <- retCbm$FPF;TPFCbm <- retCbm$TPF
  plotCbm <- data.frame(FPF = FPFCbm, TPF = TPFCbm, Model = "CBM")
  
  plotCurve <- rbind(plotProp, plotCbm, plotRsm)
  plotCurve <- as.data.frame(plotCurve)
  plotOp <- data.frame(FPF = fpf, TPF = tpf)
  plotOp <- as.data.frame(plotOp)

  ij <- paste0("D", fileName, ", i = ", i, ", j = ", j)
  Model <- NULL # to get around R CMD CHK throwing a Note
  fitPlot <- ggplot(data = plotCurve) + 
    geom_line(mapping = aes(x = FPF, y = TPF, color = Model), size = 2) + 
    geom_line(data = dashedRsm, aes(x = FPF, y = TPF, color = Model), linetype = 3, size = 2) + 
    scale_color_manual(values = c("red", "darkblue", "black")) # color corresponds to order of plots in plotCurve

  fitPlot <- fitPlot + 
    geom_point(mapping = aes(x = FPF, y = TPF), data = plotOp, size = 5) +
    theme(legend.position = "none") + 
    ggtitle(ij) + theme(plot.title = element_text(size = 20,face="bold"))

  FPF <- fpf
  TPF <- tpf
  FPF <- FPF[ciIndx]
  TPF <- TPF[ciIndx]
  ciX <- binom.confint(x = FPF * K1, n = K1, methods = "exact")
  ciY <- binom.confint(x = TPF * K2, n = K2, methods = "exact")
  ciXUpper <- ciX$upper
  ciXLower <- ciX$lower
  ciYUpper <- ciY$upper
  ciYLower <- ciY$lower
  for (i in 1:length(FPF)){
    ciX <- data.frame(FPF = c(ciXUpper[i], ciXLower[i]), TPF = c(TPF[i], TPF[i]))
    ciY <- data.frame(FPF = c(FPF[i], FPF[i]), TPF = c(ciYUpper[i], ciYLower[i]))
    fitPlot <- fitPlot + geom_line(data = ciY, aes(x = FPF, y = TPF), color = "black") + 
      geom_line(data = ciX, aes(x = FPF, y = TPF), color = "black")
    barRgt <- data.frame(FPF = c(ciXUpper[i], ciXUpper[i]), TPF = c(TPF[i] - 0.01, TPF[i] + 0.01))
    barLft <- data.frame(FPF = c(ciXLower[i], ciXLower[i]), TPF = c(TPF[i] - 0.01, TPF[i] + 0.01))
    barUp <- data.frame(FPF = c(FPF[i] - 0.01, FPF[i] + 0.01), TPF = c(ciYUpper[i], ciYUpper[i]))
    barBtm <- data.frame(FPF = c(FPF[i] - 0.01, FPF[i] + 0.01), TPF = c(ciYLower[i], ciYLower[i]))
    fitPlot <- fitPlot + geom_line(data = barRgt, aes(x = FPF, y = TPF), color = "black") + 
      geom_line(data = barLft, aes(x = FPF, y = TPF), color = "black") + 
      geom_line(data = barUp, aes(x = FPF, y = TPF), color = "black") + 
      geom_line(data = barBtm, aes(x = FPF, y = TPF), color = "black")
  }
  return(fitPlot)
}


gpfPropRocOperatingCharacteristic <- function(c1, da){
  if (c1 < 0){
    plotZeta <- seq(da/(4 * c1) * sqrt(1 + c1^2), 8, by = 0.01)
  }else if(c1 > 0){
    plotZeta <- seq(-4, da/(4 * c1) * sqrt(1 + c1^2), by = 0.01)
  }else{
    plotZeta <- seq(-4, 10, by = 0.01)
  }
  FPFProp <- pnorm(-(1 - c1) * plotZeta - da/2 * sqrt(1 + c1^2)) + 
    pnorm(-(1 - c1) * plotZeta + da/(2*c1) * sqrt(1 + c1^2)) - ifelse(c1 >= 0, 1, 0)
  TPFProp <- pnorm(-(1 + c1) * plotZeta + da/2 * sqrt(1 + c1^2)) + 
    pnorm(-(1 + c1) * plotZeta + da/(2*c1) * sqrt(1 + c1^2)) - ifelse(c1 >= 0, 1, 0)
  if ((c1 == 1) && (da == 0)) {
    FPFProp <- c(FPFProp, seq(0,1,length.out = length(FPFProp)))
    TPFProp <- c(TPFProp, rep(1,length(TPFProp)))
  }
  return(list(FPF = FPFProp,
              TPF = TPFProp
  ))
}

gpfCbmOperatingCharacteristic <- function(mu, alpha){
  
  plotZeta <- seq(-4, mu+4, by = 0.1)
  
  FPFCbm <- 1 - pnorm(plotZeta)
  TPFCbm <- (1 - alpha) * (1 - pnorm(plotZeta)) + alpha * (1 - pnorm(plotZeta, mean = mu))
  
  return(list(FPF = FPFCbm,
              TPF = TPFCbm
  ))
}



gpfRsmOperatingCharacteristic <- function(mu, lambdaP, nuP, lesDistr){
  plotZeta <- seq(-4, mu+4, by = 0.01)
  
  FPFRsm <- sapply(plotZeta, xROC, lambdaP = lambdaP)
  TPFRsm <- sapply(plotZeta, yROC, mu = mu, lambdaP = lambdaP, 
                   nuP = nuP, lesDistr = lesDistr)
  
  return(list(FPF = FPFRsm,
              TPF = TPFRsm
  ))
}


