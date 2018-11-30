#' Plot empirical operating characteristics for specified dataset, treatment and reader
#' 
#' Plot empirical operating characteristics (operating points connected by straight 
#'    lines) for specified treatments and readers, or if desired, plots only 
#'    (no operating points) averaged over specified treatments and / or readers
#' 
#' @param dataset Dataset to be used for plotting
#' @param trts List or vector: \strong{integer} indices of treatments to be plotted
#' @param rdrs List or vector: \strong{integer} indices of readers to be plotted
#' @param opChType Type of operating characteristic to be plotted:  
#'    \code{"ROC"}(the default), \code{"FROC"}, \code{"AFROC"},  \code{"wAFROC"},
#'    \code{"AFROC1"}, or \code{"wAFROC1"}
#' 
#' @details The \code{trts} and \code{rdrs} are vectors or lists of \strong{integer}  
#'    indices, not the corresponding \strong{string} IDs. For example, if the string 
#'    ID of the first reader is "0", the value in \code{rdrs} should be 
#'    \strong{1} not \strong{0}. The legend shows the string IDs. 
#' 
#' If both of \code{trts} and \code{rdrs} are vectors, all combinations of treatments 
#'    and readers are plotted. See Example 1.
#' 
#' If both \code{trts} and \code{rdrs} are lists, they must have the same length. 
#'    Only the combination of treatment and reader at the same position in their 
#'    respective lists are plotted. If some elements of the treatments and / or 
#'    readers lists are vectors, the average operating characteristic over the 
#'    implied treatments and / or readers are plotted. See Example 2.
#' 
#' @return A \pkg{ggplot2} object containing the operating characteristic plot(s) 
#' and a data frame containing the points defining the operating characteristics 
#' are returned. For example, the returned objects for \code{"ROC"} operating 
#' characteristics are as follows:
#' 
#' @return \item{Plot}{\pkg{ggplot2} object. 
#' For continuous or averaged data, operating characteristics curves are plotted 
#' \strong{without} showing operating points. For binned individual data, both operating 
#' points and connecting lines are shown. To avoid clutter, if there are more than 20 
#' operating points, they are not shown}
#' 
#' @return \item{Points}{Data frame with four columns: abscissa, ordinate, class (which 
#' codes treatment and reader) and type, which can be \code{"individual"}, \code{"continuous"} or 
#' \code{"average"}; \code{"individual"} refers to a one treatment and one reader.}
#' 
#' 
#' @examples
#' ## Example 1
#' ## Plot individual empirical ROC plots for all combinations of treatments
#' ## 1 and 2 and readers 1, 2 and 3. Six operating characteristics are plotted.
#' 
#' ret <- PlotEmpiricalOperatingCharacteristics(dataset = 
#' dataset02, trts = c(1:2), rdrs = c(1:3))
#' print(ret$Plot)
#' 
#' ## Example 2
#' ## Empirical ROC, FROC, AFROC and wAFROC plots. Each plot consists of
#' ## three parts (see Example 3 for correspondences between indices and string identifiers 
#' ## for treatments and readers):
#' ## (1) plot for the 1st treatment (string ID "1") and the 2nd reader (string ID "3") 
#' ## (2) plot for the 2nd treatment (string ID "2") AVERAGED over the 2nd and 3rd readers 
#' ##     (string IDs "3" and "4"), and 
#' ## (3) plot AVERAGED over the first two treatments (string IDs "1" and "2") AND over 
#' ## the 1st, 2nd and 3rd readers (string IDs "1", "3" and "4") 
#' 
#' plotT <- list(1, 2, c(1:2))
#' plotR <- list(2, c(2:3), c(1:3))
#' ret <- PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = plotT, rdrs = plotR)
#' print(ret$Plot)
#' 
#' ret <- PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = plotT, rdrs = plotR,
#'                   opChType = "FROC")               
#' print(ret$Plot)
#' 
#' ret <- PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = plotT, rdrs = plotR,
#'                   opChType = "AFROC")
#' print(ret$Plot)
#' 
#' ret <- PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = plotT, rdrs = plotR,
#'                   opChType = "wAFROC")                  
#' print(ret$Plot)
#' 
#' ##Example 3
#' ## Correspondences between indices and string identifiers for treatments and 
#' ## readers in this dataset. Apparently reader "2" did not complete the study.
#' 
#' str(dataset04)
#' 
#' ## List of 8
#' ## $ NL          : num [1:5, 1:4, 1:200, 1:7] -Inf -Inf -Inf -Inf -Inf ...
#' ## $ LL          : num [1:5, 1:4, 1:100, 1:3] 5 4 4 3 5 5 4 2 4 5 ...
#' ## $ lesionNum   : int [1:100] 1 1 1 1 1 1 1 1 1 1 ...
#' ## $ lesionID    : num [1:100, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
#' ## $ lesionWeight: num [1:100, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
#' ## $ dataType    : chr "FROC"
#' ## $ modalityID  : chr [1:5] "1" "2" "3" "4" "5"
#' ## $ readerID    : chr [1:4] "1" "3" "4" "5"
#' 
#' @export

PlotEmpiricalOperatingCharacteristics <- function(dataset, trts = 1, rdrs = 1, 
                                                  opChType = "ROC") 
{
  
  if (opChType == "ROC") {
    dataset <- DfFroc2Roc(dataset)
    ret <- gpfPlotGenericEmpiricalOperatingCharacteristic(dataset, trts, rdrs, opChType = "ROC")
  } else if ((opChType == "FROC") && (dataset$dataType != "ROC")) {
    ret <- gpfPlotGenericEmpiricalOperatingCharacteristic(dataset, trts, rdrs, opChType = "FROC")
  } else if ((opChType == "AFROC") && (dataset$dataType != "ROC")) {
    ret <- gpfPlotGenericEmpiricalOperatingCharacteristic(dataset, trts, rdrs, opChType = "AFROC")
  } else if ((opChType == "wAFROC") && (dataset$dataType != "ROC")) {
    ret <- gpfPlotGenericEmpiricalOperatingCharacteristic(dataset, trts, rdrs, opChType = "wAFROC")
  } else if ((opChType == "AFROC1") && (dataset$dataType != "ROC")) {
    ret <- gpfPlotGenericEmpiricalOperatingCharacteristic(dataset, trts, rdrs, opChType = "AFROC1")
  } else if ((opChType == "wAFROC1") && (dataset$dataType != "ROC")) {
    ret <- gpfPlotGenericEmpiricalOperatingCharacteristic(dataset, trts, rdrs, opChType = "wAFROC1")
  } else {
    errMsg <- sprintf("%s is not a valid operating characteristic for this dataset", opChType)
    stop(errMsg)
  }
  return(ret)
} 



gpfPlotGenericEmpiricalOperatingCharacteristic <- function(dataset, treatments2Plot, readers2Plot, opChType) { #dpc
  
  genNL <- dataset$NL
  genLL <- dataset$LL
  lesionNum <- dataset$lesionNum
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  weights <- dataset$lesionWeight
  
  # handle operating points
  # first handle no list arguments; gen stands for generic
  if (!is.list(treatments2Plot) && !is.list(readers2Plot)) {
    genPoints <- genPoints(genNL, genLL, modalityID, readerID, 
                           lesionNum, weights, treatments2Plot, readers2Plot, opChType)
  } else {
    # now handle lists case, must be identical lengths
    if (is.list(treatments2Plot) && is.list(readers2Plot) && length(treatments2Plot) == length(readers2Plot)) {
      genPoints <- data.frame(genAbscissa = NULL, genOrdinate = NULL)
      for (i in 1:length(readers2Plot)) {
        if (length(treatments2Plot[[i]]) == 1 && (length(readers2Plot[[i]]) == 1)) {
          tempGenPoints <- genPoints(genNL, genLL, modalityID, readerID,  
                                     lesionNum, weights, treatments2Plot[[i]], readers2Plot[[i]], opChType)
          genPoints <- rbind(genPoints, tempGenPoints)
        } else {
          tempGenPoints <- genAvgPoints(genNL, genLL, modalityID, readerID, 
                                        lesionNum, weights, treatments2Plot[[i]], readers2Plot[[i]], opChType)
          genPoints <- rbind(genPoints, tempGenPoints)
        }
      }
    } else if (is.list(treatments2Plot) && length(readers2Plot) == 1) {
      readers2Plot <- readers2Plot[[1]]
      genPoints <- data.frame(genAbscissa = NULL, genOrdinate = NULL)
      for (i in 1:length(treatments2Plot)) {
        if (length(treatments2Plot[[i]]) == 1) {
          tempGenPoints <- genPoints(genNL, genLL, modalityID, readerID, 
                                     lesionNum, weights, treatments2Plot[[i]], readers2Plot, opChType)
          genPoints <- rbind(genPoints, tempGenPoints)
        } else {
          tempGenPoints <- genAvgPoints(genNL, genLL, modalityID, readerID, 
                                        lesionNum, weights, treatments2Plot[[i]], readers2Plot, opChType)
          genPoints <- rbind(genPoints, tempGenPoints)
        }
      }
    } else if (is.list(readers2Plot) && length(treatments2Plot) == 1) {
      if (is.list(treatments2Plot))
        treatments2Plot <- treatments2Plot[[1]]
      genPoints <- data.frame(genAbscissa = NULL, genOrdinate = NULL)
      for (i in 1:length(readers2Plot)) {
        if (length(readers2Plot[[i]]) == 1) {
          tempGenPoints <- genPoints(genNL, genLL, modalityID, readerID, 
                                     lesionNum, weights, treatments2Plot, readers2Plot[[i]], opChType)
          genPoints <- rbind(genPoints, tempGenPoints)
        } else {
          tempGenPoints <- genAvgPoints(genNL, genLL, modalityID, readerID, 
                                        lesionNum, weights, treatments2Plot, readers2Plot[[i]], opChType)
          genPoints <- rbind(genPoints, tempGenPoints)
        }
      }
    } else {
      stop("Lengths of trts and rdrs do not match.")
    }
  }
  
  ret <- GetLimits(genPoints, opChType)
  xLim <- ret$xLim;yLim <- ret$yLim
  
  classes <- unique(genPoints$class)
  for (i in 1:length(classes)){
    indices <- genPoints$class == classes[i]
    if (sum(indices) > 20 && all(genPoints$type[indices] == "individual")){
      typeTmp <- as.character(genPoints$type)
      typeTmp[indices] <- "continuous"
      genPoints$type <- typeTmp
    }
  }
  
  # handle plots
  # no lists
  if (!is.list(treatments2Plot) && !is.list(readers2Plot)) {
    mr <- unlist(strsplit(as.character(genPoints$class), split = "\n"))
    dim(mr) <- c(2, round(length(mr)/2))
    genPoints <- cbind(genPoints, data.frame(Modality = mr[1, ], Reader = mr[2, ]))
    genOpPoints <- genPoints[genPoints$type == "individual" &
                               !((genPoints$genAbscissa == 0 & genPoints$genOrdinate == 0) |
                                   (genPoints$genAbscissa == 1 & genPoints$genOrdinate == 1)), ]
    legendLength <- length(readers2Plot)
    shapeVector <- rep(NA, length(readers2Plot))
    for (n in 1:legendLength) {
      index <- which(genPoints$Reader == levels(genPoints$Reader)[n])[1]
      if (genPoints$type[index] == "individual")
        shapeVector[n] <- 16 #http://www.sthda.com/english/wiki/ggplot2-point-shapes
    }
    if (length(treatments2Plot) == 1) {
      if (length(readers2Plot) > 1){
        # one treatment multiple readers
        genPlot <- with(genPoints, {
          tempGenPlot <- ggplot()
          mStrings <- unique(as.character(genPoints$Modality))
          for (i in 1:length(treatments2Plot)) {
            tempGenPlot <- tempGenPlot +
              geom_line(data = genPoints, aes(x = genAbscissa, y = genOrdinate, color = class))
          }
          tempGenPlot <- tempGenPlot +
            geom_point(data = genOpPoints, aes(x = genAbscissa, y = genOrdinate, color = class)) +
            theme(legend.title = element_blank())
        })
      }
      if (length(readers2Plot) == 1){
        # one treatment one reader
        genPlot <- with(genPoints, {
          tempGenPlot <- ggplot()
          mStrings <- unique(as.character(genPoints$Modality))
          for (i in 1:length(treatments2Plot)) {
            tempGenPlot <- tempGenPlot +
              geom_line(data = genPoints, aes(x = genAbscissa, y = genOrdinate))
          }
          tempGenPlot <- tempGenPlot +
            geom_point(data = genOpPoints, aes(x = genAbscissa, y = genOrdinate))
        })
      }
    } else {
      # multiple treatments and one or more readers
      genPlot <- with(genPoints, {
        tempGenPlot <- ggplot()
        mStrings <- unique(as.character(genPoints$Modality))
        for (i in 1:length(treatments2Plot)) {
          tempGenPlot <- tempGenPlot +
            geom_line(data = genPoints[genPoints$Modality == mStrings[i], ],
                      aes(x = genAbscissa, y = genOrdinate, color = Reader, linetype = Modality))
        }
        tempGenPlot <- tempGenPlot +
          geom_point(data = genOpPoints, aes(x = genAbscissa, y = genOrdinate, color = Reader)) +
          theme(legend.title = element_blank())
      })
    }
    genPoints <- data.frame(genAbscissa = genPoints$genAbscissa, genOrdinate = genPoints$genOrdinate, class = genPoints$class,
                            type = genPoints$type)
  } else {
    genOpPoints <- genPoints[genPoints$type == "individual" &
                               !((genPoints$genAbscissa == 0 & genPoints$genOrdinate == 0) |
                                   (genPoints$genAbscissa == 1 & genPoints$genOrdinate == 1)), ]
    
    legendLength <- length(levels(genPoints$class))
    shapeVector <- rep(NA, length(levels(genPoints$class)))
    for (n in 1:legendLength) {
      index <- which(genPoints$class == levels(genPoints$class)[n])[1]
      if (genPoints$type[index] == "individual")
        shapeVector[n] <- 16
    }
    genPlot <- with(genPoints, {
      ggplot(data = genPoints, aes(x = genAbscissa, y = genOrdinate, color = class)) +
        geom_line() + geom_point(data = genOpPoints) +
        theme(legend.title = element_blank())
    })
  }
  if (opChType == "ROC") genPlot <- genPlot + xlab("FPF") + ylab("TPF")
  if (opChType == "FROC") genPlot <- genPlot + xlab("NLF") + ylab("LLF")
  if (opChType == "AFROC") genPlot <- genPlot + xlab("FPF") + ylab("LLF")
  if (opChType == "wAFROC") genPlot <- genPlot + xlab("FPF") + ylab("wLLF")
  if (opChType == "AFROC1") genPlot <- genPlot + xlab("FPF1") + ylab("LLF")
  if (opChType == "wAFROC1") genPlot <- genPlot + xlab("FPF1") + ylab("wLLF")
  genPlot <- genPlot  + scale_x_continuous(limits = c(0,xLim)) + scale_y_continuous(limits = c(0,yLim))
  return(list(Plot = genPlot, Points = genPoints))
}






####################################################################################################################
GetLimits <- function (genPoints, opChType) {
  if (opChType == "ROC") {
    xLim <- 1
    yLim <- 1
  } else if (opChType == "FROC") {
    xLim <- ceiling(max(genPoints$genAbscissa) / 0.05 ) * 0.05 # rounds up to the nearest .05
    yLim <- 1
  } else if (opChType == "AFROC") {
    xLim <- 1
    yLim <- 1
  } else if (opChType == "wAFROC") {
    xLim <- 1
    yLim <- 1
  } else if (opChType == "AFROC1") {
    xLim <- 1
    yLim <- 1
  } else if (opChType == "wAFROC1") {
    xLim <- 1
    yLim <- 1
  } else stop("illegal value for opChType")
  
  return(list(
    xLim = xLim,
    yLim = yLim
  ))
}




####################################################################################################################
genPoints <- function(genNL, genLL, modalityID, readerID, lesionNum, weights, treatments2Plot, rdrs2Plot, opChType) {
  
  if (opChType == "ROC") {
    genPoints <- ROCPoints(genNL, genLL, modalityID, readerID, 
                           treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "FROC") {
    genPoints <- FROCPoints(genNL, genLL, modalityID, readerID, lesionNum, 
                            treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC") {
    genPoints <- AFROCPoints(genNL, genLL, modalityID, readerID, lesionNum, # sic 
                             treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC") {
    genPoints <- wAFROCPoints(genNL, genLL, modalityID, readerID, weights, # sic 
                              treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC1") {
    genPoints <- AFROC1Points(genNL, genLL, modalityID, readerID, lesionNum, # sic
                              treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC1") {
    genPoints <- wAFROC1Points(genNL, genLL, modalityID, readerID, weights, # sic 
                               treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  stop("genPoints: Incorrect FOM")
}




####################################################################################################################
genAvgPoints <- function(genNL, genLL, modalityID, readerID, lesionNum, weights, treatments2Plot, rdrs2Plot, opChType) {
  
  if (opChType == "ROC") {
    genPoints <- AvgROCPoints(genNL, genLL, modalityID, readerID, 
                              treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "FROC") {
    genPoints <- AvgFROCPoints(genNL, genLL, modalityID, readerID, lesionNum,
                               treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC") {
    genPoints <- AvgAFROCPoints(genNL, genLL, modalityID, readerID, lesionNum, # sic
                                treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC") {
    genPoints <- AvgwAFROCPoints(genNL, genLL, modalityID, readerID, weights, # sic 
                                 treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC1") {
    genPoints <- AvgAFROC1Points(genNL, genLL, modalityID, readerID, lesionNum, # sic 
                                 treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC1") {
    genPoints <- AvgwAFROC1Points(genNL, genLL, modalityID, readerID, weights, # sic
                                  treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  stop("genAvgPoints: Incorrect FOM")
}



# this simple code converts fp and tp arrays to counts table, operating points and zetas
# no information is lost (combining bins loses information) but there could be lots of zeroes
# and ones in the op. pts.
####################################################################################################################
RawOpPtsROC2ROC <- function (fp, tp) {
  zetas <- sort(unique(c(fp, tp)))
  nBins <- length(zetas)
  fpCounts <- rep(NA, nBins)
  tpCounts <- fpCounts
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == zetas[b])
    tpCounts[b] <- sum(tp == zetas[b])
  }
  K1 <- length(fp)  # !sic!
  K2 <- length(tp)
  fpf <- cumsum(rev(fpCounts)) / K1
  tpf <- cumsum(rev(tpCounts)) / K2
  fpf <- fpf[-length(fpf)]
  tpf <- tpf[-length(tpf)]
  return(list(
    fpCounts = fpCounts,
    tpCounts = tpCounts,
    fpf = fpf,
    tpf = tpf,
    zetas = zetas
  ))
}




# A failed attempt at combining bins; partially implemented is deletion of multiple starting zeroes
# in fpf; the counts table are not combined; the more complicated code in BinTheRocData is likely needed
####################################################################################################################
RawOpPtsROC2ROC1 <- function (fp, tp) {
  zetas <- sort(unique(c(fp, tp)))
  while (1) {  
    nBins <- length(zetas)
    fpCounts <- rep(NA, nBins)
    tpCounts <- fpCounts
    for (b in 1:nBins){
      fpCounts[b] <- sum(fp == zetas[b])
      tpCounts[b] <- sum(tp == zetas[b])
    }
    K1 <- length(fp)  # !sic!
    K2 <- length(tp)
    fpf <- cumsum(rev(fpCounts)) / K1
    tpf <- cumsum(rev(tpCounts)) / K2
    fpf <- fpf[-length(fpf)]
    tpf <- tpf[-length(tpf)]
    toDel <- length(which(fpf == 0))
    lz <- length(zetas)
    if (toDel > 1){
      fpf <- fpf[-(seq(1:(toDel-1)))]
      tpf <- tpf[-(seq(1:(toDel-1)))]
      zetas <- zetas[-(seq(lz:(lz-toDel+2)))]
    } else break
  }
  return(list(
    fpCounts = fpCounts,
    tpCounts = tpCounts,
    fpf = fpf,
    tpf = tpf,
    zetas = zetas
  ))
}





####################################################################################################################
ROCPoints <- function(NL, LL, modalityID, readerID, treatments2Plot, readers2Plot) {
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, 1)
  dim(LL) <- c(I, J, K2, 1)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(readers2Plot)
  
  NL <- NL[, readers2Plot, , ]
  LL <- LL[, readers2Plot, , ]
  dim(NL) <- c(I, J, K, 1)
  dim(LL) <- c(I, J, K2, 1)
  readerID <- readerID[readers2Plot]
  
  ROCPoints <- data.frame(FPF = NULL, TPF = NULL)
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- NL[i, j, 1:K1,1]
      tp <- LL[i, j, 1:K2,1]
      ret1 <- RawOpPtsROC2ROC (fp, tp)
      FPF <- ret1$fpf;FPF <- c(0, FPF, 1)
      TPF <- ret1$tpf;TPF <- c(0, TPF, 1)
      ROCPoints <- rbind(ROCPoints, data.frame(FPF = FPF, TPF = TPF, Modality = i, Reader = j))
    }
  }
  class <- paste("M: ", modalityID[ROCPoints$Modality], "\n", "R: ", readerID[ROCPoints$Reader], sep = "")
  ROCPoints <- data.frame(genAbscissa = ROCPoints$FPF, genOrdinate = ROCPoints$TPF, class = class, type = "individual")
  return(ROCPoints)
}


#' @importFrom stats approx
####################################################################################################################
AvgROCPoints <- function(NL, LL, modalityID, readerID, treatments2Plot, readers2Plot) {
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, 1)
  dim(LL) <- c(I, J, K2, 1)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(readers2Plot)
  NL <- NL[, readers2Plot, , ]
  LL <- LL[, readers2Plot, , ]
  dim(NL) <- c(I, J, K, 1)
  dim(LL) <- c(I, J, K2, 1)
  readerID <- readerID[readers2Plot]
  
  abscissaStep <- 0.01
  sampledFPF <- seq(0, 1, abscissaStep)
  avgTPF <- rep(0, length(sampledFPF))
  avgTPFArray <- array(dim = c(J, length(sampledFPF)))
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- NL[i, j, 1:K1, 1]
      tp <- LL[i, j, 1:K2, 1]
      ret <- RawOpPtsROC2ROC (fp, tp)
      fpf <- ret$fpf;tpf <- ret$tpf
      FPF <- c(0,fpf,1)
      TPF <- c(0,tpf,1)
      temp <- (approx(FPF, TPF, xout = sampledFPF))$y
      avgTPF <- avgTPF + temp
    }
  }
  avgTPF <- avgTPF/(I * J)
  avgTPF <- c(0,avgTPF);sampledFPF <- c(0,sampledFPF)
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  ROCPoints <- data.frame(genAbscissa = sampledFPF, genOrdinate = avgTPF, class = class, type = "rdrAveraged")
  return(ROCPoints)
}


# counterpart of RawOpPtsROC2ROC for FROC data; since denominators are different from ROC, a different function
# is needed; could be combined with previous function
####################################################################################################################
RawOpPtsFROC2FROC <- function (nl, ll, sumLL, K) {
  zetas <- sort(unique(c(nl, ll)))
  nBins <- length(zetas)
  nlCounts <- rep(NA, nBins)
  llCounts <- nlCounts
  for (b in 1:nBins){
    nlCounts[b] <- sum(nl == zetas[b])
    llCounts[b] <- sum(ll == zetas[b])
  }
  nlf <- cumsum(rev(nlCounts)) / K
  llf <- cumsum(rev(llCounts)) / sumLL
  return(list(
    nlCounts = nlCounts,
    llCounts = llCounts,
    nlf = nlf,
    llf = llf,
    zetas = zetas
  ))
}





####################################################################################################################
FROCPoints <- function(NL, LL, modalityID, readerID, lesionNum, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  
  J <- length(rdrs2Plot)
  
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  sumLL <- sum(lesionNum)
  FROCPoints <- data.frame(NLF = NULL, LLF = NULL)
  for (i in 1:I) {
    for (j in 1:J) {
      nl <- NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- RawOpPtsFROC2FROC(nl, ll, sumLL, K)
      NLF <- ret$nlf;LLF <- ret$llf
      NLF <- c(0,NLF);LLF <- c(0,LLF)
      FROCPoints <- rbind(FROCPoints, data.frame(NLF = NLF, LLF = LLF, Modality = i, Reader = j))
    }
  }
  
  class <- paste("M: ", modalityID[FROCPoints$Modality], "\n", "R: ", readerID[FROCPoints$Reader], sep = "")
  FROCPoints <- data.frame(genAbscissa = FROCPoints$NLF, genOrdinate = FROCPoints$LLF, class = class, type = "individual")
  return(FROCPoints)
}



# not sure what this does;
####################################################################################################################
AvgFROCPoints <- function(NL, LL, modalityID, readerID, lesionNum, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  sumLL <- sum(lesionNum)
  
  NLF <- list(NULL)
  LLF <- list(NULL)
  avgIndex <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      nl <- NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- RawOpPtsFROC2FROC (nl, ll, sumLL, K)
      nlf <- ret$nlf;llf <- ret$llf
      NLF[[avgIndex]] <- c(0,nlf)
      LLF[[avgIndex]] <- c(0,llf)
      avgIndex <- avgIndex + 1
    }
  }
  
  maxNLF <- min(sapply(NLF, max))
  
  abscissaStep <- maxNLF/1000
  avgNLF <- seq(0, maxNLF, abscissaStep)
  avgLLF <- rep(NA, length(avgNLF))
  avgLLFArray <- array(dim = c(I*J, length(avgNLF)))
  avgIndex <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      temp <- (approx(NLF[[j]], LLF[[j]], xout = avgNLF))$y
      avgLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }
  
  for (p in 1:length(avgNLF)) avgLLF[p] <- mean(avgLLFArray[, p])
  avgLLF <- c(0,avgLLF);avgNLF <- c(0,avgNLF)
  
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  FROCPoints <- data.frame(genAbscissa = avgNLF, genOrdinate = avgLLF, class = class, type = "rdrAveraged")
  return(FROCPoints)
}



# along the lines of RawOpPtsROC2ROC and RawOpPtsFROC2FROC ...
####################################################################################################################
FROC2AFROC <- function (fp, ll, sumLL, K1) {
  zetas <- sort(unique(c(fp, ll)))
  nBins <- length(zetas)
  fpCounts <- rep(NA, nBins)
  llCounts <- fpCounts
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == zetas[b])
    llCounts[b] <- sum(ll == zetas[b])
  }
  fpf <- cumsum(rev(fpCounts)) / K1 #sic! not K
  llf <- cumsum(rev(llCounts)) / sumLL
  return(list(
    fpCounts = fpCounts,
    llCounts = llCounts,
    fpf = fpf,
    llf = llf,
    zetas = zetas
  ))
}




# along the lines of RawOpPtsROC2ROC and RawOpPtsFROC2FROC ...
####################################################################################################################
FROC2AFROC1 <- function (fp, ll, sumLL, K) {
  zetas <- sort(unique(c(fp, ll)))
  nBins <- length(zetas)
  fpCounts <- rep(NA, nBins)
  llCounts <- fpCounts
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == zetas[b])
    llCounts[b] <- sum(ll == zetas[b])
  }
  fpf <- cumsum(rev(fpCounts)) / K
  llf <- cumsum(rev(llCounts)) / sumLL
  return(list(
    fpCounts = fpCounts,
    llCounts = llCounts,
    fpf = fpf,
    llf = llf,
    zetas = zetas
  ))
}



####################################################################################################################
AFROCPoints <- function(NL, LL, modalityID, readerID, lesionNum, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  sumLL <- sum(lesionNum)
  FP <- apply(NL, c(1, 2, 3), max)
  AFROCPoints <- data.frame(FPF = NULL, LLF = NULL)
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i,j,1:K1][FP[i,j,1:K1] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2AFROC(fp, ll, sumLL, K1) #sic! not K
      FPF <- ret$fpf;LLF <- ret$llf
      FPF <- c(0,FPF,1);LLF <- c(0,LLF,1)
      AFROCPoints <- rbind(AFROCPoints, data.frame(FPF = FPF, LLF = LLF, Modality = i, Reader = j))
    }
  }
  
  class <- paste("M: ", modalityID[AFROCPoints$Modality], "\n", "R: ", readerID[AFROCPoints$Reader], sep = "")
  AFROCPoints <- data.frame(genAbscissa = AFROCPoints$FPF, genOrdinate = AFROCPoints$LLF, class = class, type = "individual")
  return(AFROCPoints)
}






####################################################################################################################
AvgAFROCPoints <- function(NL, LL, modalityID, readerID, lesionNum, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  FP <- apply(NL, c(1, 2, 3), max)
  
  sumLL <- sum(lesionNum)
  abscissaStep <- 0.001
  sampledFPF <- seq(0, 1, abscissaStep)
  avgLLF <- rep(0, length(sampledFPF))
  avgLLFArray <- array(dim = c(I*J, length(avgLLF)))
  avgIndex <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i,j,1:K1][FP[i,j,1:K1] != UNINITIALIZED]#sic! only non-diseased cases
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2AFROC (fp, ll, sumLL, K1) #sic! not K
      fpf <- ret$fpf;llf <- ret$llf
      FPF <- c(0,fpf,1)
      LLF <- c(0,llf,1)
      temp <- (approx(FPF, LLF, xout = sampledFPF))$y
      avgLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }
  
  for (p in 1:length(sampledFPF)) avgLLF[p] <- mean(avgLLFArray[, p])
  avgLLF <- c(0,avgLLF);sampledFPF <- c(0,sampledFPF)
  
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  AFROCPoints <- data.frame(genAbscissa = sampledFPF, genOrdinate = avgLLF, class = class, type = "rdrAveraged")
  return(AFROCPoints)
}




####################################################################################################################
FROC2wAFROC <- function (fp, ll, weights, K1, K2) {
  zetas <- sort(unique(c(fp, ll)))
  nBins <- length(zetas)
  fpCounts <- rep(NA, nBins)
  wllCounts <- fpCounts
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == zetas[b])
    wllCounts[b] <- sum(weights[ll == zetas[b]])
    next
  }
  fpf <- cumsum(rev(fpCounts)) / K1#sic! only non-diseased cases
  wllf <- cumsum(rev(wllCounts)) / K2
  return(list(
    fpCounts = fpCounts,
    wllCounts = wllCounts,
    fpf = fpf,
    wllf = wllf,
    zetas = zetas
  ))
}


####################################################################################################################
FROC2wAFROC1 <- function (fp, ll, weights, K1, K2) {
  zetas <- sort(unique(c(fp, ll)))
  nBins <- length(zetas)
  fpCounts <- rep(NA, nBins)
  wllCounts <- fpCounts
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == zetas[b])
    wllCounts[b] <- sum(weights[ll == zetas[b]])
    next
  }
  fpf <- cumsum(rev(fpCounts)) / (K1+K2)#sic! all cases
  wllf <- cumsum(rev(wllCounts)) / K2
  return(list(
    fpCounts = fpCounts,
    wllCounts = wllCounts,
    fpf = fpf,
    wllf = wllf,
    zetas = zetas
  ))
}


####################################################################################################################
AFROC1Points <- function(NL, LL, modalityID, readerID, lesionNum, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  sumLL <- sum(lesionNum)
  FP <- apply(NL, c(1, 2, 3), max)
  AFROCPoints <- data.frame(FPF = NULL, LLF = NULL)
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i, j,][FP[i, j,] != UNINITIALIZED] #sic! all cases
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2AFROC(fp, ll, sumLL, K) #sic! not K1
      FPF <- ret$fpf;LLF <- ret$llf
      FPF <- c(0,FPF,1);LLF <- c(0,LLF,1)
      AFROCPoints <- rbind(AFROCPoints, data.frame(FPF = FPF, LLF = LLF, Modality = i, Reader = j))
    }
  }
  
  class <- paste("M: ", modalityID[AFROCPoints$Modality], "\n", "R: ", readerID[AFROCPoints$Reader], sep = "")
  AFROCPoints <- data.frame(genAbscissa = AFROCPoints$FPF, genOrdinate = AFROCPoints$LLF, class = class, type = "individual")
  return(AFROCPoints)
}



####################################################################################################################
AvgAFROC1Points <- function(NL, LL, modalityID, readerID, lesionNum, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  FP <- apply(NL, c(1, 2, 3), max)
  
  sumLL <- sum(lesionNum)
  abscissaStep <- 0.001
  sampledFPF <- seq(0, 1, abscissaStep)
  avgLLF <- rep(0, length(sampledFPF))
  avgLLFArray <- array(dim = c(I*J, length(avgLLF)))
  avgIndex <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i, j,][FP[i, j,] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2AFROC1 (fp, ll, sumLL, K)#sic, not K1
      fpf <- ret$fpf;llf <- ret$llf
      FPF <- c(0,fpf,1)
      LLF <- c(0,llf,1)
      temp <- (approx(FPF, LLF, xout = sampledFPF))$y
      avgLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }
  
  for (p in 1:length(sampledFPF)) avgLLF[p] <- mean(avgLLFArray[, p])
  avgLLF <- c(0,avgLLF);sampledFPF <- c(0,sampledFPF)
  
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  AFROCPoints <- data.frame(genAbscissa = sampledFPF, genOrdinate = avgLLF, class = class, type = "rdrAveraged")
  return(AFROCPoints)
}




####################################################################################################################
wAFROCPoints <- function(NL, LL, modalityID, readerID, lesionWeights, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  FP <- apply(NL, c(1, 2, 3), max)
  wAFROCPoints <- data.frame(FPF = NULL, wLLF = NULL)
  
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i,j,1:K1][FP[i,j,1:K1] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      weights <- lesionWeights[LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2wAFROC(fp, ll, weights, K1, K2)
      FPF <- ret$fpf;wLLF <- ret$wllf
      FPF <- c(0,FPF,1);wLLF <- c(0,wLLF,1)
      wAFROCPoints <- rbind(wAFROCPoints, data.frame(FPF = FPF, wLLF = wLLF, Modality = i, Reader = j))
    }
  }
  
  #class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  class <- paste("M: ", modalityID[wAFROCPoints$Modality], "\n", "R: ", readerID[wAFROCPoints$Reader], sep = "")
  wAFROCPoints <- data.frame(genAbscissa = wAFROCPoints$FPF, genOrdinate = wAFROCPoints$wLLF, class = class, type = "individual")
  return(wAFROCPoints)
}




####################################################################################################################
AvgwAFROCPoints <- function(NL, LL, modalityID, readerID, lesionWeights, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  FP <- apply(NL, c(1, 2, 3), max)
  
  abscissaStep <- 0.1
  sampledFPF <- seq(0, 1, abscissaStep)
  avgwLLF <- rep(0, length(sampledFPF))
  avgwLLFArray <- array(dim = c(I*J, length(avgwLLF)))
  avgIndex <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i,j,1:K1][FP[i,j,1:K1] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      weights <- lesionWeights[LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2wAFROC(fp, ll, weights, K1, K2)
      FPF <- ret$fpf;wLLF <- ret$wllf
      FPF <- c(0,FPF,1);wLLF <- c(0,wLLF,1)
      temp <- (approx(FPF, wLLF, xout = sampledFPF))$y
      avgwLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }
  
  for (p in 1:length(sampledFPF)) avgwLLF[p] <- mean(avgwLLFArray[, p])
  avgwLLF <- c(0,avgwLLF,1);sampledFPF <- c(0,sampledFPF,1)
  
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  #class <- paste("M: ", modalityID[wAFROCPoints$Modality], "\n", "R: ", readerID[wAFROCPoints$Reader], sep = "")
  AvgwAFROCPoints <- data.frame(genAbscissa = sampledFPF, genOrdinate = avgwLLF, class = class, type = "rdrAveraged")
  return(AvgwAFROCPoints)
}


####################################################################################################################
wAFROC1Points <- function(NL, LL, modalityID, readerID, lesionWeights, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  FP <- apply(NL, c(1, 2, 3), max)
  wAFROC1Points <- data.frame(FPF = NULL, wLLF = NULL)
  
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i, j,][FP[i, j,] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      weights <- lesionWeights[LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2wAFROC1(fp, ll, weights, K1, K2)
      FPF <- ret$fpf;wLLF <- ret$wllf
      FPF <- c(0,FPF,1);wLLF <- c(0,wLLF,1)
      wAFROC1Points <- rbind(wAFROC1Points, data.frame(FPF = FPF, wLLF = wLLF, Modality = i, Reader = j))
    }
  }
  
  class <- paste("M: ", modalityID[wAFROC1Points$Modality], "\n", "R: ", readerID[wAFROC1Points$Reader], sep = "")
  wAFROC1Points <- data.frame(genAbscissa = wAFROC1Points$FPF, genOrdinate = wAFROC1Points$wLLF, class = class, type = "individual")
  return(wAFROC1Points)
}




####################################################################################################################
AvgwAFROC1Points <- function(NL, LL, modalityID, readerID, lesionWeights, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  
  NL <- NL[treatments2Plot, , , ]
  LL <- LL[treatments2Plot, , , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  modalityID <- modalityID[treatments2Plot]
  
  J <- length(rdrs2Plot)
  NL <- NL[, rdrs2Plot, , ]
  LL <- LL[, rdrs2Plot, , ]
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, maxLL)
  readerID <- readerID[rdrs2Plot]
  
  FP <- apply(NL, c(1, 2, 3), max)
  
  abscissaStep <- 0.1
  sampledFPF <- seq(0, 1, abscissaStep)
  avgwLLF <- rep(0, length(sampledFPF))
  avgwLLFArray <- array(dim = c(I*J, length(avgwLLF)))
  avgIndex <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i,j,1:K1][FP[i,j,1:K1] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      weights <- lesionWeights[LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2wAFROC1(fp, ll, weights, K1, K2)
      FPF <- ret$fpf;wLLF <- ret$wllf
      FPF <- c(0,FPF,1);wLLF <- c(0,wLLF,1)
      temp <- (approx(FPF, wLLF, xout = sampledFPF))$y
      avgwLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }
  
  for (p in 1:length(sampledFPF)) avgwLLF[p] <- mean(avgwLLFArray[, p])
  avgwLLF <- c(0,avgwLLF,1);sampledFPF <- c(0,sampledFPF,1)
  
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  wAFROC1Points <- data.frame(genAbscissa = sampledFPF, genOrdinate = avgwLLF, class = class, type = "rdrAveraged")
  return(wAFROC1Points)
}


