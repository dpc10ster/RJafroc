####################################################################################################################
gpfPlotGenericEmpiricalOperatingCharacteristic <- function(dataset, treatments2Plot, readers2Plot, opChType, legPos, maxDiscrete)
{
  # these silly statements are needed to avoid a NOTE on R CMD check
  genAbscissa <- NULL
  genOrdinate <- NULL
  Reader <- NULL
  Modality <- NULL
  # handle operating points
  if (!is.list(treatments2Plot) && !is.list(readers2Plot)) {
    # first handle no list arguments; gen stands for generic
    pts <- genPoints(dataset, treatments2Plot, readers2Plot, opChType)
  } else {
    # now handle lists case, must be identical lengths
    if (is.list(treatments2Plot) && is.list(readers2Plot) && length(treatments2Plot) == length(readers2Plot)) {
      pts <- data.frame(genAbscissa = NULL,
                        genOrdinate = NULL,
                        stringsAsFactors = TRUE)
      for (i in 1:length(readers2Plot)) {
        if (length(treatments2Plot[[i]]) == 1 && (length(readers2Plot[[i]]) == 1)) {
          tempGenPoints <- genPoints(dataset, treatments2Plot[[i]], readers2Plot[[i]], opChType)
          pts <- rbind(pts, tempGenPoints)
        } else {
          tempGenPoints <- genAvgPoints(dataset, treatments2Plot[[i]], readers2Plot[[i]], opChType)
          pts <- rbind(pts, tempGenPoints)
        }
      }
    } else if (is.list(treatments2Plot) && length(readers2Plot) == 1) {
      readers2Plot <- readers2Plot[[1]]
      pts <- data.frame(genAbscissa = NULL, genOrdinate = NULL, stringsAsFactors = TRUE)
      for (i in 1:length(treatments2Plot)) {
        if (length(treatments2Plot[[i]]) == 1) {
          tempGenPoints <- genPoints(dataset, treatments2Plot[[i]], readers2Plot, opChType)
          pts <- rbind(pts, tempGenPoints)
        } else {
          tempGenPoints <- genAvgPoints(dataset, treatments2Plot[[i]], readers2Plot, opChType)
          pts <- rbind(pts, tempGenPoints)
        }
      }
    } else if (is.list(readers2Plot) && length(treatments2Plot) == 1) {
      if (is.list(treatments2Plot))
        treatments2Plot <- treatments2Plot[[1]]
      pts <- data.frame(genAbscissa = NULL, genOrdinate = NULL, stringsAsFactors = TRUE)
      for (i in 1:length(readers2Plot)) {
        if (length(readers2Plot[[i]]) == 1) {
          tempGenPoints <- genPoints(dataset, treatments2Plot, readers2Plot[[i]], opChType)
          pts <- rbind(pts, tempGenPoints)
        } else {
          tempGenPoints <- genAvgPoints(dataset, treatments2Plot, readers2Plot[[i]], opChType)
          pts <- rbind(pts, tempGenPoints)
        }
      }
    } else {
      stop("Lengths of trts and rdrs lists do not match.")
    }
  }

  ret <- GetLimits(pts, opChType)
  xLim <- ret$xLim;yLim <- ret$yLim

  classes <- unique(pts$class)
  for (i in 1:length(classes)){
    indices <- pts$class == classes[i]
    if (sum(indices) > maxDiscrete && all(pts$type[indices] == "D")){
      typeTmp <- as.character(pts$type)
      typeTmp[indices] <- "C"
      pts$type <- typeTmp
    }
  }

  # handle plots
  if (!is.list(treatments2Plot) && !is.list(readers2Plot)) {
    # handle no lists
    mr <- unlist(strsplit(as.character(pts$class), split = "\n"))
    dim(mr) <- c(2, round(length(mr)/2))
    pts <- cbind(pts, data.frame(Modality = mr[1, ], Reader = mr[2, ], stringsAsFactors = TRUE))
    genOpPoints <- pts[pts$type == "D" &
                         !((pts$genAbscissa == 0 & pts$genOrdinate == 0) |
                             (pts$genAbscissa == 1 & pts$genOrdinate == 1)), ]
    legendLength <- length(readers2Plot)
    shapeVector <- rep(NA, length(readers2Plot))
    for (n in 1:legendLength) {
      index <- which(pts$Reader == levels(pts$Reader)[n])[1]
      if (pts$type[index] == "D")
        shapeVector[n] <- 16 #http://www.sthda.com/english/wiki/ggplot2-point-shapes
    }

    # simplified plotting routines
    if ((length(treatments2Plot) == 1) && (length(readers2Plot) == 1)) {
      genPlot <- ggplot2::ggplot(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate)) +
        ggplot2::geom_point(data = genOpPoints, ggplot2::aes(x = genAbscissa, y = genOrdinate)) +
        ggplot2::geom_line(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate))

    } else if ((length(treatments2Plot) == 1) && (length(readers2Plot) > 1)) {
     genPlot <- ggplot2::ggplot(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate)) +
        ggplot2::geom_line(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate, color = Reader)) +
        ggplot2::geom_point(data = genOpPoints, ggplot2::aes(x = genAbscissa, y = genOrdinate))

    } else if ((length(treatments2Plot) > 1) && (length(readers2Plot) == 1)) {
      genPlot <- ggplot2::ggplot(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate)) +
        ggplot2::geom_line(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate, color = Modality)) +
        ggplot2::geom_point(data = genOpPoints, ggplot2::aes(x = genAbscissa, y = genOrdinate))

    } else if ((length(treatments2Plot) > 1) && (length(readers2Plot) > 1)) {
      genPlot <- ggplot2::ggplot(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate)) +
        ggplot2::geom_line(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate, color = Modality, linetype = Reader)) +
        ggplot2::geom_point(data = genOpPoints, ggplot2::aes(x = genAbscissa, y = genOrdinate))

    }
  } else {
    # handle lists
    genOpPoints <- pts[pts$type == "D" & !((pts$genAbscissa == 0 & pts$genOrdinate == 0) | (pts$genAbscissa == 1 & pts$genOrdinate == 1)), ]

    legendLength <- length(levels(pts$class))
    shapeVector <- rep(NA, length(levels(pts$class)))
    for (n in 1:legendLength) {
      index <- which(pts$class == levels(pts$class)[n])[1]
      if (pts$type[index] == "D")
        shapeVector[n] <- 16
    }

    # simplified plotting routines
    genPlot <- ggplot2::ggplot(data = pts, ggplot2::aes(x = genAbscissa, y = genOrdinate, color = class)) +
      ggplot2::geom_line() + ggplot2::geom_point(data = genOpPoints) +
      ggplot2::theme(legend.title = element_blank())

  }

  if (opChType == "ROC") genPlot <- genPlot + xlab("FPF") + ylab("TPF")
  if (opChType == "FROC") genPlot <- genPlot + xlab("NLF") + ylab("LLF")
  if (opChType == "AFROC") genPlot <- genPlot + xlab("FPF") + ylab("LLF")
  if (opChType == "wAFROC") genPlot <- genPlot + xlab("FPF") + ylab("wLLF")
  if (opChType == "AFROC1") genPlot <- genPlot + xlab("FPF1") + ylab("LLF")
  if (opChType == "wAFROC1") genPlot <- genPlot + xlab("FPF1") + ylab("wLLF")
  if (opChType == "LROC") genPlot <- genPlot + xlab("FPF") + ylab("PCL")

  genPlot <- genPlot  + scale_x_continuous(limits = c(0,xLim)) + scale_y_continuous(limits = c(0,yLim))

  genPlot <- genPlot + theme(legend.position = "inside", legend.position.inside = legPos)

  return(list(Plot = genPlot, Points = pts))
}






####################################################################################################################
GetLimits <- function (pts, opChType) {
  if (opChType == "ROC") {
    xLim <- 1
    yLim <- 1
  } else if (opChType == "FROC") {
    xLim <- ceiling(max(pts$genAbscissa) / 0.05 ) * 0.05 # rounds up to the nearest .05
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
  } else if (opChType == "LROC") {
    xLim <- 1
    yLim <- 1
  } else stop("illegal value for opChType")

  return(list(
    xLim = xLim,
    yLim = yLim
  ))
}




####################################################################################################################
genPoints <- function(dataset, treatments2Plot, rdrs2Plot, opChType) {

  if (opChType == "ROC") {
    genPoints <- ROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "FROC") {
    genPoints <- FROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC") {
    genPoints <- AFROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC") {
    genPoints <- wAFROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC1") {
    genPoints <- AFROC1Points(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC1") {
    genPoints <- wAFROC1Points(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "LROC") { ###WIP###
    genPoints <- LROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  stop("genPoints: Incorrect FOM")
}




####################################################################################################################
genAvgPoints <- function(dataset, treatments2Plot, rdrs2Plot, opChType) {

  if (opChType == "ROC") {
    genPoints <- AvgROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "FROC") {
    genPoints <- AvgFROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC") {
    genPoints <- AvgAFROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC") {
    genPoints <- AvgwAFROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "AFROC1") {
    genPoints <- AvgAFROC1Points(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "wAFROC1") {
    genPoints <- AvgwAFROC1Points(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  if (opChType == "LROC") {
    genPoints <- AvgLROCPoints(dataset, treatments2Plot, rdrs2Plot)
    return(genPoints)
  }
  stop("genAvgPoints: Incorrect FOM")
}



# converts fp and tp arrays to counts table, operating points and zetas
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




# Failed attempt at combining bins; partially implemented is deletion of multiple starting zeroes
# in fpf; the counts table are not combined; the more complicated code in BinTheRocData is likely needed
####################################################################################################################
# RawOpPtsROC2ROC1 <- function (fp, tp) {
#   zetas <- sort(unique(c(fp, tp)))
#   while (1) {
#     nBins <- length(zetas)
#     fpCounts <- rep(NA, nBins)
#     tpCounts <- fpCounts
#     for (b in 1:nBins){
#       fpCounts[b] <- sum(fp == zetas[b])
#       tpCounts[b] <- sum(tp == zetas[b])
#     }
#     K1 <- length(fp)  # !sic!
#     K2 <- length(tp)
#     fpf <- cumsum(rev(fpCounts)) / K1
#     tpf <- cumsum(rev(tpCounts)) / K2
#     fpf <- fpf[-length(fpf)]
#     tpf <- tpf[-length(tpf)]
#     toDel <- length(which(fpf == 0))
#     lz <- length(zetas)
#     if (toDel > 1){
#       fpf <- fpf[-(seq(1:(toDel-1)))]
#       tpf <- tpf[-(seq(1:(toDel-1)))]
#       zetas <- zetas[-(seq(lz:(lz-toDel+2)))]
#     } else break
#   }
#   return(list(
#     fpCounts = fpCounts,
#     tpCounts = tpCounts,
#     fpf = fpf,
#     tpf = tpf,
#     zetas = zetas
#   ))
# }










####################################################################################################################
ROCPoints <- function(dataset, treatments2Plot, readers2Plot) {
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

  I <- length(treatments2Plot) # found possible error 11/10/19
  # I <- dim(NL)[1] # No; this gives error
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

  ROCPoints <- data.frame(FPF = NULL, TPF = NULL, stringsAsFactors = TRUE)
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- NL[i, j, 1:K1,1]
      tp <- LL[i, j, 1:K2,1]
      ret1 <- RawOpPtsROC2ROC (fp, tp)
      FPF <- ret1$fpf;FPF <- c(0, FPF, 1)
      TPF <- ret1$tpf;TPF <- c(0, TPF, 1)
      ROCPoints <- rbind(ROCPoints, data.frame(FPF = FPF,
                                               TPF = TPF,
                                               Modality = i,
                                               Reader = j,
                                               stringsAsFactors = TRUE))
    }
  }
  class <- paste("M: ", modalityID[ROCPoints$Modality], "\n", "R: ", readerID[ROCPoints$Reader], sep = "")
  ROCPoints <- data.frame(genAbscissa = ROCPoints$FPF,
                          genOrdinate = ROCPoints$TPF, class = class,
                          type = "D", stringsAsFactors = TRUE)
  return(ROCPoints)
}


#' @importFrom stats approx
####################################################################################################################
AvgROCPoints <- function(dataset, treatments2Plot, readers2Plot) {

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

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
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- NL[i, j, 1:K1, 1]
      tp <- LL[i, j, 1:K2, 1]
      ret <- RawOpPtsROC2ROC (fp, tp)
      fpf <- ret$fpf;tpf <- ret$tpf
      FPF <- c(0,fpf,1)
      TPF <- c(0,tpf,1)
      # note added 11/9/19 ???DPC???
      # since I had to remove the `approx` function for small LROC datasets,
      # this too may need "hand calculation" checking
      temp <- (approx(FPF, TPF, xout = sampledFPF, ties = min))$y
      avgTPF <- avgTPF + temp
    }
  }
  avgTPF <- avgTPF/(I * J)
  avgTPF <- c(0,avgTPF);sampledFPF <- c(0,sampledFPF)
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  ROCPoints <- data.frame(genAbscissa = sampledFPF,
                          genOrdinate = avgTPF, class = class, type = "A", stringsAsFactors = TRUE)
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
FROCPoints <- function(dataset, treatments2Plot, rdrs2Plot) {

  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID


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

  sumLL <- sum(lesionVector)
  FROCPoints <- data.frame(NLF = NULL, LLF = NULL, stringsAsFactors = TRUE)
  for (i in 1:I) {
    for (j in 1:J) {
      nl <- NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- RawOpPtsFROC2FROC(nl, ll, sumLL, K)
      NLF <- ret$nlf;LLF <- ret$llf
      NLF <- c(0,NLF);LLF <- c(0,LLF)
      FROCPoints <- rbind(FROCPoints, data.frame(NLF = NLF,
                                                 LLF = LLF,
                                                 Modality = i,
                                                 Reader = j, stringsAsFactors = TRUE))
    }
  }

  class <- paste("M: ", modalityID[FROCPoints$Modality], "\n", "R: ", readerID[FROCPoints$Reader], sep = "")
  FROCPoints <- data.frame(
    genAbscissa = FROCPoints$NLF,
    genOrdinate = FROCPoints$LLF, class = class, type = "D", stringsAsFactors = TRUE)
  return(FROCPoints)
}



# not sure what this does;
####################################################################################################################
AvgFROCPoints <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

  I <- length(treatments2Plot)
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  # K1 <- K - K2
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

  sumLL <- sum(lesionVector)

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
      temp <- (approx(NLF[[j]], LLF[[j]], xout = avgNLF, ties = min))$y
      avgLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }

  for (p in 1:length(avgNLF)) avgLLF[p] <- mean(avgLLFArray[, p])
  avgLLF <- c(0,avgLLF);avgNLF <- c(0,avgNLF)

  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), "\n", sep = "")
  FROCPoints <- data.frame(genAbscissa = avgNLF,
                           genOrdinate = avgLLF,
                           class = class,
                           type = "A",
                           stringsAsFactors = TRUE)
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
AFROCPoints <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

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

  sumLL <- sum(lesionVector)
  FP <- apply(NL, c(1, 2, 3), max)
  AFROCPoints <- data.frame(FPF = NULL, LLF = NULL)#, stringsAsFactors = TRUE)
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i,j,1:K1][FP[i,j,1:K1] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2AFROC(fp, ll, sumLL, K1) #sic! not K
      FPF <- ret$fpf;LLF <- ret$llf
      FPF <- c(0,FPF,1);LLF <- c(0,LLF,1)
      AFROCPoints <- rbind(AFROCPoints, data.frame(FPF = FPF,
                                                   LLF = LLF,
                                                   Modality = i,
                                                   Reader = j,
                                                   stringsAsFactors = TRUE))
    }
  }

  class <- paste("M: ", modalityID[AFROCPoints$Modality], "\n", "R: ", readerID[AFROCPoints$Reader], sep = "")
  AFROCPoints <- data.frame(
    genAbscissa = AFROCPoints$FPF,
    genOrdinate = AFROCPoints$LLF,
    class = class,
    type = "D",
    stringsAsFactors = TRUE)
  return(AFROCPoints)
}






####################################################################################################################
AvgAFROCPoints <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

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

  sumLL <- sum(lesionVector)
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
      temp <- (approx(FPF, LLF, xout = sampledFPF, ties = min))$y
      avgLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }

  for (p in 1:length(sampledFPF)) avgLLF[p] <- mean(avgLLFArray[, p])
  avgLLF <- c(0,avgLLF);sampledFPF <- c(0,sampledFPF)

  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  AFROCPoints <- data.frame(genAbscissa = sampledFPF,
                            genOrdinate = avgLLF,
                            class = class,
                            type = "A",
                            stringsAsFactors = TRUE)
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
AFROC1Points <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

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

  sumLL <- sum(lesionVector)
  FP <- apply(NL, c(1, 2, 3), max)
  AFROCPoints <- data.frame(FPF = NULL, LLF = NULL, stringsAsFactors = TRUE)
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i, j,][FP[i, j,] != UNINITIALIZED] #sic! all cases
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2AFROC(fp, ll, sumLL, K) #sic! not K1
      FPF <- ret$fpf;LLF <- ret$llf
      FPF <- c(0,FPF,1);LLF <- c(0,LLF,1)
      AFROCPoints <- rbind(AFROCPoints, data.frame(FPF = FPF,
                                                   LLF = LLF,
                                                   Modality = i,
                                                   Reader = j, stringsAsFactors = TRUE))
    }
  }

  class <- paste("M: ", modalityID[AFROCPoints$Modality], "\n", "R: ", readerID[AFROCPoints$Reader], sep = "")
  AFROCPoints <- data.frame(genAbscissa = AFROCPoints$FPF,
                            genOrdinate = AFROCPoints$LLF,
                            class = class,
                            type = "D",
                            stringsAsFactors = TRUE)
  return(AFROCPoints)
}



####################################################################################################################
AvgAFROC1Points <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

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

  sumLL <- sum(lesionVector)
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
      temp <- (approx(FPF, LLF, xout = sampledFPF, ties = min))$y
      avgLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }

  for (p in 1:length(sampledFPF)) avgLLF[p] <- mean(avgLLFArray[, p])
  avgLLF <- c(0,avgLLF);sampledFPF <- c(0,sampledFPF)

  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  AFROCPoints <- data.frame(genAbscissa = sampledFPF,
                            genOrdinate = avgLLF,
                            class = class,
                            type = "A",
                            stringsAsFactors = TRUE)
  return(AFROCPoints)
}




####################################################################################################################
wAFROCPoints <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  lesionWeights <- dataset$lesions$weights

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
  wAFROCPoints <- data.frame(FPF = NULL, wLLF = NULL, stringsAsFactors = TRUE)

  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i,j,1:K1][FP[i,j,1:K1] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      weights <- lesionWeights[LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2wAFROC(fp, ll, weights, K1, K2)
      FPF <- ret$fpf;wLLF <- ret$wllf
      FPF <- c(0,FPF,1);wLLF <- c(0,wLLF,1)
      wAFROCPoints <- rbind(wAFROCPoints, data.frame(FPF = FPF,
                                                     wLLF = wLLF,
                                                     Modality = i,
                                                     Reader = j,
                                                     stringsAsFactors = TRUE))
    }
  }

  #class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  class <- paste("M: ", modalityID[wAFROCPoints$Modality], "\n", "R: ", readerID[wAFROCPoints$Reader], "\n", sep = "")
  wAFROCPoints <- data.frame(genAbscissa = wAFROCPoints$FPF,
                             genOrdinate = wAFROCPoints$wLLF,
                             class = class,
                             type = "D",
                             stringsAsFactors = TRUE)
  return(wAFROCPoints)
}




####################################################################################################################
AvgwAFROCPoints <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  lesionWeights <- dataset$lesions$weights

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
      temp <- (approx(FPF, wLLF, xout = sampledFPF, ties = min))$y
      avgwLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }

  for (p in 1:length(sampledFPF)) avgwLLF[p] <- mean(avgwLLFArray[, p])
  avgwLLF <- c(0,avgwLLF,1);sampledFPF <- c(0,sampledFPF,1)

  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), "\n", sep = "")
  #class <- paste("M: ", modalityID[wAFROCPoints$Modality], "\n", "R: ", readerID[wAFROCPoints$Reader], sep = "")
  AvgwAFROCPoints <- data.frame(genAbscissa = sampledFPF,
                                genOrdinate = avgwLLF,
                                class = class,
                                type = "A",
                                stringsAsFactors = TRUE)
  return(AvgwAFROCPoints)
}


####################################################################################################################
wAFROC1Points <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  lesionWeights <- dataset$lesions$weights

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
  wAFROC1Points <- data.frame(FPF = NULL, wLLF = NULL, stringsAsFactors = TRUE)

  for (i in 1:I) {
    for (j in 1:J) {
      fp <- FP[i, j,][FP[i, j,] != UNINITIALIZED]
      ll <- LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED]
      weights <- lesionWeights[LL[i, j, , ] != UNINITIALIZED]
      ret <- FROC2wAFROC1(fp, ll, weights, K1, K2)
      FPF <- ret$fpf;wLLF <- ret$wllf
      FPF <- c(0,FPF,1);wLLF <- c(0,wLLF,1)
      wAFROC1Points <- rbind(wAFROC1Points, data.frame(FPF = FPF,
                                                       wLLF = wLLF,
                                                       Modality = i,
                                                       Reader = j,
                                                       stringsAsFactors = TRUE))
    }
  }

  class <- paste("M: ", modalityID[wAFROC1Points$Modality], "\n", "R: ", readerID[wAFROC1Points$Reader], sep = "")
  wAFROC1Points <- data.frame(genAbscissa = wAFROC1Points$FPF,
                              genOrdinate = wAFROC1Points$wLLF,
                              class = class,
                              type = "D",
                              stringsAsFactors = TRUE)
  return(wAFROC1Points)
}




####################################################################################################################
AvgwAFROC1Points <- function(dataset, treatments2Plot, rdrs2Plot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  lesionWeights <- dataset$lesions$weights

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
      temp <- (approx(FPF, wLLF, xout = sampledFPF, ties = min))$y
      avgwLLFArray[avgIndex, 1:length(temp)] <- temp
      avgIndex <- avgIndex + 1
    }
  }

  for (p in 1:length(sampledFPF)) avgwLLF[p] <- mean(avgwLLFArray[, p])
  avgwLLF <- c(0,avgwLLF,1);sampledFPF <- c(0,sampledFPF,1)

  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  wAFROC1Points <- data.frame(genAbscissa = sampledFPF,
                              genOrdinate = avgwLLF,
                              class = class,
                              type = "A",
                              stringsAsFactors = TRUE)
  return(wAFROC1Points)
}




genericPlotROC <- function(fp, tp, fpfPred, tpfPred, method = "ROC") {
  ret1 <- RawOpPtsROC2ROC(fp, tp)
  fpf <- ret1$fpf;tpf <- ret1$tpf

  color <- "black"
  ROCPred <- rbind(data.frame(fpf = fpfPred,
                              tpf = tpfPred,
                              stringsAsFactors = TRUE))
  ROCOpPoints <- rbind(data.frame(fpf = fpf,
                                  tpf = tpf,
                                  stringsAsFactors = TRUE))
  dfROCPred <- data.frame(fpf = ROCPred$fpf,
                          tpf = ROCPred$tpf,
                          color = color,
                          type = "D",
                          stringsAsFactors = TRUE)
  dfROCPoints <- data.frame(fpf = ROCOpPoints$fpf,
                            tpf = ROCOpPoints$tpf,
                            color = color,
                            type = "D",
                            stringsAsFactors = TRUE)

  fittedPlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = fpf, y = tpf), color = "black") +
    ggplot2::geom_line(data = dfROCPred, linewidth = 1) +
    ggplot2::geom_point(data = dfROCPoints, size = 4)

  if (method == "RSM"){
    ROCDashes <- rbind(data.frame(fpf = c(fpfPred[1], 1), tpf = c(tpfPred[1], 1)), stringsAsFactors = TRUE)
    dfROCDashes <- data.frame(fpf = ROCDashes$fpf,
                              tpf = ROCDashes$tpf,
                              color = color,
                              type = "D",
                              stringsAsFactors = TRUE)
    fittedPlot <- fittedPlot +
      ggplot2::geom_line(data = dfROCDashes, linetype = 3, linewidth = 2)
  }

  if (TRUE){
    K1 <- length(fp)
    K2 <- length(tp)
    ciX <- binom.confint(x = fpf * K1, n = K1, methods = "exact")
    ciY <- binom.confint(x = tpf * K2, n = K2, methods = "exact")
    ciXUpper <- ciX$upper
    ciXLower <- ciX$lower
    ciYUpper <- ciY$upper
    ciYLower <- ciY$lower
    for (pt in 1:length(fpf)){
      if (((pt != 1) && pt != length(fpf))) next
      ciX <- data.frame(fpf = c(ciXUpper[pt], ciXLower[pt]),
                        tpf = c(tpf[pt], tpf[pt]),
                        stringsAsFactors = TRUE)
      ciY <- data.frame(fpf = c(fpf[pt], fpf[pt]),
                        tpf = c(ciYUpper[pt], ciYLower[pt]),
                        stringsAsFactors = TRUE)
      fittedPlot <- fittedPlot + ggplot2::geom_line(data = ciY, ggplot2::aes(x = fpf, y = tpf), color = "black") +
        ggplot2::geom_line(data = ciX, ggplot2::aes(x = fpf, y = tpf), color = "black")
      barRgt <- data.frame(fpf = c(ciXUpper[pt], ciXUpper[pt]),
                           tpf = c(tpf[pt] - 0.01, tpf[pt] + 0.01),
                           stringsAsFactors = TRUE)
      barLft <- data.frame(fpf = c(ciXLower[pt], ciXLower[pt]),
                           tpf = c(tpf[pt] - 0.01, tpf[pt] + 0.01),
                           stringsAsFactors = TRUE)
      barUp <- data.frame(fpf = c(fpf[pt] - 0.01, fpf[pt] + 0.01),
                          tpf = c(ciYUpper[pt], ciYUpper[pt]),
                          stringsAsFactors = TRUE)
      barBtm <- data.frame(fpf = c(fpf[pt] - 0.01, fpf[pt] + 0.01),
                           tpf = c(ciYLower[pt], ciYLower[pt]),
                           stringsAsFactors = TRUE)
      fittedPlot <- fittedPlot +
        ggplot2::geom_line(data = barRgt, ggplot2::aes(x = fpf, y = tpf), color = "black") +
        ggplot2::geom_line(data = barLft, ggplot2::aes(x = fpf, y = tpf), color = "black") +
        ggplot2::geom_line(data = barUp, ggplot2::aes(x = fpf, y = tpf), color = "black") +
        ggplot2::geom_line(data = barBtm, ggplot2::aes(x = fpf, y = tpf), color = "black") +
        xlab("FPF") + ylab("TPF")
    }
  }
  return(
    fittedPlot = fittedPlot
  )
}




LrocPlots1 <- function (zjk1, zjk2)
{
  J <- length(zjk1[,1])
  lrocPlotData <- NULL
  for (j in 1:J) {
    zjk1Temp <- zjk1[j,]
    zk2Temp <- zjk2[j,]
    lroc <- LrocOperatingPointsFromRatings( zjk1Temp, zk2Temp )
    FPF <- lroc$FPF
    PCL <- lroc$PCL
    reader = paste0("R-", as.character(j))
    lrocPlotData <- rbind(lrocPlotData, data.frame(FPF = FPF, PCL = PCL, reader = reader, stringsAsFactors = TRUE))
  }

  lrocPlot <- ggplot2::ggplot(data = lrocPlotData, ggplot2::aes(x = FPF, y = PCL, color = reader)) + ggplot2::geom_line()
  g <- ggplot_build(lrocPlot)
  colors <- as.character(unique(g$data[[1]]$colour))
  sizes <- rep(1, J)
  lrocPlot <- ggplot2::ggplot(data = lrocPlotData, ggplot2::aes(x = FPF, y = PCL, color = reader)) + ggplot2::geom_line(ggplot2::aes(linewidth = reader)) +
    scale_color_manual(values = colors) + scale_size_manual(values = sizes) +
    theme(legend.position = "inside", legend.position.inside = c(1,0)) +
    theme(legend.title = element_blank(), legend.justification = c(1, 0)) +
    scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1))
  return(list(
    lrocPlot = lrocPlot
  ))
}

###WIP###
####################################################################################################################
LROCPoints <- function(dataset, treatments2Plot, readers2Plot) {

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID


  I <- length(treatments2Plot) # found possible error 11/10/19
  # I <- dim(NL)[1] # No; this gives error
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

  LROCPoints <- data.frame(FPF = NULL, PCL = NULL, stringsAsFactors = TRUE)
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- NL[i, j, 1:K1,1]
      cl <- LL[i, j, 1:K2,1]
      ret1 <- RawOpPtsLROC2LROC (fp, cl)
      FPF <- ret1$fpf;FPF <- c(0, FPF, 1)
      PCL <- ret1$pcl;PCL <- c(0, PCL, PCL[length(PCL)]) # extend flat part to FPF = 1
      LROCPoints <- rbind(LROCPoints, data.frame(FPF = FPF,
                                                 PCL = PCL,
                                                 Modality = i,
                                                 Reader = j,
                                                 stringsAsFactors = TRUE))
    }
  }
  class <- paste("M: ", modalityID[LROCPoints$Modality], "\n", "R: ", readerID[LROCPoints$Reader], sep = "")
  LROCPoints <- data.frame(genAbscissa = LROCPoints$FPF,
                           genOrdinate = LROCPoints$PCL,
                           class = class,
                           type = "D",
                           stringsAsFactors = TRUE)
  return(LROCPoints)
}




RawOpPtsLROC2LROC <- function (fp, cl) {
  zetas <- sort(unique(c(fp, cl)))
  nBins <- length(zetas)
  fpCounts <- rep(NA, nBins)
  clCounts <- fpCounts
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == zetas[b])
    clCounts[b] <- sum(cl == zetas[b])
  }
  K1 <- length(fp)  # !sic!
  K2 <- length(cl)
  fpf <- cumsum(rev(fpCounts)) / K1
  pcl <- cumsum(rev(clCounts)) / K2
  fpf <- fpf[-length(fpf)]
  pcl <- pcl[-length(pcl)]
  return(list(
    fpCounts = fpCounts,
    clCounts = clCounts,
    fpf = fpf,
    pcl = pcl,
    zetas = zetas
  ))
}



####################################################################################################################
AvgLROCPoints <- function(dataset, treatments2Plot, readers2Plot) {

  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID

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
  avgPCL <- rep(0, length(sampledFPF))
  for (i in 1:I) {
    for (j in 1:J) {
      fp <- NL[i, j, 1:K1, 1]
      cl <- LL[i, j, 1:K2, 1]
      ret1 <- RawOpPtsLROC2LROC (fp, cl)
      FPF <- ret1$fpf;FPF <- c(0, FPF, 1)
      PCL <- ret1$pcl;PCL <- c(0, PCL, PCL[length(PCL)]) # extend flat part to FPF = 1
      temp <- (approx(FPF, PCL, xout = sampledFPF, ties = min))$y
      avgPCL <- avgPCL + temp
    }
  }
  avgPCL <- avgPCL/(I * J)
  avgPCL <- c(0,avgPCL);sampledFPF <- c(0,sampledFPF)
  class <- paste(paste("M: "), paste(modalityID, collapse = " "), "\n", paste("R: "), paste(readerID, collapse = " "), sep = "")
  LROCPoints <- data.frame(genAbscissa = sampledFPF,
                           genOrdinate = avgPCL,
                           class = class,
                           type = "A",
                           stringsAsFactors = TRUE)
  return(LROCPoints)
}
