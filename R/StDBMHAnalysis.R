StDBMHAnalysis <- function(dataset, FOM, FPFValue, alpha, analysisOption) 
{
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  I <- dim(dataset$ratings$NL)[1]
  
  modalityID <- dataset$descriptions$modalityID

  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  ret <- UtilDBMVarComp(dataset, FOM, FPFValue)
  
  VarCom <- ret$VarCom
  TRCanova <- ret$TRCanova
  IndividualTrt <- ret$IndividualTrt
  IndividualRdr <- ret$IndividualRdr

  ANOVA <- list()
  ANOVA$TRCanova <- TRCanova
  ANOVA$VarCom <- VarCom
  ANOVA$IndividualTrt <- IndividualTrt
  ANOVA$IndividualRdr <- IndividualRdr
  
  trtMeans <- rowMeans(foms) 
  trtMeans <- as.data.frame(trtMeans)
  colnames(trtMeans) <- "Estimate"
  
  trtMeanDiffs <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i,"Estimate"] - trtMeans[ip,"Estimate"]
      diffTRName[ii] <- paste0("trt", modalityID[i], sep = "-", "trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("Estimate" = trtMeanDiffs,
                             row.names = diffTRName,
                             stringsAsFactors = FALSE) 
  
  FOMs <- list(
    foms = foms,
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs
  )
  
  if (analysisOption == "RRRC") {
    RRRC <- DBMSummaryRRRC(dataset, FOM, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- DBMSummaryFRRC(dataset, FOM, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- DBMSummaryRRFC(dataset, FOM, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- DBMSummaryRRRC(dataset, FOM, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- DBMSummaryFRRC(dataset, FOM, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- DBMSummaryRRFC(dataset, FOM, FOMs, ANOVA, alpha, diffTRName)
    
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
} 




pseudoValueMeanSquares <- function (pseudoValues)
{
  I <- length(pseudoValues[,1,1])
  J <- length(pseudoValues[1,,1])
  K <- length(pseudoValues[1,1,])
  
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(pseudoValues[i, , ]) - mean(pseudoValues))^2
  }
  msT <- msT * K * J/(I - 1)
  
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(pseudoValues[, j, ]) - mean(pseudoValues))^2
  }
  msR <- msR * K * I/(J - 1)
  
  
  msC <- 0
  for (k in 1:K) {
    msC <- msC + (mean(pseudoValues[, , k]) - mean(pseudoValues))^2
  }
  msC <- msC * I * J/(K - 1)
  
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, j, ]) + mean(pseudoValues))^2
    }
  }
  msTR <- msTR * K/((I - 1) * (J - 1))
  
  
  msTC <- 0
  for (i in 1:I) {
    for (k in 1:K) {
      msTC <- msTC + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
    }
  }
  msTC <- msTC * J/((I - 1) * (K - 1))
  
  
  msRC <- 0
  for (j in 1:J) {
    for (k in 1:K) {
      msRC <- msRC + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
    }
  }
  msRC <- msRC * I/((J - 1) * (K - 1))
  
  msTRC <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K) {
        msTRC <- msTRC + 
          (pseudoValues[i, j, k] - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) - mean(pseudoValues[, j, k]) + 
             mean(pseudoValues[i, , ]) + mean(pseudoValues[, j, ]) + mean(pseudoValues[, , k]) - mean(pseudoValues))^2
      }
    }
  }
  msTRC <- msTRC/((I - 1) * (J - 1) * (K - 1))
  
  mSquares <- data.frame(msT = msT,
                         msR = msR,
                         msC = msC,
                         msTR = msTR,
                         msTC = msTC,
                         msRC = msRC,
                         msTRC = msTRC)
  
  return(mSquares) 
}



  
  