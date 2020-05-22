StORHAnalysis <- function(dataset, FOM, FPFValue, alpha = 0.05, covEstMethod = "jackknife", 
                          nBoots = 200, analysisOption = "ALL")  
{
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID <- dataset$modalityID
  I <- length(modalityID)

  ret <- UtilVarComponentsOR(dataset, FOM, FPFValue, covEstMethod, nBoots)
  
  foms <- ret$foms
  TRanova <- ret$TRanova
  VarCom <-  ret$VarCom
  IndividualTrt <- ret$IndividualTrt
  IndividualRdr <- ret$IndividualRdr
  
  ANOVA <- list()
  ANOVA$TRanova <- TRanova
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
      trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
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
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  

  if (analysisOption == "FRRC") {
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
  
} 

