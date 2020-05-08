OutputExcelFile <- function(dataset,
                            method,
                            methodTxt,
                            ReportFileName,
                            covEstMethod,
                            summaryInfo,
                            alpha,
                            FOM,
                            analysisOption,
                            StResult)
{
  NL <- dataset$NL
  LL <- dataset$LL
  I <- length(dataset$NL[,1,1,1])
  J <- length(dataset$NL[1,,1,1])
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  #############################################################    
  ## setup up empty excel output file containing Summary worksheet     
  wb <- createWorkbook()
  addWorksheet(wb, "Summary")
  writeData(wb, sheet = "Summary", x = summaryInfo, rowNames = TRUE, colNames = FALSE)
  
  modalityID <- data.frame(output = dataset$modalityID, input = names(dataset$modalityID))
  colnames(modalityID) <- c("Modality ID in output file", "Modality ID in input file")
  writeData(wb, sheet = "Summary", x = modalityID, startRow = 5, colNames = TRUE)
  
  readerID <- data.frame(output = dataset$readerID, input = names(dataset$readerID))
  colnames(readerID) <- c("Reader ID in output file", "Reader ID in input file")
  writeData(wb, sheet = "Summary", x = readerID, startRow = 5, startCol = 3, colNames = TRUE)
  
  if (method == "DBMH"){
    varEstMethod <- "Jackknife"
  }else{
    varEstMethod <- covEstMethod
  }
  
  analysisInfo <- data.frame(info = c(K1, K2, FOM, method, varEstMethod))
  rownames(analysisInfo) <- c("Number of non-diseased cases", 
                              "Number of diseased cases", 
                              "FOM", 
                              "Significance testing", 
                              "Variability estimation method")
  writeData(wb, sheet = "Summary", 
            x = analysisInfo, 
            startRow = 7 + max(I, J), 
            startCol = 1, 
            rowNames = TRUE, 
            colNames = FALSE)
  sty <- createStyle(halign = "center", valign = "center")
  addStyle(wb,  sheet = "Summary", 
           style = sty, rows = seq(1, 11 + max(I, J)), cols = 1:4, gridExpand = TRUE)
  setColWidths(wb, sheet = "Summary", 
               cols = 1:4, widths = "auto", ignoreMergedCells = TRUE)
  
  #############################################################    
  # done with Summary, now create contents of FOMs worksheet    
  addWorksheet(wb, "FOMs")
  setColWidths(wb, sheet = "FOMs", cols = 1:(J + 3), widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "FOMs", cols = 1, widths = 10)
  addStyle(wb,  sheet = "FOMs", style = sty, rows = 1:(I + 2), cols = 1:(J + 3), gridExpand = TRUE)
  fomArray <- as.data.frame(t(StResult$FVCA$foms))
  # since StSignificanceTesting returned transpose for `foms`
  if (I == 2){
    fomArray <- cbind(fomArray, apply(fomArray, 1, mean), diff(rev(apply(fomArray, 1, mean))))
  }else{
    fomArray <- cbind(fomArray, apply(fomArray, 1, mean))
  }
  
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  rowNames <- NULL
  for (i in 1:I){
    rowNames <- c(rowNames, paste("Trt", "-", modalityID[i]))
  }
  rownames(fomArray) <- rowNames
  
  colNames <- NULL
  for (j in 1:J){
    colNames <- c(colNames, paste("Rdr", "-", readerID[j]))
  }
  if (I == 2){
    colNames <- c(colNames, "Rdr. Avg.", "Observed effect size")
  }else{
    colNames <- c(colNames, "Rdr. Avg.")
  }
  colnames(fomArray) <- colNames
  
  if (I == 2){
    mergeCells(wb, "FOMs", rows = 1, cols = 1:(J+3))
    mergeCells(wb, "FOMs", rows = 3:(I + 2), cols = (J+3))
  }else{
    mergeCells(wb, "FOMs", rows = 1, cols = 1:(J+2))
  }
  
  writeData(wb, sheet = "FOMs", 
            startRow = 1, 
            x = "FOMs: reader vs. treatment", 
            rowNames = FALSE, colNames = FALSE)
  writeData(wb, sheet = "FOMs", 
            startRow = 2, 
            x = fomArray, 
            rowNames = TRUE, colNames = TRUE)
  
  #############################################################    
  # done with FOMs, now create contents of RRRC worksheet    
  addWorksheet(wb, "RRRC")
  setColWidths(wb, sheet = "RRRC", cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "RRRC", cols = 1, widths = 10)
  testTable <- data.frame(f = StResult$RRRC$FTests$f, ddf = StResult$RRRC$FTests$ddf, p = StResult$RRRC$FTests$p)
  names(testTable) <- c("F statistic", "ddf", "P-value")
  writeData(wb, sheet = "RRRC", x = testTable, rowNames = FALSE, colNames = TRUE)
  
  diffTRName <- NULL
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRName <- c(diffTRName, paste(modalityID[i], modalityID[ip], sep = "-"))
    }
  }
  
  diffTable <- StResult$RRRC$ciDiffTrt
  # diffTable[ , 1] <- diffTRName
  # diffTable[ , 2] <- as.numeric(diffTable[ , 2])
  # names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr>t",	"Lower",	"Upper")
  writeData(wb, sheet = "RRRC", 
            startRow = 5, 
            x = diffTable, 
            rowNames = FALSE, colNames = TRUE)
  
  mergeCells(wb, "RRRC", rows = 4, cols = 1:8)
  writeData(wb, sheet = "RRRC", startRow = 4, x = "95% CI's FOMs, treatment difference", 
            rowNames = FALSE, colNames = FALSE)
  
  ciTable <- StResult$RRRC$ciAvgRdrEachTrt
  ciTable$StdErr <- as.numeric(ciTable$StdErr)
  ciTable$DF <- as.numeric(ciTable$DF)
  ciTable[ , 1] <- modalityID
  names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
  writeData(wb, sheet = "RRRC", 
            startRow = 8 + nrow(diffTable), 
            x = ciTable, rowNames = FALSE, colNames = TRUE)
  addStyle(wb,  sheet = "RRRC", 
           style = sty, 
           rows = 1:(8 + nrow(diffTable) + nrow(ciTable)), 
           cols = 1:(J + 3), gridExpand = TRUE)
  
  writeData(wb, sheet = "RRRC", startRow = 7 + nrow(diffTable), 
            x = "95% CI's FOMs, each treatment", rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "RRRC", rows = 7 + nrow(diffTable), cols = 1:6)
  
  #############################################################    
  # done with RRRC, now create contents of FRRC worksheet    
  addWorksheet(wb, "FRRC")
  setColWidths(wb, sheet = "FRRC", cols = 1:9, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "FRRC", cols = 1, widths = 10)
  testTable <- data.frame(f = StResult$FRRC$FTests$f, ddf = StResult$FRRC$FTests$ddf, p = StResult$FRRC$FTests$p)
  if (method == "ORH"){
    testTable$ddf <- "Inf"
  }
  names(testTable) <- c("F statistic", "ddf", "P-value")
  writeData(wb, sheet = "FRRC", x = testTable, rowNames = FALSE, colNames = TRUE)
  
  diffTable <- StResult$FRRC$ciDiffTrt
  diffTable[ , 1] <- diffTRName
  diffTable[ , 2] <- as.numeric(diffTable[ , 2])
  if (method == "ORH"){
    diffTable[ , 4] <- "Inf"
  }
  
  names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr > t",	"Lower",	"Upper")
  writeData(wb, sheet = "FRRC", 
            x = diffTable, startRow = 5, 
            rowNames = FALSE, 
            colNames = TRUE)
  
  writeData(wb, sheet = "FRRC", 
            startRow = 4, x = "95% CI's FOMs, treatment difference", 
            rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "FRRC", rows = 4, cols = 1:8)
  
  ciTable <- StResult$FRRC$ciAvgRdrEachTrt
  ciTable$StdErr <- as.numeric(ciTable$StdErr)
  ciTable$DF <- as.numeric(ciTable$DF)
  ciTable[ , 1] <- modalityID
  if (method == "ORH"){
    ciTable[ , 4] <- "Inf"
  }
  
  names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
  writeData(wb, sheet = "FRRC", 
            startRow = 8 + nrow(diffTable), 
            x = ciTable, 
            rowNames = FALSE, 
            colNames = TRUE)
  
  writeData(wb, sheet = "FRRC", 
            startRow = 7 + nrow(diffTable), 
            x = "95% CI's FOMs, each treatment", 
            rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "FRRC", rows = 7 + nrow(diffTable), cols = 1:6)
  
  readerNames <- rep(readerID, choose(I, 2))
  trNames <- rep(diffTRName, J)
  diffTableEchR <- StResult$FRRC$ciDiffTrtEachRdr
  diffTableEchR$Reader <- readerNames
  diffTableEchR$Treatment <- trNames
  if (method == "ORH"){
    diffTableEchR$DF <- "Inf"
  }
  
  names(diffTableEchR)[8:9] <- c("Lower",	"Upper")
  writeData(wb, sheet = "FRRC", 
            startRow = 11 + nrow(diffTable) + nrow(ciTable), 
            x = diffTableEchR, rowNames = FALSE, colNames = TRUE)
  addStyle(wb,  sheet = "FRRC", 
           style = sty, 
           rows = 1:(11 + nrow(diffTable) + nrow(ciTable) + nrow(diffTableEchR)), 
           cols = 1:9, gridExpand = TRUE)
  writeData(wb, sheet = "FRRC", 
            startRow = 10 + nrow(diffTable) + nrow(ciTable), 
            x = "95% CI's FOMs, treatment difference, each reader", 
            rowNames = FALSE, 
            colNames = FALSE)
  mergeCells(wb, "FRRC", rows = 10 + nrow(diffTable) + nrow(ciTable), cols = 1:9)
  
  #############################################################    
  # done with FRRC, now create contents of RRFC worksheet    
  addWorksheet(wb, "RRFC")
  setColWidths(wb, sheet = "RRFC", cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "RRFC", cols = 1, widths = 10)
  testTable <- data.frame(f = StResult$RRFC$FTests$f, ddf = StResult$RRFC$FTests$ddf, p = StResult$RRFC$FTests$p)
  names(testTable) <- c("F statistic", "ddf", "P-value")
  writeData(wb, sheet = "RRFC", x = testTable, rowNames = FALSE, colNames = TRUE)
  
  diffTable <- StResult$RRFC$ciDiffTrt
  # diffTable[ , 1] <- diffTRName
  # diffTable[ , 2] <- as.numeric(diffTable[ , 2])
  # names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr > t",	"Lower",	"Upper")
  writeData(wb, sheet = "RRFC", startRow = 5, 
            x = diffTable, rowNames = FALSE, colNames = TRUE)
  
  writeData(wb, sheet = "RRFC", startRow = 4, 
            x = "95% CI's FOMs, treatment difference", rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "RRFC", rows = 4, cols = 1:8)
  
  ciTable <- StResult$RRFC$ciAvgRdrEachTrt
  ciTable$StdErr <- as.numeric(ciTable$StdErr)
  ciTable$DF <- as.numeric(ciTable$DF)
  ciTable[ , 1] <- modalityID
  names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
  writeData(wb, sheet = "RRFC", startRow = 8 + nrow(diffTable), 
            x = ciTable, rowNames = FALSE, colNames = TRUE)
  writeData(wb, sheet = "RRFC", startRow = 7 + nrow(diffTable), 
            x = "95% CI's FOMs, each treatment", rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "RRFC", rows = 7 + nrow(diffTable), cols = 1:6)
  addStyle(wb,  sheet = "RRFC", style = sty, rows = 1:(8 + nrow(diffTable) + nrow(ciTable)), 
           cols = 1:8, gridExpand = TRUE)
  
  if (method == "DBMH"){
    #############################################################    
    # done with RRFC, now create contents of ANOVA worksheet    
    addWorksheet(wb, "ANOVA")
    setColWidths(wb, sheet = "ANOVA", 
                 cols = 1:9, 
                 widths = "auto", 
                 ignoreMergedCells = TRUE)
    
    setColWidths(wb, sheet = "ANOVA", 
                 cols = 1, widths = 10)
    
    writeData(wb, sheet = "ANOVA", 
              x = "DBMH variance components", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 1, cols = 1:2)
    
    writeData(wb, sheet = "ANOVA", 
              x = StResult$FVCA$DBMVarComp, 
              startRow = 2, 
              rowNames = FALSE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "ANOVA", 
              x = StResult$FVCA$TRCanovaDBM, 
              startRow = 10, 
              rowNames = TRUE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "ANOVA", 
              startRow = 9,  
              x = "TREATMENT X READER X CASE ANOVA", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 9, cols = 1:4)
    
    colnames(StResult$FVCA$RCanovaDBMSingleTrt) <- c("Source", "DF", rownames(fomArray))
    writeData(wb, sheet = "ANOVA", 
              x = StResult$FVCA$RCanovaDBMSingleTrt, 
              startRow = 21, 
              rowNames = FALSE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "ANOVA", 
              startRow = 20,  
              x = "READER X CASE ANOVA for each Trt", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 20, cols = 1:4)
    
    colnames(StResult$FRRC$msAnovaEachRdr) <- c("Source", "DF", colnames(fomArray)[1:J])
    writeData(wb, sheet = "ANOVA", 
              x = StResult$FRRC$msAnovaEachRdr, 
              startRow = 27, 
              rowNames = FALSE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "ANOVA", startRow = 26,  
              x = "TREATMENT X CASE ANOVAs (MS) for each reader, assuming fixed reader analysis", 
              rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 26, cols = 1:(J + 2))
    addStyle(wb,  sheet = "ANOVA", style = sty, rows = 1:30, 
             cols = 1:(2+J), gridExpand = TRUE)
  } else {
    #############################################################    
    # method == "ORH"
    # done with RRFC, now create contents of VarComp worksheet    
    addWorksheet(wb, "VarComp")
    setColWidths(wb, sheet = "VarComp", cols = 1:9, widths = "auto", ignoreMergedCells = TRUE)
    setColWidths(wb, sheet = "VarComp", cols = 1, widths = 10)
    
    writeData(wb, sheet = "VarComp", 
              startRow = 1,  
              x = "OR FOM Variance Covariance Components", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "VarComp", rows = 1, cols = 1:2)
    
    writeData(wb, sheet = "VarComp", 
              x = StResult$FVCA$ORVarComp, 
              startRow = 2, 
              rowNames = FALSE, 
              colNames = TRUE)
    StResult$FRRC$varCovEachRdr[ , 1] <- readerID
    
    writeData(wb, sheet = "VarComp", 
              x = StResult$FRRC$varCovEachRdr, 
              startRow = 10, 
              rowNames = FALSE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "VarComp", startRow = 9,  
              x = "OR Variance Covariance Components for each reader, assuming fixed reader analysis", 
              rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "VarComp", rows = 9, cols = 1:3)
    addStyle(wb,  sheet = "VarComp", style = sty, rows = 1:(10 + J), 
             cols = 1:3, gridExpand = TRUE)
  }
  saveWorkbook(wb, ReportFileName, overwrite = TRUE)
  sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)
  return(sucessfulOutput)
}

