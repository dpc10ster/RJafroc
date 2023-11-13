OutputExcelORH <- function(dataset,
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
  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
  K <- dim(dataset$ratings$NL)[3]
  K2 <- dim(dataset$ratings$LL)[3]
  K1 <- K - K2
  
  #############################################################    
  ## setup up empty excel output file containing Summary worksheet   
  wb <- createWorkbook()
  addWorksheet(wb, "Summary")
  writeData(wb, sheet = "Summary", x = summaryInfo, rowNames = TRUE, colNames = FALSE)
  
  modalityID <- data.frame(output = dataset$descriptions$modalityID, input = names(dataset$descriptions$modalityID))
  colnames(modalityID) <- c("Modality ID in output file", "Modality ID in input file")
  writeData(wb, sheet = "Summary", x = modalityID, startRow = 5, colNames = TRUE)
  
  readerID <- data.frame(output = dataset$descriptions$readerID, input = names(dataset$descriptions$readerID))
  colnames(readerID) <- c("Reader ID in output file", "Reader ID in input file")
  writeData(wb, sheet = "Summary", x = readerID, startRow = 5, startCol = 3, colNames = TRUE)
  
  analysisInfo <- data.frame(info = c(K1, K2, FOM, method, covEstMethod))
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
  sheet <- "FOMs"
  addWorksheet(wb, sheet)
  setColWidths(wb, sheet = sheet, cols = 1:(J + 3), widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = sheet, cols = 1, widths = 10)
  
  startRow <- 1
  df <- StResult$FOMs$foms
  hdr <- "FOMs: reader vs. modality"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <- StResult$FOMs$trtMeans
  hdr <- "modality means"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <- StResult$FOMs$trtMeanDiffs
  hdr <- "modality differences"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  #############################################################    
  # done with FOMs, now create contents of ANOVA worksheet
  sheet <- "ANOVA"
  addWorksheet(wb, sheet)
  setColWidths(wb, sheet = sheet, cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = sheet, cols = 1, widths = 10)
  
  startRow <- 1
  df <- StResult$ANOVA$TRanova
  hdr <- "OR modality reader ANOVA"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <- StResult$ANOVA$VarCom
  hdr <- "OR Variance Components"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <- UtilOR2DBMVarCom(K, df)
  hdr <- "DBM Variance Components"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <-StResult$ANOVA$IndividualTrt
  hdr <- "Individual modality var. comp."
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <-StResult$ANOVA$IndividualRdr
  hdr <- "Individual reader var. comp."
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)

  #############################################################    
  # done with ANOVA, now create contents of RRRC worksheet
  sheet <- "RRRC"
  addWorksheet(wb, sheet)
  setColWidths(wb, sheet = sheet, cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = sheet, cols = 1, widths = 10)
  
  startRow <- 1
  df <- StResult$RRRC$FTests
  hdr <- "(a) F-Tests of NH"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <- StResult$RRRC$ciDiffTrt
  hdr <- "(b) 95% confidence interval for modality differences"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <-StResult$RRRC$ciAvgRdrEachTrt
  hdr <- "(c) 95% confidence interval for each modality"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  #############################################################    
  # done with RRRC, now create contents of FRRC worksheet
  sheet <- "FRRC"
  addWorksheet(wb, sheet)
  setColWidths(wb, sheet = sheet, cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = sheet, cols = 1, widths = 10)
  
  startRow <- 1
  df <- StResult$FRRC$FTests
  hdr <- "(a) F-Tests of NH"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <- StResult$FRRC$ciDiffTrt
  hdr <- "(b) 95% confidence interval for modality differences"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <-StResult$FRRC$ciAvgRdrEachTrt
  hdr <- "(c) 95% confidence interval for each modality"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <-StResult$FRRC$ciDiffTrtEachRdr
  hdr <- "(d) 95% confidence interval modality differences, each reader"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  #############################################################    
  # done with FRRC, now create contents of RRFC worksheet
  sheet <- "RRFC"
  addWorksheet(wb, sheet)
  setColWidths(wb, sheet = sheet, cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = sheet, cols = 1, widths = 10)
  
  startRow <- 1
  df <- StResult$RRFC$FTests
  hdr <- "(a) F-Tests of NH"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <- StResult$RRFC$ciDiffTrt
  hdr <- "(b) 95% confidence interval for modality differences"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  df <-StResult$RRFC$ciAvgRdrEachTrt
  hdr <- "(c) 95% confidence interval for each modality"
  startRow <- OutputDataFrame (wb, sheet, startRow, df, sty, hdr)
  
  #############################################################    
  # ALL DONE, save workbook
  saveWorkbook(wb, ReportFileName, overwrite = TRUE)
  sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)
  return(sucessfulOutput)
}


OutputDataFrame <- function (wb, sheet, startRow, df, sty, hdr) {
  
  addStyle(wb,  
           sheet = sheet, 
           style = sty, 
           rows = startRow:(startRow + nrow(df) + 1), 
           cols = 1:(ncol(df) + 1), 
           gridExpand = TRUE)
  mergeCells(wb, sheet, rows = startRow, cols = 1:(ncol(df)+1))
  writeData(wb, sheet = sheet, 
            startRow = startRow, 
            x = hdr, 
            rowNames = FALSE, colNames = FALSE)
  startRow <- startRow + 1
  writeData(wb, sheet = sheet, 
            startRow = startRow, 
            x = df, 
            rowNames = TRUE, colNames = TRUE)
  
  startRow <-  startRow + nrow(df) + 2 
  
  return(startRow)
}
