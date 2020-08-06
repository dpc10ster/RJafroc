ReadJAFROCNewFormat <- function(fileName, sequentialNames) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  wb <- loadWorkbook(fileName)
  temp <- sort(toupper(names(wb)))
  if (!(temp[1] %in% c("FP", "NL"))) stop("FP or NL sheet not found\n")
  if (!(temp[2] %in% c("TP", "LL"))) stop("TP or LL sheet not found\n")
  if (!(temp[3] %in% c("TRUTH"))) stop("Truth sheet not found\n")
  sheetNames <- toupper(names(wb)) 
  
  ########################## CHECK TRUTH TABLE ##############################
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is, i.e., 1st, 2nd or 3rd tab position in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (length(truthFileIndex) == 0) stop("TRUTH table worksheet cannot be found in the Excel file.")
  truthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:6)
  if (length(truthTable) != 6) stop("Old Excel format file encountered; cannot use newExcelFileFormat = TRUE")
  cTT <- checkTruthTable(truthTable) # cTT = checkTruthTable
  
  truthTableSort <- cTT$truthTableSort 
  rdrArr1D <- cTT$rdrArr1D
  trtArr1D <- cTT$trtArr1D
  truthTableStr <- cTT$truthTableStr
  truthCaseID <-  cTT$caseID # these need not be unique for FROC datasets, as more than one mark is possible
  type <- cTT$type
  design <- cTT$design
  weights <- cTT$weights
  perCase <- cTT$perCase
  IDs <- cTT$IDs
  lesionIDCol <- cTT$lesionIDCol
  normalCases <- cTT$normalCases
  abnormalCases <- unique(cTT$abnormalCases)
  
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  ########################### CHECK NL TABLE ################################
  nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  if (length(nlFileIndex) == 0) stop("FP/NL table worksheet cannot be found in the Excel file.")
  NLTable <- read.xlsx(fileName, nlFileIndex, cols = 1:4)
  
  # grep "^\\s*$" matches blank lines; see learnGrep in desktop
  # grep("^\\s*$", "") = 1
  # grep("^\\s*$", c("","")) = 1 2 etc
  # following replaces empty cells with NAs
  # it is not needed as the excel read function already does that
  # for (i in 1:4){
  #   NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
  # }
  
  naRows <- colSums(is.na(NLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      NLTable <- NLTable[1:(nrow(NLTable) - max(naRows)), ]
    }
  }
  
  for (i in 3:4) {
    if (any(is.na(as.numeric(as.character(NLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(NLTable[, i])))) + 1
      errorMsg <- paste0("Data entry error at line ", paste(naLines, collapse = ", "), " in the FP worksheet")
      stop(errorMsg)
    }
  }
  
  NLReaderIDCol <- as.character(NLTable$ReaderID)
  NLModalityIDCol <- as.character(NLTable$ModalityID)
  NLCaseIDCol <- NLTable$CaseID
  if (is.null(NLTable$FP_Rating)) NLRatingCol <- NLTable$NL_Rating else NLRatingCol <- NLTable$FP_Rating
  
  if (any(!(NLCaseIDCol %in% truthCaseID))) {
    naCases <- NLCaseIDCol[which(!(NLCaseIDCol %in% truthCaseID))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), 
                       " in the FP table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  
  ########################### CHECK LL TABLE ################################
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (length(llFileIndex) == 0) stop("TP/LL table worksheet cannot be found in the Excel file.")
  LLTable <- read.xlsx(fileName, llFileIndex, cols = 1:5)
  
  for (i in 1:5){
    LLTable[grep("^\\s*$", LLTable[ , i]), i] <- NA
  }
  
  naRows <- colSums(is.na(LLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      LLTable <- LLTable[1:(nrow(LLTable) - max(naRows)), ]
    }
  }
  
  for (i in 3:5) {
    if (any(is.na(as.numeric(as.character(LLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(LLTable[, i])))) + 1
      errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", 
                         paste(naLines, collapse = ", "), " in the TP table.")
      stop(errorMsg)
    }
  }
  
  LLReaderIDCol <- as.character(LLTable$ReaderID)
  LLModalityIDCol <- as.character(LLTable$ModalityID)
  LLCaseIDCol <- LLTable$CaseID
  LLLesionIDCol <- LLTable$LesionID
  if (is.null(LLTable$TP_Rating)) LLRatingCol <- LLTable$LL_Rating else LLRatingCol <- LLTable$TP_Rating
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseIDCol[i]) & (lesionIDCol == LLLesionIDCol[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], 
                         " Reader(s) ", LLTable[i, 1], 
                         " Case(s) ", LLTable[i, 3], 
                         " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  if (any(LLCaseIDCol %in% normalCases)) {
    errorMsg <- paste0("Normal case(s) found in TP table.")
    stop(errorMsg)
  }
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseIDCol[i]) & (lesionIDCol == LLLesionIDCol[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], " Reader(s) ", 
                         LLTable[i, 1], " Case(s) ", LLTable[i, 3], " Lesion(s) ", 
                         LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  if (anyDuplicated(LLTable[, 1:4])) {
    naLines <- which(duplicated(LLTable[, 1:4]))
    errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), 
                       " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), 
                       " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
                       paste(LLTable[naLines, 4], collapse = ", "), 
                       " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  modalityIDUnique <- as.character(unique(c(NLModalityIDCol, LLModalityIDCol)))
  I <- length(modalityIDUnique)
  readerIDUnique <- as.character(unique(c(NLReaderIDCol, LLReaderIDCol)))
  J <- length(readerIDUnique)
  
  maxNL <- 0
  for (i in modalityIDUnique) {
    for (j in readerIDUnique) {
      casePresent_ij <- (NLModalityIDCol == i) & (NLReaderIDCol == j)
      if ((sum(casePresent_ij) == 0)) 
        next
      maxNL <- max(maxNL, max(table(NLCaseIDCol[casePresent_ij])))
    }
  }
  
  L <- length(NLModalityIDCol)
  NL <- array(dim = c(I, J, K, maxNL))
  NLRatingCol <- as.numeric(NLRatingCol)
  if(any(is.na(NLRatingCol))) stop ("found NAs in NLRatingCol in NL/FP sheet")
  ############################ INIT NL ARRAY ################################
  for (l in 1:L) {
    i <- which(trtArr1D == NLModalityIDCol[l])
    j <- which(rdrArr1D == NLReaderIDCol[l])
    k <- which(unique(truthTableSort$CaseID) == NLCaseIDCol[l])
    nMatches <- which((NLCaseIDCol == NLCaseIDCol[l]) & (NLModalityIDCol == NLModalityIDCol[l]) & (NLReaderIDCol == NLReaderIDCol[l]))
    if (NLCaseIDCol[l] %in% normalCases) tt2 <- truthTableStr[i,j,k,1] else tt2 <- truthTableStr[i,j,k,2] 
    if (is.na(tt2)) stop("Error in reading NL/FP table: is.na(tt2)") else {
      if (tt2 != 1)  stop("Error in reading NL/FP table: tt2 != 1") else 
        for (el in 1:length(nMatches)) {
          # if a modality-reader-case has multiple marks, then enter the corresponding ratings
          # the is.na() check ensures that an already recorded mark is not overwritten
          # CANNOT determine el as in the LL case, see below, since the number of FROC NL marks is potentially unlimited
          # The first rating comes from l, the next from l+1, etc.
          if (is.na( NL[i, j, k, el])) NL[i, j, k, el] <- NLRatingCol[l+el-1]
        }
    }
  }
  NL[is.na(NL)] <- UNINITIALIZED
  
  ############################ INIT LL ARRAY ################################
  L <- length(LLModalityIDCol)
  LL <- array(dim = c(I, J, K2, max(perCase)))
  LLRatingCol <- as.numeric(LLRatingCol)
  if(any(is.na(LLRatingCol))) stop ("found NAs in LLRatingCol in LL/TP sheet")
  for (l in 1:L) {
    i <- which(trtArr1D == LLModalityIDCol[l])
    j <- which(rdrArr1D == LLReaderIDCol[l])
    k <- which(unique(truthTableSort$CaseID) == LLCaseIDCol[l]) - K1 # offset into abnormal cases
    # CAN determine el since the number of FROC LL marks is LIMITED to number of lesions in case
    el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l]) - 1
    tt2 <- truthTableStr[i,j,k+K1,el+1]
    if (is.na(tt2)) stop("Error in reading LL/TP table") else {
      if (tt2 != 1)  stop("Error in reading LL/TP table") else 
        # the is.na() check ensures that an already recorded mark is not overwritten
        if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
    }
  }
  
  LL[is.na(LL)] <- UNINITIALIZED
  weights[is.na(weights)] <- UNINITIALIZED
  lesionIDCol[is.na(lesionIDCol)] <- UNINITIALIZED
  
  if (type == "ROC" && design == "FCTRL") {
    if (!(((max(table(truthCaseID)) == 1) && (maxNL == 1)) 
          && (all((NL[, , (K1 + 1):K, ] == UNINITIALIZED))) 
          && (all((NL[, , 1:K1, ] != UNINITIALIZED)))
          && (all((LL[, , 1:K2, ] != UNINITIALIZED))))) {
      stop("This does not appear to be an ROC dataset.")
    }    
  }
  
  modalityNames <- modalityIDUnique
  readerNames <- readerIDUnique
  
  if (sequentialNames){
    modalityIDUnique <- 1:I
    readerIDUnique <- 1:J
  }
  
  names(modalityIDUnique) <- modalityNames; modalityID <- modalityIDUnique
  names(readerIDUnique) <- readerNames; readerID <- readerIDUnique
  
  fileName <- fileName
  name <- NA
  if ((design == "FCTRL") || (design == "CROSSED")) design <- "FCTRL"
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
  
} 

