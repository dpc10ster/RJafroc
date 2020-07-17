ReadJAFROCNewFormat <- function(fileName, sequentialNames) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  wb <- loadWorkbook(fileName)
  temp <- sort(toupper(names(wb)))
  if (!(temp[1] %in% c("FP", "NL"))) stop("FP or NL sheet not found\n")
  if (!(temp[2] %in% c("TP", "LL"))) stop("TP or LL sheet not found\n")
  if (!(temp[3] %in% c("TRUTH"))) stop("Truth sheet not found\n")
  # need to undo sorting to find position correctly as otherwise FP and TP can get interchanged
  # dpc 1/28/20
  sheetNames <- toupper(names(wb)) 
  
  ########################## CHECK TRUTH TABLE ##############################
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is, i.e., 1st, 2nd or 3rd tab position in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (length(truthFileIndex) == 0) stop("TRUTH table worksheet cannot be found in the Excel file.")
  truthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:6)
  if (length(truthTable) != 6) stop("Old Excel format file encountered; cannot use newExcelFileFormat = TRUE")
  truth <- checkTruthTable(truthTable) 
  
  truthTableStr <- truth$truthTableStr
  truthCaseID <-  truth$caseID # these need not be unique for FROC datasets
  type <- truth$type
  design <- truth$design
  weights <- truth$weights
  perCase <- truth$perCase
  IDs <- truth$IDs
  lesionIDCol <- truth$lesionIDCol
  normalCases <- truth$normalCases
  abnormalCases <- truth$abnormalCases
  
  allCases <- c(normalCases, abnormalCases)
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
  
  ########################################################
  # sort the data read from the Excel worksheet
  ########################################################
  # df1 <- as.data.frame(NLTable, stringsAsFactors = FALSE)
  # NLTable <- df1[order(df1$ModalityID, df1$ReaderID, df1$CaseID),]
  # NLTable <- df1 # temporary line to bypass sorting
  
  # Col means the entire column is included
  NLReaderIDCol <- as.character(NLTable$ReaderID)
  NLModalityIDCol <- as.character(NLTable$ModalityID)
  NLCaseIDCol <- NLTable$CaseID
  if (is.null(NLTable$FP_Rating)) NLRatingCol <- NLTable$NL_Rating else
    NLRatingCol <- NLTable$FP_Rating
  
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
  
  ########################################################
  # sort the data read from the Excel worksheet
  ########################################################
  # df11 <- as.data.frame(LLTable, stringsAsFactors = FALSE)
  # LLTable <- df11[order(df11$ModalityID, df11$ReaderID, df11$CaseID, df11$LesionID),]
  # LLTable <- df11 # temporary line to bypass sorting
  
  LLReaderIDCol <- as.character(LLTable$ReaderID)
  LLModalityIDCol <- as.character(LLTable$ModalityID)
  LLCaseIDCol <- LLTable$CaseID
  LLLesionIDCol <- LLTable$LesionID
  if (is.null(LLTable$TP_Rating)) LLRatingCol <- LLTable$LL_Rating else
    LLRatingCol <- LLTable$TP_Rating
  
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
  
  
  ############################ CALC NL ARRAY ################################
  maxNL <- 0
  for (i in modalityIDUnique) {
    for (j in readerIDUnique) {
      casePresent_ij <- (NLModalityIDCol == i) & (NLReaderIDCol == j)
      if ((sum(casePresent_ij) == 0)) 
        next
      maxNL <- max(maxNL, max(table(NLCaseIDCol[casePresent_ij])))
    }
  }
  
  NL <- array(dim = c(I, J, K, maxNL))
  for (i in 1:I) {
    for (j in 1:J) {
      if (all(is.na(truthTableStr[i,j,,1]))) next
      casePresent_ij <- (NLModalityIDCol == modalityIDUnique[i]) & 
        (NLReaderIDCol == readerIDUnique[j])
      if ((sum(casePresent_ij) == 0)) next 
      caseNLTable <- table(NLCaseIDCol[casePresent_ij]) 
      # following are the actual caseIDs, not the indices into the array
      caseIDs_ij <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
      for (k1 in 1:length(caseIDs_ij)) {
        if (caseIDs_ij[k1] %in% normalCases) {
          tt2 <- truthTableStr[i,j,which(caseIDs_ij[k1] == allCases), 1]
          errMsg <- sprintf("Missing lesionID field, check caseID %s in Truth worksheet.", caseIDs_ij[k1])
          errMsg2 <- sprintf("Unknown error, check caseID %s in Truth worksheet.", caseIDs_ij[k1])
          if (is.na(tt2)) {
            stop(errMsg)
          } else if (tt2 != 1) stop(errMsg2)
        } else if (caseIDs_ij[k1] %in% abnormalCases) {
          tt2 <- truthTableStr[i,j,which(caseIDs_ij[k1] == allCases), 2]
          errMsg <- sprintf("Missing lesionID field, check caseID %s in Truth worksheet.", caseIDs_ij[k1])
          errMsg2 <- sprintf("Unknown error, check caseID %s in Truth worksheet.", caseIDs_ij[k1])
          if (is.na(tt2)) {
            stop(errMsg)
          } else if (tt2 != 1) stop(errMsg2)
        } else stop("Should never get here")
        for (el in 1:caseNLTable[k1]) {
          NL[i, j, which(caseIDs_ij[k1] == allCases), el] <- 
            NLRatingCol[casePresent_ij][which(NLCaseIDCol[casePresent_ij] == 
                                                   caseIDs_ij[k1])][el]
        }
      }
    }
  }
  NL[is.na(NL)] <- UNINITIALIZED
  
  ############################ CALC LL ARRAY ################################
  LL <- array(dim = c(I, J, K2, max(perCase)))
  for (i in 1:I) {
    for (j in 1:J) {
      casePresent_ij <- (LLModalityIDCol == modalityIDUnique[i]) & 
        (LLReaderIDCol == readerIDUnique[j])
      if ((sum(casePresent_ij) == 0)) next
      caseLLTable <- table(LLCaseIDCol[casePresent_ij])
      caseIDs_ij <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
      for (k2 in 1:length(caseIDs_ij)) {
        k2p <- which(caseIDs_ij[k2] == abnormalCases)
        x2 <- which(LLCaseIDCol[casePresent_ij] == caseIDs_ij[k2])
        x3 <- IDs[k2p, ]
        for (el in 1:caseLLTable[k2]) {
          elp <- which(LLLesionIDCol[casePresent_ij][x2][el] == x3)
          tt2 <- truthTableStr[i,j,which(caseIDs_ij[k2] == allCases),elp+1]
          errMsg <- sprintf("Missing lesionID field, check caseID %s in Truth worksheet.", caseIDs_ij[k2])
          errMsg2 <- sprintf("Unknown error, check caseID %s in Truth worksheet.", caseIDs_ij[k2])
          # elp+1 because lesionIDCol = 0 occupies first position, etc.
          if (is.na(tt2)) {
            stop(errMsg)
          } else if (tt2 != 1) stop(errMsg2)
          LL[i, j, k2p, elp] <- LLRatingCol[casePresent_ij][x2][el]
        }
      }
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
  
  fileName <- NA
  name <- NA
  if ((design == "FCTRL") || (design == "CROSSED")) design <- "FCTRL"
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
  
} 

