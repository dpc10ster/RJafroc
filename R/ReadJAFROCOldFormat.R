ReadJAFROCOldFormat <- function(fileName, renumber) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  # wb <- loadWorkbook(fileName)  # openxlsx
  wb <- excel_sheets(fileName)    # readxl
  sheetNames <- toupper(wb)
  
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (truthFileIndex == 0) 
    stop("TRUTH table cannot be found in the dataset.")
  truthTable <- data.frame( read_xlsx(fileName, truthFileIndex, range = cell_cols(1:3) ) )
  
  for (i in 1:3){
    truthTable[grep("^\\s*$", truthTable[ , i]), i] <- NA
  }
  
  naRows <- colSums(is.na(truthTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      truthTable <- truthTable[1:(nrow(truthTable) - max(naRows)), ]
    }
  }
  
  for (i in 1:2) {
    if (any((as.numeric(as.character(truthTable[, i]))) %% 1 != 0 )) {
      naLines <- which(!is.integer(as.numeric(as.character(truthTable[, i])))) + 1
      errorMsg <- paste0("There are non-integer values(s) for CaseID or LesionID at the line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
      stop(errorMsg)
    }
  }
  
  if (any(is.na(as.numeric(as.character(truthTable[, 3]))))) {
    naLines <- which(is.na(as.numeric(as.character(truthTable[, 3])))) + 1
    errorMsg <- paste0("There are non-numeric values(s) for weights at the line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
    stop(errorMsg)
  }
  
  caseIDColumn <- as.integer(truthTable[[1]])  # all 3 have same lenghts
  lesionIDColumn <- as.integer(truthTable[[2]])
  weightsColumn <- truthTable[[3]]
  
  normalCases <- sort(unique(caseIDColumn[lesionIDColumn == 0]))
  abnormalCases <- sort(unique(caseIDColumn[lesionIDColumn > 0]))
  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  if (anyDuplicated(cbind(caseIDColumn, lesionIDColumn))) {
    naLines <- which(duplicated(cbind(caseIDColumn, lesionIDColumn))) + 1
    errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated with previous line(s) .")
    stop(errorMsg)
  }
  
  nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  if (nlFileIndex == 0) 
    stop("FP table cannot be found in the dataset.")
  NLTable <- data.frame( read_xlsx(fileName, nlFileIndex, range = cell_cols(1:4) ) )
  
  for (i in 1:4){
    NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
  }
  
  naRows <- colSums(is.na(NLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      NLTable <- NLTable[1:(nrow(NLTable) - max(naRows)), ]
    }
  }
  
  for (i in 3:4) {
    if (any(is.na(as.numeric(as.character(NLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(NLTable[, i])))) + 1
      errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", paste(naLines, collapse = ", "), " in the FP table.")
      stop(errorMsg)
    }
  }
  
  NLReaderID <- as.character(NLTable[[1]])
  NLModalityID <- as.character(NLTable[[2]])
  NLCaseID <- NLTable[[3]]
  
  if (any(!(NLCaseID %in% caseIDColumn))) {
    naCases <- NLCaseID[which(!(NLCaseID %in% caseIDColumn))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), " in the FP table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  NLRating <- as.numeric(NLTable[[4]])
  
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (llFileIndex == 0) 
    stop("TP table cannot be found in the dataset.")
  LLTable <- data.frame( read_xlsx(fileName, llFileIndex, range = cell_cols(1:5) ) )
  
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
      errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", paste(naLines, collapse = ", "), " in the TP table.")
      stop(errorMsg)
    }
  }
  
  LLReaderID <- as.character(LLTable[[1]])
  LLModalityID <- as.character(LLTable[[2]])
  LLCaseID <- LLTable[[3]]
  LLLesionID <- LLTable[[4]]
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((caseIDColumn == LLCaseID[i]) & (lesionIDColumn == LLLesionID[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], " Reader(s) ", LLTable[i, 1], " Case(s) ", LLTable[i, 3], " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  LLRating <- as.numeric(LLTable[[5]])
  
  if (anyDuplicated(LLTable[, 1:4])) {
    naLines <- which(duplicated(LLTable[, 1:4]))
    errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
                       paste(LLTable[naLines, 4], collapse = ", "), " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  perCase  <- as.vector(table(caseIDColumn[caseIDColumn %in% abnormalCases]))
  
  weights <- array(dim = c(length(abnormalCases), max(perCase )))
  IDs <- array(dim = c(length(abnormalCases), max(perCase )))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(caseIDColumn == abnormalCases[k2])
    IDs[k2, ] <- c(sort(lesionIDColumn[k]), rep(UNINITIALIZED, max(perCase ) - length(k)))
    if (all(weightsColumn[k] == 0)) {
      weights[k2, 1:length(k)] <- 1/perCase [k2]
    } else {
      weights[k2, ] <- c(weightsColumn[k][order(lesionIDColumn[k])], rep(UNINITIALIZED, max(perCase ) - length(k)))
      sumWeight <- sum(weights[k2, weights[k2, ] != UNINITIALIZED])
      if (sumWeight != 1){
        if (sumWeight <= 1.01 && sumWeight >= 0.99){
          weights[k2, ] <- weights[k2, ] / sumWeight
        }else{
          errorMsg <- paste0("The sum of the weights for Case ", k2, " is not 1.")
          stop(errorMsg)
        }
      }
    }
  }
  
  modalityID <- as.character(sort(unique(c(NLModalityID, LLModalityID))))
  I <- length(modalityID)
  
  readerID <- as.character(sort(unique(c(NLReaderID, LLReaderID))))
  J <- length(readerID)
  
  maxNL <- 0
  for (i in modalityID) {
    for (j in readerID) {
      k <- (NLModalityID == i) & (NLReaderID == j)
      if ((sum(k) == 0)) 
        next
      maxNL <- max(maxNL, max(table(NLCaseID[k])))
    }
  }
  
  NL <- array(dim = c(I, J, K, maxNL))
  for (i in 1:I) {
    for (j in 1:J) {
      k <- (NLModalityID == modalityID[i]) & (NLReaderID == readerID[j])
      if ((sum(k) == 0)) 
        next
      caseNLTable <- table(NLCaseID[k])
      temp <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
      for (k1 in 1:length(temp)) {
        for (el in 1:caseNLTable[k1]) {
          NL[i, j, which(temp[k1] == allCases), el] <- NLRating[k][which(NLCaseID[k] == temp[k1])][el]
        }
      }
    }
  }
  
  LL <- array(dim = c(I, J, K2, max(perCase )))
  for (i in 1:I) {
    for (j in 1:J) {
      k <- (LLModalityID == modalityID[i]) & (LLReaderID == readerID[j])
      if ((sum(k) == 0)) 
        next
      caseLLTable <- table(LLCaseID[k])
      temp <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
      for (k1 in 1:length(temp)) {
        temp1 <- which(temp[k1] == abnormalCases)
        for (el in 1:caseLLTable[k1]) {
          temp2 <- which(LLLesionID[k][which(LLCaseID[k] == temp[k1])][el] == IDs[which(temp[k1] == abnormalCases), ])
          LL[i, j, temp1, temp2] <- LLRating[k][which(LLCaseID[k] == temp[k1])][el]
        }
      }
    }
  }
  
  weights[is.na(weights)] <- UNINITIALIZED
  IDs[is.na(IDs)] <- UNINITIALIZED
  NL[is.na(NL)] <- UNINITIALIZED
  LL[is.na(LL)] <- UNINITIALIZED
  
  isROI <- TRUE
  for (i in 1:I) {
    for (j in 1:J) {
      if (any(NL[i, j, 1:K1, ] == UNINITIALIZED)) {
        isROI <- FALSE
        break
      }
      temp <- LL[i, j, , ] != UNINITIALIZED
      dim(temp) <- c(K2, max(perCase ))
      if (!all(perCase  == rowSums(temp))) {
        isROI <- FALSE
        break
      }
      temp <- NL[i, j, (K1 + 1):K, ] == UNINITIALIZED
      dim(temp) <- c(K2, maxNL)
      if (!all(perCase  == rowSums(temp))) {
        isROI <- FALSE
        break
      }
    }
  }
  
  if ((max(table(caseIDColumn)) == 1) && (maxNL == 1) && (all((NL[, , (K1 + 1):K, ] == UNINITIALIZED))) && (all((NL[, , 1:K1, ] != UNINITIALIZED)))) {
    type <- "ROC"
  } else {
    if (isROI) {
      type <- "ROI"
    } else {
      type <- "FROC"
    }
  }
  
  modalityNames <- modalityID
  readerNames <- readerID
  
  if (renumber){
    modalityID <- 1:I
    readerID <- 1:J
  }
  
  names(modalityID) <- modalityNames
  names(readerID) <- readerNames
  
  truthTableStr <- array(dim = c(I, J, K, (max(lesionIDColumn)+1)))
  truthTableStr[,,1:K1,1] <- 1
  for (k2 in 1:K2) {
    # this code checks out for file frocCr013Lesions.xlsx which contains
    # 2 modalities, 3 readers, froc data,
    # 3 normal cases, 5 abnormal cases, four containing 1 lesion and 1 containing
    # 3 lesions. See RJafrocChecks/truthTableStr.xlsx, OldFormat.
    truthTableStr[,,k2+K1,(1:perCase[k2])+1] <- 1
  }
  
  name <- NA
  design <- "FCTRL" # default when using old read function
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
  
} 



