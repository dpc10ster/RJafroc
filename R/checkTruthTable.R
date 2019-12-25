is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

checkTruthTable <- function (truthTable) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  for (i in 1:3){
    truthTable[grep("^\\s*$", truthTable[ , i]), i] <- NA
  }
  
  naRows <- colSums(is.na(truthTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      truthTable <- truthTable[1:(nrow(truthTable) - max(naRows)), ]
    }
  }
  
  errorMsg <- ""
  for (i in 1:3) {
    if (any(is.na(truthTable[, i]))) {
      naLines <- which(is.na(truthTable[, i])) + 1
      errorMsg <- paste0(errorMsg, 
                         "\nThere are empty cells for CaseID or LesionID at line(s) ", 
                         paste(naLines, collapse = ", "), " in the TRUTH table.")
    }
  }
  if (errorMsg != "") stop(errorMsg)
  
  for (i in 1:3)
  if (any(is.na(suppressWarnings(as.numeric(as.character(truthTable[, i])))))) {
    suppressWarnings({naLines <- which(is.na(as.numeric(as.character(truthTable[, i])))) + 1})
    if (i == 1) errorMsg <- paste0(errorMsg, "\nThere are non-integer values(s) for caseID at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
    if (i == 2) errorMsg <- paste0(errorMsg, "\nThere are non-integer values(s) for LessionID at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
    if (i == 3) errorMsg <- paste0(errorMsg, "\nThere are non-numeric values(s) for weights at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
  }
  if (errorMsg != "") stop(errorMsg)
  
  if (any(!is.wholenumber(as.numeric(truthTable[[1]])))) stop("Non-integer values in Truth worksheet column 1")
  if (any(!is.wholenumber(as.numeric(truthTable[[2]])))) stop("Non-integer values in Truth worksheet column 2")
  if (any(!is.double(as.numeric(truthTable[[3]])))) stop("Non-float values in Truth worksheet column 3")
  
  caseID <- as.integer(truthTable[[1]])  # all 3 have same lengths
  lesionID <- as.integer(truthTable[[2]])
  weights <- as.numeric(truthTable[[3]])
  readerID <- truthTable[[4]]
  L <- length(truthTable[[1]])
  for (i in 1:4) if ((length(truthTable[[i]])) != L) 
    stop("Columns of unequal height in Truth Excel worksheet")  
  
  normalCases <- sort(unique(caseID[lesionID == 0]))
  abnormalCases <- sort(unique(caseID[lesionID > 0]))
  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  # DPC: check for duplicate lesionIDs
  if (anyDuplicated(cbind(caseID, lesionID))) {
    naLines <- which(duplicated(cbind(caseID, lesionID))) + 1
    errorMsg <- paste0(errorMsg, "Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated lesionIDs for given caseID.")
  }
  if (errorMsg != "") stop(errorMsg)
  
  if (anyDuplicated(cbind(caseID, lesionID, weights))) {
    naLines <- which(duplicated(cbind(caseID, lesionID, weights))) + 1
    errorMsg <- paste0(errorMsg, "Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicates of previous line(s) .")
  }
  if (errorMsg != "") stop(errorMsg)
  
  I <- length(unlist(strsplit(truthTable$ModalityID[1],split = ",")))
  isReaderIDArray <- is.character(truthTable$ReaderID[1]) && (nchar(truthTable$ReaderID[1]) > 1)
  if (isReaderIDArray) {
    J <- length(unlist(strsplit(truthTable$ReaderID[1],split = ",")))
    
    readerIDArray <- array(dim = c(J,L))
    x <- sort(trimws(unlist(strsplit(truthTable$ReaderID,split = ","))))
    for (j in 1:J) readerIDArray[j,] <- x[((j-1)*L + 1):(j*L)]
    readerIDUnique <- unique(readerIDArray)
    
    modalityIDArray <- array(dim = c(I,L))
    x <- sort(trimws(unlist(strsplit(truthTable$ModalityID,split = ","))))
    for (i in 1:I) modalityIDArray[i,] <- x[((i-1)*L + 1):(i*L)]
    modalityIDUnique <- unique(modalityIDArray)
  }
  else {
    J <- length(unique(truthTable$ReaderID))
    readerIDUnique <- unique(readerID)
    
    modalityIDArray <- array(dim = c(I,L))
    x <- sort(trimws(unlist(strsplit(truthTable$ModalityID,split = ","))))
    for (i in 1:I) modalityIDArray[i,] <- x[((i-1)*L + 1):(i*L)]
    modalityIDUnique <- unique(modalityIDArray)
  }
  
  lesionIDUnique <- unique(lesionID)
  
  lesionVector <- as.vector(table(caseID[caseID %in% abnormalCases]))
  lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector)))
  lesionIDLabels <- array(dim = c(length(abnormalCases), max(lesionVector)))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(caseID == abnormalCases[k2])
    lesionIDLabels[k2, ] <- c(sort(lesionID[k]), 
                              rep(UNINITIALIZED, max(lesionVector) - length(k)))
    if (all(weights[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/lesionVector[k2]
    } else {
      lesionWeight[k2, ] <- as.numeric(c(weights[k][order(lesionID[k])], 
                                         rep(UNINITIALIZED, max(lesionVector) - length(k))))
      sumWeight <- sum(lesionWeight[k2, lesionWeight[k2, ] != UNINITIALIZED])
      if (sumWeight != 1){
        if (sumWeight <= 1.01 && sumWeight >= 0.99){
          lesionWeight[k2, ] <- lesionWeight[k2, ] / sumWeight
        }else{
          errorMsg <- paste0("The sum of the weights of Case ", k2, " is not 1.")
          stop(errorMsg)
        }
      }
    }
  }
  
  TruthTableStr <- array(dim = c(I, J, K, length(unique(lesionID))))
  for (i in 1:I) {
    for (j in 1:J) {
      for (el in unique(lesionID)) { 
        if (!isReaderIDArray) {
          casePresent_ij <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerID == readerIDUnique[j]) # &
              # (lesionID == unique(lesionID)[which(unique(lesionID) == el)])
          )
        } else {
          casePresent_ij <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerIDArray[j,] == readerIDUnique[j]) # &
              # (lesionID == unique(lesionID)[which(unique(lesionID) == el)])
          )
        }
        if ((sum(casePresent_ij) == 0)) next 
        caseIDTable <- table(caseID[casePresent_ij])
        caseIDs_ij <- as.numeric(unlist(attr(caseIDTable, "dimnames")))
        for (k in 1:length(caseIDs_ij))
          TruthTableStr[i, j, which(caseIDs_ij[k] == allCases), which(unique(lesionID) == el)] <-1
      }  
    }
  }
  
  paradigm <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  
  if (!(paradigm %in% c("FROC", "ROC"))) stop("Unsupported declared paradigm: must be ROC or FROC.\n")
  if (!(design %in% c("CROSSED", "SPLIT-PLOT"))) stop("Unsupported declared study design: must be crossed or split-plot\n")
  
  if ((paradigm == "ROC") && (design == "CROSSED") && (sum(!is.na(TruthTableStr)) != 2*I*J*K)) 
    stop("Dataset does not appear to be crossed ROC")
  
  if ((paradigm == "ROC") && (design == "SPLIT-PLOT") && (sum(!is.na(TruthTableStr)) != 2*I*K)) 
    stop("Dataset does not appear to be split plot ROC")
  
  dataType <- ""
  if (design == "SPLIT-PLOT") {
    if (paradigm == "ROC") dataType <- "ROC-SPLIT-PLOT"
    if (paradigm == "FROC") dataType <- "FROC-SPLIT-PLOT"
  } else {
    if (paradigm == "ROC") dataType <- "ROC"
    if (paradigm == "FROC") dataType <- "FROC"
  }
  
  return (list(
    TruthTableStr = TruthTableStr,
    dataType = dataType,
    paradigm = paradigm,
    design = design,
    caseID = caseID,
    lesionVector = lesionVector,
    lesionID = lesionID,
    lesionIDLabels = lesionIDLabels,
    lesionWeight = lesionWeight,
    normalCases = normalCases,
    abnormalCases = abnormalCases
  ))
  
}
