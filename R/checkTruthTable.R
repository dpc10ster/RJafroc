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
  
  for (i in 1:2) {
    if (any((as.numeric(as.character(truthTable[, i]))) %% 1 != 0 )) {
      naLines <- which(!is.integer(as.numeric(as.character(truthTable[, i])))) + 1
      errorMsg <- paste0("There are non-integer values(s) for CaseID or LesionID at the line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
      stop(errorMsg)
    }
  }
  
  if (any(is.na(as.numeric(as.character(truthTable[, 3]))))) {
    naLines <- which(is.na(as.numeric(as.character(truthTable[, 3])))) + 1
    errorMsg <- paste0("There are non-numeric values(s) for weightsColumn at the line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
    stop(errorMsg)
  }
  
  caseID <- as.integer(truthTable[[1]])  # all 3 have same lengths
  lesionIDColumn <- as.integer(truthTable[[2]])
  weightsColumn <- as.numeric(truthTable[[3]])
  readerIDColumn <- truthTable[[4]]
  
  normalCases <- sort(unique(caseID[lesionIDColumn == 0]))
  abnormalCases <- sort(unique(caseID[lesionIDColumn > 0]))
  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  # DPC: check for duplicate lesionIDColumns
  if (anyDuplicated(cbind(caseID, lesionIDColumn))) {
    naLines <- which(duplicated(cbind(caseID, lesionIDColumn))) + 1
    errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated lesionIDColumns for given caseID.")
    stop(errorMsg)
  }
  
  if (anyDuplicated(cbind(caseID, lesionIDColumn, weightsColumn))) {
    naLines <- which(duplicated(cbind(caseID, lesionIDColumn, weightsColumn))) + 1
    errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicates of previous line(s) .")
    stop(errorMsg)
  }
  
  I <- length(unlist(strsplit(truthTable$ModalityID[1],split = ",")))
  if (is.character(truthTable$ReaderID[1])) {
    J <- length(unlist(strsplit(truthTable$ReaderID[1],split = ",")))
    
    readerIDArray <- array(dim = c(J,K))
    x <- sort(trimws(unlist(strsplit(truthTable$ReaderID,split = ","))))
    for (j in 1:J) readerIDArray[j,] <- x[((j-1)*K + 1):(j*K)]
    readerIDUnique <- unique(readerIDArray)
    
    modalityIDArray <- array(dim = c(I,K))
    x <- sort(trimws(unlist(strsplit(truthTable$ModalityID,split = ","))))
    for (i in 1:I) modalityIDArray[i,] <- x[((i-1)*K + 1):(i*K)]
    modalityIDUnique <- unique(modalityIDArray)
  }
  else {
    J <- length(unique(truthTable$ReaderID))
    readerIDUnique <- unique(readerIDColumn)
    
    modalityIDArray <- array(dim = c(I,K))
    x <- sort(trimws(unlist(strsplit(truthTable$ModalityID,split = ","))))
    for (i in 1:I) modalityIDArray[i,] <- x[((i-1)*K + 1):(i*K)]
    modalityIDUnique <- unique(modalityIDArray)
  }
  
  lesionIDUnique <- unique(lesionIDColumn)
  
  lesionVector <- as.vector(table(caseID[caseID %in% abnormalCases]))
  lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector)))
  lesionID <- array(dim = c(length(abnormalCases), max(lesionVector)))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(caseID == abnormalCases[k2])
    lesionID[k2, ] <- c(sort(lesionIDColumn[k]), rep(UNINITIALIZED, max(lesionVector) - length(k)))
    if (all(weightsColumn[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/lesionVector[k2]
    } else {
      lesionWeight[k2, ] <- as.numeric(c(weightsColumn[k][order(lesionIDColumn[k])], 
                                         rep(UNINITIALIZED, max(lesionVector) - length(k))))
      sumWeight <- sum(lesionWeight[k2, lesionWeight[k2, ] != UNINITIALIZED])
      if (sumWeight != 1){
        if (sumWeight <= 1.01 && sumWeight >= 0.99){
          lesionWeight[k2, ] <- lesionWeight[k2, ] / sumWeight
        }else{
          errorMsg <- paste0("The sum of the weightsColumn of Case ", k2, " is not 1.")
          stop(errorMsg)
        }
      }
    }
  }
  
  TruthTableStr <- array(dim = c(I, J, K))
  for (i in 1:I) {
    for (j in 1:J) {
      for (t in 1:2) {
        if (!is.character(truthTable$ReaderID[1])) {
          casePresent_ijt <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerIDColumn == readerIDUnique[j]) & 
              (lesionIDColumn == lesionIDUnique[t])
          )
        } else {
          casePresent_ijt <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerIDArray[j,] == readerIDUnique[j]) & 
              (lesionIDColumn == lesionIDUnique[t])
          )
        }
        if ((sum(casePresent_ijt) == 0)) next 
        caseIDTable <- table(caseID[casePresent_ijt])
        caseIDColumns_ijt <- as.numeric(unlist(attr(caseIDTable, "dimnames")))
        for (k1 in 1:length(caseIDColumns_ijt)) {
          TruthTableStr[i, j, which(caseIDColumns_ijt[k1] == allCases)] <-1
        }
      }
    }
  }
  
  declaredParadigm <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  declaredDesign <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  
  if (!(declaredParadigm %in% c("FROC", "ROC"))) stop("Unsupported declared paradigm: must be ROC or FROC.\n")
  if (!(declaredDesign %in% c("CROSSED", "SPLIT-PLOT"))) stop("Unsupported declared study design: must be crossed or split-plot\n")
  
  if ((declaredParadigm == "ROC") && (declaredDesign == "CROSSED") && (sum(!is.na(TruthTableStr)) != I*J*K)) 
    stop("Dataset does not appear to be crossed ROC")

  if ((declaredDesign == "SPLIT-PLOT") && (sum(!is.na(TruthTableStr)) != I*K)) 
    stop("Dataset does not appear to be split plot ROC")
  
  dataType <- ""
  if (declaredDesign == "SPLIT-PLOT") {
    if (declaredParadigm == "ROC") dataType <- "SplitPlotRoc"
    if (declaredParadigm == "FROC") dataType <- "SplitPlotFroc"
  } else {
    if (declaredParadigm == "ROC") dataType <- "ROC"
    if (declaredParadigm == "FROC") dataType <- "FROC"
  }
  
  return (list(
    TruthTableStr = TruthTableStr,
    dataType = dataType,
    declaredParadigm = declaredParadigm,
    declaredDesign = declaredDesign,
    caseID = caseID,
    lesionVector = lesionVector,
    lesionID = lesionID,
    lesionWeight = lesionWeight,
    normalCases = normalCases,
    abnormalCases = abnormalCases
  ))
  
}
