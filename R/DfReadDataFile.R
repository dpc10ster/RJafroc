#' Read a data file 
#' 
#' @description Read a disk file and create a dataset object from it.
#' 
#' @param fileName A string specifying the name of the file. 
#'    The file-extension must match the format specified below
#' @param format A string specifying the format of the data in the file. 
#'    It can be \code{"JAFROC"} (the default), \code{"MRMC"} or \code{"iMRMC"}. 
#'    For \code{"MRMC"} the format is determined by the data file extension 
#'    as specified in \url{http://perception.radiology.uiowa.edu/}, i.e.,  
#'    \code{.csv} or \code{.txt} or \code{.lrc}. For file extension 
#'    \code{.imrmc} the format is described in \url{https://code.google.com/p/imrmc/}.
#' @param newExcelFileFormat This argument only applies to the \code{"JAFROC"} format. 
#'    The default is \code{FALSE}. if \code{TRUE} the function accommodates 3 
#'    additional columns
#'    in the \code{Truth} worksheet. If \code{FALSE}, the original function, as in version 
#'    1.2.0 is used, and the three extra columns, if present, throws an error.  
#' @param delimiter The string delimiter to be used for the \code{"MRMC"} 
#'    format ("," is the default), see \url{http://perception.radiology.uiowa.edu/}.
#'    This parameter is not used when reading \code{"JAFROC"} 
#'    or \code{"iMRMC"} data files.
#' @param sequentialNames A logical variable: if \code{TRUE}, consecutive integers 
#'    (starting from 1) will be used as the 
#'    treatment and reader IDs (i.e., names). Otherwise, treatment 
#'    and reader IDs in the original data file will be used.
#' 
#' @return A dataset with the structure specified in \code{\link{RJafroc-package}}.
#' 
#' @examples
#' fileName <- system.file("extdata", "toyFiles/ROC/rocCr.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' x <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#'
#' {
#' fileName <- system.file("extdata", "toyFiles/FROC/frocCr.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' x <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#' }
#' 
#' \donttest{
#' fileName <- system.file("extdata", "RocData.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' RocDataXlsx <- DfReadDataFile(fileName)
#' 
#' fileName <- system.file("extdata", "RocData.csv", 
#' package = "RJafroc", mustWork = TRUE)
#' RocDataCsv<- DfReadDataFile(fileName, format = "MRMC")
#' 
#' fileName <- system.file("extdata", "RocData.imrmc", 
#' package = "RJafroc", mustWork = TRUE)
#' RocDataImrmc<- DfReadDataFile(fileName, format = "iMRMC")
#' 
#' fileName <- system.file("extdata", "FrocData.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' FrocDataXlsx <- DfReadDataFile(fileName, sequentialNames = TRUE)
#' }
#' 
#' @importFrom tools file_ext
#' @importFrom stringr str_trim str_length
#' @export

DfReadDataFile <- function (fileName, format = "JAFROC", 
                            newExcelFileFormat = FALSE, delimiter = ",", 
                            sequentialNames = FALSE) 
{
  
  if (format == "JAFROC") {
    # handle JAFROC format Excel files
    if (!(file_ext(fileName) %in% c("xls", "xlsx"))) 
      stop("The extension of JAFROC data file must be \"*.xls\" or \"*.xlsx\" ")
    if (!newExcelFileFormat) 
      return((ReadJAFROCOldFormat(fileName, sequentialNames))) 
    else 
      return(ReadJAFROCNewFormat(fileName, sequentialNames))
  } else {
    # handle non-JAFROC format text files
    if (format == "iMRMC") {
      return(ReadImrmc(fileName, sequentialNames))
    } else if (format == "MRMC") {
      if (file_ext(fileName) == "lrc") {
        return(ReadLrc(fileName, sequentialNames))
      } else {
        return(ReadOrDbmMrmc(fileName, delimiter, sequentialNames))
      }
    } else {
      errMsg <- sprintf("%s is not an available file format.", format)
      stop(errMsg)
    }
  }
} 


###################### NEW FORMAT READ FUNCTION  ############################
ReadJAFROCNewFormat <- function(fileName, sequentialNames) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  wb <- loadWorkbook(fileName)
  sheetNames <- toupper(names(wb))
  
  ########################## CHECK TRUTH TABLE ##############################
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is, i.e., 1st, 2nd or 3rd tab position in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (length(truthFileIndex) == 0) stop("TRUTH table worksheet cannot be found in the Excel file.")
  truthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:6)
  truth <- checkTruthTable(truthTable) 
  
  truthTableStr <- truth$truthTableStr
  truthCaseID <-  truth$caseID # these need not be unique for FROC datasets
  dataType <- truth$dataType
  design <- truth$design
  lesionWeight <- truth$lesionWeight
  lesionVector <- truth$lesionVector
  lesionID <- truth$lesionID
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
  
  # grep("^\\s*$", "") = 1
  # grep("^\\s*$", c("","")) = 1 2 etc
  # following replaces empty cells with NAs
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
      errorMsg <- paste0("There are unavailable cell(s) at the line(s) 
                         ", paste(naLines, collapse = ", "), " in the FP table.")
      stop(errorMsg)
    }
  }
  
  # Column means the entire column is included in vector
  NLReaderIDColumn <- as.character(NLTable[[1]])
  NLModalityIDColumn <- as.character(NLTable[[2]])
  NLCaseIDColumn <- NLTable[[3]]
  NLRatingColumn <- NLTable[[4]]
  
  if (any(!(NLCaseIDColumn %in% truthCaseID))) {
    naCases <- NLCaseIDColumn[which(!(NLCaseIDColumn %in% truthCaseID))]
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
  
  LLReaderIDColumn <- as.character(LLTable[[1]])
  LLModalityIDColumn <- as.character(LLTable[[2]])
  LLCaseIDColumn <- LLTable[[3]]
  LLLesionIDColumn <- LLTable[[4]]
  LLRatingColumn <- LLTable[[5]]
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseIDColumn[i]) & (lesionIDCol == LLLesionIDColumn[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], 
                         " Reader(s) ", LLTable[i, 1], 
                         " Case(s) ", LLTable[i, 3], 
                         " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  if (any(LLCaseIDColumn %in% normalCases)) {
    errorMsg <- paste0("Normal case(s) found in TP table.")
    stop(errorMsg)
  }
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseIDColumn[i]) & (lesionIDCol == LLLesionIDColumn[i]))
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
  
  modalityIDUnique <- as.character(sort(unique(c(NLModalityIDColumn, LLModalityIDColumn))))
  I <- length(modalityIDUnique)
  readerIDUnique <- as.character(sort(unique(c(NLReaderIDColumn, LLReaderIDColumn))))
  J <- length(readerIDUnique)
  
  
  ############################ CALC NL ARRAY ################################
  maxNL <- 0
  for (i in modalityIDUnique) {
    for (j in readerIDUnique) {
      casePresent_ij <- (NLModalityIDColumn == i) & (NLReaderIDColumn == j)
      if ((sum(casePresent_ij) == 0)) 
        next
      maxNL <- max(maxNL, max(table(NLCaseIDColumn[casePresent_ij])))
    }
  }
  
  NL <- array(dim = c(I, J, K, maxNL))
  for (i in 1:I) {
    for (j in 1:J) {
      casePresent_ij <- (NLModalityIDColumn == modalityIDUnique[i]) & 
        (NLReaderIDColumn == readerIDUnique[j])
      if ((sum(casePresent_ij) == 0)) next 
      caseNLTable <- table(NLCaseIDColumn[casePresent_ij]) 
      caseIDs_ij <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
      for (k1 in 1:length(caseIDs_ij)) {
        if (caseIDs_ij[k1] %in% normalCases) {
          if (truthTableStr[i,j,which(caseIDs_ij[k1] == allCases), 1] != 1) # ,1: for normal cases, the fourth dimension is lesionIDCol = 0, which occupies the first position
            stop("incorrect truthCaseID in NL/FP worksheet")
        } else if (caseIDs_ij[k1] %in% abnormalCases) {
          if (truthTableStr[i,j,which(caseIDs_ij[k1] == allCases), 2] != 1) # ,1: for normal cases, the fourth dimension is lesionIDCol = 0, which occupies the first position
            stop("incorrect truthCaseID in NL/FP worksheet")
        } else stop("Should never get here")
        for (el in 1:caseNLTable[k1]) {
          NL[i, j, which(caseIDs_ij[k1] == allCases), el] <- 
            NLRatingColumn[casePresent_ij][which(NLCaseIDColumn[casePresent_ij] == 
                                                   caseIDs_ij[k1])][el]
        }
      }
    }
  }
  NL[is.na(NL)] <- UNINITIALIZED
  
  ############################ CALC LL ARRAY ################################
  LL <- array(dim = c(I, J, K2, max(lesionVector)))
  for (i in 1:I) {
    for (j in 1:J) {
      casePresent_ij <- (LLModalityIDColumn == modalityIDUnique[i]) & 
        (LLReaderIDColumn == readerIDUnique[j])
      if ((sum(casePresent_ij) == 0)) next
      caseLLTable <- table(LLCaseIDColumn[casePresent_ij])
      caseIDs_ij <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
      for (k2 in 1:length(caseIDs_ij)) {
        k2p <- which(caseIDs_ij[k2] == abnormalCases)
        x2 <- which(LLCaseIDColumn[casePresent_ij] == caseIDs_ij[k2])
        x3 <- lesionID[k2p, ]
        for (el in 1:caseLLTable[k2]) {
          elp <- which(LLLesionIDColumn[casePresent_ij][x2][el] == x3)
          if (truthTableStr[i,j,which(caseIDs_ij[k2] == allCases),elp+1] != 1) # elp+1 because lesionIDCol = 0 occupies first position, etc.
            stop("incorrect truthCaseID in LL/TP worksheet")
          LL[i, j, k2p, elp] <- LLRatingColumn[casePresent_ij][x2][el]
        }
      }
    }
  }
  LL[is.na(LL)] <- UNINITIALIZED
  lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
  lesionIDCol[is.na(lesionIDCol)] <- UNINITIALIZED
  
  if (dataType == "ROC" && design == "CROSSED") {
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
  
  names(modalityIDUnique) <- modalityNames
  names(readerIDUnique) <- readerNames
  
  return(list(
    NL = NL, 
    LL = LL, 
    lesionVector = lesionVector,
    lesionID = lesionID, 
    lesionWeight = lesionWeight, 
    dataType = dataType, 
    modalityID = modalityIDUnique, 
    readerID = readerIDUnique,
    # these are the additional members added 12/27/2019 by DPC
    # makes it easier to correlate the NL and LL values with those in the Excel file
    design = design,
    normalCases = normalCases,
    abnormalCases = abnormalCases,
    truthTableStr = truthTableStr))
} 


# is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

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
  lesionIDCol <- as.integer(truthTable[[2]])
  weights <- as.numeric(truthTable[[3]])
  readerID <- truthTable[[4]]
  L <- length(truthTable[[1]])
  for (i in 1:4) if ((length(truthTable[[i]])) != L) 
    stop("Columns of unequal height in Truth Excel worksheet")  
  
  normalCases <- sort(unique(caseID[lesionIDCol == 0]))
  abnormalCases <- sort(unique(caseID[lesionIDCol > 0]))
  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  # DPC: check for duplicate lesionIDs
  if (anyDuplicated(cbind(caseID, lesionIDCol))) {
    naLines <- which(duplicated(cbind(caseID, lesionIDCol))) + 1
    errorMsg <- paste0(errorMsg, "Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated lesionIDs for given caseID.")
  }
  if (errorMsg != "") stop(errorMsg)
  
  if (anyDuplicated(cbind(caseID, lesionIDCol, weights))) {
    naLines <- which(duplicated(cbind(caseID, lesionIDCol, weights))) + 1
    errorMsg <- paste0(errorMsg, "Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicates of previous line(s) .")
  }
  if (errorMsg != "") stop(errorMsg)
  
  lesionIDUnique <- sort(unique(lesionIDCol)) # fix to bug when abnormal cases occur first
  
  lesionVector <- as.vector(table(caseID[caseID %in% abnormalCases]))
  lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector)))
  lesionID <- array(dim = c(length(abnormalCases), max(lesionVector)))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(caseID == abnormalCases[k2])
    lesionID[k2, ] <- c(sort(lesionIDCol[k]), 
                        rep(UNINITIALIZED, max(lesionVector) - length(k)))
    if (all(weights[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/lesionVector[k2]
    } else {
      lesionWeight[k2, ] <- as.numeric(c(weights[k][order(lesionIDCol[k])], 
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
  
  I <- length(unlist(strsplit(truthTable$ModalityID[1],split = ",")))
  isReaderIDArray <- is.character(truthTable$ReaderID[1]) && (nchar(truthTable$ReaderID[1]) > 1)
  
  if (isReaderIDArray) {
    J <- length(unlist(strsplit(truthTable$ReaderID[1],split = ",")))
    
    readerIDArray <- array(dim = c(J,L))
    x <- sort(trimws(unlist(strsplit(truthTable$ReaderID,split = ","))))
    for (j in 1:J) readerIDArray[j,] <- x[((j-1)*L + 1):(j*L)]
    readerIDUnique <- unique(readerIDArray)
  }
  else {
    J <- length(unique(truthTable$ReaderID))
    readerIDUnique <- unique(readerID)
    # following fixed single reader datasets, ROC and FROC
    readerIDArray <- array(dim = c(J,L))
    for (j in 1:J) readerIDArray[j,] <- readerIDUnique[j]
  }
  
  # assuming > 1 modalities for each reader: balanced data
  # following code should break with single modality dataset
  # but it does not; go figure
  modalityIDArray <- array(dim = c(I,L))
  x <- sort(trimws(unlist(strsplit(truthTable$ModalityID,split = ","))))
  for (i in 1:I) modalityIDArray[i,] <- x[((i-1)*L + 1):(i*L)]
  modalityIDUnique <- unique(modalityIDArray)
  
  truthTableStr <- array(dim = c(I, J, K, length(lesionIDUnique)))
  for (i in 1:I) {
    for (j in 1:J) {
      for (el in lesionIDUnique) { 
        el1 <- which(lesionIDUnique == el)
        if (!isReaderIDArray) {
          casePresent_ij <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerID == readerIDUnique[j]) &
              (lesionIDCol == lesionIDUnique[el1])
          )
        } else {
          casePresent_ij <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerIDArray[j,] == readerIDUnique[j]) &
              (lesionIDCol == lesionIDUnique[el1])
          )
        }
        if ((sum(casePresent_ij) == 0)) next 
        caseIDTable <- table(caseID[casePresent_ij])
        caseIDs_ij <- as.numeric(unlist(attr(caseIDTable, "dimnames")))
        for (k in 1:length(caseIDs_ij))
          truthTableStr[i, j, which(caseIDs_ij[k] == allCases), el1] <-1
      }  
    }
  }
  
  dataType <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  
  if (!(dataType %in% c("FROC", "ROC"))) stop("Unsupported declared dataType: must be ROC or FROC.\n")
  if (!(design %in% c("CROSSED", "SPLIT-PLOT"))) stop("Unsupported declared study design: must be crossed or split-plot\n")
  
  if (dataType == "ROC") {
    if ((design == "CROSSED") && (sum(!is.na(truthTableStr)) != 
                                  L*length(readerIDArray[,1])*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be crossed ROC")
    
    if ((design == "SPLIT-PLOT") && (sum(!is.na(truthTableStr)) != L*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be split plot ROC")
  }
  
  if (dataType == "FROC") {
    if ((design == "CROSSED") && (sum(!is.na(truthTableStr)) != 
                                  L*length(readerIDArray[,1])*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be crossed ROC")
    
    if ((design == "SPLIT-PLOT") && (sum(!is.na(truthTableStr)) != L*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be split plot ROC")
  }
  
  return (list(
    truthTableStr = truthTableStr,
    dataType = dataType,
    design = design,
    caseID = caseID,
    lesionVector = lesionVector,
    lesionIDCol = lesionIDCol,
    lesionID = lesionID,
    lesionWeight = lesionWeight,
    normalCases = normalCases,
    abnormalCases = abnormalCases
  ))
  
}


###################### ORIGINAL OLD FORMAT READ FUNCTION  ############################
# ReadJAFROCOldFormat <- function(fileName, sequentialNames) {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   wb <- loadWorkbook(fileName)
#   sheetNames <- toupper(names(wb))
#   
#   truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
#   if (truthFileIndex == 0) 
#     stop("TRUTH table cannot be found in the dataset.")
#   truthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:3)
#   
#   # added following lines to check if this appears to be a new Format dataset as this code should not be called
#   # if there are additional columns in the Truth sheet.
#   # suppressWarnings (
#   # {truthTable2 <- read.xlsx(fileName, truthFileIndex, cols = 4:6)
#   # if (sum(!is.na(truthTable2[[1]])) == sum(!is.na(truthTable[[1]])))
#   #   stop("This appears to be a new format Excel file as it has additional columns in the Truth Sheet")
#   # })
#   
#   for (i in 1:3){
#     truthTable[grep("^\\s*$", truthTable[ , i]), i] <- NA
#   }
#   
#   naRows <- colSums(is.na(truthTable))
#   if (max(naRows) > 0) {
#     if (max(naRows) == min(naRows)) {
#       truthTable <- truthTable[1:(nrow(truthTable) - max(naRows)), ]
#     }
#   }
#   
#   for (i in 1:2) {
#     if (any((as.numeric(as.character(truthTable[, i]))) %% 1 != 0 )) {
#       naLines <- which(!is.integer(as.numeric(as.character(truthTable[, i])))) + 1
#       errorMsg <- paste0("There are non-integer values(s) for CaseID or LesionID at the line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
#       stop(errorMsg)
#     }
#   }
#   
#   if (any(is.na(as.numeric(as.character(truthTable[, 3]))))) {
#     naLines <- which(is.na(as.numeric(as.character(truthTable[, 3])))) + 1
#     errorMsg <- paste0("There are non-numeric values(s) for weights at the line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
#     stop(errorMsg)
#   }
#   
#   caseID <- as.integer(truthTable[[1]])  # all 3 have same lengths
#   lesionID <- as.integer(truthTable[[2]])
#   weights <- truthTable[[3]]
#   
#   normalCases <- sort(unique(caseID[lesionID == 0]))
#   abnormalCases <- sort(unique(caseID[lesionID > 0]))
#   allCases <- c(normalCases, abnormalCases)
#   K1 <- length(normalCases)
#   K2 <- length(abnormalCases)
#   K <- (K1 + K2)
#   
#   if (anyDuplicated(cbind(caseID, lesionID, weights))) {
#     naLines <- which(duplicated(cbind(caseID, lesionID, weights))) + 1
#     errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated with previous line(s) .")
#     stop(errorMsg)
#   }
#   
#   if (anyDuplicated(cbind(caseID, lesionID))) {
#     naLines <- which(duplicated(cbind(caseID, lesionID))) + 1
#     errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated lesionIDs for given caseID.")
#     stop(errorMsg)
#   }
#   
#   nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
#   if (nlFileIndex == 0) 
#     stop("FP table cannot be found in the dataset.")
#   NLTable <- read.xlsx(fileName, nlFileIndex, cols = 1:4)
#   
#   for (i in 1:4){
#     NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
#   }
#   
#   naRows <- colSums(is.na(NLTable))
#   if (max(naRows) > 0) {
#     if (max(naRows) == min(naRows)) {
#       NLTable <- NLTable[1:(nrow(NLTable) - max(naRows)), ]
#     }
#   }
#   
#   for (i in 3:4) {
#     if (any(is.na(as.numeric(as.character(NLTable[, i]))))) {
#       naLines <- which(is.na(as.numeric(as.character(NLTable[, i])))) + 1
#       errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", paste(naLines, collapse = ", "), " in the FP table.")
#       stop(errorMsg)
#     }
#   }
#   
#   NLReaderID <- as.character(NLTable[[1]])
#   NLModalityID <- as.character(NLTable[[2]])
#   NLCaseID <- NLTable[[3]]
#   NLRating <- NLTable[[4]]
#   
#   if (any(!(NLCaseID %in% caseID))) {
#     naCases <- NLCaseID[which(!(NLCaseID %in% caseID))]
#     errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), " in the FP table cannot be found in TRUTH table.")
#     stop(errorMsg)
#   }
#   
#   llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
#   if (llFileIndex == 0) stop("TP table cannot be found in the dataset.")
#   LLTable <- read.xlsx(fileName, llFileIndex, cols = 1:5)
#   
#   for (i in 1:5){
#     LLTable[grep("^\\s*$", LLTable[ , i]), i] <- NA
#   }
#   
#   naRows <- colSums(is.na(LLTable))
#   if (max(naRows) > 0) {
#     if (max(naRows) == min(naRows)) {
#       LLTable <- LLTable[1:(nrow(LLTable) - max(naRows)), ]
#     }
#   }
#   
#   for (i in 3:5) {
#     if (any(is.na(as.numeric(as.character(LLTable[, i]))))) {
#       naLines <- which(is.na(as.numeric(as.character(LLTable[, i])))) + 1
#       errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", paste(naLines, collapse = ", "), " in the TP table.")
#       stop(errorMsg)
#     }
#   }
#   
#   LLReaderID <- as.character(LLTable[[1]])
#   LLModalityID <- as.character(LLTable[[2]])
#   LLCaseID <- LLTable[[3]]
#   LLLesionID <- LLTable[[4]]
#   LLRating <- LLTable[[5]]
#   
#   for (i in 1:nrow(LLTable)) {
#     lineNum <- which((caseID == LLCaseID[i]) & (lesionID == LLLesionID[i]))
#     if (!length(lineNum)) {
#       errorMsg <- paste0("Modality ", LLTable[i, 2], " Reader(s) ", LLTable[i, 1], " Case(s) ", LLTable[i, 3], " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
#       stop(errorMsg)
#     }
#   }
#   
#   if (anyDuplicated(LLTable[, 1:4])) {
#     naLines <- which(duplicated(LLTable[, 1:4]))
#     errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
#                        paste(LLTable[naLines, 4], collapse = ", "), " have multiple ratings in TP table .")
#     stop(errorMsg)
#   }
#   
#   lesionVector <- as.vector(table(caseID[caseID %in% abnormalCases]))
#   lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector)))
#   lesionID  <- array(dim = c(length(abnormalCases), max(lesionVector)))
#   
#   for (k2 in 1:length(abnormalCases)) {
#     casePresent_ij <- which(caseID == abnormalCases[k2])
#     lesionID [k2, ] <- c(sort(lesionID[casePresent_ij]), 
#                               rep(UNINITIALIZED, max(lesionVector) - length(casePresent_ij)))
#     if (all(weights[casePresent_ij] == 0)) {
#       lesionWeight[k2, 1:length(casePresent_ij)] <- 1/lesionVector[k2]
#     } else {
#       lesionWeight[k2, ] <- as.numeric(c(weights[casePresent_ij][order(lesionID[casePresent_ij])], 
#                                          rep(UNINITIALIZED, max(lesionVector) - length(casePresent_ij))))
#       sumWeight <- sum(lesionWeight[k2, lesionWeight[k2, ] != UNINITIALIZED])
#       if (sumWeight != 1){
#         if (sumWeight <= 1.01 && sumWeight >= 0.99){
#           lesionWeight[k2, ] <- lesionWeight[k2, ] / sumWeight
#         }else{
#           errorMsg <- paste0("The sum of the weights of Case ", k2, " is not 1.")
#           stop(errorMsg)
#         }
#       }
#     }
#   }
#   
#   modalityID <- as.character(sort(unique(c(NLModalityID, LLModalityID))));I <- length(modalityID)
#   readerID <- as.character(sort(unique(c(NLReaderID, LLReaderID))));J <- length(readerID)
#   
#   maxNL <- 0
#   for (i in modalityID) {
#     for (j in readerID) {
#       casePresent_ij <- (NLModalityID == i) & (NLReaderID == j)
#       if ((sum(casePresent_ij) == 0)) next
#       maxNL <- max(maxNL, max(table(NLCaseID[casePresent_ij])))
#     }
#   }
# 
#   NL <- array(dim = c(I, J, K, maxNL))
#   for (i in 1:I) {
#     for (j in 1:J) {
#       casePresent_ij <- (NLModalityID == modalityID[i]) & (NLReaderID == readerID[j])
#       if ((sum(casePresent_ij) == 0)) next
#       caseNLTable <- table(NLCaseID[casePresent_ij])
#       IDs <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
#       for (k1 in 1:length(IDs)) {
#         for (el in 1:caseNLTable[k1]) {
#           NL[i, j, which(IDs[k1] == allCases), el] <- 
#             NLRating[casePresent_ij][which(NLCaseID[casePresent_ij] == IDs[k1])][el]
#         }
#       }
#     }
#   }
#   NL[is.na(NL)] <- UNINITIALIZED
#   
#   LL <- array(dim = c(I, J, K2, max(lesionVector)))
#   for (i in 1:I) {
#     for (j in 1:J) {
#       casePresent_ij <- (LLModalityID == modalityID[i]) & (LLReaderID == readerID[j])
#       if ((sum(casePresent_ij) == 0)) next
#       caseLLTable <- table(LLCaseID[casePresent_ij])
#       caseIDs_ij <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
#       for (k2 in 1:length(caseIDs_ij)) {
#         for (el in 1:caseLLTable[k2]) {
#           k2p <- which(caseIDs_ij[k2] == abnormalCases)
#           x2 <- which(LLCaseID[casePresent_ij] == caseIDs_ij[k2])
#           x3 <- lesionID [k2p, ]
#           elp <- which(LLLesionID[casePresent_ij][x2][el] == x3)
#           LL[i, j, k2p, elp] <- LLRating[casePresent_ij][x2][el]
#         }
#       }
#     }
#   }
#   LL[is.na(LL)] <- UNINITIALIZED
#   lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
#   lesionID [is.na(lesionID )] <- UNINITIALIZED
#   
#   isROI <- TRUE
#   for (i in 1:I) {
#     for (j in 1:J) {
#       if (any(NL[i, j, 1:K1, ] == UNINITIALIZED)) {
#         isROI <- FALSE
#         break
#       }
#       temp <- LL[i, j, , ] != UNINITIALIZED
#       dim(temp) <- c(K2, max(lesionVector))
#       if (!all(lesionVector == rowSums(temp))) {
#         isROI <- FALSE
#         break
#       }
#       temp <- NL[i, j, (K1 + 1):K, ] == UNINITIALIZED
#       dim(temp) <- c(K2, maxNL)
#       if (!all(lesionVector == rowSums(temp))) {
#         isROI <- FALSE
#         break
#       }
#     }
#   }
#   
#   if ((max(table(caseID)) == 1) && (maxNL == 1) 
#       && (all((NL[, , (K1 + 1):K, ] == UNINITIALIZED))) 
#       && (all((NL[, , 1:K1, ] != UNINITIALIZED)))) {
#     fileType <- "ROC"
#   } else {
#     if (isROI) {
#       fileType <- "ROI"
#     } else {
#       fileType <- "FROC"
#     }
#   }
#   
#   modalityNames <- modalityID
#   readerNames <- readerID
#   
#   if (sequentialNames){
#     modalityID <- 1:I
#     readerID <- 1:J
#   }
#   
#   names(modalityID) <- modalityNames
#   names(readerID) <- readerNames
#   
#   return(list(NL = NL, 
#               LL = LL, 
#               lesionVector = lesionVector, 
#               lesionID = lesionID , 
#               lesionWeight = lesionWeight, 
#               dataType = fileType, 
#               modalityID = modalityID, 
#               readerID = readerID))
# } 


ReadJAFROCOldFormat <- function(fileName, renumber) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  wb <- loadWorkbook(fileName)
  sheetNames <- toupper(names(wb))
  
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (truthFileIndex == 0) 
    stop("TRUTH table cannot be found in the dataset.")
  truthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:3)
  
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
  
  caseID <- as.integer(truthTable[[1]])  # all 3 have same lenghts
  lesionID <- as.integer(truthTable[[2]])
  weights <- truthTable[[3]]
  
  normalCases <- sort(unique(caseID[lesionID == 0]))
  abnormalCases <- sort(unique(caseID[lesionID > 0]))
  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  if (anyDuplicated(cbind(caseID, lesionID))) {
    naLines <- which(duplicated(cbind(caseID, lesionID))) + 1
    errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated with previous line(s) .")
    stop(errorMsg)
  }
  
  nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  if (nlFileIndex == 0) 
    stop("FP table cannot be found in the dataset.")
  NLTable <- read.xlsx(fileName, nlFileIndex, cols = 1:4)
  
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
  
  if (any(!(NLCaseID %in% caseID))) {
    naCases <- NLCaseID[which(!(NLCaseID %in% caseID))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), " in the FP table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  NLRating <- NLTable[[4]]
  
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (llFileIndex == 0) 
    stop("TP table cannot be found in the dataset.")
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
      errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", paste(naLines, collapse = ", "), " in the TP table.")
      stop(errorMsg)
    }
  }
  
  LLReaderID <- as.character(LLTable[[1]])
  
  LLModalityID <- as.character(LLTable[[2]])
  
  LLCaseID <- LLTable[[3]]
  LLLesionID <- LLTable[[4]]
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((caseID == LLCaseID[i]) & (lesionID == LLLesionID[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], " Reader(s) ", LLTable[i, 1], " Case(s) ", LLTable[i, 3], " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  LLRating <- LLTable[[5]]
  
  if (anyDuplicated(LLTable[, 1:4])) {
    naLines <- which(duplicated(LLTable[, 1:4]))
    errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
                       paste(LLTable[naLines, 4], collapse = ", "), " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  lesionVector  <- as.vector(table(caseID[caseID %in% abnormalCases]))
  
  lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector )))
  lesionIDTable <- array(dim = c(length(abnormalCases), max(lesionVector )))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(caseID == abnormalCases[k2])
    lesionIDTable[k2, ] <- c(sort(lesionID[k]), rep(UNINITIALIZED, max(lesionVector ) - length(k)))
    if (all(weights[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/lesionVector [k2]
    } else {
      lesionWeight[k2, ] <- c(weights[k][order(lesionID[k])], rep(UNINITIALIZED, max(lesionVector ) - length(k)))
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
      IDs <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
      for (k1 in 1:length(IDs)) {
        for (el in 1:caseNLTable[k1]) {
          NL[i, j, which(IDs[k1] == allCases), el] <- NLRating[k][which(NLCaseID[k] == IDs[k1])][el]
        }
      }
    }
  }
  
  LL <- array(dim = c(I, J, K2, max(lesionVector )))
  for (i in 1:I) {
    for (j in 1:J) {
      k <- (LLModalityID == modalityID[i]) & (LLReaderID == readerID[j])
      if ((sum(k) == 0)) 
        next
      caseLLTable <- table(LLCaseID[k])
      IDs <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
      for (k1 in 1:length(IDs)) {
        for (el in 1:caseLLTable[k1]) {
          LL[i, j, which(IDs[k1] == abnormalCases), which(LLLesionID[k][which(LLCaseID[k] == IDs[k1])][el] == lesionIDTable[which(IDs[k1] == abnormalCases), ])] <- LLRating[k][which(LLCaseID[k] == IDs[k1])][el]
        }
      }
    }
  }
  
  lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
  lesionIDTable[is.na(lesionIDTable)] <- UNINITIALIZED
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
      dim(temp) <- c(K2, max(lesionVector ))
      if (!all(lesionVector  == rowSums(temp))) {
        isROI <- FALSE
        break
      }
      temp <- NL[i, j, (K1 + 1):K, ] == UNINITIALIZED
      dim(temp) <- c(K2, maxNL)
      if (!all(lesionVector  == rowSums(temp))) {
        isROI <- FALSE
        break
      }
    }
  }
  
  if ((max(table(caseID)) == 1) && (maxNL == 1) && (all((NL[, , (K1 + 1):K, ] == UNINITIALIZED))) && (all((NL[, , 1:K1, ] != UNINITIALIZED)))) {
    fileType <- "ROC"
  } else {
    if (isROI) {
      fileType <- "ROI"
    } else {
      fileType <- "FROC"
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
  
  truthTableStr <- array(dim = c(I, J, K, length(unique(lesionID))))
  truthTableStr[,,1:K1,1] <- 1
  for (k2 in 1:K2) {
    truthTableStr[,,k2+K1,(1:lesionVector[k2])+1] <- 1
  }
  
  return(list(
    NL = NL, 
    LL = LL, 
    lesionVector  = lesionVector , 
    lesionID = lesionIDTable, 
    lesionWeight = lesionWeight, 
    dataType = fileType, 
    modalityID = modalityID, 
    readerID = readerID,
    # these are the additional members added 12/27/2019 by DPC
    # makes it easier to correlate the NL and LL values with those in the Excel file
    design = "CROSSED", # default when using old read function
    normalCases = normalCases,
    abnormalCases = abnormalCases,
    truthTableStr = truthTableStr))
} 


# checkNLTable <- function (retTruth, NLTable) 
# {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   
#   I <-  dim(retTruth$studyDesign)[1]
#   J <-  dim(retTruth$studyDesign)[2]
#   K <- dim(retTruth$studyDesign)[3]
#   
#   NL <- array(dim=c(I,J,K))
#   
#   
#   # check for empty cells in NL/FP sheet
#   for (i in 1:4) {
#     if (any(is.na(NLTable[, i]))) stop("check for empty cells in NL/FP sheet")
#   }
#   
#   # for (i in 1:I){
#   #   for (j in 1:J) {
#   #     for (k in (1:K)) {
#   #       if (studyDesign[i,j,k,j]) stop("hi")
#   #     }
#   #   }
#   # }
#   
#   NLReaderID <- as.character(NLTable[[1]])
#   NLModalityID <- as.character(NLTable[[2]])
#   NLRating <- NLTable$NL_Rating
#   
#   NL <- array(dim = c(I,J,K,J))
#   
#   # check that CaseIDs in FP worksheet actually occur in the TRUTH sheet
#   # and if not stop with the line number of offending cells
#   
#   Truth_CaseIDColumn <- retTruth$Truth_CaseIDColumn
#   NLCaseID <- NLTable[[3]]
#   if (any(!(NLCaseID %in% Truth_CaseIDColumn))) {
#     naCases <- NLCaseID[which(!(NLCaseID %in% Truth_CaseIDColumn))]
#     errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), 
#                        " in the FP/NL table cannot be found in TRUTH table.")
#     stop(errorMsg)
#   }
#   NLRating <- NLTable[[4]]
#   
#   return (list(
#     NLReaderID = NLReaderID,
#     NLModalityID = NLModalityID,
#     NLCaseID = NLCaseID,
#     NLRating = NLRating
#   ))
#   
# }




# checkLLTable <- function (retTruth, retNL, LLTable) 
# {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   
#   # check that CaseID, LesionID and Ratings are all numeric values
#   for (i in 3:5) {
#     if (any(is.na(as.numeric(as.character(LLTable[, i]))))) {
#       naLines <- which(is.na(as.numeric(as.character(LLTable[, i])))) + 1
#       errorMsg <- paste0("There are missing cell(s) at line(s) ", 
#                          paste(naLines, collapse = ", "), " in the TP/LL table.")
#       stop(errorMsg)
#     }
#   }
#   
#   LLReaderID <- as.character(LLTable[[1]])
#   LLModalityID <- as.character(LLTable[[2]])
#   Truth_CaseIDColumn <- retTruth$Truth_CaseIDColumn
#   Truth_LesionIDColumn <- retTruth$Truth_LesionIDColumn
#   
#   # check that CaseID, LesionID do occur in Truth sheet
#   errorMsg <- ""
#   LLCaseID <- LLTable[[3]]
#   LLLesionID <- LLTable[[4]]
#   if (!all(LLCaseID %in% retTruth$abnormalCases)) stop("Check LL/TP sheet for non-diseased cases\n")
#   
#   for (i in 1:nrow(LLTable)) {
#     lineNum <- which((Truth_CaseIDColumn == LLCaseID[i]) & (Truth_LesionIDColumn == LLLesionID[i]))
#     if (!length(lineNum)) {
#       errorMsg <- paste0(errorMsg, "\nLL sheet errors:\nModality ", LLTable[i, 2], "\nReader ", 
#                          LLTable[i, 1], "\nCase ", LLTable[i, 3], 
#                          "\nLesion ", LLTable[i, 4], "\ncannot be found in TRUTH table.\n\n")
#     }
#   }
#   if (errorMsg != "") stop(errorMsg)
#   
#   LLRating <- LLTable[[5]]
#   LLRating[is.na(LLRating)] <- UNINITIALIZED
#   
#   if (anyDuplicated(LLTable[, 1:4])) {
#     naLines <- which(duplicated(LLTable[, 1:4]))
#     errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), 
#                        " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), 
#                        " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
#                        paste(LLTable[naLines, 4], collapse = ", "), 
#                        " have multiple ratings in TP table .")
#     stop(errorMsg)
#   }
#   
#   return (list(
#     LLReaderID = LLReaderID,
#     LLModalityID = LLModalityID,
#     LLCaseID = LLCaseID,
#     LLLesionID = LLLesionID,
#     LLRating = LLRating
#   ))
#   
# }




# still working on this
# isCrossedRocDataset <- function(TruthTable, NLTable, LLTable)
# {
#   K <- length(TruthTable[[1]])  # total number of cases read by each reader
#   K1 <- length(TruthTable[[2]][TruthTable[[2]] == 0]) 
#   K2 <- length(TruthTable[[2]][TruthTable[[2]] > 0]) 
#   if (K != (K1 + K2)) stop ("Cases dont add up in isCrossedRocDataset()")
#   
#   readerIDColumn <- strsplit(TruthTable$ReaderID, split = ",", fixed = TRUE)
#   readerID <- unlist(unique(readerIDColumn)) 
#   J <- length(readerID) # number of readers
#   
#   modalityIDColumn <- strsplit(TruthTable$ModalityID, split = ",", fixed = TRUE)
#   modalityID <- unlist(unique(modalityIDColumn)) 
#   I <- length(modalityID)
#   
#   # check lengths of all three worksheets
#   for (i in 1:5) if (length(TruthTable[[i]]) != K) return (FALSE) 
#   for (i in 1:4) if (length(NLTable[[i]]) != (I*J*K1)) return (FALSE) 
#   for (i in 1:5) if (length(LLTable[[i]]) != (I*J*K2)) return (FALSE) 
#   
#   # examine TRUTH worksheet 
#   caseID <- TruthTable$CaseID 
#   designArray <- array(dim = c(I, J, K)) 
#   for (i in 1:I) {
#     for (j in 1:J) {
#       for (k in 1:K) {
#         designArray[which(modalityID == modalityID[i]),
#                     which(readerID == readerID[j]),
#                     which(caseID == caseID[k])] <- 1
#       }
#     }
#   }
#   if (any(is.na(designArray))) return (FALSE)
#   
#   # examine NL worksheet 
#   caseID <- unique(NLTable$CaseID )
#   readerIDColumn <- strsplit(NLTable$ReaderID, split = ",", fixed = TRUE)
#   readerID <- unlist(unique(readerIDColumn)) 
#   
#   modalityIDColumn <- strsplit(NLTable$ModalityID, split = ",", fixed = TRUE)
#   modalityID <- unlist(unique(modalityIDColumn)) 
#   
#   designArray <- array(dim = c(I, J, K1)) 
#   for (i in 1:I) {
#     for (j in 1:J) {
#       for (k in 1:K1) {
#         designArray[which(modalityID == modalityID[i]),
#                     which(readerID == readerID[j]),
#                     which(caseID == caseID[k])] <- 1
#       }
#     }
#   }
#   if (any(is.na(designArray))) return (FALSE)
#   
#   # examine LL worksheet 
#   caseID <- unique(LLTable$CaseID )
#   readerIDColumn <- strsplit(LLTable$ReaderID, split = ",", fixed = TRUE)
#   readerID <- unlist(unique(readerIDColumn)) 
#   
#   modalityIDColumn <- strsplit(LLTable$ModalityID, split = ",", fixed = TRUE)
#   modalityID <- unlist(unique(modalityIDColumn)) 
#   
#   designArray <- array(dim = c(I, J, K2)) 
#   for (i in 1:I) {
#     for (j in 1:J) {
#       for (k in 1:K2) {
#         designArray[which(modalityID == modalityID[i]),
#                     which(readerID == readerID[j]),
#                     which(caseID == caseID[k])] <- 1
#       }
#     }
#   }
#   if (any(is.na(designArray))) return (FALSE)
#   return (TRUE)
# }




# isSplitPlotRocDataset <- function(TruthTable, NLTable, LLTable)
# {
#   # UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   modalityID <- unique(NLTable[[2]])
#   readerID <- unique(TruthTable[[4]])
#   I <- length(modalityID) # number of modalities
#   J <- length(readerID) # number of readers
#   K <- length(TruthTable[[1]]) # total number of cases read by all readers
#   Kj <- array(dim = J) # total number of cases read by reader j
#   K1j <- array(dim = J) # total number of normal cases read by reader j
#   K2j <- array(dim = J) # total number of abnormal cases read by reader j
#   Knested <- array(dim = c(J, K)) # IDs of cases read by reader j
#   K1nested <- array(dim = c(J, K)) # IDs of normal cases read by reader j
#   K2nested <- array(dim = c(J, K)) # IDs of abnormal cases read by reader j
#   for (j in 1:J) {
#     temp <- TruthTable[[1]][which(TruthTable[[4]] == j)]
#     temp1 <- TruthTable[[1]][which((TruthTable[[4]] == j) & (TruthTable[[2]] == 0))]
#     temp2 <- TruthTable[[1]][which((TruthTable[[4]] == j) & (TruthTable[[2]] == 1))]
#     Kj[j] <- length(temp)
#     K1j[j] <- length(temp1)
#     K2j[j] <- length(temp2)
#     Knested[j,1:Kj[j]] <- temp
#     K1nested[j,1:K1j[j]] <- temp1
#     K2nested[j,1:K2j[j]] <- temp2
#   }
#   
#   for (j in 1:J) {
#     cat("\nj = ", j, "\n")
#     cat("Cases, non-diseased followed by diseased", "\n")
#     cat(K1nested[j,1:K1j[j]],"\n")
#     cat(K2nested[j,1:K2j[j]],"\n")
#     cat("Non-diseased ratings, in modality 1 followed by modality 2", "\n")
#     cat(NLTable[[4]][which((NLTable[[1]] == j) & (NLTable[[3]] %in% K1nested[j,]) & (NLTable[[2]] == 1))],"\n")
#     cat(NLTable[[4]][which((NLTable[[1]] == j) & (NLTable[[3]] %in% K1nested[j,]) & (NLTable[[2]] == 2))],"\n")
#     cat("Diseased ratings, in modality 1 followed by modality 2", "\n")
#     cat(LLTable[[5]][which((LLTable[[1]] == j) & (LLTable[[3]] %in% K2nested[j,]) & (LLTable[[2]] == 1))],"\n")
#     cat(LLTable[[5]][which((LLTable[[1]] == j) & (LLTable[[3]] %in% K2nested[j,]) & (LLTable[[2]] == 2))],"\n")
#   }
#   # inspect NL table
#   
#   # tests for ROI data
#   if (!all(is.finite(NLTable[[4]]))) return (FALSE)
#   if (!all(is.finite(LLTable[[5]]))) return (FALSE)
#   for (j in 1:J) {
#     if (!(sum(NLTable[[3]] %in% K1nested[j,1:K1j[j]]) == I*K1j[j])) return (FALSE)
#     if (!(sum(LLTable[[3]] %in% K2nested[j,1:K2j[j]]) == I*K2j[j])) return (FALSE)
#     if (!(nrow(NLTable) == I*sum(K1j))) return (FALSE)
#     if (!(nrow(LLTable) == I*sum(K2j))) return (FALSE)
#   }
#   return (TRUE)
# }




# isROCDataset <- function(NL, LL, Truth_CaseIDColumn)
# {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   
#   K <- length(NL[1,1,,1])
#   K2 <- length(LL[1,1,,1])
#   K1 <- K - K2
#   maxNL <- length(NL[1,1,1,])
#   maxLL <- length(LL[1,1,1,])
#   
#   if (max(table(Truth_CaseIDColumn)) != 1) return (FALSE) # number of occurrences of each Truth_CaseIDColumn value
#   if (maxNL != 1) return (FALSE)
#   if (all((NL[, , (K1 + 1):K, ] != UNINITIALIZED))) return (FALSE)
#   if (any((NL[, , 1:K1, ] == UNINITIALIZED))) return (FALSE)
#   if (maxLL != 1) return (FALSE)
#   if (any((LL[, , 1:K2, ] == UNINITIALIZED))) return (FALSE)
#   return (TRUE)
# }




##stop("ROI paradigm not yet implemented")
## !!!DPC!!!
# isROIDataset <- function(NL, LL, Truth_LesionIDColumn)
# {
#   return (FALSE)
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   
#   I <- length(NL[,1,1,1])
#   J <- length(NL[1,,1,1])
#   K <- length(NL[1,1,,1])
#   K2 <- length(LL[1,1,,1])
#   K1 <- K - K2
#   maxNL <- length(NL[1,1,1,])
#   lesionVector <- array(dim = K2)
#   for (k in 1:K2) {
#     lesionVector[k] <- sum(is.finite(LL[1,1,k,]))
#   }
#   
#   isROI <- TRUE
#   for (i in 1:I) {
#     for (j in 1:J) {
#       # in following code, non-diseased cases with non-diseased 
#       # ROIs (i.e., -Infs) are counted
#       # On such cases, all ROIs are marked and one does not expect any missing 
#       # entries; if a missing entry is found, then dataset is not ROI.
#       # As an example, in RoiData.xlsx all caseIDs < 51 correspond to non-diseased cases
#       # For each such case, one has 4 ratings, i.e., Q = 4, assumed constant for all 
#       # cases
#       if (any(NL[i, j, 1:K1, ] == UNINITIALIZED)) {
#         isROI <- FALSE
#         break
#       }
#       # in following code, diseased cases with diseased 
#       # ROIS (i.e., not = -Infs) are counted
#       # These must sum to the lesionVector for that case
#       # note: this test may be redundant with above definition:
#       #  lesionVector[k] <- sum(is.finite(LL[1,1,k,]))
#       temp <- LL[i, j, , ] != UNINITIALIZED
#       dim(temp) <- c(K2, max(lesionVector))
#       if (!all(lesionVector == rowSums(temp))) {
#         isROI <- FALSE
#         break
#       }
#       # in following code, diseased cases (dc) with non-diseased 
#       # (nd) ROIs (i.e., -Infs) are counted
#       temp <- NL[i, j, (K1 + 1):K, ] == UNINITIALIZED # this is the array of nd counts on dcs
#       dim(temp) <- c(K2, maxNL)
#       if (!all(lesionVector == rowSums(temp))) { 
#         # sum of nd counts on dcs must equal that of diseased counts on
#         # dcs (each empty count makes space for a LL count so total number of marks
#         # per case is the same and equal to 4 for this dataset)
#         # As an example, on FP sheet, for case 57, i = j = 1, there are three rated regions:
#         # NL[1,1,57,]
#         # [1] -0.967084 -0.394129  0.264821      -Inf
#         # This means that there can be only one lesion on this case: 
#         # lesionVector[7]
#         # [1] 1 
#         # and this diseased case can (rather must) have only one LL mark:
#         # LL[1,1,7,]
#         # [1] 0.535298     -Inf     -Inf     -Inf
#         isROI <- FALSE
#         break
#       }
#     }
#   }
#   return (isROI)
# }




ReadLrc <- function(fileName, sequentialNames) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  fileExt <- file_ext(fileName)
  if (fileExt != "lrc") {
    stop("The extension of LRC file name must be *.lrc")
  }
  whiteSpaces <- c("", " ", "\t")
  
  fileLines <- readLines(fileName)
  fileLines <- str_trim(fileLines)
  lineNum <- 2
  
  J <- 1
  readerID <- fileLines[lineNum]
  lineNum <- 3
  modalityID <- unlist(strsplit(fileLines[lineNum], split = "\""))
  iRmv <- NULL
  ifRmv <- FALSE
  for (i in 1:length(modalityID)) {
    modalityChar <- unlist(strsplit(modalityID[i], split = ""))
    if (all(modalityChar %in% whiteSpaces)) {
      iRmv <- c(iRmv, i)
      ifRmv <- TRUE
    }
  }
  if (ifRmv) {
    modalityID <- modalityID[-iRmv]
  }
  I <- length(modalityID)
  
  lineNum <- 4
  largeOrSmall <- splitWhiteSpaces(fileLines[lineNum])
  if (length(largeOrSmall) != I) {
    stop("The numbers of elements in the third and fourth lines does not match.")
  }
  
  lineNum <- 5
  NL <- NA
  LL <- NA
  while (TRUE) {
    K1 <- NA
    K1Temp <- 0
    NLSnglR <- NULL
    while (fileLines[lineNum] != "*") {
      K1Temp <- K1Temp + 1
      ratings <- splitWhiteSpaces(fileLines[lineNum])
      if (length(ratings) < I) {
        errMsg <- sprintf("The number of ratings in line %d is less than the number of maodalities.", lineNum)
        stop(errMsg)
      }
      NLSnglR <- rbind(NLSnglR, ratings[1:I])
      lineNum <- lineNum + 1
    }
    if (is.na(K1)) {
      K1 <- K1Temp
    } else if (K1 != K1Temp) {
      errMsg <- sprintf("The number of non-diseased cases for reader %d does not match that for previous readers.", J)
      stop(errMsg)
    }
    NLTemp <- array(dim = c(I, J, K1, 1))
    if (is.null(dim(NL))) {
      NLTemp[, , , 1] <- as.numeric(t(NLSnglR))
      NL <- NLTemp
    } else {
      NLTemp[, 1:(J - 1), , ] <- NL
      NLTemp[, J, , 1] <- as.numeric(t(NLSnglR))
      NL <- NLTemp
    }
    
    K2 <- NA
    K2Temp <- 0
    LLSnglR <- NULL
    lineNum <- lineNum + 1
    while (fileLines[lineNum] != "*") {
      K2Temp <- K2Temp + 1
      ratings <- splitWhiteSpaces(fileLines[lineNum])
      if (length(ratings) < I) {
        errMsg <- sprintf("The number of ratings in line %d is less than the number of maodalities.", lineNum)
        stop(errMsg)
      }
      LLSnglR <- rbind(LLSnglR, ratings[1:I])
      lineNum <- lineNum + 1
    }
    if (is.na(K2)) {
      K2 <- K2Temp
    } else if (K2 != K2Temp) {
      errMsg <- sprintf("The number of diseased cases for reader %d does not match that for previous readers.", J)
      stop(errMsg)
    }
    LLTemp <- array(dim = c(I, J, K2, 1))
    if (is.null(dim(LL))) {
      LLTemp[, , , 1] <- as.numeric(t(LLSnglR))
      LL <- LLTemp
    } else {
      LLTemp[, 1:(J - 1), , ] <- LL
      LLTemp[, J, , 1] <- as.numeric(t(LLSnglR))
      LL <- LLTemp
    }
    lineNum <- lineNum + 1
    if (fileLines[lineNum] != "#") {
      J <- J + 1
      readerID <- c(readerID, fileLines[lineNum])
      lineNum <- lineNum + 1
    } else {
      break
    }
  }
  maxRating <- max(NL, LL)
  for (i in 1:I) {
    if (toupper(largeOrSmall[i]) %in% c("S", "SMALL")) {
      NL[i, , , ] <- maxRating + 1 - NL[i, , , ]
      LL[i, , , ] <- maxRating + 1 - LL[i, , , ]
    } else if (!toupper(largeOrSmall[i]) %in% c("L", "LARGE")) {
      stop("The confidence indicater in line 4 must be \"L\", \"LARGE\", \"S\", or \"SMALL\".")
    }
  }
  K <- K1 + K2
  NLTemp <- array(UNINITIALIZED, dim = c(I, J, K, 1))
  NLTemp[, , 1:K1, ] <- NL
  NL <- NLTemp
  lesionVector <- rep(1, K2)
  lesionID <- array(1, dim = c(K2, 1))
  lesionWeight <- lesionID
  maxNL <- 1
  dataType <- "ROC"
  
  modalityNames <- modalityID
  readerNames <- readerID
  
  if (sequentialNames){
    modalityID <- 1:I
    readerID <- 1:J
  }
  
  names(modalityID) <- modalityNames
  names(readerID) <- readerNames
  
  return(list(NL = NL, 
              LL = LL, 
              lesionVector = lesionVector, 
              lesionID = lesionID, 
              lesionWeight = lesionWeight, 
              dataType = dataType, 
              modalityID = modalityID, 
              readerID = readerID))
}




splitWhiteSpaces <- function(string) 
{
  whiteSpaces <- c("", " ", "\t")
  string <- unlist(strsplit(string, split = " |\t"))
  string <- string[!string %in% whiteSpaces]
  return(string)
} 




ReadImrmc <- function(fileName, sequentialNames) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  fileExt <- file_ext(fileName)
  if (fileExt != "imrmc") {
    stop("The extension of iMRMC file name must be *.imrmc")
  }
  fileLines <- str_trim(readLines(fileName))
  lineNum <- 1
  
  for (n in 1:4) {
    lineTxt <- unlist(strsplit(fileLines[lineNum], split = ":", fixed = TRUE))
    while (!(lineTxt[1] %in% c("N0", "N1", "NR", "NM"))) {
      lineNum <- lineNum + 1
      lineTxt <- unlist(strsplit(fileLines[lineNum], split = ":", fixed = TRUE))
    }
    if (lineTxt[1] == "N0") {
      K1 <- as.numeric(lineTxt[2])
    } else if (lineTxt[1] == "N1") {
      K2 <- as.numeric(lineTxt[2])
    } else if (lineTxt[1] == "NM") {
      I <- as.numeric(lineTxt[2])
    } else if (lineTxt[1] == "NR") {
      J <- as.numeric(lineTxt[2])
    }
    lineNum <- lineNum + 1
  }
  K <- K1 + K2
  
  while (fileLines[lineNum] != "BEGIN DATA:") lineNum <- lineNum + 1
  lineNum <- lineNum + 1
  while (fileLines[lineNum] == "") lineNum <- lineNum + 1
  dataTable <- NULL
  while (length(lineTxt <- unlist(strsplit(fileLines[lineNum], split = ",", fixed = TRUE))) == 4) {
    dataTable <- rbind(dataTable, lineTxt)
    lineNum <- lineNum + 1
  }
  
  caseId <- unique(as.numeric(dataTable[, 2]))
  if (any(is.na(caseId))) {
    errMsg <- "Case IDs must be integers."
    stop(errMsg)
  }
  if (any(is.na(as.numeric(dataTable[, 4])))) {
    errMsg <- "Ratings must be numeric."
    stop(errMsg)
  }
  if (length(caseId) != K) {
    errMsg <- "The number of cases in the dataset is different from the number in the study description."
    stop(errMsg)
  }
  
  TruthTable <- dataTable[as.numeric(dataTable[, 1]) == -1, ]
  if (any(!(caseId %in% as.numeric(TruthTable[, 2])))) {
    errMsg <- sprintf("Case %d cannot be found in rows recording truth states.", caseId[!(caseId %in% as.numeric(TruthTable[, 2]))])
    stop(errMsg)
  }
  
  if (any(!(as.numeric(TruthTable[, 4]) %in% c(0, 1)))) {
    errMsg <- "Cases' truth states must be 0 or 1."
    stop(errMsg)
  }
  normalCases <- unique(as.numeric(TruthTable[as.numeric(TruthTable[, 4]) == 0, 2]))
  abnormalCases <- unique(as.numeric(TruthTable[as.numeric(TruthTable[, 4]) == 1, 2]))
  
  fpTable <- dataTable[(as.numeric(dataTable[, 2]) %in% normalCases) & as.numeric(dataTable[, 1]) != -1, ]
  tpTable <- dataTable[(as.numeric(dataTable[, 2]) %in% abnormalCases) & as.numeric(dataTable[, 1]) != -1, ]
  
  readerID <- as.character(sort(unique(c(fpTable[, 1], tpTable[, 1]))))
  if (J != length(readerID)) {
    errMsg <- "The number of readers in the dataset is different from the number in the study description."
    stop(errMsg)
  }
  
  modalityID <- as.character(sort(unique(c(fpTable[, 3], tpTable[, 3]))))
  if (I != length(modalityID)) {
    errMsg <- "The number of treatments in the dataset is different from the number in the study description."
    stop(errMsg)
  }
  
  NL <- array(UNINITIALIZED, dim = c(I, J, K, 1))
  LL <- array(UNINITIALIZED, dim = c(I, J, K2, 1))
  for (i in 1:I) {
    for (j in 1:J) {
      for (k1 in 1:K1) {
        caseIndx <- which((fpTable[, 1] == readerID[j]) & (fpTable[, 3] == modalityID[i]) & (as.numeric(fpTable[, 2]) == normalCases[k1]))
        NL[i, j, k1, 1] <- as.numeric(fpTable[caseIndx, 4])
      }
      for (k2 in 1:K2) {
        caseIndx <- which((tpTable[, 1] == readerID[j]) & (tpTable[, 3] == modalityID[i]) & (as.numeric(tpTable[, 2]) == abnormalCases[k2]))
        LL[i, j, k2, 1] <- as.numeric(tpTable[caseIndx, 4])
      }
    }
  }
  
  lesionVector <- rep(1, K2)
  lesionID <- array(1, dim = c(K2, 1))
  lesionWeight <- lesionID
  #maxNL <- 1
  dataType <- "ROC"
  
  modalityNames <- modalityID
  readerNames <- readerID
  
  if (sequentialNames){
    modalityID <- 1:I
    readerID <- 1:J
  }
  
  names(modalityID) <- modalityNames
  names(readerID) <- readerNames
  
  return(list(
    NL = NL, 
    LL = LL, 
    lesionVector = lesionVector, 
    lesionID = lesionID, 
    lesionWeight = lesionWeight, 
    dataType = dataType, 
    modalityID = modalityID, 
    readerID = readerID))
} 





