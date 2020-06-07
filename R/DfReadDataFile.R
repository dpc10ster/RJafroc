#' Read a data file 
#' 
#' @description Read a disk file and create a dataset object from it.
#' 
#' @param fileName A string specifying the name of the file. 
#'    The file-extension must match the format specified below
#' @param format A string specifying the format of the data in the file. 
#'    It can be \code{"JAFROC"}, the default, which requires a .xlsx Excel file,
#'    \bold{not .xls}, \code{"MRMC"} or \code{"iMRMC"}. 
#'    For \code{"MRMC"} the format is determined by the data file extension 
#'    as specified in \url{http://perception.radiology.uiowa.edu/}, i.e.,  
#'    \code{.csv} or \code{.txt} or \code{.lrc}. For file extension 
#'    \code{.imrmc} the format is described in \url{https://code.google.com/p/imrmc/}.
#' @param newExcelFileFormat This argument only applies to the \code{"JAFROC"} format. 
#'    The default is \code{FALSE}. if \code{TRUE} the function accommodates 3 
#'    additional columns
#'    in the \code{Truth} worksheet. If \code{FALSE}, the original function (as in version 
#'    1.2.0) is used and the three extra columns, if present, throws an error.  
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
#' fileName <- system.file("extdata", "toyFiles/FROC/FrocDataSpVaryK1K2.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' x <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#' 
#' {
#' fileName <- system.file("extdata", "toyFiles/FROC/frocCr.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' x <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#' 
#' x1 <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
#' testthat::expect_equal(x,x1)
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
    if (!(file_ext(fileName) == "xlsx")) 
      stop("The extension of JAFROC data file must be .xlsx, NOT .xls.\n")
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
  
  df <- as.data.frame(truthTable[1:5], stringsAsFactors = FALSE)
  df["caseLevelTruth"] <- (truthTable$LesionID > 0)
  TruthTableSort <- df[order(df$caseLevelTruth, df$ReaderID, df$CaseID), ]
  # TruthTableSort <- df # temporary line to bypass sorting
  
  caseIDCol <- as.integer(TruthTableSort$CaseID)  # all 3 have same lengths
  lesionIDColumn <- as.integer(TruthTableSort$LesionID)
  weightsCol <- as.numeric(TruthTableSort$Weight)
  readerID <- TruthTableSort$ReaderID
  L <- length(TruthTableSort$CaseID)
  for (i in 1:4) if ((length(TruthTableSort[[i]])) != L) 
    stop("Columns of unequal height in Truth Excel worksheet")  
  
  normalCases <- sort(unique(caseIDCol[lesionIDColumn == 0]))
  abnormalCases <- sort(unique(caseIDCol[lesionIDColumn > 0]))
  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  # DPC: check for duplicate lesionIDs
  if (anyDuplicated(cbind(caseIDCol, lesionIDColumn))) {
    naLines <- which(duplicated(cbind(caseIDCol, lesionIDColumn))) + 1
    errorMsg <- paste0(errorMsg, "Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicated lesionIDs for given caseID.")
  }
  if (errorMsg != "") stop(errorMsg)
  
  if (anyDuplicated(cbind(caseIDCol, lesionIDColumn, weightsCol))) {
    naLines <- which(duplicated(cbind(caseIDCol, lesionIDColumn, weightsCol))) + 1
    errorMsg <- paste0(errorMsg, "Line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table are duplicates of previous line(s) .")
  }
  if (errorMsg != "") stop(errorMsg)
  
  lesionIDColUnique <- sort(unique(lesionIDColumn)) # fix to bug when abnormal cases occur first
  
  perCase <- as.vector(table(caseIDCol[caseIDCol %in% abnormalCases]))
  weights <- array(dim = c(K2, max(perCase)))
  IDs <- array(dim = c(K2, max(perCase)))
  
  for (k2 in 1:K2) {
    k <- which(caseIDCol == abnormalCases[k2])
    IDs[k2, ] <- c(sort(lesionIDColumn[k]), 
                   rep(UNINITIALIZED, max(perCase) - length(k)))
    if (all(weightsCol[k] == 0)) {
      weights[k2, 1:length(k)] <- 1/perCase[k2]
    } else {
      weights[k2, ] <- as.numeric(c(weightsCol[k][order(lesionIDColumn[k])], 
                                    rep(UNINITIALIZED, max(perCase) - length(k))))
      sumWeight <- sum(weights[k2, weights[k2, ] != UNINITIALIZED])
      if (sumWeight != 1){
        if (sumWeight <= 1.01 && sumWeight >= 0.99){
          weights[k2, ] <- weights[k2, ] / sumWeight
        }else{
          errorMsg <- paste0("The sum of the weights of Case ", k2, " is not 1.")
          stop(errorMsg)
        }
      }
    }
  }
  
  isGT1RperCase <- array(dim = length(caseIDCol))
  for (i in 1:length(caseIDCol)) 
    isGT1RperCase[i] <- is.character(TruthTableSort$ReaderID[i]) && (nchar(TruthTableSort$ReaderID[i]) > 1)  
  if (all(isGT1RperCase == FALSE)) GT1RperCase <- FALSE else 
    if (all(isGT1RperCase == TRUE)) GT1RperCase <- TRUE else 
      stop("Unequal number of readers in ReaderID column\n")
  
  isGT1TperCase <- array(dim = length(caseIDCol))
  for (i in 1:length(caseIDCol)) 
    isGT1TperCase[i] <- is.character(TruthTableSort$ModalityID[i]) && (nchar(TruthTableSort$ModalityID[i]) > 1)  
  if (all(isGT1TperCase == FALSE)) GT1TperCase <- FALSE 
  else if (all(isGT1TperCase == TRUE)) GT1TperCase <- TRUE 
  else stop("Unequal number of modalities in ModalityID column\n")
  
  I <- length(unlist(strsplit(TruthTableSort$ModalityID[1],split = ",")))
  
  if (GT1RperCase) {
    J <- length(unlist(strsplit(TruthTableSort$ReaderID[1],split = ",")))
    
    readerIDArray <- array(dim = c(J,L))
    x <- sort(trimws(unlist(strsplit(TruthTableSort$ReaderID,split = ","))))
    for (j in 1:J) readerIDArray[j,] <- x[((j-1)*L + 1):(j*L)]
    readerIDUnique <- unique(readerIDArray)
    if(!all((dim(readerIDArray) == dim(readerIDUnique)))) stop("Non-unique reader ids in dataset")
  } else {
    J <- length(unique(TruthTableSort$ReaderID))
    readerIDUnique <- unique(readerID)
    # following fixed single reader datasets, ROC and FROC
    readerIDArray <- array(dim = c(J,L))
    for (j in 1:J) readerIDArray[j,] <- readerIDUnique[j]
  }
  
  # if (!GT1TperCase) stop("Must have > 1 modalities per case, balanced data, for each reader")
  # following code should break with single modality dataset
  # but it does not; go figure TBA-DPC!!!
  modalityIDArray <- array(dim = c(I,L))
  x <- sort(trimws(unlist(strsplit(TruthTableSort$ModalityID,split = ","))))
  for (i in 1:I) modalityIDArray[i,] <- x[((i-1)*L + 1):(i*L)]
  modalityIDUnique <- unique(modalityIDArray)
  if(!all((dim(modalityIDArray) == dim(modalityIDUnique)))) stop("Non-unique modality ids in dataset")
  
  # truthTableStr <- array(dim = c(I, J, K, length(lesionIDColUnique))) # TBA-DPC!!!
  truthTableStr <- array(dim = c(I, J, K, max(lesionIDColUnique)+1)) # possible bug if lesionIDColUnique does not consist of sequential integers
  for (i in 1:I) {
    for (j in 1:J) {
      for (el in lesionIDColUnique) { 
        el1 <- which(lesionIDColUnique == el)
        if (!GT1RperCase) {
          casePresent_ij <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerID == readerIDUnique[j]) &
              (lesionIDColumn == lesionIDColUnique[el1])
          )
        } else {
          casePresent_ij <- (
            (modalityIDArray[i,] == modalityIDUnique[i]) & 
              (readerIDArray[j,] == readerIDUnique[j]) &
              (lesionIDColumn == lesionIDColUnique[el1])
          )
        }
        if ((sum(casePresent_ij) == 0)) next 
        caseIDTable <- table(caseIDCol[casePresent_ij])
        caseIDs_ij <- as.numeric(unlist(attr(caseIDTable, "dimnames")))
        for (k in 1:length(caseIDs_ij)) {
          k1 <- which(caseIDs_ij[k] == allCases)
          # this code checks out for file frocCr013Lesions.xlsx which contains
          # 2 modalities, 3 readers, froc data,
          # 3 normal cases, 5 abnormal cases, four containing 1 lesion and 1 containing
          # 3 lesions. See RJafrocChecks/truthTableStr.xlsx, NewFormat.
          # cat("i = ", i, ", j = ", j, ", el1 = ", el1,
          #     ", k1 = ", k1, ", caseIDs_ij[k]", caseIDs_ij[k], "\n")
          truthTableStr[i, j, k1, el1] <-1
        }
      }  
    }
  }
  
  type <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  
  if (!(type %in% c("FROC", "ROC"))) stop("Unsupported declared type: must be ROC or FROC.\n")
  if (!(design %in% c("FCTRL", "SPLIT-PLOT"))) stop("Study design must be FCTRL or SPLIT-PLOT\n")
  
  if (type == "ROC") {
    if ((design == "FCTRL") && (sum(!is.na(truthTableStr)) != 
                                  L*length(readerIDArray[,1])*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be crossed ROC")
    
    if ((design == "SPLIT-PLOT") && (sum(!is.na(truthTableStr)) != L*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be split plot ROC")
  }
  
  if (type == "FROC") {
    if ((design == "FCTRL") && (sum(!is.na(truthTableStr)) != 
                                  L*length(readerIDArray[,1])*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be crossed FROC")
    
    if ((design == "SPLIT-PLOT") && (sum(!is.na(truthTableStr)) != L*length(modalityIDArray[,1]))) 
      stop("Dataset does not appear to be split plot FROC")
  }
  
  return (list(
    truthTableStr = truthTableStr,
    type = type,
    design = design,
    caseID = caseIDCol,
    perCase = perCase,
    lesionIDColumn = lesionIDColumn,
    IDs = IDs,
    weights = weights,
    normalCases = normalCases,
    abnormalCases = abnormalCases
  ))
  
}



###################### NEW FORMAT READ FUNCTION  ############################
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
  lesionIDColumn <- truth$lesionIDColumn
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
  
  df1 <- as.data.frame(NLTable, stringsAsFactors = FALSE)
  NLTableSort <- df1[order(df1$ModalityID, df1$ReaderID, df1$CaseID),]
  # NLTableSort <- df1 # temporary line to bypass sorting
  
  # Column means the entire column is included
  NLReaderIDColumn <- as.character(NLTableSort$ReaderID)
  NLModalityIDColumn <- as.character(NLTableSort$ModalityID)
  NLCaseIDColumn <- NLTableSort$CaseID
  if (is.null(NLTableSort$FP_Rating)) NLRatingColumn <- NLTableSort$NL_Rating else
    NLRatingColumn <- NLTableSort$FP_Rating
  
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
  
  df11 <- as.data.frame(LLTable, stringsAsFactors = FALSE)
  LLTableSort <- df11[order(df11$ModalityID, df11$ReaderID, df11$CaseID, df11$LesionID),]
  # LLTableSort <- df11 # temporary line to bypass sorting
  
  LLReaderIDColumn <- as.character(LLTableSort$ReaderID)
  LLModalityIDColumn <- as.character(LLTableSort$ModalityID)
  LLCaseIDColumn <- LLTableSort$CaseID
  LLLesionIDColumn <- LLTableSort$LesionID
  if (is.null(LLTableSort$TP_Rating)) LLRatingColumn <- LLTableSort$LL_Rating else
    LLRatingColumn <- LLTableSort$TP_Rating
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseIDColumn[i]) & (lesionIDColumn == LLLesionIDColumn[i]))
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
    lineNum <- which((truthCaseID == LLCaseIDColumn[i]) & (lesionIDColumn == LLLesionIDColumn[i]))
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
      # following are the actual caseIDs, not the indices into the array
      caseIDs_ij <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
      for (k1 in 1:length(caseIDs_ij)) {
        if (caseIDs_ij[k1] %in% normalCases) {
          if (truthTableStr[i,j,which(caseIDs_ij[k1] == allCases), 1] != 1) # ,1: for normal cases, the fourth dimension is lesionIDColumn = 0, which occupies the first position
            stop("incorrect truthCaseID in NL/FP worksheet")
        } else if (caseIDs_ij[k1] %in% abnormalCases) {
          if (truthTableStr[i,j,which(caseIDs_ij[k1] == allCases), 2] != 1) # ,1: for normal cases, the fourth dimension is lesionIDColumn = 0, which occupies the first position
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
  LL <- array(dim = c(I, J, K2, max(perCase)))
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
        x3 <- IDs[k2p, ]
        for (el in 1:caseLLTable[k2]) {
          elp <- which(LLLesionIDColumn[casePresent_ij][x2][el] == x3)
          if (truthTableStr[i,j,which(caseIDs_ij[k2] == allCases),elp+1] != 1) # elp+1 because lesionIDColumn = 0 occupies first position, etc.
            stop("incorrect truthCaseID in LL/TP worksheet")
          LL[i, j, k2p, elp] <- LLRatingColumn[casePresent_ij][x2][el]
        }
      }
    }
  }
  LL[is.na(LL)] <- UNINITIALIZED
  weights[is.na(weights)] <- UNINITIALIZED
  lesionIDColumn[is.na(lesionIDColumn)] <- UNINITIALIZED
  
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
  
  binned <- isBinned(NL, LL)
  fileName <- NA
  name <- NA
  if (design == "FCTRL") design <- "FCTRL"
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         binned, fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
  
} 



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
  
  if (any(!(NLCaseID %in% caseIDColumn))) {
    naCases <- NLCaseID[which(!(NLCaseID %in% caseIDColumn))]
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
    lineNum <- which((caseIDColumn == LLCaseID[i]) & (lesionIDColumn == LLLesionID[i]))
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
  
  binned <- isBinned(NL, LL)
  fileName <- NA
  name <- NA
  design <- "FCTRL" # default when using old read function
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         binned, fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
  
} 



splitWhiteSpaces <- function(string) 
{
  whiteSpaces <- c("", " ", "\t")
  string <- unlist(strsplit(string, split = " |\t"))
  string <- string[!string %in% whiteSpaces]
  return(string)
} 

