#' Read a factorial data file
#'
#' @description Read an Excel file and create an ROC, FROC or LROC dataset
#'   object from it.
#'
#' @param fileName A string specifying the name of the file. The file-extension
#'   must match the \code{format} specified below.
#'
#' @param format A string specifying the format of the data file. It can be
#'   \code{"JAFROC"}, the default, which requires an \code{.xlsx} Excel file
#'   (\bold{not \code{.xls}}), \code{"MRMC"} or \code{"iMRMC"}. For
#'   \code{"MRMC"} the format is determined by the data file extension
#'   (\code{.csv} or \code{.txt} or \code{.lrc}) as specified in
#'   \url{https://perception.lab.uiowa.edu/}. For \code{"iMRMC"} the file
#'   extension is \code{.imrmc} and the format is described in
#'   \url{https://code.google.com/archive/p/imrmc/}.
#'    \bold{See note for important information about deprecation of the
#'    \code{"MRMC"} format}.
#'
#'
#' @param newExcelFileFormat Logical. Must be true to read LROC data. This
#'   argument only applies to the \code{"JAFROC"} format. The default is
#'   \code{TRUE}. If \code{TRUE} the function accommodates 3 additional columns
#'   in the \code{Truth} worksheet. If \code{FALSE}, the original function (as
#'   in version 1.2.0) is used and the three extra columns, if present, throw an
#'   error.
#'
#' @param lrocForcedMark Logical: For LROC dataset only: is a forced mark
#'   required on every image? The default is \code{NA}. If a mark is not
#'   required, set it to \code{FALSE} otherwise to \code{TRUE}.
#'
#' @param delimiter The string delimiter to be used for the \code{"MRMC"} format
#'   ("," is the default), see \url{https://perception.lab.uiowa.edu/}. This
#'   parameter is not used when reading \code{"JAFROC"} or \code{"iMRMC"} data
#'   files.
#'
#' @param sequentialNames A logical variable: if \code{TRUE}, consecutive
#'   integers (starting from 1) will be used as the modality and reader IDs
#'   (i.e., names). Otherwise, modality and reader IDs in the original data file
#'   will be used.
#'
#' @note The \code{"MRMC"} format is deprecated. For non-JAFROC formats four
#'   file extensions (\code{.csv}, \code{.txt}, \code{.lrc} and \code{.imrmc})
#'   are possible, all of which are restricted to ROC data. Only \code{iMRMC}
#'   format is now supported, i.e, files with extension \code{.imrmc}. Other
#'   formats (\code{.csv}, \code{.txt}, \code{.lrc}) are deprecated. Such files
#'   can still be read by this function and then saved to a JAFROC format file
#'   for further analysis within this package.
#'    \bold{For non-JAFROC data file formats, the \code{readerID} and
#'    \code{modalityID} fields must be unique integers}.
#'
#' @return A dataset with the structure specified in
#'   \code{\link{RJafroc-package}}.
#'
#' @note This function is used only for factorial datasets.
#'
#' @examples
#' fileName <- system.file("extdata", "toyFiles/ROC/rocCr.xlsx",
#' package = "RJafroc", mustWork = TRUE)
#' rdrArr1D <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#'
#'
#' \donttest{
#' fileName <- system.file("extdata", "Roc.xlsx",
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
#' fileName <- system.file("extdata", "Froc.xlsx",
#' package = "RJafroc", mustWork = TRUE)
#' FrocDataXlsx <- DfReadDataFile(fileName, sequentialNames = TRUE)
#' }
#'
#' @importFrom tools file_ext
#' @importFrom stringr str_trim str_length
#' @export

DfReadDataFile <- function (fileName, format = "JAFROC", 
                            newExcelFileFormat = FALSE, 
                            lrocForcedMark = NA,
                            delimiter = ",", 
                            sequentialNames = FALSE) 
{
  
  if (format == "JAFROC") {
    # handle JAFROC format Excel files
    if (!(file_ext(fileName) == "xlsx")) 
      stop("The extension of JAFROC data file must be .xlsx, NOT .xls.\n")
    if (!newExcelFileFormat) { 
      if (!is.na(lrocForcedMark)) stop("Attempt to read possibly LROC dataset 
                                       with newExcelFileFormat flag set to FALSE") 
      return((ReadJAFROCOldFormat(fileName, sequentialNames)))
    } else {
      return(ReadJAFROCNewFormat(fileName, lrocForcedMark, sequentialNames))
    }
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



preCheck4BadEntries <- function(truthTable) {
  
  # START not sure what this does
  # for (i in 1:3){
  #   truthTable[grep("^\\s*$", truthTable[ , i]), i] <- NA
  # }
  # 
  # naRows <- colSums(is.na(truthTable))
  # if (max(naRows) > 0) {
  #   if (max(naRows) == min(naRows)) {
  #     truthTable <- truthTable[1:(nrow(truthTable) - max(naRows)), ]
  #   }
  # }
  # END not sure what this does
  
  # check for blank cells in Truth worksheet
  errorMsg <- ""
  for (i in 1:5) {
    if (any(is.na(truthTable[, i]))) {
      # each blank Excel cell is returned as NA
      # blank lines in Excel sheet are ignored i.e. skipped, as if they were not there
      naLines <- which(is.na(truthTable[, i])) + 1
      errorMsg <- paste0(errorMsg, "\nThere are empty cells at line(s) ", 
                         paste(naLines, collapse = ", "), " in the TRUTH table.")
    }
  }
  if (errorMsg != "") stop(errorMsg)
  
  for (i in 1:3)
    if (any(is.na(suppressWarnings(as.numeric(as.character(truthTable[, i])))))) {
      suppressWarnings({naLines <- which(is.na(as.numeric(as.character(truthTable[, i])))) + 1})
      if (i == 1) errorMsg <- paste0(errorMsg, 
                                     "\nThere are non-integer values(s) for caseID at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
      if (i == 2) errorMsg <- paste0(errorMsg, 
                                     "\nThere are non-integer values(s) for LessionID at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
      if (i == 3) errorMsg <- paste0(errorMsg, 
                                     "\nThere are non-numeric values(s) for Weights at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
    }
  if (errorMsg != "") stop(errorMsg)
  
  if (any(!is.wholenumber(as.numeric(truthTable[[1]])))) stop("Non-integer values in Truth worksheet column 1")
  if (any(!is.wholenumber(as.numeric(truthTable[[2]])))) stop("Non-integer values in Truth worksheet column 2")
  if (any(!is.double(as.numeric(truthTable[[3]])))) stop("Non-floating point values in Truth worksheet column 3")
  
  # code to check for sequential lesionIDs in Truth sheet: 0,0,1,2,0,1,2,3,0,1 etc
  # normal case lesionIDS are all 0
  # for each abnormal case, the lesionID starts from 1 and works up, sequentially, to number of lesions on the case
  # a case can start abruptly wiht lesionID = 0 or 1, but not with lesionID = 2 or more
  # if it starts with lesionID = 2 or more, the previous one must be one less, i.e., sequential
  t <- as.numeric(truthTable$LesionID) # at this stage the cells in truthTable could be characters, 
  # which would break the following code; hence we convert to numerics; the lesionID field is convertible
  # to integers, even if entered as characters; if not there is an error in the data file
  for (k in 1:length(t)) {
    if (t[k] %in% c(0,1)) next else {
      if (t[k] != (t[k-1] + 1)) {
        errorMsg <- paste0(errorMsg, "\nNon-sequential lesionID encountered at line(s) ",
                           paste(k + 1, collapse = ", "), " in the TRUTH table.")
      }
    }
  }
  if (errorMsg != "") stop(errorMsg)
  
}





checkTruthTable <- function (truthTable, lrocForcedMark) 
{
  
  preCheck4BadEntries (truthTable)
  
  type <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  if (design == "CROSSED") design <- "FCTRL"
  if (!(type %in% c("FROC", "ROC", "LROC"))) stop("Unsupported data type: must be ROC, FROC or LROC.\n")
  # if (!(design %in% c("FCTRL"))) stop("Study design must be FCTRL.\n")
  
  if (type == "LROC") {
    if (is.na(lrocForcedMark)) stop("For LROC dataset one must set the lrocForcedMark flag to a logical") 
  } else {
    if (!is.na(lrocForcedMark)) stop("For non-LROC dataset one cannot set the lrocForcedMark flag to a logical") 
  }
  
  df <- truthTable[1:5]
  df["caseLevelTruth"] <- (truthTable$LesionID > 0)
  ########################################################
  # sort the TRUTH sheet of the Excel worksheet on the lesionID field
  # this puts normal cases first, regardless of how they are entered
  ########################################################
  truthTableSort <- df[order(df$caseLevelTruth),]
  
  caseIDCol <- as.integer(truthTable$CaseID)
  # TBA need a note on use of indx, why it is not used for readerID, etc.
  lesionIDCol <- as.integer(truthTable$LesionID)
  weightsCol <- as.numeric(truthTable$Weight)
  # readerIDCol <- truthTable$ReaderID # temp for testing non-character input
  # modalityIDCol <- truthTable$ModalityID# do:
  # bug non-character input error for HUGE dataset
  readerIDCol <- as.character(truthTable$ReaderID) # bug fix to avoid non-character input error below
  modalityIDCol <- as.character(truthTable$ModalityID) # do:
  # 
  L <- length(truthTable$CaseID) # length in the Excel sheet
  for (i in 1:5) if ((length(truthTable[[i]])) != L) stop("Cols of unequal length in Truth Excel worksheet")  
  
  normalCases <- sort(unique(caseIDCol[lesionIDCol == 0]))
  abnormalCases <- sort(unique(caseIDCol[lesionIDCol > 0]))
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  if (design == "FCTRL") {
    # preserve the strings; DO NOT convert to integers
    J <- length(strsplit(readerIDCol[1], split = ",")[[1]]) # bug non-character input error for HUGE dataset
    rdrArr <- array(dim = c(L,J))
    for (l in 1:L) {
      val <- strsplit(readerIDCol[l], split = ",|\\s")[[1]]
      val <- val[val != ""]
      for (i in 1:length(val)) {
        rdrArr[l,i] <- val[i]
      }
      # preserve the strings; DO NOT convert to integers
      I <- length(strsplit(modalityIDCol[1], split = ",")[[1]])
      trtArr <- array(dim = c(L,I))
      for (l in 1:L) {
        val <- strsplit(modalityIDCol[l], split = ",|\\s")[[1]]
        val <- val[val != ""]
        for (i in 1:length(val)) {
          trtArr[l,i] <- val[i]
        }
      }
    }
  } else stop("incorrect design value")
  
    if (any(is.na(rdrArr))) stop("Illegal value in ReaderID column in Truth sheet")
    rdrArr1D <- as.vector(unique(rdrArr)) # rdrArr is 2-dimensional; rdrArr1D is a one-dimensional array of all the readers in the study
  if (any(is.na(trtArr))) stop("Illegal value in ModalityID column in Truth sheet")
  trtArr1D <- as.vector(unique(trtArr))
  
  I <- length(trtArr1D)
  J <- length(rdrArr1D)
  
  truthTableStr <- array(dim = c(I, J, K, max(lesionIDCol)+1)) 
  for (l in 1:L) {
    k <- which(unique(truthTableSort$CaseID) == truthTable$CaseID[l])
    el <- lesionIDCol[l] + 1
    if (design == "FCTRL") {
      i <- which(unique(trtArr) == trtArr[l,])
      j <- which(rdrArr1D == rdrArr[l,])
      truthTableStr[i, j, k, el] <- 1
    } else stop("incorrect study design")
  }
  
  perCase <- as.vector(table(caseIDCol[caseIDCol %in% abnormalCases]))
  weights <- array(dim = c(K2, max(perCase)))
  IDs <- array(dim = c(K2, max(perCase)))
  
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  for (k2 in 1:K2) {
    k <- which(caseIDCol == abnormalCases[k2])
    IDs[k2, ] <- c(sort(lesionIDCol[k]), 
                   rep(UNINITIALIZED, max(perCase) - length(k)))
    if (all(weightsCol[k] == 0)) {
      weights[k2, 1:length(k)] <- 1/perCase[k2]
    } else {
      weights[k2, ] <- as.numeric(c(weightsCol[k][order(lesionIDCol[k])], 
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
  
  return (list(
    rdrArr1D = rdrArr1D,
    trtArr1D = trtArr1D,
    truthTableSort = truthTableSort,
    truthTableStr = truthTableStr,
    type = type,
    design = design,
    caseID = caseIDCol,
    perCase = perCase,
    lesionIDCol = lesionIDCol,
    IDs = IDs,
    weights = weights,
    normalCases = normalCases,
    abnormalCases = abnormalCases
  ))
  
}





ReadJAFROCNewFormat <- function(fileName, lrocForcedMark, sequentialNames) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  # wb <- loadWorkbook(fileName) # openxlsx
  wb <- excel_sheets(fileName)   # readxl
  temp <- sort(toupper(wb))
  if (!(temp[1] %in% c("FP", "NL"))) stop("FP or NL sheet not found\n")
  if (!(temp[2] %in% c("TP", "LL"))) stop("TP or LL sheet not found\n")
  if (!(temp[3] %in% c("TRUTH"))) stop("Truth sheet not found\n")
  sheetNames <- toupper(wb) 
  
  ########################## CHECK TRUTH TABLE ##############################
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is, i.e., 1st, 2nd or 3rd tab position in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (length(truthFileIndex) == 0) stop("TRUTH table worksheet cannot be found in the Excel file.")
  truthTable <- data.frame( read_xlsx(fileName, truthFileIndex, range = cell_cols(1:6) ) )
  if (length(truthTable) != 6) stop("Old Excel format file encountered; cannot use newExcelFileFormat = TRUE")
  cTT <- checkTruthTable(truthTable, lrocForcedMark) # cTT = checkTruthTable
  
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
  NLTable <- data.frame(read_xlsx(fileName, nlFileIndex, range=cell_cols(1:4)))
  
  # check column names
  if (is.null(NLTable$ReaderID)) stop ("Check FP or NL worksheet column names: should be ReaderID\n")
  if (is.null(NLTable$ModalityID)) stop ("Check FP or NL worksheet column names: should be ModalityID\n")
  if (is.null(NLTable$CaseID)) stop ("Check FP or NL worksheet column names: should be CaseID\n")
  if (is.null(NLTable$FP_Rating)) stop ("Check FP or NL worksheet column names: should be FP_Rating\n")
  
  # Issue 89
  NLTable <- NLTable[order(NLTable$ModalityID, NLTable$ReaderID, NLTable$CaseID, NLTable$FP_Rating),]
  
  
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
  LLTable <- data.frame(read_xlsx(fileName, llFileIndex, range = cell_cols(1:5) ))
  
  # check column names
  if (is.null(LLTable$ReaderID)) stop ("Check TP or LL worksheet column names: should be ReaderID\n")
  if (is.null(LLTable$ModalityID)) stop ("Check TP or LL worksheet column names: should be ModalityID\n")
  if (is.null(LLTable$CaseID)) stop ("Check TP or LL worksheet column names: should be CaseID\n")
  if (is.null(LLTable$LesionID)) stop ("Check TP or LL worksheet column names: should be LesionID\n")
  if (is.null(LLTable$TP_Rating)) stop ("Check TP or LL worksheet column names: should be TP_Rating\n")
  
  # Issue 89
  LLTable <- LLTable[order(LLTable$ModalityID, LLTable$ReaderID, LLTable$CaseID, LLTable$TP_Rating),]
  
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
  # following  to preserve ordering does not work as names are prededed with Rdr
  # readerIDUnique <- as.character(sort(unique(as.integer(c(NLReaderIDCol, LLReaderIDCol)))))
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
    if (K1 != 0) {
      # this gives 0,1,2,..,max num of lesions
      # which includes zero, hence the minus 1
      el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l]) - 1
    } else {
      # this gives 1,2,..,max num of lesions
      # which does not include zero, hence no minus 1
      el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l])
    }
    tt2 <- truthTableStr[i,j,k+K1,el+1]
    if (is.na(tt2)) next else {
      if (tt2 != 1)  stop("Error in reading LL/TP table") else 
        # the is.na() check ensures that an already recorded mark is not overwritten
        if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
    }
    # if (is.na(tt2)) stop("Error in reading LL/TP table") else {
    #   if (tt2 != 1)  stop("Error in reading LL/TP table") else 
    #     # the is.na() check ensures that an already recorded mark is not overwritten
    #     if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
    # }
  }
  
  LL[is.na(LL)] <- UNINITIALIZED
  weights[is.na(weights)] <- UNINITIALIZED
  lesionIDCol[is.na(lesionIDCol)] <- UNINITIALIZED
  
  if (type == "ROC" && design == "FCTRL") {
    if (!(((max(table(truthCaseID)) == 1) && (maxNL == 1)) 
          && (all((NL[, , (K1 + 1):K, ] == UNINITIALIZED))) 
          && (all((NL[, , 1:K1, ] != UNINITIALIZED)))
          && (all((LL[, , 1:K2, ] != UNINITIALIZED))))) {
      stop("This does not appear to be an ROC dataset: check TRUTH worksheet.")
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
  
  name <- NA
  if ((design == "FCTRL") || (design == "CROSSED")) design <- "FCTRL"
  
  if (type != "LROC") {
    # return the ROC or FROC dataset object
    return(convert2dataset(NL, LL, LL_IL = NA, 
                           perCase, IDs, weights,
                           fileName, type, name, truthTableStr, design,
                           modalityID, readerID))
  } else {
    # code added 6/11/21
    # handle LROC dataset here
    # move the ratings of diseased cases from NL array to LL_IL
    LL_IL <- NL[,,(K1+1):(K1+K2),,drop = F]
    # this completes the move of the ratings; replace the moved ratings
    # with negative infinities
    NL[,,(K1+1):(K1+K2),] <- -Inf
    # the following checks that if a case does not
    # appear in TP sheet it must appear in FP sheet, i.e., the forced mark in 
    # LROC paradigm; if it is not forced, then it is possible for a mark to not
    # appear in either TP or FP sheet, in which case the check is bypassed
    if (lrocForcedMark) {
      x1 <- LL_IL
      x2 <- LL
      if (any(which(x1 != -Inf) != which(x2 == -Inf))) stop("Error in LROC file")
    }
    # return the LROC dataset object
    return(convert2dataset(NL, LL, LL_IL, 
                           perCase, IDs, weights,
                           fileName, type, name, truthTableStr, design,
                           modalityID, readerID))
  }
} 



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
  
  #readerID <- as.character(sort(unique(c(NLReaderID, LLReaderID))))
  # to preserve ordering "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
  readerID <- as.character(sort(unique(as.integer(c(NLReaderID, LLReaderID)))))
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



