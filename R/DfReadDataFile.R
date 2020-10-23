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



# SPLIT-PLOT-A: Reader nested within test; Hillis 2014 Table VII part (a)
# SPLIT-PLOT-C: Case nested within reader; Hillis 2014 Table VII part (c)
checkTruthTable <- function (truthTable) 
{
  
  preCheck4BadEntries (truthTable)
  
  type <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  if (design == "CROSSED") design <- "FCTRL"
  if (!(type %in% c("FROC", "ROC"))) stop("Unsupported data type: must be ROC or FROC.\n")
  if (!(design %in% c("FCTRL", "SPLIT-PLOT-A", "SPLIT-PLOT-C"))) stop("Study design must be FCTRL, SPLIT-PLOT-A or SPLIT-PLOT-C\n")
  
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
  readerIDCol <- truthTable$ReaderID
  modalityIDCol <- truthTable$ModalityID
  L <- length(truthTable$CaseID) # length in the Excel sheet
  # stop if only one reader; all split plot designs must have more than one reader
  if (length(readerIDCol) == L) stop("cannot handle one reader case with newExcelFormat")
  for (i in 1:5) if ((length(truthTable[[i]])) != L) stop("Cols of unequal length in Truth Excel worksheet")  
  
  normalCases <- sort(unique(caseIDCol[lesionIDCol == 0]))
  abnormalCases <- sort(unique(caseIDCol[lesionIDCol > 0]))
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  if (design == "SPLIT-PLOT-A") {
    # for this design the length is twice what it needs to be
    caseIDCol <- as.integer(truthTable$CaseID)[1:(L/2)]
    # lesionIDCol <- as.integer(truthTable$LesionID)[1:(L/2)]
    weightsCol <- truthTable$Weight[1:(L/2)]
    # preserve the strings; DO NOT convert to integers
    J <-  0 # find max number of readers, given that his data has 3 readers in one group and 4 in the other group
    for (el in 1:length(readerIDCol)) {
      if (length(strsplit(readerIDCol[el], split = ",")[[1]]) > J) J <- length(strsplit(readerIDCol[el], split = ",")[[1]])
    }
    rdrArr <- array(dim = c(L,J))
    for (l in 1:L) {
      val <- strsplit(readerIDCol[l], split = ",|\\s")[[1]]
      val <- val[val != ""]
      for (i in 1:length(val)) {
        rdrArr[l,i] <- val[i]
      }
    }
    # preserve the strings; DO NOT convert to integers
    I <- length(strsplit(modalityIDCol[1], split = ",")[[1]])
    trtArr <- array(dim = c(L,I))
    for (l in 1:L) {
      if (grep("^\\(.\\)", modalityIDCol[l]) == 1) { # match found to something like (1), i.e., one nested factor
        val <- grep("^\\(.\\)", modalityIDCol[l], value = T)
        val <- strsplit(val, split = "\\(|\\)")[[1]]
        val <- val[val != ""]
        for (i in 1:length(val)) {
          trtArr[l] <- val[i]
        }
      } else stop("Was expecting nested notation, using () brackets ...")
    }
  } else if (design == "SPLIT-PLOT-C") {
    # preserve the strings; DO NOT convert to integers
    J <- length(strsplit(readerIDCol[1], split = ",")[[1]])
    rdrArr <- array(dim = c(L,J))
    for (l in 1:L) {
      if (grep("^\\(.\\)", readerIDCol[l]) == 1) { # match found to something like (1), i.e., one nested factor
        val <- grep("^\\(.\\)", readerIDCol[l], value = T)
        val <- strsplit(val, split = "\\(|\\)")[[1]]
        val <- val[val != ""]
        for (i in 1:length(val)) {
          rdrArr[l] <- val[i]
        }
      } else stop("Was expecting nested notation, using () brackets ...")
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
  } else if (design == "FCTRL") {
    # preserve the strings; DO NOT convert to integers
    J <- length(strsplit(readerIDCol[1], split = ",")[[1]])
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
  
  if (design == "SPLIT-PLOT-A") {
    rdrArr1D <- t(unique(rdrArr)) # rdrArr is 2-dimensional; rdrArr1D is a one-dimensional array of all the readers in the study
    rdrArr1D <- rdrArr1D[!is.na(rdrArr1D)] # this modification is needed for HYK dataset with 3 readers in one group and 4 in the other
  } else {
    if (any(is.na(rdrArr))) stop("Illegal value in ReaderID column in Truth sheet")
    rdrArr1D <- as.vector(unique(rdrArr)) # rdrArr is 2-dimensional; rdrArr1D is a one-dimensional array of all the readers in the study
  }
  if (any(is.na(trtArr))) stop("Illegal value in ModalityID column in Truth sheet")
  trtArr1D <- as.vector(unique(trtArr))
  
  I <- length(trtArr1D)
  J <- length(rdrArr1D)
  
  truthTableStr <- array(dim = c(I, J, K, max(lesionIDCol)+1)) 
  for (l in 1:L) {
    k <- which(unique(truthTableSort$CaseID) == truthTable$CaseID[l])
    el <- lesionIDCol[l] + 1
    if (design == "SPLIT-PLOT-A") {
      i <- which(unique(trtArr) == trtArr[l])
      for (j1 in 1:length(rdrArr[l,])) {
        j <- which(rdrArr1D == rdrArr[l,j1])
        truthTableStr[i, j, k, el] <- 1
      }
    }
    else if (design == "SPLIT-PLOT-C") {
      i <- which(unique(trtArr) == trtArr[l,])
      j <- which(rdrArr1D == rdrArr[l])
      truthTableStr[i, j, k, el] <- 1
    } else if (design == "FCTRL") {
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


