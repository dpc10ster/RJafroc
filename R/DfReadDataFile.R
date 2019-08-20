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
#' @param delimiter The string delimiter to be used for the \code{"MRMC"} 
#'    format ("," is the default), see \url{http://perception.radiology.uiowa.edu/}.
#'    This parameter is not used when reading \code{"JAFROC"} 
#'    or \code{"iMRMC"} data files.
#' @param sequentialNames A logical variable: if \code{TRUE}, consecutive integers 
#'    (starting from 1) will be used as the 
#'    treatment and reader IDs (i.e., names). Otherwise, treatment 
#'    and reader IDs in the original data file will be used.
#' @param splitPlot A logical variable, default \code{FALSE}, denoting a split plot design.
#'    If \code{TRUE} each reader interprets one case in all modalities. Currently only
#'    ROC dataset is supported. 
#' 
#' @return A dataset with the structure specified in \code{\link{RJafroc-package}}.
#' 
#' @examples
#' \donttest{
#' fileName <- system.file("extdata", "includedRocData.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' RocDataXlsx <- DfReadDataFile(fileName)
#' 
#' fileName <- system.file("extdata", "includedRocData.csv", 
#' package = "RJafroc", mustWork = TRUE)
#' RocDataCsv<- DfReadDataFile(fileName, format = "MRMC")
#' 
#' fileName <- system.file("extdata", "includedRocData.imrmc", 
#' package = "RJafroc", mustWork = TRUE)
#' RocDataImrmc<- DfReadDataFile(fileName, format = "iMRMC")
#' 
#' fileName <- system.file("extdata", "includedFrocData.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' FrocDataXlsx <- DfReadDataFile(fileName, sequentialNames = TRUE)
#' 
#' }
#' 
#' @importFrom tools file_ext
#' @importFrom stringr str_trim
#' @export

DfReadDataFile <- function (fileName, format = "JAFROC", delimiter = ",", sequentialNames = FALSE, splitPlot = FALSE) 
{
  if (format == "JAFROC") {
    if (!(file_ext(fileName) %in% c("xls", "xlsx"))) 
      stop("The extension of JAFROC data file must be \"*.xls\" or \"*.xlsx\" ")
    return(ReadJAFROC(fileName, sequentialNames, splitPlot))
  } else {
    if (splitPlot) 
      stop("splitPlot data entry is only possible with JAFROC (Excel) format.")
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

##
## added ReadJAFROCSplitPlot 07/10/2019 to read split plot data file
## later realized that the other function, formely called
## original ReadJAFROC is no longer needed and has been deleted
## Renamed ReadJAFROCSplitPlot to ReadJAFROC 
##  
ReadJAFROC <- function(fileName, sequentialNames, splitPlot) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  wb <- loadWorkbook(fileName)
  sheetNames <- toupper(names(wb))
  
  ###################### START CHECK TRUTH TABLE ############################
  ###################### NOTE THAT ORDER OF CHECKS IS IMPORTANT #############
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is, i.e., 
  # (1st, 2nd or 3rd tab position) in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (truthFileIndex == 0) 
    stop("TRUTH table cannot be found in the dataset.")
  TruthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:7)
  paradigm <- toupper(TruthTable[,6][which(!is.na(TruthTable[,6]))])
  if (!(paradigm %in% c("FROC", "ROC"))) stop("Unsupported paradigm.\n")
  design <- toupper(TruthTable[,7][which(!is.na(TruthTable[,7]))])
  if (!(design %in% c("CROSSED", "CROSSED"))) stop("Unsupported design.\n")
  retTruth <- checkTruthTable(TruthTable)
  Truth_CaseIDColumn <- retTruth$Truth_CaseIDColumn
  lesionVector <- retTruth$lesionVector
  normalCases <- retTruth$normalCases
  abnormalCases <- retTruth$abnormalCases
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  ###################### END CHECK TRUTH TABLE ##############################
  
  
  ###################### START CHECK NL TABLE$ ##############################
  nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  if (nlFileIndex == 0) 
    stop("FP/NL table cannot be found in the dataset.")
  NLTable <- read.xlsx(fileName, nlFileIndex, cols = 1:4)
  retNL <- checkNLTable(retTruth, NLTable)
  NLReaderID <- retNL$NLReaderID
  ###################### END CHECK NL TABLE #################################
  
  
  ###################### START CHECK NL TABLE ###############################
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (llFileIndex == 0) 
    stop("TP/LL table cannot be found in the dataset.")
  LLTable <- read.xlsx(fileName, llFileIndex, cols = 1:5)
  retLL <- checkLLTable(retTruth, retNL, LLTable)
  ###################### END CHECK LL TABLE #################################
  
  modalityID <- as.character(sort(unique(c(retNL$NLModalityID, retLL$LLModalityID))))
  I <- length(modalityID)
  
  readerID <- as.character(sort(unique(c(retNL$NLReaderID, retLL$LLReaderID))))
  J <- length(readerID)
  
  maxNL <- 0
  for (i in modalityID) {
    for (j in readerID) {
      k <- (retNL$NLModalityID == i) & (retNL$NLReaderID == j)
      if ((sum(k) == 0)) 
        next
      maxNL <- max(maxNL, max(table(retNL$NLCaseID[k])))
    }
  }
  
  allCases <- retTruth$allCases
  
  NL <- array(dim = c(I, J, K, maxNL))
  for (i in 1:I) {
    for (j in 1:J) {
      k <- (retNL$NLModalityID == modalityID[i]) & (retNL$NLReaderID == readerID[j])
      if ((sum(k) == 0)) 
        next
      caseNLTable <- table(retNL$NLCaseID[k])
      IDs <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
      for (k1 in 1:length(IDs)) {
        for (el in 1:caseNLTable[k1]) {
          NL[i, j, which(IDs[k1] == allCases), el] <- 
            retNL$NLRating[k][which(retNL$NLCaseID[k] == IDs[k1])][el]
        }
      }
    }
  }
  
  lesionIDLabels <- retTruth$lesionIDLabels # !!!DPC!!!
  LLLesionID <- retLL$LLLesionID
  LLCaseID <- retLL$LLCaseID
  LLRating <- retLL$LLRating
  
  NL[is.na(NL)] <- UNINITIALIZED
  LL <- array(dim = c(I, J, K2, max(lesionVector)))
  for (i in 1:I) {
    for (j in 1:J) {
      k <- (retLL$LLModalityID == modalityID[i]) & (retLL$LLReaderID == readerID[j])
      if ((sum(k) == 0)) next
      caseLLTable <- table(LLCaseID[k])
      IDs <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
      for (k1 in 1:length(IDs)) {
        for (el in 1:caseLLTable[k1]) {
          x1 <- which(IDs[k1] == abnormalCases)
          x2 <- which(LLCaseID[k] == IDs[k1])
          x3 <- lesionIDLabels[which(IDs[k1] == abnormalCases), ]
          x4 <- which(LLLesionID[k][x2][el] == x3)
          x5 <- which(LLCaseID[k] == IDs[k1])
          LL[i, j, x1, x4] <- LLRating[k][x5][el]
        }
      }
    }
  }
  
  LL[is.na(LL)] <- UNINITIALIZED
  
  #temp <- isCrossedRocDataset (TruthTable, NLTable, LLTable)
  
  if (isROCDataset(NL, LL, Truth_CaseIDColumn)) {
    fileType <- "ROC"
  } else if (isROIDataset(NL, LL, lesionVector)) {
    fileType <- "ROI" # not yet implemented
  } else if (splitPlot && isSplitPlotRocDataset (TruthTable, NLTable, LLTable)) {
    # since FROC is so general, must first eliminate a user-declared splitPlot dataset
    # isSplitPlotRocDataset checks for a valid splitPlot dataset 
    fileType <- "SplitPlotRoc"
    # the following is the most general data structure, all else having been eliminated
  } else  fileType <- "FROC"
  
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
              lesionVector = retTruth$lesionVector,
              lesionID = retTruth$lesionIDLabels, 
              lesionWeight = retTruth$lesionWeight, 
              dataType = fileType, 
              modalityID = modalityID, 
              readerID = readerID))
} 


checkTruthTable <- function (TruthTable) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  errorMsg <- ""
  for (i in 1:4) { # check for empty cells in Truth worksheet
    if (any(is.na(TruthTable[, i]))) errorMsg <- paste0(errorMsg, "check for empty cells in Truth sheet in column # ", i, "\n")
  }
  
  if (any(is.na(as.numeric(as.character(TruthTable[, 3]))))) {
    naLines <- which(is.na(as.numeric(as.character(TruthTable[, 3])))) + 1
    errorMsg <- paste0(errorMsg, "There are non-numeric values(s) for weights at line(s) ", 
                       paste(naLines, collapse = ", "), " in the TRUTH table.\n")
  }
  if (errorMsg != "") stop(errorMsg)
  
  errorMsg <- ""
  for (i in 1:2) { 
    # Truth_CaseIDColumn and Truth_LesionIDColumn checks
    # no empty cells in Truth worksheet
    # now check that Truth_CaseIDColumn and lesionIDs are integers, if not it stop
    # with an error message indicating which line is at fault
    ID <- ((as.numeric(as.character(TruthTable[, i])) %% 1) != 0) 
    if (any(ID)) {
      naLines <- which(ID) + 1 # to account for header row
      errorMsg <- paste0(errorMsg, "There are non-integer values(s) for CaseID and/or LesionID at line(s) ", 
                         paste(naLines, collapse = ", "), " in the TRUTH table.\n")
    }
    if (errorMsg != "") stop(errorMsg)
  }
  
  # having established Truth_CaseIDColumn and Truth_LesionIDColumn are integers
  # now assign the IDs of cases
  # with integers 1, 2, 3, ....
  Truth_CaseIDColumn <- as.integer(TruthTable[[1]])  # all 3 columns have same lengths
  Truth_LesionIDColumn <- as.integer(TruthTable[[2]])
  Truth_WeightColumn <- as.numeric(TruthTable[[3]])
  
  # asign ID or normal, abnormal and all cases
  # initialize K, K1 and K2
  normalCases <- sort(unique(Truth_CaseIDColumn[Truth_LesionIDColumn == 0]))
  abnormalCases <- sort(unique(Truth_CaseIDColumn[Truth_LesionIDColumn > 0]))
  allCases <- c(normalCases, abnormalCases)
  lesionVector <- as.vector(table(Truth_CaseIDColumn[Truth_CaseIDColumn %in% abnormalCases]))
  # !!!DPC!!! better name? nLesPerCase?
  
  # the following line checks that no Truth_CaseIDColumn, Truth_LesionIDColumn
  # combination is used more than once, and if
  # not stops with an error message indicating which line is at fault
  if (anyDuplicated(cbind(Truth_CaseIDColumn, Truth_LesionIDColumn, Truth_WeightColumn))) {
    naLines <- which(duplicated(cbind(Truth_CaseIDColumn, Truth_LesionIDColumn, Truth_WeightColumn))) + 1
    errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), 
                       " in the TRUTH table are duplicate(s) of previous line(s).")
    stop(errorMsg)
  }
  
  # this duplicates original Xuetong code below, but what the hell
  if (length(abnormalCases) == length(Truth_LesionIDColumn)) {
    # looks like single lesion per abn. case data, therefore
    # weights should be zero or one
    wghts <- as.numeric(as.character(TruthTable[, 3]))
    badWeights <- allCases[!(wghts %in% c(0,1))]
    if (length(badWeights) > 0) {
      errorMsg <- paste0("There are incorrect weight entries for CaseID(s) ",
                         paste(badWeights, collapse = ", "), " in the TRUTH table.")
      stop(errorMsg)
    }
  }
  
  # original Xuetong code
  # the following lines check that LesionWeights are valid numeric values:
  # i.e., either all zeroes or summing to one for abnormal cases
  # otherwise it stops with an error message indicating which line is at fault
  lesionWeight <- array(dim = c(length(abnormalCases), max(Truth_LesionIDColumn)))
  lesionIDLabels <- array(dim = c(length(abnormalCases), max(Truth_LesionIDColumn)))
  
  errorMsg <- ""
  for (k2 in 1:length(abnormalCases)) {
    k <- which(Truth_CaseIDColumn == abnormalCases[k2])
    lesionIDLabels[k2, ] <- c(sort(Truth_LesionIDColumn[k]), rep(UNINITIALIZED, max(Truth_LesionIDColumn) - length(k)))
    if (all(Truth_WeightColumn[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/Truth_LesionIDColumn[k2]
    } else {
      lesionWeight[k2, ] <- as.numeric(c(Truth_WeightColumn[k][order(Truth_LesionIDColumn[k])], 
                                         rep(UNINITIALIZED, max(Truth_LesionIDColumn) - length(k))))
      sumWeight <- sum(lesionWeight[k2, lesionWeight[k2, ] != UNINITIALIZED])
      if (sumWeight != 1){ # we accept a 1% error (either way) in entering weights that should add up to unity
        if (sumWeight <= 1.01 && sumWeight >= 0.99){ 
          lesionWeight[k2, ] <- lesionWeight[k2, ] / sumWeight
        }else{
          errorMsg <- paste0(errorMsg, "The sum of the Truth_WeightColumn for case# ", abnormalCases[k2], " is not 1.\n")
        }
      }
    }
  }
  if (errorMsg != "") stop(errorMsg)
  
  lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
  lesionIDLabels[is.na(lesionIDLabels)] <- UNINITIALIZED
  
  return (list(
    normalCases = normalCases,
    abnormalCases = abnormalCases,
    allCases = allCases,
    Truth_CaseIDColumn = Truth_CaseIDColumn,
    Truth_LesionIDColumn = Truth_LesionIDColumn,
    lesionVector = lesionVector,
    lesionIDLabels = lesionIDLabels,
    lesionWeight = lesionWeight
  ))
  
}



checkNLTable <- function (retTruth, NLTable) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  # check that CaseID, LesionID do occur in Truth sheet
  for (i in 1:4) {
    if (any(is.na(NLTable[, i]))) stop("check for empty cells in NL/FP sheet")
  }
  
  NLReaderID <- as.character(NLTable[[1]])
  NLModalityID <- as.character(NLTable[[2]])
  
  # check that CaseIDs in FP worksheet actually occur in the TRUTH sheet
  # and if not stop with the line number of offending cells
  
  Truth_CaseIDColumn <- retTruth$Truth_CaseIDColumn
  NLCaseID <- NLTable[[3]]
  if (any(!(NLCaseID %in% Truth_CaseIDColumn))) {
    naCases <- NLCaseID[which(!(NLCaseID %in% Truth_CaseIDColumn))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), 
                       " in the FP/NL table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  NLRating <- NLTable[[4]]
  
  return (list(
    NLReaderID = NLReaderID,
    NLModalityID = NLModalityID,
    NLCaseID = NLCaseID,
    NLRating = NLRating
  ))
  
}



checkLLTable <- function (retTruth, retNL, LLTable) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  # check that CaseID, LesionID and Ratings are all numeric values
  for (i in 3:5) {
    if (any(is.na(as.numeric(as.character(LLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(LLTable[, i])))) + 1
      errorMsg <- paste0("There are missing cell(s) at line(s) ", 
                         paste(naLines, collapse = ", "), " in the TP/LL table.")
      stop(errorMsg)
    }
  }
  
  LLReaderID <- as.character(LLTable[[1]])
  LLModalityID <- as.character(LLTable[[2]])
  Truth_CaseIDColumn <- retTruth$Truth_CaseIDColumn
  Truth_LesionIDColumn <- retTruth$Truth_LesionIDColumn
  
  # check that CaseID, LesionID do occur in Truth sheet
  errorMsg <- ""
  LLCaseID <- LLTable[[3]]
  LLLesionID <- LLTable[[4]]
  if (!all(LLCaseID %in% retTruth$abnormalCases)) stop("Check LL/TP sheet for non-diseased cases\n")
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((Truth_CaseIDColumn == LLCaseID[i]) & (Truth_LesionIDColumn == LLLesionID[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0(errorMsg, "\nLL sheet errors:\nModality ", LLTable[i, 2], "\nReader ", 
                         LLTable[i, 1], "\nCase ", LLTable[i, 3], 
                         "\nLesion ", LLTable[i, 4], "\ncannot be found in TRUTH table.\n\n")
    }
  }
  if (errorMsg != "") stop(errorMsg)
  
  LLRating <- LLTable[[5]]
  LLRating[is.na(LLRating)] <- UNINITIALIZED
  
  if (anyDuplicated(LLTable[, 1:4])) {
    naLines <- which(duplicated(LLTable[, 1:4]))
    errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), 
                       " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), 
                       " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
                       paste(LLTable[naLines, 4], collapse = ", "), 
                       " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  return (list(
    LLReaderID = LLReaderID,
    LLModalityID = LLModalityID,
    LLCaseID = LLCaseID,
    LLLesionID = LLLesionID,
    LLRating = LLRating
  ))
  
}



# still working on this
isCrossedRocDataset <- function(TruthTable, NLTable, LLTable)
{
  # examine TRUTH worksheet 
  readerColumn <- strsplit(TruthTable$ReaderID, split = ",", fixed = TRUE)
  readerID <- unique(readerColumn) 
  K <- length(readerColumn)# total number of cases read by all readers
  J <- length(readerID[[1]]) # number of readers
  
  rdrColInt <- scan(text = TruthTable$ReaderID, sep = ",", quiet = TRUE)
  dim(rdrColInt) <- c(J, length(rdrColInt)/J)
  for (k in 1:K) { 
    # this shows user intends this to be a fully crossed file
    if (!all(readerColumn[[k]] == readerColumn[[1]])) return (FALSE)
  }
  if (length(TruthTable[[1]]) != K)  return (FALSE) # total number of cases read by all readers
  
  # examine NL worksheet 
  modalityID <- unique(NLTable[[2]])
  I <- length(modalityID) # number of modalities
  Kj <- array(dim = J) # total number of cases read by reader j
  K1j <- array(dim = J) # total number of normal cases read by reader j
  K2j <- array(dim = J) # total number of abnormal cases read by reader j
  Knested <- array(dim = c(J, K)) # IDs of cases read by reader j
  K1nested <- array(dim = c(J, K)) # IDs of normal cases read by reader j
  K2nested <- array(dim = c(J, K)) # IDs of abnormal cases read by reader j
  for (j in 1:J) {
    temp <- TruthTable[[1]][which(TruthTable[[4]] == j)]
    temp1 <- TruthTable[[1]][which((TruthTable[[4]] == j) & (TruthTable[[2]] == 0))]
    temp2 <- TruthTable[[1]][which((TruthTable[[4]] == j) & (TruthTable[[2]] == 1))]
    Kj[j] <- length(temp)
    K1j[j] <- length(temp1)
    K2j[j] <- length(temp2)
    Knested[j,1:Kj[j]] <- temp
    K1nested[j,1:K1j[j]] <- temp1
    K2nested[j,1:K2j[j]] <- temp2
  }
  
  for (j in 1:J) {
    cat("\nj = ", j, "\n")
    cat("Cases, non-diseased followed by diseased", "\n")
    cat(K1nested[j,1:K1j[j]],"\n")
    cat(K2nested[j,1:K2j[j]],"\n")
    cat("Non-diseased ratings, in modality 1 followed by modality 2", "\n")
    cat(NLTable[[4]][which((NLTable[[1]] == j) & (NLTable[[3]] %in% K1nested[j,]) & (NLTable[[2]] == 1))],"\n")
    cat(NLTable[[4]][which((NLTable[[1]] == j) & (NLTable[[3]] %in% K1nested[j,]) & (NLTable[[2]] == 2))],"\n")
    cat("Diseased ratings, in modality 1 followed by modality 2", "\n")
    cat(LLTable[[5]][which((LLTable[[1]] == j) & (LLTable[[3]] %in% K2nested[j,]) & (LLTable[[2]] == 1))],"\n")
    cat(LLTable[[5]][which((LLTable[[1]] == j) & (LLTable[[3]] %in% K2nested[j,]) & (LLTable[[2]] == 2))],"\n")
  }
  # inspect NL table
  
  # tests for ROI data
  if (!all(is.finite(NLTable[[4]]))) return (FALSE)
  if (!all(is.finite(LLTable[[5]]))) return (FALSE)
  for (j in 1:J) {
    if (!(sum(NLTable[[3]] %in% K1nested[j,1:K1j[j]]) == I*K1j[j])) return (FALSE)
    if (!(sum(LLTable[[3]] %in% K2nested[j,1:K2j[j]]) == I*K2j[j])) return (FALSE)
    if (!(nrow(NLTable) == I*sum(K1j))) return (FALSE)
    if (!(nrow(LLTable) == I*sum(K2j))) return (FALSE)
  }
  return (TRUE)
}


isSplitPlotRocDataset <- function(TruthTable, NLTable, LLTable)
{
  # UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  modalityID <- unique(NLTable[[2]])
  readerID <- unique(TruthTable[[4]])
  I <- length(modalityID) # number of modalities
  J <- length(readerID) # number of readers
  K <- length(TruthTable[[1]]) # total number of cases read by all readers
  Kj <- array(dim = J) # total number of cases read by reader j
  K1j <- array(dim = J) # total number of normal cases read by reader j
  K2j <- array(dim = J) # total number of abnormal cases read by reader j
  Knested <- array(dim = c(J, K)) # IDs of cases read by reader j
  K1nested <- array(dim = c(J, K)) # IDs of normal cases read by reader j
  K2nested <- array(dim = c(J, K)) # IDs of abnormal cases read by reader j
  for (j in 1:J) {
    temp <- TruthTable[[1]][which(TruthTable[[4]] == j)]
    temp1 <- TruthTable[[1]][which((TruthTable[[4]] == j) & (TruthTable[[2]] == 0))]
    temp2 <- TruthTable[[1]][which((TruthTable[[4]] == j) & (TruthTable[[2]] == 1))]
    Kj[j] <- length(temp)
    K1j[j] <- length(temp1)
    K2j[j] <- length(temp2)
    Knested[j,1:Kj[j]] <- temp
    K1nested[j,1:K1j[j]] <- temp1
    K2nested[j,1:K2j[j]] <- temp2
  }
  
  for (j in 1:J) {
    cat("\nj = ", j, "\n")
    cat("Cases, non-diseased followed by diseased", "\n")
    cat(K1nested[j,1:K1j[j]],"\n")
    cat(K2nested[j,1:K2j[j]],"\n")
    cat("Non-diseased ratings, in modality 1 followed by modality 2", "\n")
    cat(NLTable[[4]][which((NLTable[[1]] == j) & (NLTable[[3]] %in% K1nested[j,]) & (NLTable[[2]] == 1))],"\n")
    cat(NLTable[[4]][which((NLTable[[1]] == j) & (NLTable[[3]] %in% K1nested[j,]) & (NLTable[[2]] == 2))],"\n")
    cat("Diseased ratings, in modality 1 followed by modality 2", "\n")
    cat(LLTable[[5]][which((LLTable[[1]] == j) & (LLTable[[3]] %in% K2nested[j,]) & (LLTable[[2]] == 1))],"\n")
    cat(LLTable[[5]][which((LLTable[[1]] == j) & (LLTable[[3]] %in% K2nested[j,]) & (LLTable[[2]] == 2))],"\n")
  }
  # inspect NL table
  
  # tests for ROI data
  if (!all(is.finite(NLTable[[4]]))) return (FALSE)
  if (!all(is.finite(LLTable[[5]]))) return (FALSE)
  for (j in 1:J) {
    if (!(sum(NLTable[[3]] %in% K1nested[j,1:K1j[j]]) == I*K1j[j])) return (FALSE)
    if (!(sum(LLTable[[3]] %in% K2nested[j,1:K2j[j]]) == I*K2j[j])) return (FALSE)
    if (!(nrow(NLTable) == I*sum(K1j))) return (FALSE)
    if (!(nrow(LLTable) == I*sum(K2j))) return (FALSE)
  }
  return (TRUE)
}

isROCDataset <- function(NL, LL, Truth_CaseIDColumn)
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- K - K2
  maxNL <- length(NL[1,1,1,])
  maxLL <- length(LL[1,1,1,])
  
  if (max(table(Truth_CaseIDColumn)) != 1) return (FALSE) # number of occurrences of each Truth_CaseIDColumn value
  if (maxNL != 1) return (FALSE)
  if (all((NL[, , (K1 + 1):K, ] != UNINITIALIZED))) return (FALSE) 
  if (any((NL[, , 1:K1, ] == UNINITIALIZED))) return (FALSE) 
  if (maxLL != 1) return (FALSE)
  if (any((LL[, , 1:K2, ] == UNINITIALIZED))) return (FALSE)
  return (TRUE)
}

##stop("ROI paradigm not yet implemented")
## !!!DPC!!!
isROIDataset <- function(NL, LL, Truth_LesionIDColumn)
{
  return (FALSE)
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  I <- length(NL[,1,1,1])
  J <- length(NL[1,,1,1])
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- K - K2
  maxNL <- length(NL[1,1,1,])
  lesionVector <- array(dim = K2)
  for (k in 1:K2) {
    lesionVector[k] <- sum(is.finite(LL[1,1,k,]))
  }
  
  isROI <- TRUE
  for (i in 1:I) {
    for (j in 1:J) {
      # in following code, non-diseased cases with non-diseased 
      # ROIs (i.e., -Infs) are counted
      # On such cases, all ROIs are marked and one does not expect any missing 
      # entries; if a missing entry is found, then dataset is not ROI.
      # As an example, in includedRoiData.xlsx all caseIDs < 51 correspond to non-diseased cases
      # For each such case, one has 4 ratings, i.e., Q = 4, assumed constant for all 
      # cases
      if (any(NL[i, j, 1:K1, ] == UNINITIALIZED)) {
        isROI <- FALSE
        break
      }
      # in following code, diseased cases with diseased 
      # ROIS (i.e., not = -Infs) are counted
      # These must sum to the lesionVector for that case
      # note: this test may be redundant with above definition:
      #  lesionVector[k] <- sum(is.finite(LL[1,1,k,]))
      temp <- LL[i, j, , ] != UNINITIALIZED
      dim(temp) <- c(K2, max(lesionVector))
      if (!all(lesionVector == rowSums(temp))) {
        isROI <- FALSE
        break
      }
      # in following code, diseased cases (dc) with non-diseased 
      # (nd) ROIs (i.e., -Infs) are counted
      temp <- NL[i, j, (K1 + 1):K, ] == UNINITIALIZED # this is the array of nd counts on dcs
      dim(temp) <- c(K2, maxNL)
      if (!all(lesionVector == rowSums(temp))) { 
        # sum of nd counts on dcs must equal that of diseased counts on
        # dcs (each empty count makes space for a LL count so total number of marks
        # per case is the same and equal to 4 for this dataset)
        # As an example, on FP sheet, for case 57, i = j = 1, there are three rated regions:
        # NL[1,1,57,]
        # [1] -0.967084 -0.394129  0.264821      -Inf
        # This means that there can be only one lesion on this case: 
        # lesionVector[7]
        # [1] 1 
        # and this diseased case can (rather must) have only one LL mark:
        # LL[1,1,7,]
        # [1] 0.535298     -Inf     -Inf     -Inf
        isROI <- FALSE
        break
      }
    }
  }
  return (isROI)
}


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
              lesionVector = lesionVector, 
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


