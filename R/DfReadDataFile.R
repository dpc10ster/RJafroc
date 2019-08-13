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
#' @param renumber A logical variable: if \code{TRUE}, consecutive integers 
#'    (starting from 1) will be used as the 
#'    treatment and reader IDs. Otherwise, treatment 
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
#' FrocDataXlsx <- DfReadDataFile(fileName, renumber = TRUE)
#' 
#' }
#' 
#' @importFrom tools file_ext
#' @importFrom stringr str_trim
#' 
#' @export

DfReadDataFile <- function(fileName, format = "JAFROC", delimiter = ",", renumber = FALSE, splitPlot = FALSE) {
  if (format == "JAFROC") {
    if (!(file_ext(fileName) %in% c("xls", "xlsx"))) 
      stop("The extension of JAFROC data file must be \"*.xls\" or \"*.xlsx\" ")
    return(ReadJAFROC(fileName, renumber, splitPlot))
  } else if (format == "iMRMC") {
    return(ReadImrmc(fileName, renumber))
  } else if (format == "MRMC") {
    if (file_ext(fileName) == "lrc") {
      return(ReadLrc(fileName, renumber))
    } else {
      return(ReadOrDbmMrmc(fileName, delimiter, renumber))
    }
  } else {
    errMsg <- sprintf("%s is not an available file format.", format)
    stop(errMsg)
  }
} 

##
## added this 07/10/2019 to read split plot data file
## later realized that the other function, formely called
## ReadJAFROC is no longer needed
## Renamed ReadJAFROCSplitPlot to ReadJAFROC 
##  
ReadJAFROC <- function(fileName, renumber, splitPlot) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  wb <- loadWorkbook(fileName)
  sheetNames <- toupper(names(wb))
  
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is (1st, 2nd or 3rd tab position) in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (truthFileIndex == 0) 
    stop("TRUTH table cannot be found in the dataset.")
  truthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:4)
  readerColumn <- scan(text = truthTable$ReaderID, sep = ",", quiet = TRUE)
  rdrs <- unique(readerColumn); nrdrs <- length(rdrs)
  dim(readerColumn) <- c(nrdrs, length(readerColumn)/nrdrs)
  
  ## fill empty rows with NAs (and delete them)
  for (i in 1:4){
    truthTable[grep("^\\s*$", truthTable[ , i]), i] <- NA
  }
  
  ## delete empty rows
  naRows <- colSums(is.na(truthTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      truthTable <- truthTable[1:(nrow(truthTable) - max(naRows)), ]
    }
  }
  
  for (i in 1:2) {
    if (any((as.numeric(as.character(truthTable[, i]))) %% 1 != 0 )) {
      naLines <- which(!is.integer(as.numeric(as.character(truthTable[, i])))) + 1
      errorMsg <- paste0("There are non-integer values(s) for CaseID or LesionID at the line(s) ", 
                         paste(naLines, collapse = ", "), " in the TRUTH table.")
      stop(errorMsg)
    }
  }
  
  if (any(is.na(as.numeric(as.character(truthTable[, 3]))))) {
    naLines <- which(is.na(as.numeric(as.character(truthTable[, 3])))) + 1
    errorMsg <- paste0("There are non-numeric values(s) for truthWeights at the line(s) ", 
                       paste(naLines, collapse = ", "), " in the TRUTH table.")
    stop(errorMsg)
  }
  
  truthCaseID <- as.integer(truthTable[[1]])  # all 3 columns have same lengths
  truthlesionID <- as.integer(truthTable[[2]])
  truthWeights <- truthTable[[3]]
  #truthReaderID <- as.character(truthTable[[4]])
  
  normalCases <- sort(unique(truthCaseID[truthlesionID == 0]))
  abnormalCases <- sort(unique(truthCaseID[truthlesionID > 0]))
  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  if (anyDuplicated(cbind(truthCaseID, truthlesionID))) {
    naLines <- which(duplicated(cbind(truthCaseID, truthlesionID))) + 1
    errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), 
                       " in the TRUTH table are duplicated with previous line(s) .")
    stop(errorMsg)
  }
  
  nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  if (nlFileIndex == 0) 
    stop("FP/NL table cannot be found in the dataset.")
  NLTable <- read.xlsx(fileName, nlFileIndex, cols = 1:4)
  
  ## fill empty rows with NAs (and delete them)
  for (i in 1:4){
    NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
  }
  
  ## delete empty rows
  naRows <- colSums(is.na(NLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      NLTable <- NLTable[1:(nrow(NLTable) - max(naRows)), ]
    }
  }
  
  for (i in 3:4) {
    if (any(is.na(as.numeric(as.character(NLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(NLTable[, i])))) + 1
      errorMsg <- paste0("There are missing cell(s) at line(s) ", 
                         paste(naLines, collapse = ", "), " in the FP table.")
      stop(errorMsg)
    }
  }
  
  NLReaderID <- as.character(NLTable[[1]])
  
  NLModalityID <- as.character(NLTable[[2]])
  
  NLCaseID <- NLTable[[3]]
  if (any(!(NLCaseID %in% truthCaseID))) {
    naCases <- NLCaseID[which(!(NLCaseID %in% truthCaseID))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), 
                       " in the FP/NL table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  NLRating <- NLTable[[4]]
  
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (llFileIndex == 0) 
    stop("TP/LL table cannot be found in the dataset.")
  LLTable <- read.xlsx(fileName, llFileIndex, cols = 1:5)
  
  ## fill empty rows with NAs (and delete them)
  for (i in 1:5){
    LLTable[grep("^\\s*$", LLTable[ , i]), i] <- NA
  }
  
  ## delete empty rows
  naRows <- colSums(is.na(LLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      LLTable <- LLTable[1:(nrow(LLTable) - max(naRows)), ]
    }
  }
  
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
  
  LLCaseID <- LLTable[[3]]
  LLLesionID <- LLTable[[4]]
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseID[i]) & (truthlesionID == LLLesionID[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], " Reader(s) ", 
                         LLTable[i, 1], " Case(s) ", LLTable[i, 3], 
                         " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  LLRating <- LLTable[[5]]
  
  if (anyDuplicated(LLTable[, 1:4])) {
    naLines <- which(duplicated(LLTable[, 1:4]))
    errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), 
                       " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), 
                       " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
                       paste(LLTable[naLines, 4], collapse = ", "), 
                       " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  lesionVector <- as.vector(table(truthCaseID[truthCaseID %in% abnormalCases]))
  # for (k2 in 1:length(abnormalCases)) { lesionVector[k2] <- sum(truthCaseID == abnormalCases[k2]) }
  
  lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector)))
  lesionIDTable <- array(dim = c(length(abnormalCases), max(lesionVector)))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(truthCaseID == abnormalCases[k2])
    lesionIDTable[k2, ] <- c(sort(truthlesionID[k]), rep(UNINITIALIZED, max(lesionVector) - length(k)))
    if (all(truthWeights[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/lesionVector[k2]
    } else {
      lesionWeight[k2, ] <- c(truthWeights[k][order(truthlesionID[k])], rep(UNINITIALIZED, max(lesionVector) - length(k)))
      sumWeight <- sum(lesionWeight[k2, lesionWeight[k2, ] != UNINITIALIZED])
      if (sumWeight != 1){
        if (sumWeight <= 1.01 && sumWeight >= 0.99){
          lesionWeight[k2, ] <- lesionWeight[k2, ] / sumWeight
        }else{
          errorMsg <- paste0("The sum of the truthWeights of Case ", k2, " is not 1.")
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
  
  LL <- array(dim = c(I, J, K2, max(lesionVector)))
  for (i in 1:I) {
    for (j in 1:J) {
      k <- (LLModalityID == modalityID[i]) & (LLReaderID == readerID[j])
      if ((sum(k) == 0)) 
        next
      caseLLTable <- table(LLCaseID[k])
      IDs <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
      for (k1 in 1:length(IDs)) {
        for (el in 1:caseLLTable[k1]) {
          kk <- which(IDs[k1] == abnormalCases)
          ll <- which(LLLesionID[k][which(LLCaseID[k] == IDs[k1])][el] == lesionIDTable[which(IDs[k1] == abnormalCases), ])
          LL[i, j, kk, ll] <- LLRating[k][which(LLCaseID[k] == IDs[k1])][el]
        }
      }
    }
  }
  
  lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
  lesionIDTable[is.na(lesionIDTable)] <- UNINITIALIZED
  NL[is.na(NL)] <- UNINITIALIZED
  LL[is.na(LL)] <- UNINITIALIZED
  
  if (isROCDataset(NL, LL, truthCaseID)) {
    fileType <- "ROC"
  } else if (isROIDataset(NL, LL, lesionVector)) {
    fileType <- "ROI" # not yet implemented
  } else if (splitPlot && isSplitPlotRocDataset (truthTable, NLTable, LLTable)) {
    # since FROC is so general, must first eliminate a user-declared splitPlot dataset
    # isSplitPlotRocDataset checks for a valid splitPlot dataset 
    fileType <- "SplitPlotRoc"
    # the following is the most general data structure, all else having been eliminated
  } else  fileType <- "FROC"
  
  modalityNames <- modalityID
  readerNames <- readerID
  
  if (renumber){
    modalityID <- 1:I
    readerID <- 1:J
  }
  
  names(modalityID) <- modalityNames
  names(readerID) <- readerNames
  
  return(list(NL = NL, 
              LL = LL, 
              lesionVector = lesionVector, 
              truthlesionID = lesionIDTable, 
              lesionWeight = lesionWeight, 
              dataType = fileType, 
              modalityID = modalityID, 
              readerID = readerID))
} 


isSplitPlotRocDataset <- function(truthTable, NLTable, LLTable)
{
  # UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  modalityID <- unique(NLTable[[2]])
  readerID <- unique(truthTable[[4]])
  I <- length(modalityID) # number of modalities
  J <- length(readerID) # number of readers
  K <- length(truthTable[[1]]) # total number of cases read by all readers
  Kj <- array(dim = J) # total number of cases read by reader j
  K1j <- array(dim = J) # total number of normal cases read by reader j
  K2j <- array(dim = J) # total number of abnormal cases read by reader j
  Knested <- array(dim = c(J, K)) # IDs of cases read by reader j
  K1nested <- array(dim = c(J, K)) # IDs of normal cases read by reader j
  K2nested <- array(dim = c(J, K)) # IDs of abnormal cases read by reader j
  for (j in 1:J) {
    temp <- truthTable[[1]][which(truthTable[[4]] == j)]
    temp1 <- truthTable[[1]][which((truthTable[[4]] == j) & (truthTable[[2]] == 0))]
    temp2 <- truthTable[[1]][which((truthTable[[4]] == j) & (truthTable[[2]] == 1))]
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


isROCDataset <- function(NL, LL, truthCaseID)
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- K - K2
  maxNL <- length(NL[1,1,1,])
  maxLL <- length(LL[1,1,1,])
  
  if (max(table(truthCaseID)) != 1) return (FALSE) # number of occurrences of each truthCaseID value
  if (maxNL != 1) return (FALSE)
  if (all((NL[, , (K1 + 1):K, ] != UNINITIALIZED))) return (FALSE) 
  if (any((NL[, , 1:K1, ] == UNINITIALIZED))) return (FALSE) 
  if (maxLL != 1) return (FALSE)
  if (any((LL[, , 1:K2, ] == UNINITIALIZED))) return (FALSE)
  return (TRUE)
}


##stop("ROI paradigm not yet implemented")
isROIDataset <- function(NL, LL, lesionVector)
{
  ##return (FALSE)
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


# ReadJAFROC <- function(fileName, renumber) {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   wb <- loadWorkbook(fileName)
#   sheetNames <- toupper(names(wb))
#   
#   truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
#   if (truthFileIndex == 0) 
#     stop("TRUTH table cannot be found in the dataset.")
#   truthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:3)
#   
#   ## fill empty rows with NAs (and delete them)
#   for (i in 1:3){
#     truthTable[grep("^\\s*$", truthTable[ , i]), i] <- NA
#   }
#   
#   ## delete empty rows
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
#       errorMsg <- paste0("There are non-integer values(s) for CaseID or LesionID at the line(s) ", 
#                          paste(naLines, collapse = ", "), " in the TRUTH table.")
#       stop(errorMsg)
#     }
#   }
#   
#   if (any(is.na(as.numeric(as.character(truthTable[, 3]))))) {
#     naLines <- which(is.na(as.numeric(as.character(truthTable[, 3])))) + 1
#     errorMsg <- paste0("There are non-numeric values(s) for truthWeights at the line(s) ", 
#                        paste(naLines, collapse = ", "), " in the TRUTH table.")
#     stop(errorMsg)
#   }
#   
#   truthCaseID <- as.integer(truthTable[[1]])  # all 3 have same lengths
#   truthlesionID <- as.integer(truthTable[[2]])
#   truthWeights <- truthTable[[3]]
#   
#   normalCases <- sort(unique(truthCaseID[truthlesionID == 0]))
#   abnormalCases <- sort(unique(truthCaseID[truthlesionID > 0]))
#   allCases <- c(normalCases, abnormalCases)
#   K1 <- length(normalCases)
#   K2 <- length(abnormalCases)
#   K <- (K1 + K2)
#   
#   if (anyDuplicated(cbind(truthCaseID, truthlesionID))) {
#     naLines <- which(duplicated(cbind(truthCaseID, truthlesionID))) + 1
#     errorMsg <- paste0("Line(s) ", paste(naLines, collapse = ", "), 
#                        " in the TRUTH table are duplicated with previous line(s) .")
#     stop(errorMsg)
#   }
#   
#   nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
#   if (nlFileIndex == 0) 
#     stop("FP/NL table cannot be found in the dataset.")
#   NLTable <- read.xlsx(fileName, nlFileIndex, cols = 1:4)
#   
#   ## fill empty rows with NAs (and delete them)
#   for (i in 1:4){
#     NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
#   }
#   
#   ## delete empty rows
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
#       errorMsg <- paste0("There are missing cell(s) at line(s) ", 
#                          paste(naLines, collapse = ", "), " in the FP table.")
#       stop(errorMsg)
#     }
#   }
#   
#   NLReaderID <- as.character(NLTable[[1]])
#   
#   NLModalityID <- as.character(NLTable[[2]])
#   
#   NLCaseID <- NLTable[[3]]
#   if (any(!(NLCaseID %in% truthCaseID))) {
#     naCases <- NLCaseID[which(!(NLCaseID %in% truthCaseID))]
#     errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), 
#                        " in the FP table cannot be found in TRUTH table.")
#     stop(errorMsg)
#   }
#   NLRating <- NLTable[[4]]
#   
#   llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
#   if (llFileIndex == 0) 
#     stop("TP/LL table cannot be found in the dataset.")
#   LLTable <- read.xlsx(fileName, llFileIndex, cols = 1:5)
#   
#   ## fill empty rows with NAs (and delete them)
#   for (i in 1:5){
#     LLTable[grep("^\\s*$", LLTable[ , i]), i] <- NA
#   }
#   
#   ## delete empty rows
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
#       errorMsg <- paste0("There are missing cell(s) at line(s) ", 
#                          paste(naLines, collapse = ", "), " in the TP table.")
#       stop(errorMsg)
#     }
#   }
#   
#   LLReaderID <- as.character(LLTable[[1]])
#   
#   LLModalityID <- as.character(LLTable[[2]])
#   
#   LLCaseID <- LLTable[[3]]
#   LLLesionID <- LLTable[[4]]
#   for (i in 1:nrow(LLTable)) {
#     lineNum <- which((truthCaseID == LLCaseID[i]) & (truthlesionID == LLLesionID[i]))
#     if (!length(lineNum)) {
#       errorMsg <- paste0("Modality ", LLTable[i, 2], 
#                          " Reader(s) ", LLTable[i, 1], 
#                          " Case(s) ", LLTable[i, 3], 
#                          " Lesion(s) ", LLTable[i, 4], 
#                          " cannot be found in TRUTH table .")
#       stop(errorMsg)
#     }
#   }
#   
#   LLRating <- LLTable[[5]]
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
#   lesionVector <- as.vector(table(truthCaseID[truthCaseID %in% abnormalCases]))
#   # for (k2 in 1:length(abnormalCases)) { lesionVector[k2] <- sum(truthCaseID == abnormalCases[k2]) }
#   
#   lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector)))
#   lesionIDTable <- array(dim = c(length(abnormalCases), max(lesionVector)))
#   
#   for (k2 in 1:length(abnormalCases)) {
#     k <- which(truthCaseID == abnormalCases[k2])
#     lesionIDTable[k2, ] <- c(sort(truthlesionID[k]), rep(UNINITIALIZED, max(lesionVector) - length(k)))
#     if (all(truthWeights[k] == 0)) {
#       lesionWeight[k2, 1:length(k)] <- 1/lesionVector[k2]
#     } else {
#       lesionWeight[k2, ] <- c(truthWeights[k][order(truthlesionID[k])], rep(UNINITIALIZED, max(lesionVector) - length(k)))
#       sumWeight <- sum(lesionWeight[k2, lesionWeight[k2, ] != UNINITIALIZED])
#       if (sumWeight != 1){
#         if (sumWeight <= 1.01 && sumWeight >= 0.99){
#           lesionWeight[k2, ] <- lesionWeight[k2, ] / sumWeight
#         }else{
#           errorMsg <- paste0("The sum of the truthWeights of Case ", k2, " is not 1.")
#           stop(errorMsg)
#         }
#       }
#     }
#   }
#   
#   modalityID <- as.character(sort(unique(c(NLModalityID, LLModalityID))))
#   I <- length(modalityID)
#   
#   readerID <- as.character(sort(unique(c(NLReaderID, LLReaderID))))
#   J <- length(readerID)
#   
#   maxNL <- 0
#   for (i in modalityID) {
#     for (j in readerID) {
#       k <- (NLModalityID == i) & (NLReaderID == j)
#       if ((sum(k) == 0)) 
#         next
#       maxNL <- max(maxNL, max(table(NLCaseID[k])))
#     }
#   }
#   
#   NL <- array(dim = c(I, J, K, maxNL))
#   for (i in 1:I) {
#     for (j in 1:J) {
#       k <- (NLModalityID == modalityID[i]) & (NLReaderID == readerID[j])
#       if ((sum(k) == 0)) 
#         next
#       caseNLTable <- table(NLCaseID[k])
#       IDs <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
#       for (k1 in 1:length(IDs)) {
#         for (el in 1:caseNLTable[k1]) {
#           NL[i, j, which(IDs[k1] == allCases), el] <- NLRating[k][which(NLCaseID[k] == IDs[k1])][el]
#         }
#       }
#     }
#   }
#   
#   LL <- array(dim = c(I, J, K2, max(lesionVector)))
#   for (i in 1:I) {
#     for (j in 1:J) {
#       k <- (LLModalityID == modalityID[i]) & (LLReaderID == readerID[j])
#       if ((sum(k) == 0)) 
#         next
#       caseLLTable <- table(LLCaseID[k])
#       IDs <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
#       for (k1 in 1:length(IDs)) {
#         for (el in 1:caseLLTable[k1]) {
#           kk <- which(IDs[k1] == abnormalCases)
#           ll <- which(LLLesionID[k][which(LLCaseID[k] == IDs[k1])][el] == lesionIDTable[which(IDs[k1] == abnormalCases), ])
#           LL[i, j, kk, ll] <- LLRating[k][which(LLCaseID[k] == IDs[k1])][el]
#         }
#       }
#     }
#   }
#   
#   lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
#   lesionIDTable[is.na(lesionIDTable)] <- UNINITIALIZED
#   NL[is.na(NL)] <- UNINITIALIZED
#   LL[is.na(LL)] <- UNINITIALIZED
#   
#   if (isROCDataset(NL, LL, truthCaseID)) {
#     fileType <- "ROC"
#   } else {
#     if (isROIDataset(NL, LL, lesionVector)) {
#       fileType <- "ROI" # not yet implemented
#     } else {
#       fileType <- "FROC"
#     }
#   }
#   
#   modalityNames <- modalityID
#   readerNames <- readerID
#   
#   if (renumber){
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
#               truthlesionID = lesionIDTable, 
#               lesionWeight = lesionWeight, 
#               dataType = fileType, 
#               modalityID = modalityID, 
#               readerID = readerID))
# } 
# 

ReadLrc <- function(fileName, renumber) {
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
  truthlesionID <- array(1, dim = c(K2, 1))
  lesionWeight <- truthlesionID
  maxNL <- 1
  dataType <- "ROC"
  
  modalityNames <- modalityID
  readerNames <- readerID
  
  if (renumber){
    modalityID <- 1:I
    readerID <- 1:J
  }
  
  names(modalityID) <- modalityNames
  names(readerID) <- readerNames
  
  return(list(NL = NL, 
              LL = LL, 
              lesionVector = lesionVector, 
              truthlesionID = truthlesionID, 
              lesionWeight = lesionWeight, 
              dataType = dataType, 
              modalityID = modalityID, 
              readerID = readerID))
}



splitWhiteSpaces <- function(string) {
  whiteSpaces <- c("", " ", "\t")
  string <- unlist(strsplit(string, split = " |\t"))
  string <- string[!string %in% whiteSpaces]
  return(string)
} 


ReadImrmc <- function(fileName, renumber) {
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
  
  truthTable <- dataTable[as.numeric(dataTable[, 1]) == -1, ]
  if (any(!(caseId %in% as.numeric(truthTable[, 2])))) {
    errMsg <- sprintf("Case %d cannot be found in rows recording truth states.", caseId[!(caseId %in% as.numeric(truthTable[, 2]))])
    stop(errMsg)
  }
  
  if (any(!(as.numeric(truthTable[, 4]) %in% c(0, 1)))) {
    errMsg <- "Cases' truth states must be 0 or 1."
    stop(errMsg)
  }
  normalCases <- unique(as.numeric(truthTable[as.numeric(truthTable[, 4]) == 0, 2]))
  abnormalCases <- unique(as.numeric(truthTable[as.numeric(truthTable[, 4]) == 1, 2]))
  
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
  truthlesionID <- array(1, dim = c(K2, 1))
  lesionWeight <- truthlesionID
  maxNL <- 1
  dataType <- "ROC"
  
  modalityNames <- modalityID
  readerNames <- readerID
  
  if (renumber){
    modalityID <- 1:I
    readerID <- 1:J
  }
  
  names(modalityID) <- modalityNames
  names(readerID) <- readerNames
  
  return(list(NL = NL, 
              LL = LL, 
              lesionVector = lesionVector, 
              truthlesionID = truthlesionID, 
              lesionWeight = lesionWeight, 
              dataType = dataType, 
              modalityID = modalityID, 
              readerID = readerID))
} 


