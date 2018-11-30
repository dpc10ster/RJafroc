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
#' 
#' @return A dataset with the structure specified in \link{RJafroc-package}.
#' 
#' @examples
#' \dontrun{
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

DfReadDataFile <- function(fileName, format = "JAFROC", delimiter = ",", renumber = FALSE) {
  if (format == "JAFROC") {
    if (!(file_ext(fileName) %in% c("xls", "xlsx"))) 
      stop("The extension of JAFROC data file must be \"*.xls\" or \"*.xlsx\" ")
    return(ReadJAFROC(fileName, renumber))
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


ReadJAFROC <- function(fileName, renumber) {
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
  
  lesionNum <- as.vector(table(caseID[caseID %in% abnormalCases]))
  # for (k2 in 1:length(abnormalCases)) { lesionNum[k2] <- sum(caseID == abnormalCases[k2]) }
  
  lesionWeight <- array(dim = c(length(abnormalCases), max(lesionNum)))
  lesionIDTable <- array(dim = c(length(abnormalCases), max(lesionNum)))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(caseID == abnormalCases[k2])
    lesionIDTable[k2, ] <- c(sort(lesionID[k]), rep(UNINITIALIZED, max(lesionNum) - length(k)))
    if (all(weights[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/lesionNum[k2]
    } else {
      lesionWeight[k2, ] <- c(weights[k][order(lesionID[k])], rep(UNINITIALIZED, max(lesionNum) - length(k)))
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
  
  LL <- array(dim = c(I, J, K2, max(lesionNum)))
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
      dim(temp) <- c(K2, max(lesionNum))
      if (!all(lesionNum == rowSums(temp))) {
        isROI <- FALSE
        break
      }
      temp <- NL[i, j, (K1 + 1):K, ] == UNINITIALIZED
      dim(temp) <- c(K2, maxNL)
      if (!all(lesionNum == rowSums(temp))) {
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
  
  return(list(NL = NL, LL = LL, lesionNum = lesionNum, lesionID = lesionIDTable, lesionWeight = lesionWeight, dataType = fileType, modalityID = modalityID, readerID = readerID))
} 


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
  lesionNum <- rep(1, K2)
  lesionID <- array(1, dim = c(K2, 1))
  lesionWeight <- lesionID
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
  
  return(list(NL = NL, LL = LL, lesionNum = lesionNum, lesionID = lesionID, lesionWeight = lesionWeight, dataType = dataType, modalityID = modalityID, readerID = readerID))
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
  
  lesionNum <- rep(1, K2)
  lesionID <- array(1, dim = c(K2, 1))
  lesionWeight <- lesionID
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
  
  return(list(NL = NL, LL = LL, lesionNum = lesionNum, lesionID = lesionID, lesionWeight = lesionWeight, dataType = dataType, modalityID = modalityID, readerID = readerID))
} 


