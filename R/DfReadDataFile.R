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
#'    The default is \code{TRUE}: the function accommodates 3 additional columns
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
#' fileName <- system.file("extdata", "toyFiles/ROC/OK.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' ds <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#'
#' fileName <- system.file("extdata", "toyFiles/FROC/OK.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' ds <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#' 
#' fileName <- system.file("extdata", "toyFiles/FROC/OK-OldFormat.xlsx",
#' package = "RJafroc", mustWork = TRUE)
#' ds1Old <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
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
                            newExcelFileFormat = TRUE, delimiter = ",", 
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
  
  ###################### START CHECK TRUTH TABLE ############################
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is, i.e., 1st, 2nd or 3rd tab position in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (truthFileIndex == 0) stop("TRUTH table cannot be found in the dataset.")
  TruthTable <- read.xlsx(fileName, truthFileIndex, cols = 1:6)
  rTruth <- checkTruthTable(TruthTable) # rTruth = return Truth
  
  TruthTableStr <- rTruth$TruthTableStr
  TruthDataType <- rTruth$dataType
  TruthCaseID <-  rTruth$caseID # these need not be unique for FROC datasets
  declaredParadigm <- rTruth$declaredParadigm
  declaredDesign <- rTruth$declaredDesign
  lesionWeight <- rTruth$lesionWeight
  lesionVector <- rTruth$lesionVector
  lesionID <- rTruth$lesionID
  normalCases <- rTruth$normalCases
  abnormalCases <- rTruth$abnormalCases

  allCases <- c(normalCases, abnormalCases)
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  ###################### END CHECK TRUTH TABLE ##############################
  
  
  ###################### START CHECK NL TABLE$ ##############################
  nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  if (nlFileIndex == 0) stop("FP/NL table cannot be found in the dataset.")
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
      errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", paste(naLines, collapse = ", "), " in the FP table.")
      stop(errorMsg)
    }
  }
  
  # Column means the entire column is included in vector
  NLReaderIDColumn <- as.character(NLTable[[1]])
  NLModalityIDColumn <- as.character(NLTable[[2]])
  NLCaseIDColumn <- NLTable[[3]]
  NLRatingColumn <- NLTable[[4]]
  
  if (any(!(NLCaseIDColumn %in% TruthCaseID))) {
    naCases <- NLCaseIDColumn[which(!(NLCaseIDColumn %in% TruthCaseID))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), " in the FP table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  ###################### END CHECK NL TABLE #################################
  
  ###################### START CHECK LL TABLE ###############################
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (llFileIndex == 0) stop("TP/LL table cannot be found in the dataset.")
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
  
  LLReaderIDColumn <- as.character(LLTable[[1]])
  LLModalityIDColumn <- as.character(LLTable[[2]])
  LLCaseIDColumn <- LLTable[[3]]
  LLLesionIDColumn <- LLTable[[4]]
  LLRatingColumn <- LLTable[[5]]
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((TruthCaseID == LLCaseIDColumn[i]) & (lesionID == LLLesionIDColumn[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], " Reader(s) ", LLTable[i, 1], " Case(s) ", LLTable[i, 3], " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  if (anyDuplicated(LLTable[, 1:4])) {
    naLines <- which(duplicated(LLTable[, 1:4]))
    errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
                       paste(LLTable[naLines, 4], collapse = ", "), " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  # lesionVector <- as.vector(table(TruthCaseID[TruthCaseID %in% abnormalCases]))
  # lesionWeight <- array(dim = c(length(abnormalCases), max(lesionVector)))
  # lesionID <- array(dim = c(length(abnormalCases), max(lesionVector)))
  # 
  # for (k2 in 1:length(abnormalCases)) {
  #   k <- which(TruthCaseID == abnormalCases[k2])
  #   lesionID[k2, ] <- c(sort(lesionID[k]), rep(UNINITIALIZED, max(lesionVector) - length(k)))
  #   if (all(lesionWeight[k] == 0)) {
  #     lesionWeight[k2, 1:length(k)] <- 1/lesionVector[k2]
  #   } else {
  #     lesionWeight[k2, ] <- as.numeric(c(lesionWeight[k][order(TruthLesionID[k])], 
  #                                        rep(UNINITIALIZED, max(lesionVector) - length(k))))
  #     sumWeight <- sum(lesionWeight[k2, lesionWeight[k2, ] != UNINITIALIZED])
  #     if (sumWeight != 1){
  #       if (sumWeight <= 1.01 && sumWeight >= 0.99){
  #         lesionWeight[k2, ] <- lesionWeight[k2, ] / sumWeight
  #       }else{
  #         errorMsg <- paste0("The sum of the TruthWeights of Case ", k2, " is not 1.")
  #         stop(errorMsg)
  #       }
  #     }
  #   }
  # }
  ###################### END CHECK LL TABLE #################################
  
  modalityIDUnique <- as.character(sort(unique(c(NLModalityIDColumn, LLModalityIDColumn))));I <- length(modalityIDUnique)
  readerIDUnique <- as.character(sort(unique(c(NLReaderIDColumn, LLReaderIDColumn))));J <- length(readerIDUnique)
  
  
  ######################       CALC NL ARRAY    ############################
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
      casePresent_ij <- (NLModalityIDColumn == modalityIDUnique[i]) & (NLReaderIDColumn == readerIDUnique[j])
      if ((sum(casePresent_ij) == 0)) next 
      caseNLTable <- table(NLCaseIDColumn[casePresent_ij]) 
      rTruthcaseIDs_ij <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
      for (k1 in 1:length(rTruthcaseIDs_ij)) {
        if (TruthTableStr[i,j,which(rTruthcaseIDs_ij[k1] == allCases)] != 1) stop("incorrect TruthCaseID in NL/FP worksheet")
        for (el in 1:caseNLTable[k1]) {
          NL[i, j, which(rTruthcaseIDs_ij[k1] == allCases), el] <- 
            NLRatingColumn[casePresent_ij][which(NLCaseIDColumn[casePresent_ij] == rTruthcaseIDs_ij[k1])][el]
        }
      }
    }
  }
  NL[is.na(NL)] <- UNINITIALIZED
  
  ######################       CALC LL ARRAY    ############################
  LL <- array(dim = c(I, J, K2, max(lesionVector)))
  for (i in 1:I) {
    for (j in 1:J) {
      casePresent_ij <- (LLModalityIDColumn == modalityIDUnique[i]) & (LLReaderIDColumn == readerIDUnique[j])
      if ((sum(casePresent_ij) == 0)) next
      caseLLTable <- table(LLCaseIDColumn[casePresent_ij])
      rTruthcaseIDs_ij <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
      for (k2 in 1:length(rTruthcaseIDs_ij)) {
        if (TruthTableStr[i,j,which(rTruthcaseIDs_ij[k2] == allCases)] != 1) stop("incorrect TruthCaseID in LL/TP worksheet")
        for (el in 1:caseLLTable[k2]) {
          k2p <- which(rTruthcaseIDs_ij[k2] == abnormalCases)
          x2 <- which(LLCaseIDColumn[casePresent_ij] == rTruthcaseIDs_ij[k2])
          x3 <- lesionID[which(rTruthcaseIDs_ij[k2] == abnormalCases), ]
          elp <- which(LLLesionIDColumn[casePresent_ij][x2][el] == x3)
          LL[i, j, k2p, elp] <- LLRatingColumn[casePresent_ij][x2][el]
        }
      }
    }
  }
  LL[is.na(LL)] <- UNINITIALIZED
  lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
  lesionID[is.na(lesionID)] <- UNINITIALIZED
  
  if (declaredParadigm == "ROC" && declaredDesign != "SPLIT-PLOT") {
    if (!(((max(table(TruthCaseID)) == 1) && (maxNL == 1)) 
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
  
  return(list(NL = NL, 
              LL = LL, 
              lesionVector = lesionVector,
              lesionID = lesionID, 
              lesionWeight = lesionWeight, 
              dataType = TruthDataType, 
              modalityID = modalityIDUnique, 
              readerID = readerIDUnique))
} 








