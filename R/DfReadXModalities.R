#' Read a crossed-modality data file
#' 
#' @description Read a crossed-modality data file, in which the 
#'    two modality factors are crossed
#' 
#' @param fileName A string specifying the name of the file that contains the dataset, 
#'    which must be an extended-JAFROC format Excel file containing an 
#'    additional modality factor.
#' @param sequentialNames If \code{TRUE}, consecutive integers (starting from 1) will be used 
#'    as the modality and reader IDs. Otherwise, modality and reader IDs in the 
#'    original data file will be used. The default is \code{FALSE}. 
#' 
#' @details The data format is  similar to the JAFROC format (see \code{\link{RJafroc-package}}). 
#'    The difference is that there are two modality factors. TBA For an example see ... add
#'    reference to FROC book chapter \url{https://dpc10ster.github.io/RJafrocFrocBook/}
#'    
#' 
#' @return A dataset with the specified structure, similar to a standard 
#'    \pkg{RJafroc} dataset (see \code{\link{RJafroc-package}}). Because of the extra modality factor, 
#'    \code{NL} and \code{LL} are each five dimensional arrays. There are also two 
#'    modality IDS: \code{modalityID1} and \code{modalityID2}.
#' 
#' 
#' @references 
#' Thompson JD, Chakraborty DP, Szczepura K, et al. (2016) Effect of reconstruction 
#' methods and x-ray tube current-time product  on nodule detection in an 
#' anthropomorphic thorax phantom: a crossed-modality JAFROC observer study. 
#' Medical Physics. 43(3):1265-1274.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' 
#' @import readxl gtools
#' 
#' @export
DfReadXModalities <- function(fileName, sequentialNames = FALSE) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  #wb <- loadWorkbook(fileName) # openxlsx
  wb <- excel_sheets(fileName)
  sheetNames <- toupper(wb)
  
  # DfReadXModalities FCTRL-X-MOD in truth worksheet
  # 
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (truthFileIndex == 0) 
    stop("TRUTH table cannot be found in the dataset.")
  truthTable <- data.frame(read_xlsx(fileName, truthFileIndex, range = cell_cols(1:3) ) )
  
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
  
  caseID <- as.integer(truthTable[[1]])  # all 3 have same lengths
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
    stop("FP/NL table cannot be found in the dataset.")

  NLTable <- data.frame( read_xlsx(fileName, nlFileIndex, range = cell_cols(1:5) ) )
  
  for (i in 1:5){
    NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
  }
  
  naRows <- colSums(is.na(NLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      NLTable <- NLTable[1:(nrow(NLTable) - max(naRows)), ]
    }
  }
  
  for (i in 4:5) {
    if (any(is.na(as.numeric(as.character(NLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(NLTable[, i])))) + 1
      errorMsg <- paste0("There are missing cell(s) at line(s) ", paste(naLines, collapse = ", "), " in the FP table.")
      stop(errorMsg)
    }
  }
  
  NLReaderID <- as.character(NLTable[[1]])
  
  NLModalityID1 <- as.character(NLTable[[2]])
  NLModalityID2 <- as.character(NLTable[[3]])
  
  NLCaseID <- NLTable[[4]]
  if (any(!(NLCaseID %in% caseID))) {
    naCases <- NLCaseID[which(!(NLCaseID %in% caseID))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), " in the FP table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  NLRating <- NLTable[[5]]
  
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (llFileIndex == 0) 
    stop("TP/LL table cannot be found in the dataset.")
  LLTable <- data.frame(read_xlsx(fileName, llFileIndex, range = cell_cols(1:6) ) )
  
  for (i in 1:6){
    LLTable[grep("^\\s*$", LLTable[ , i]), i] <- NA
  }
  
  naRows <- colSums(is.na(LLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      LLTable <- LLTable[1:(nrow(LLTable) - max(naRows)), ]
    }
  }
  
  for (i in 4:6) {
    if (any(is.na(as.numeric(as.character(LLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(LLTable[, i])))) + 1
      errorMsg <- paste0("There are missing cell(s) at line(s) ", paste(naLines, collapse = ", "), " in the TP table.")
      stop(errorMsg)
    }
  }
  
  LLReaderID <- as.character(LLTable[[1]])
  
  LLModalityID1 <- as.character(LLTable[[2]])
  LLModalityID2 <- as.character(LLTable[[3]])
  
  LLCaseID <- LLTable[[4]]
  LLLesionID <- LLTable[[5]]
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((caseID == LLCaseID[i]) & (lesionID == LLLesionID[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 1], "and ", LLTable[i, 2], " Reader(s) ", LLTable[i, 1], " Case(s) ", LLTable[i, 4], " Lesion(s) ", LLTable[i, 5], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  LLRating <- LLTable[[6]]
  
  if (anyDuplicated(LLTable[, 1:5])) {
    naLines <- which(duplicated(LLTable[, 1:5]))
    errorMsg <- paste0("Modality1 ", paste(LLTable[naLines, 2], collapse = ", "), "Modality2 ", paste(LLTable[naLines, 3], collapse = ", "), 
                       " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), " Case(s) ", paste(LLTable[naLines, 4], collapse = ", "), 
                       " Lesion(s) ", paste(LLTable[naLines, 5], collapse = ", "), " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  perCase <- as.vector(table(caseID[caseID %in% abnormalCases]))
  # for (k2 in 1:length(abnormalCases)) { perCase[k2] <- sum(caseID == abnormalCases[k2]) }
  
  lesionWeight <- array(dim = c(length(abnormalCases), max(perCase)))
  lesionIDTable <- array(dim = c(length(abnormalCases), max(perCase)))
  
  for (k2 in 1:length(abnormalCases)) {
    k <- which(caseID == abnormalCases[k2])
    lesionIDTable[k2, ] <- c(sort(lesionID[k]), rep(UNINITIALIZED, max(perCase) - length(k)))
    if (all(weights[k] == 0)) {
      lesionWeight[k2, 1:length(k)] <- 1/perCase[k2]
    } else {
      lesionWeight[k2, ] <- c(weights[k][order(lesionID[k])], rep(UNINITIALIZED, max(perCase) - length(k)))
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
  
  modalityID1 <- mixedsort(unique(c(NLModalityID1, LLModalityID1)))
  I1 <- length(modalityID1)
  
  modalityID2 <- mixedsort(unique(c(NLModalityID2, LLModalityID2)))
  I2 <- length(modalityID2)
  
  readerID <- mixedsort(unique(c(NLReaderID, LLReaderID)))
  J <- length(readerID)
  
  maxNL <- 0
  for (i1 in modalityID1) {
    for (i2 in modalityID2) {
      for (j in readerID) {
        k <- (NLModalityID1 == i1) & (NLModalityID2 == i2) & (NLReaderID == j)
        if ((sum(k) == 0)) 
          next
        maxNL <- max(maxNL, max(table(NLCaseID[k])))
      }
    }
  }
  
  NL <- array(dim = c(I1, I2, J, K, maxNL))
  for (i1 in 1:I1) {
    for (i2 in 1:I2) {
      for (j in 1:J) {
        k <- (NLModalityID1 == modalityID1[i1]) & (NLModalityID2 == modalityID2[i2]) & (NLReaderID == readerID[j])
        if ((sum(k) == 0)) 
          next
        caseNLTable <- table(NLCaseID[k])
        IDs <- as.numeric(unlist(attr(caseNLTable, "dimnames")))
        for (k1 in 1:length(IDs)) {
          for (el in 1:caseNLTable[k1]) {
            NL[i1, i2, j, which(IDs[k1] == allCases), el] <- NLRating[k][which(NLCaseID[k] == IDs[k1])][el]
          }
        }
      }
    }
  }
  
  LL <- array(dim = c(I1, I2, J, K2, max(perCase)))
  for (i1 in 1:I1) {
    for (i2 in 1:I2) {
      for (j in 1:J) {
        k <- (LLModalityID1 == modalityID1[i1]) & (LLModalityID2 == modalityID2[i2]) & (LLReaderID == readerID[j])
        if ((sum(k) == 0)) 
          next
        caseLLTable <- table(LLCaseID[k])
        IDs <- as.numeric(unlist(attr(caseLLTable, "dimnames")))
        for (k1 in 1:length(IDs)) {
          for (el in 1:caseLLTable[k1]) {
            LL[i1, i2, j, which(IDs[k1] == abnormalCases), which(LLLesionID[k][which(LLCaseID[k] == IDs[k1])][el] == lesionIDTable[which(IDs[k1] == abnormalCases), ])] <- LLRating[k][which(LLCaseID[k] == IDs[k1])][el]
          }
        }
      }
    }
  }
  
  lesionWeight[is.na(lesionWeight)] <- UNINITIALIZED
  lesionIDTable[is.na(lesionIDTable)] <- UNINITIALIZED
  NL[is.na(NL)] <- UNINITIALIZED
  LL[is.na(LL)] <- UNINITIALIZED
  
  isROI <- TRUE
  for (i1 in 1:I1) {
    for (i2 in 1:I2) {
      for (j in 1:J) {
        if (any(NL[i1, i2, j, 1:K1, ] == UNINITIALIZED)) {
          isROI <- FALSE
          break
        }
        temp <- LL[i1, i2, j, , ] != UNINITIALIZED
        dim(temp) <- c(K2, max(perCase))
        if (!all(perCase == rowSums(temp))) {
          isROI <- FALSE
          break
        }
        temp <- NL[i1, i2, j, (K1 + 1):K, ] == UNINITIALIZED
        dim(temp) <- c(K2, maxNL)
        if (!all(perCase == rowSums(temp))) {
          isROI <- FALSE
          break
        }
      }
    }
  }
  
  if ((max(table(caseID)) == 1) && (maxNL == 1) && (all((NL[, , , (K1 + 1):K, ] == UNINITIALIZED))) && (all((NL[, , , 1:K1, ] != UNINITIALIZED)))) {
    type <- "ROC"
  } else {
    if (isROI) {
      type <- "ROI"
    } else {
      type <- "FROC"
    }
  }
  
  modality1Names <- modalityID1
  modality2Names <- modalityID2
  readerNames <- readerID
  
  if (sequentialNames){
    modalityID1 <- 1:I1
    modalityID2 <- 1:I2
    readerID <- 1:J
  }
  
  names(modalityID1) <- modality1Names
  names(modalityID2) <- modality2Names
  names(readerID) <- readerNames
  
  fileName <- paste0("DfReadXModalities(", tools::file_path_sans_ext(basename(fileName)), ")")
  name <- "THOMPSON-X-MOD"
  design <- "FCTRL-X-MOD"
  truthTableStr <- NA
  IDs <- lesionIDTable
  weights <- lesionWeight
  return(convert2Xdataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID1, modalityID2, readerID))
} 


convert2Xdataset <- function(NL, LL, LL_IL, 
                             perCase, IDs, weights,
                             fileName, type, name, truthTableStr, design,
                             modalityID1,  modalityID2, readerID) {
  ratings <- list(NL = NL,
                  LL = LL,
                  LL_IL = LL_IL)
  
  lesions <- list(perCase = perCase,
                  IDs = IDs,
                  weights = weights)
  
  descriptions <- list(fileName = tools::file_path_sans_ext(basename(fileName)),
                       type = type,
                       name = name,
                       truthTableStr = truthTableStr,
                       design = design,
                       modalityID1 = modalityID1,
                       modalityID2 = modalityID2,
                       readerID = readerID)
  
  dataset <- list(ratings = ratings, 
                  lesions = lesions, 
                  descriptions = descriptions)
  
  return(dataset)
  
}

