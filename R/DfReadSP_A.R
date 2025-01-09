#' Read a split plot data file
#'
#' @description Read disk file and create a dataset object
#'
#' @param fileName String specifying the name of the file.
#'
#' @return A dataset with the structure specified in
#'   \code{\link{RJafroc-package}}.
#'
#' @examples
#' fileName <- system.file("extdata", "toyFiles/ROC/rocSpA.xlsx",
#' package = "RJafroc", mustWork = TRUE)
#' ds <- DfReadSP_A(fileName)
#'
#'
#' @importFrom tools file_ext
#' @importFrom stringr str_trim str_length
#' @importFrom readxl excel_sheets
#' @export

DfReadSP_A <- function (fileName) 
{
  
  temp <- chkExcelSP_A(fileName)
  stop("code here\n")
  I <- temp$I 
  J_i <- temp$J_i
  trtArr1D <- temp$trtArr1D
  truthCaseID <- temp$caseID 
  type <- temp$type
  design <- temp$design
  weights <- temp$weights
  perCase <- temp$perCase
  IDs <- temp$IDs
  lesionIDCol <- temp$lesionIDCol
  normalCases <- temp$normalCases
  abnormalCases <- unique(temp$abnormalCases)
  NLTable <- temp$NLTable
  
  J <- sum(J_i)
  
  wb <- readxl::excel_sheets(fileName)
  sheetNames <- toupper(wb) 

  truthSheetIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  truthTable <- data.frame( read_xlsx(fileName, truthSheetIndex, range = cell_cols(1:6) ) )
  truthCaseID <- truthTable$CaseID
    
  L <- length(truthTable$CaseID) # column length in the Truth Excel worksheet, an even integer
  caseIDCol <- as.integer(truthTable$CaseID)[1:(L/2)] # for this design L is twice the number of cases
  lesionIDCol <- as.integer(truthTable$LesionID)[1:(L/2)]
  weightsCol <- truthTable$Weight[1:(L/2)]

  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  truthTableStr <- array(dim = c(I, J, K, max(lesionIDCol)+1)) 
  for (i in 1:I) {
    for (j in 1:J_i[i]) {
      if (i == 1) truthTableStr[i,j,,] <- 1
      if (i == 2) truthTableStr[i,J_i[1]+j,,] <- 1
    }
  }
  
  type <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  
  df <- truthTable[1:5]
  df["caseLevelTruth"] <- (truthTable$LesionID > 0)
  # sort the TRUTH worksheet of the Excel file on the lesionID field
  # this puts normal cases first, regardless of how they are entered in Excel worksheet
  truthTableSort <- df[order(df$caseLevelTruth),]
  
  NLReaderIDCol <- as.character(NLTable$ReaderID)
  NLModalityIDCol <- as.character(NLTable$ModalityID)
  NLCaseIDCol <- NLTable$CaseID
  # allow for worksheet name to be either NL_Rating or FP_Rating
  if (is.null(NLTable$FP_Rating)) NLRatingCol <- NLTable$NL_Rating else NLRatingCol <- NLTable$FP_Rating
  NLRatingCol <- as.numeric(NLRatingCol)
  if(any(is.na(NLRatingCol))) stop ("found NAs in NLRatingCol in NL/FP sheet")
  
  ########################### CHECK LL TABLE ################################
  llSheetIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  LLTable <- data.frame(read_xlsx(fileName, llSheetIndex, range = cell_cols(1:5) ))
  
  LLReaderIDCol <- as.character(LLTable$ReaderID)
  LLModalityIDCol <- as.character(LLTable$ModalityID)
  LLCaseIDCol <- LLTable$CaseID
  LLLesionIDCol <- LLTable$LesionID
  # allow for worksheet name to be either LL_Rating or TP_Rating
  if (is.null(LLTable$TP_Rating)) LLRatingCol <- LLTable$LL_Rating else LLRatingCol <- LLTable$TP_Rating
  
  modalityIDUnique <- as.character(unique(c(NLModalityIDCol, LLModalityIDCol)))
  I <- length(modalityIDUnique)
  readerIDUnique <- as.character(unique(c(NLReaderIDCol, LLReaderIDCol)))
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
  
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  len_NL <- length(NLModalityIDCol)
  NL <- array(dim = c(I, J, K, maxNL))
  
  ############################ INIT NL ARRAY ################################
  for (l in 1:len_NL) {
    i <- which(trtArr1D == NLModalityIDCol[l])
    j <- which(J_i == NLReaderIDCol[l])
    k <- which(unique(truthTableSort$CaseID) == NLCaseIDCol[l])
    nMatches <- which((NLCaseIDCol == NLCaseIDCol[l]) & (NLModalityIDCol == NLModalityIDCol[l]) & (NLReaderIDCol == NLReaderIDCol[l]))
    if (NLCaseIDCol[l] %in% normalCases) tt2 <- truthTableStr[i,j,k,1] else tt2 <- truthTableStr[i,j,k,2] 
    if (is.na(tt2)) stop("Error in reading NL/FP worksheet: is.na(tt2)") else {
      if (tt2 != 1)  stop("Error in reading NL/FP worksheet: tt2 != 1") else 
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
      if (tt2 != 1)  stop("Error in reading LL/TP worksheet") else 
        # the is.na() check ensures that an already recorded mark is not overwritten
        if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
    }
    # if (is.na(tt2)) stop("Error in reading LL/TP worksheet") else {
    #   if (tt2 != 1)  stop("Error in reading LL/TP worksheet") else 
    #     # the is.na() check ensures that an already recorded mark is not overwritten
    #     if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
    # }
  }
  
  LL[is.na(LL)] <- UNINITIALIZED
  weights[is.na(weights)] <- UNINITIALIZED
  lesionIDCol[is.na(lesionIDCol)] <- UNINITIALIZED
  
  modalityNames <- modalityIDUnique
  readerNames <- readerIDUnique
  
  names(modalityIDUnique) <- modalityNames; modalityID <- modalityIDUnique
  names(readerIDUnique) <- readerNames; readerID <- readerIDUnique
  
  name <- NA
  # return the dataset object
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
} 

