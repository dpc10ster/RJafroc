preCheck4BadEntries_SP_A <- function(truthTable) {
  
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
chkExcelFileSP_A <- function (fileName) 
{
  
  wb <- readxl::excel_sheets(fileName)
  
  ########################### CHECK WORSHEET NAMES ##############################
  temp <- sort(toupper(wb))
  if (length(temp) != 3) stop("Input Excel File must have three worksheets\n")
  if (!(temp[1] %in% c("FP", "NL"))) stop("FP or NL sheet not found in Excel input file\n")
  if (!(temp[2] %in% c("TP", "LL"))) stop("TP or LL sheet not found in Excel input file\n")
  if (!(temp[3] %in% c("TRUTH"))) stop("Truth sheet not found in Excel file\n")
  sheetNames <- toupper(wb) 
  
  ########################### CHECK TRUTH TABLE ################################
  truthSheetIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  truthTable <- data.frame( read_xlsx(fileName, truthSheetIndex, range = cell_cols(1:6) ) )
  if (all(is.na(truthTable[[6]]))) stop("For split plot analysis TRUTH worksheet requires entries in column 6\n")
  
  preCheck4BadEntries_SP_A (truthTable) # need to incorporate these tests in this function
  
  type <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  if (!(type %in% c("FROC", "ROC", "LROC"))) stop("Data must be ROC, FROC or LROC.\n")
  if (design != "SPLIT-PLOT-A") stop("Study design must be SPLIT-PLOT-A\n")
  
  L <- length(truthTable$CaseID) # column length in the Truth Excel worksheet
  # L is an even integer
  # need to cast as.integer as values are numerics
  caseIDCol <- as.integer(truthTable$CaseID)[1:(L/2)] # for this design L is twice the number of cases
  lesionIDCol <- as.integer(truthTable$LesionID)[1:(L/2)]
  weightsCol <- truthTable$Weight[1:(L/2)]
  
  normalCases <- sort(unique(caseIDCol[lesionIDCol == 0]))
  abnormalCases <- sort(unique(caseIDCol[lesionIDCol > 0]))
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  if (!is.character(truthTable$ReaderID) || (!is.character(truthTable$ModalityID))) stop("ReaderID and ModalityID columns must be characters\n")
  readerIDCol <- as.character(truthTable$ReaderID) 
  modalityIDCol <- as.character(truthTable$ModalityID)
  trtArr <- array(dim = L)
  
  # grep "^\\s*$" matches blank lines; see learnGrep in desktop
  # grep("^\\s*$", "") = 1
  # grep("^\\s*$", c("","")) = 1 2 etc
  # following replaces empty cells with NAs
  # it is not needed as the excel read function already does that
  # for (i in 1:4){
  #   NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
  # }
  
  for (l in 1:L) {
    if (grep("^\\(.\\)", modalityIDCol[l]) == 1) { # match found to something like (1), i.e., one nested factor
      val <- grep("^\\(.\\)", modalityIDCol[l], value = T)
      val <- strsplit(val, split = "\\(|\\)")[[1]]
      val <- val[val != ""]
      if (length(val) != 1) stop("multiple treatments found on same row in ModalityID column\n")
      trtArr[l] <- val
    } else stop("Must use nested notation for modality using () brackets ...")
  }
  trtArr1D <- as.vector(unique(trtArr))
  I <- length(trtArr1D)
  
  # determine number of readers in each treatment, J_i is a vector
  J_i <-  unique(count.fields(textConnection(readerIDCol), sep = ","))
  if (!is.vector(J_i))  stop("J_i: expecting a vector")
  if (length(J_i) != I) stop("length of J_i must equal I")
  J <- sum(J_i)
  
  # CHECK weights
  perCase <- as.vector(table(caseIDCol[caseIDCol %in% abnormalCases]))
  weights <- array(dim = c(K2, max(perCase)))
  IDs <- array(dim = c(K2, max(perCase)))
  
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  for (k2 in 1:K2) {
    k <- which(caseIDCol == abnormalCases[k2])
    IDs[k2, ] <- c(sort(lesionIDCol[k]), # sort is needed for FROC data
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
  
  ########################### CHECK NL TABLE ################################
  nlSheetIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  NLTable <- data.frame(read_xlsx(fileName, nlSheetIndex, range = cell_cols(1:4)))
  NLCaseIDCol <- as.integer(NLTable$CaseID)
  NLModalityIDCol <- as.character(NLTable$ModalityID)
  NLReaderIDCol <- as.character(NLTable$ReaderID)
  
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
  
  if (any(!(NLCaseIDCol %in% truthTable$CaseID))) {
    naCases <- NLCaseIDCol[which(!(NLCaseIDCol %in% truthTable$CaseID))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), 
                       " in the NL_Rating or FP_Rating worksheet cannot be found in TRUTH worksheet.")
    stop(errorMsg)
  }
  
  ########################### CHECK LL TABLE ################################
  llSheetIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  LLTable <- data.frame(read_xlsx(fileName, llSheetIndex, range = cell_cols(1:5) ))
  
  LLReaderIDCol <- as.character(LLTable$ReaderID)
  LLModalityIDCol <- as.character(LLTable$ModalityID)
  LLCaseIDCol <- as.integer(LLTable$CaseID)
  LLLesionIDCol <- as.integer(LLTable$LesionID)
  # allow for worksheet name to be either LL_Rating or TP_Rating
  if (is.null(LLTable$TP_Rating)) LLRatingCol <- LLTable$LL_Rating else LLRatingCol <- LLTable$TP_Rating
  
  modalityIDUnique <- as.character(unique(c(NLModalityIDCol, LLModalityIDCol)))
  # I <- length(modalityIDUnique)
  readerIDUnique <- as.character(unique(c(NLReaderIDCol, LLReaderIDCol)))
  # J <- length(readerIDUnique)
  
  maxNL <- 0
  for (i in modalityIDUnique) {
    for (j in readerIDUnique) {
      casePresent_ij <- (NLModalityIDCol == i) & (NLReaderIDCol == j)
      if ((sum(casePresent_ij) == 0)) 
        next
      maxNL <- max(maxNL, max(table(NLCaseIDCol[casePresent_ij])))
    }
  }
  
  L_NL <- length(NLModalityIDCol)
  NL <- array(dim = c(I, J, K, maxNL))
  
  return (list(
    I = I,
    J_i = J_i,
    trtArr1D = trtArr1D,
    type = type,
    design = design,
    caseID = caseIDCol,
    perCase = perCase,
    lesionIDCol = lesionIDCol,
    IDs = IDs,
    weights = weights,
    normalCases = normalCases,
    abnormalCases = abnormalCases,
    NLTable = NLTable
  ))
  
}




