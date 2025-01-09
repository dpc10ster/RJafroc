preCheck4BadEntries_SP_C <- function(truthTable) {
  
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
  
  if (any(!FRACTION::is.wholenumber(as.numeric(truthTable[[1]])))) stop("Non-integer values in Truth worksheet column 1")
  if (any(!FRACTION::is.wholenumber(as.numeric(truthTable[[2]])))) stop("Non-integer values in Truth worksheet column 2")
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



# SPLIT-PLOT-C: Case nested within reader; Hillis 2014 Table VII part (c)
checkTruthTableSP <- function (fileName) 
{
  
  wb <- readxl::excel_sheets(fileName)
  
  ########################## CHECK WORSHEET NAMES ##############################
  temp <- sort(toupper(wb))
  if (!(temp[1] %in% c("FP", "NL"))) stop("FP and/or NL sheet not found in Excel input file\n")
  if (!(temp[2] %in% c("TP", "LL"))) stop("TP and/or LL sheet not found in Excel input file\n")
  if (!(temp[3] %in% c("TRUTH"))) stop("Truth sheet not found in Excel file\n")
  sheetNames <- toupper(wb) 
  
  truthSheetIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  truthTable <- data.frame( read_xlsx(fileName, truthSheetIndex, range = cell_cols(1:6) ) )
  if (all(is.na(truthTable[[6]]))) stop("Old Excel format file encountered: Truth worksheet requires entries in column 6\n")
  
  preCheck4BadEntries_SP_C (truthTable)
  
  type <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  if (!(type %in% c("FROC", "ROC", "LROC"))) stop("Data must be ROC, FROC or LROC.\n")
  if (!(design %in% c("SPLIT-PLOT-A", "SPLIT-PLOT-C"))) stop("Study design must be SPLIT-PLOT-A or SPLIT-PLOT-C\n")
  
  L <- length(truthTable$CaseID) # column length in the Truth Excel worksheet
  # L is an even integer
  caseIDCol <- as.integer(truthTable$CaseID)[1:(L/2)] # for this design L is twice the number of cases
  lesionIDCol <- as.integer(truthTable$LesionID)[1:(L/2)]
  weightsCol <- truthTable$Weight[1:(L/2)]
  
  normalCases <- sort(unique(caseIDCol[lesionIDCol == 0]))
  abnormalCases <- sort(unique(caseIDCol[lesionIDCol > 0]))
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  # grep "^\\s*$" matches blank lines; see learnGrep in desktop
  # grep("^\\s*$", "") = 1
  # grep("^\\s*$", c("","")) = 1 2 etc
  # following replaces empty cells with NAs
  # it is not needed as the excel read function already does that
  # for (i in 1:4){
  #   NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
  # }
  
  readerIDCol <- as.character(truthTable$ReaderID) 
  modalityIDCol <- as.character(truthTable$ModalityID)
  if (design == "SPLIT-PLOT-A") {
    trtArr <- array(dim = L)
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
    if (length(J_i) != I) stop("length of J_i must equal I")
    J <- sum(J_i)
  } else if (design == "SPLIT-PLOT-C") {
    stop("Need to fix following code\n")
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
      browser()
      if (any(is.na(trtArr))) stop("Illegal value in ModalityID column in Truth sheet")
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
  } else stop("incorrect study design: must be SPLIT-PLOT-A or SPLIT-PLOT-C")
  
  truthTableStr <- array(dim = c(I, J, K, max(lesionIDCol)+1)) 
  if (design == "SPLIT-PLOT-A") {  
    for (i in 1:I) {
      for (j in 1:J_i[i]) {
        if (i == 1) truthTableStr[i,j,,] <- 1
        if (i == 2) truthTableStr[i,J_i[1]+j,,] <- 1
      }
    }
  } else {
    browser()
    stop("Need to add code here\n")
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
  
  df <- truthTable[1:5]
  df["caseLevelTruth"] <- (truthTable$LesionID > 0)
  # sort the TRUTH worksheet of the Excel file on the lesionID field
  # this puts normal cases first, regardless of how they are entered in Excel worksheet
  truthTableSort <- df[order(df$caseLevelTruth),]
  
  ########################### CHECK NL TABLE ################################
  nlSheetIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  NLTable <- data.frame(read_xlsx(fileName, nlSheetIndex, range = cell_cols(1:4)))
  
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
  
  return (list(
    J_i = J_i,
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
    abnormalCases = abnormalCases,
    NLTable = NLTable
  ))
  
}




