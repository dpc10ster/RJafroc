#' Check the validity of a dataset for FOM and other input parameters
#' 
#' @description Checks the validity of a specified dataset for FOM and other 
#'     input parameters. 
#' 
#' @param dataset The dataset object to be checked. 
#' 
#' @param FOM The figure of merit. 
#' 
#' @param method The analysis method "OR" (default) or "DBM".
#' 
#' @param covEstMethod The covariance estimation method "jackknife" (default), 
#'     "bootstrap" or "DeLong" (for an ROC dataset).
#' 
#' @param analysisOption Specification of the random factor(s): "RRRC" (default), "RRFC", 
#'     or "FRRC. 
#' 
#' 
#' @return None.
#' 
#' @export
#' 
isValidDataset <- function(dataset, FOM, method = "OR", covEstMethod = "jackknife", analysisOption = "RRRC")
{
  
  isValidFOM(dataset, FOM)
  isValidAnalysisOption(dataset, analysisOption)
  isValidCovEstMethod(FOM, covEstMethod)
    
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  
  if ((dataset$descriptions$design == "FCTRL-X-MOD") && 
      (dataset$descriptions$type %in% c("ROI", "LROC"))) {
    errMsg <- paste0("ROI or LROC **cross-modality** analyses is not currently supported.\n")
    stop(errMsg)
  }
  
  if (!(method %in% c("DBM", "OR"))){
    errMsg <- sprintf("%s is not a valid analysis method.\n", method)
    stop(errMsg)
  }
  
  if ((typeof(dataset) != "list") && !(length(dataset) != 3)) {
    errMsg <- sprintf("dataset is not a list or list has incorrect length.\n")
    stop(errMsg)
  }
  
  if (!(all(names(dataset) == c("ratings", "lesions", "descriptions")))) {
    errMsg <- sprintf("dataset list names are incorrect.\n")
    stop(errMsg)
  }
  
  if (!(all(names(dataset$ratings) == c("NL", "LL", "LL_IL")))) {
    errMsg <- sprintf("dataset$ratings list names are incorrect.\n")
    stop(errMsg)
  }
  
  if (!(all(names(dataset$lesions) == c("perCase", "IDs", "weights")))) {
    errMsg <- sprintf("dataset$lesions list names are incorrect.\n")
    stop(errMsg)
  }
  
  if ((dataset$descriptions$design == "FCTRL") && !(all(names(dataset$descriptions) == c("fileName", "type", "name", "truthTableStr", "design", "modalityID", "readerID")))) {
    errMsg <- sprintf("dataset$descriptions list names are incorrect for factorial one treatment dataset.\n")
    stop(errMsg)
  }
  
  if ((dataset$descriptions$design == "FCTRL-X-MOD") && !(all(names(dataset$descriptions) == c("fileName", "type", "name", "truthTableStr", "design", "modalityID1", "modalityID2", "readerID")))) {
    errMsg <- sprintf("dataset$descriptions list names are incorrect for factorial cross modality dataset.\n")
    stop(errMsg)
  }
  
  if (!(dataset$descriptions$type %in% c("ROC", "FROC", "LROC", "ROI"))) {
    errMsg <- sprintf("%s is not a valid dataType.\n", dataset$descriptions$type)
    stop(errMsg)
  }
  
  if (!(dataset$descriptions$design %in% c("FCTRL", "FCTRL-X-MOD"))) {
    errMsg <- sprintf("Study design must be 'FCTRL' or 'FCTRL-X-MOD'.\n")
    stop(errMsg)
  }
  
  if (dataset$descriptions$design == "FCTRL") {
    if ((length(dim(dataset$ratings$NL)) != 4) ||
        (length(dim(dataset$ratings$LL)) != 4)) {
      errMsg <- paste0("Invalid NL, LL array dimensions for FCTRL dataset.\n")
      stop(errMsg)
    }
  } else {
    # cross modality dataset with two treatment factors
    
    if ((length(dim(NL)) != 5) || (length(dim(LL)) != 5)){
      errMsg <- paste0("Conflict with dataset$descriptions$design = FCTRL-X-MOD: incorrect dimensions for NL or LL.\n")
      stop(errMsg)
    }
    
    K <- dim(NL)[4]
    K2 <- dim(LL)[4]
    K1 <- K - K2  
    
    if ((K1 == 0) && !(FOM %in% c("AFROC1", "wAFROC1"))) {
      errMsg <- paste0("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.\n")
      stop(errMsg)
    }
    
    if (!(FOM %in% c("Wilcoxon", "HrAuc", "AFROC", "AFROC1", "wAFROC1", "wAFROC"))) {
      errMsg <- paste0("Unsupported FOM for cross-modality analysis.\n")
      stop(errMsg)
    }
    
  }
  
}  

isValidFOM <- function (dataset, FOM) {
  
  if ((dataset$descriptions$type == "ROC") && !(FOM %in% c("Wilcoxon"))) {
    errMsg <- sprintf("For ROC data only Wilcoxon FOM is allowed.\n")
    stop(errMsg)
  }
  
  if ((dataset$descriptions$type == "FROC") && 
       !(FOM %in% c("HrAuc", "HrSe", "HrSp", "AFROC", "wAFROC", "AFROC1", "wAFROC1", "MaxLLF", "MaxNLF", "MaxNLFAllCases"))) {
    errMsg <- sprintf("FOM = %s not allowed with FROC data.\n", FOM)
    stop(errMsg)
  }
  
  if ((dataset$descriptions$type == "LROC") && !(FOM %in% c("Wilcoxon", "PCL", "ALROC"))) {
    errMsg <- sprintf("FOM = %s not allowed with LROC data.\n", FOM)
    stop(errMsg)
  }
  
  if ((dataset$descriptions$type != "ROI") && (FOM == "ROI")) {
    errMsg <- sprintf("non-ROI data cannot be analyzed using ROI figure of merit.\n")
    stop(errMsg)
  }
  
  if ((dataset$descriptions$type == "ROI") && (FOM != "ROI")) {
    errMsg <- sprintf("ROI data can only be analyzed using ROI figure of merit.\n")
    stop(errMsg)
  }
  
  if ((length(dim(dataset$ratings$NL)) == 4)
      && (dataset$descriptions$design == "FCTRL")) {
    K <- dim(dataset$ratings$NL)[3]
    K2 <- dim(dataset$ratings$LL)[3]
    K1 <- K - K2  
    
    if ((K1 == 0) && !(FOM %in% c("AFROC1", "wAFROC1"))) {
      errMsg <- sprintf("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.\n")
      stop(errMsg)
    }
  } 
}

isValidCovEstMethod <- function(FOM, covEstMethod) {
  
  if (!(covEstMethod %in% c("jackknife", "bootstrap", "DeLong"))) {
    errMsg <- sprintf("Incorrect covEstMethod specified.\n")
    stop(errMsg)
  }
  
  if ((covEstMethod == "DeLong") && (FOM != "Wilcoxong")) {
    errMsg <- sprintf("For `DeLong` covEstMethod method the FOM must be `Wilcoxon`.\n")
    stop(errMsg)
  }
}

isValidAnalysisOption <- function(dataset, analysisOption) {
  
  if (!(analysisOption %in% c("RRRC", "FRRC", "RRFC"))) {
    errMsg <- sprintf("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`.\n")
    stop(errMsg)
  }
  
}

