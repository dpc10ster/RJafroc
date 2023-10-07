#' Check the validity of a dataset and FOM combination
#' 
#' @description Checks the validity of a specified dataset and FOM combination. 
#' 
#' 
#' @param dataset The dataset object to be checked. 
#' 
#' @param FOM The figure of merit. 
#' 
#' @param method The analysis method, defaults to "OR".
#' 
#' @param analysisOption The desired generalization: "RRRC", "RRFC", "FRRC" or "ALL", 
#'     defaults to "RRRC" 
#' 
#' 
#' @return \code{TRUE} if combination is valid, \code{FALSE} otherwise.
#' 
#' @export
#' 
isValidDataset <- function(dataset, FOM, method = "OR", analysisOption = "RRRC") {
  
  if (!(analysisOption %in% c("RRRC", "FRRC", "RRFC", "ALL"))) {
    cat("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
    return(FALSE)
  }
  
  if (!(method %in% c("DBM", "OR"))){
    cat(sprintf("%s is not a valid analysis method.\n", method))
    return(FALSE)
  }
  
  if (typeof(dataset) != "list") return (FALSE)
  if (!(length(dataset) == 3)) return (FALSE)
  
  if (!(all(names(dataset) == c("ratings", "lesions", "descriptions")))) return (FALSE)
  
  if (!(all(names(dataset$ratings) == c("NL", "LL", "LL_IL")))) return (FALSE)
  if (!(all(names(dataset$lesions) == c("perCase", "IDs", "weights")))) return (FALSE)
  
  dataType <- dataset$descriptions$type
  if (!(dataType %in% c("ROC", "FROC", "LROC", "ROI")))  return (FALSE)
  if ((dataType == "ROC") && !(FOM %in% c("Wilcoxon"))) return(FALSE)
  if ((dataType %in% c("FROC")) && (FOM == "Wilcoxon")) return(FALSE)
  
  if (!(dataset$descriptions$design %in% c("FCTRL", "FCTRL-X-MOD"))) {
    cat("Study design must be 'FCTRL' or 'FCTRL-X-MOD'")
    return(FALSE)
  }
  
  if (dataset$descriptions$design == "FCTRL") {
    if (!(length(dim(dataset$ratings$NL)) == 4) ||
        !(length(dim(dataset$ratings$LL)) == 4)) {
      cat("Invalid NL and or LL array dimension.\n")
      return(FALSE)
    }
  } else {
    if (!(length(dim(dataset$ratings$NL)) == 5) ||
        !(length(dim(dataset$ratings$LL)) == 5)) {
      cat("Invalid NL and or LL array dimension.\n")
      return(FALSE)
    }
    
    if (!(FOM %in% c("Wilcoxon", "HrAuc", "AFROC", "AFROC1", "wAFROC1", "wAFROC"))) {
      cat("Unsupported FOM for cross-modality analysis.\n")
      return(FALSE)
    }
    
  }
  
  
  if ((dataType %in% c("FROC", "ROI")) && (FOM == "Wilcoxon")) {
    cat("Cannot use `Wilcoxon` FOM with `FROC` or `ROI` data.")
    return(FALSE)
  }
  
  if ((dataType != "ROI") && (FOM == "ROI")) {
    cat(paste0("Only ROI data can be analyzed using ROI figure of merit."))
    return(FALSE)
  }
  
  if (dataType == "LROC") {
    if (FOM != "Wilcoxon") { 
      if (!between(FPFValue, 0, 1)) {
        cat("FPFValue is outside valid range")
        return(FALSE)
      }
    }
  }
  
  if (dataType == "LROC") {
    if (FOM %in% c("Wilcoxon", "ALROC", "PCL")) {
      if (dataType != "LROC") {
        NL <- dataset$ratings$NL
        LL <- dataset$ratings$LL
      } else {
        if (FOM == "Wilcoxon"){
          datasetRoc <- DfLroc2Roc(dataset)
          NL <- datasetRoc$ratings$NL
          LL <- datasetRoc$ratings$LL
        } else if (FOM %in% c("PCL", "ALROC")){
          NL <- dataset$ratings$NL
          LL <- dataset$ratings$LL
        } else {
          cat("incorrect FOM for LROC data")
          return(FALSE)
        }
      }
    } else {
      cat("Incorrect FOM specified for LROC data.\n")
      return(FALSE)
    }
    
    if ((length(dim(dataset$ratings$NL)) == 4)
        && (dataset$descriptions$design == "FCTRL")) { 
      
      I <- dim(NL)[1]
      J <- dim(NL)[2]
      K <- dim(NL)[3]
      K2 <- dim(LL)[3]
      K1 <- K - K2  
      
      if ((K1 == 0) && !(FOM %in% c("AFROC1", "wAFROC1"))) {
        cat(paste0("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases."))
        return(FALSE)
      }
      
      if (!(all(names(dataset$descriptions) == c("fileName", "type",
                                                 "name", "truthTableStr", "design",
                                                 "modalityID", "readerID")))) return (FALSE)
    } else if ((length(dim(dataset$ratings$NL)) == 5) 
               && (dataset$descriptions$design == "FCTRL-X-MOD")) { 
      if (!(all(names(dataset$descriptions) == c("fileName", "type",
                                                 "name", "truthTableStr", "design",
                                                 "modalityID1", "modalityID2", "readerID")))) return (FALSE)
    } else {
      cat("Incorrect study design and or NL length.\n")
      return(FALSE)
    }
  } 
  
  return (TRUE)
  
}


isValidCovEstMethod <- function(FOM, covEstMethod) {
  
  if (!(covEstMethod %in% c("jackknife", "bootstrap", "DeLong"))) {
    cat("Incorrect covEstMethod: must be `jackknife`, `bootstrap` or `DeLong`,\n")
    return(FALSE)
  }
  
  if ((covEstMethod == "DeLong") && (FOM != "Wilcoxong")) {
    cat("For `DeLong` covEstMethod the FOM must be `Wilcoxon`.\n")
    return(FALSE)
  }
  
  return (TRUE)
  
}
