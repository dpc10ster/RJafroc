# extracts three ratings arrays from the 
# dataset; the first is always dataset$ratings$NL
# The second depends on the FOM, and the 
# third is the incorrect localizations array
dataset2ratings <- function (dataset, FOM){
  # if ((length(dataset) == 13) && (dataset$design == "SPLIT-PLOT")) {
  #   # check for SPLIT-PLOT dataset with unimplemmented FOM
  #   # All end-point based FOMs are unimplemented
  #   # cannot use "MaxNLF", "ExpTrnsfmSp", "HrSp" etc. here 
  #   if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp", "MaxLLF", "HrSe")) 
  #     stop("Cannot use MaxNLF, ExpTrnsfmSp, HrSp, MaxLLF, HrSe FOMs with SPLIT-PLOT dataset")
  # }
  
  # OldFormat dataset or NewFormat CROSSED or SPLIT-PLOT dataset with implemented FOM
  dataType <- dataset$descriptions$type
  if (dataType != "LROC") {
    K2 <- length(dataset$ratings$LL[1,1,,1])
  } else if (dataType == "LROC") {
    K2 <- length(dataset$ratings$LL[1,1,,1])
  } else stop("Incorrect data type") # should never get here
  
  if (dataType == "ROC") {
    zjk1 <- drop(dataset$ratings$NL) # must retain the full length K of the  array
    # otherwise the number of cases is thrown off
    # so array returned is J x K
    # This messes up FPF calculation in LrocOperatingPointsFromRatings as K1 is (120-80 = 40) 
    # and FPF-CAD exceeds unity in the middle 
    # and other readers plots do not go to FPF = 1.
    # Did not notice this before as plots flag = TRUE was not tested
    zjk2 <- dataset$LL[,,1:K2,1]
    zjk2Il <- NA
  } else if (dataType == "LROC") {
    if (FOM %in% c("ALROC", "PCL")) {
      zjk1 <- drop(dataset$ratings$NL) # do: must retain the full length K of the  array
      zjk2 <- dataset$ratings$LL[,,1:K2,1]
      zjk2Il <- dataset$ratings$LL_IL[,,1:K2,1]
    } else if (FOM == "Wilcoxon")  {
      datasetRoc <- DfLroc2Roc(dataset)
      zjk1 <- drop(datasetRoc$ratings$NL) # do: must retain the full length K of the  array
      zjk2 <- datasetRoc$ratings$LL[,,1:K2,1]
      zjk2Il <- NA
    } 
  } else if ((dataType == "FROC") && FOM %in% c("HrAuc", "AFROC", "wAFROC")) {
    zjk1 <- dataset$ratings$NL
    zjk2 <- dataset$ratings$LL
    zjk2Il <- NA
  } else if ((dataType == "ROI") && FOM == "ROI") {
    zjk1 <- dataset$ratings$NL 
    zjk2 <- dataset$ratings$LL
    zjk2Il <- NA
  } else stop("incorrect FOM or dataType")
  
  return(list(
    zjk1 = zjk1,
    zjk2 = zjk2,
    zjk2Il = zjk2Il # this is needed in 2T analysis
  ))
}
