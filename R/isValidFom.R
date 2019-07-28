isValidFom <- function (dataset, FOM){
  if ((dataset$dataType == "ROC") && (FOM != "Wilcoxon")) return (FALSE)
  if ((dataset$dataType == "FROC") && (FOM == "Wilcoxon")) return (FALSE)
  return(TRUE)
}
