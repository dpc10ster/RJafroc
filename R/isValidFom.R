isValidFom <- function (dataset, FOM){
  if ((dataset$descriptions$type == "ROC") && (FOM != "Wilcoxon")) return (FALSE)
  if ((dataset$descriptions$type == "FROC") && (FOM == "Wilcoxon")) return (FALSE)
  return(TRUE)
}
