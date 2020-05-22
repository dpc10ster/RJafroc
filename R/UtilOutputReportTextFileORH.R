OutputTextFileORH <- function(dataset,
                              method,
                              methodTxt,
                              ReportFileName,
                              alpha, # decided to restrict to alpha = 0.05 5/21/20
                              FOM,
                              analysisOption,
                              OR)
{
  sink(ReportFileName)
  Preamble(dataset, FOM, ReportFileName, OR, methodTxt)
  
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  
  x <- c("\n",
         "===========================================================================", 
         "*****            ANOVA Tables (OR analysis of reader FOMs)            *****", 
         "===========================================================================\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c(
    "TREATMENT X READER ANOVA of FOMs",
    "Used for global test of equal treatment AUCs",
    "and for treatment differences confidence intervals ",
    "in parts (a) and (b) of the analyses.\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$ANOVA$TRanova
  print(format(df, digits = 5, justify = "left"))
  
  msR <- as.data.frame(t(OR$ANOVA$IndividualTrt[,"msREachTrt"]))
  colnames(msR) <- paste0("trt", dataset$modalityID)
  rownames(msR) <- "msR"
  
  x <- c(
    "\nREADER ANOVAs of AUCs for each treatment",
    "(Used for single treatment confidence intervals in part (c) of the analyses).\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  df <- msR
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\n",
         "===========================================================================", 
         "*****                   OR  Variance Components                       *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c(
    "\nObuchowski-Rockette variance component and covariance estimates",
    "(for sample size estimation for future studies)",
    "Note: These are ANOVA estimates which can be negative.\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$ANOVA$VarCom
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\n",
         "===========================================================================", 
         "*****         Corresponding DBM  Variance Components                  *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- UtilOR2DBMVarComp(dim(dataset$NL)[3], df)
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\n",
         "===========================================================================", 
         "*****       RRRC: Random Reader Random Case Analysis                  *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nRRRC (a): Test of NH of no treatment effect\n\n")
  
  df <- OR$RRRC$FTests
  print(format(df, digits = 5, justify = "left"))
  
  if (OR$RRRC$FTests$p[1] < 0.05) {
    x <- c("\nConclusion:", 
           sprintf("The treatment FOMs are not equal [F(%2d,%7.4f) = %6.3f, p = %7.4f].",
                   OR$RRRC$FTests$DF[1], OR$RRRC$FTests$DF[2], OR$RRRC$FTests$FStat[1], OR$RRRC$FTests$p[1]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  } else {
    x <- c("\nConclusion:", 
           sprintf("The treatment FOMs are not significantly different [F(%2d,%7.4f) = %6.3f, p = %7.4f].",
                   OR$RRRC$FTests$DF[1], OR$RRRC$FTests$DF[2], OR$RRRC$FTests$FStat[1], OR$RRRC$FTests$p[1]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  }
  
  x <- c(
    "\nCOMMENT:",
    "Df(error term) = [MS(T*R) + J*max(Cov2 - Cov3,0)]**2/{MS(T*R)**2/[(I-1)(J-1)]}",
    "Note: Error term is the denominator of the F statistic and is a linear",
    "combination of mean squares. The value of this linear",
    "combination is given under the MS column.\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  x <- c(
    "\nRRRC (b): 95% confidence intervals and hypothesis tests (H0: difference = 0)",
    "for treatment FOM differences\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$RRRC$ciDiffTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENT:",
    "StdErr = sqrt{(2/J)*[MS(T*R) + J*max(Cov2 - Cov3,0)]}",
    "Df same as df(error term) from (a)",
    "95% CI: Difference +- t(.025;df) * StdErr\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  x <- c(
    "\nRRRC (c): Single-treatment 95% confidence intervals",
    "(Each analysis is based only on data for the specified treatment, i.e.,",
    "on the treatment-specific reader ANOVA of AUCs and Cov2 estimates.)\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$RRRC$ciAvgRdrEachTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENT:",
    "StdErr = sqrt{1/J * [MS(R) + J*max(Cov2,0)]}",
    "Df = [MS(R)+ max(J*cov2,0)]**2/[(MS(R)**2/(J-1)]",
    "Note: Df is called `ddf_H_single` in Hillis (2007)",
    "95% CI: FOM +- t(.025;df) * StdErr\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c("\n",
         "===========================================================================", 
         "*****       FRRC: Fixed Reader Random Case Analysis                   *****", 
         "===========================================================================\n", 
         "(Results apply to the population of cases but only for the readers used in",
         "this study. Chi-square or Z tests are used; these are appropriate for",
         "moderate or large case sample sizes)\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nFRRC (a): Test of NH of no treatment effect\n\n")
  
  df <- OR$FRRC$FTests
  print(format(df, digits = 5, justify = "left"))
  
  if (OR$FRRC$FTests$p[1] < 0.05) {
    x <- c("\nConclusion:", 
           sprintf("The treatment AUCs are not equal [X2(%1d) = %6.3f, p = %7.4f].",
                   OR$FRRC$FTests$DF[1], OR$FRRC$FTests$Chisq[1], OR$FRRC$FTests$p[1]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  } else {
    x <- c("\nConclusion:", 
           sprintf("The treatment AUCs are not significantly different [X2(%1d) = %6.3f, p = %7.4f].",
                   OR$FRRC$FTests$DF[1], OR$FRRC$FTests$Chisq[1], OR$FRRC$FTests$p[1]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  }
  
  x <- c("\nCOMMENT:", 
         "X2 = (I-1)*MS(T)/[Var - Cov1 + (J-1)*max(Cov2 - Cov3,0)]\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nFRRC (b): 95% confidence intervals for reader-averaged \ninter-treatment FOM differences\n\n")
  
  df <- OR$FRRC$ciDiffTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\nCOMMENT:", 
         "StdErr = sqrt{2/J * [(Var(error) - Cov1 + (J-1)*max(Cov2 - Cov3,0)]}",
         "95% CI: difference +- z(.025) * StdErr\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("TREATMENT X CASE ANOVAs for each reader\n")
  df <- OR$ANOVA$IndividualRdr
  print(format(df, digits = 5, justify = "left"))
  
  cat("\n====================")
  x <- c("\nFRRC (c): Single treatment FOM 95% confidence intervals",
         "(Each analysis is based only on data for the specified treatment, i.e., on",
         "the specific reader ANOVA of AUCs and error-variance and Cov2 estimates.)\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$FRRC$ciAvgRdrEachTrt
  print(format(df, digits = 5, justify = "left"))
  
  cat("\n")
  
  df <- OR$ANOVA$IndividualTrt[,c(3,4)]
  print(format(df, digits = 5, justify = "left"))
  
  cat("\n====================")
  x <- c("\nFRRC (d): Single-reader 95% confidence intervals",
         "and tests (H0: difference = 0) for treatment FOM differences.",
         "(Each analysis is based only on data for the specified reader,",
         "i.e., on the reader-specific FOM, error-variance and Cov1 estimates.)\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$FRRC$ciDiffTrtEachRdr
  print(format(df, digits = 5, justify = "left"))
  
  cat("\n")
  
  df <- OR$ANOVA$IndividualRdr[,c(3,4)]
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENT:",
    "StdErr = sqrt[2*(Var - Cov1)]", 
    "95% CI: Difference +- z(.025) * StdErr\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c("\n",
         "===========================================================================", 
         "*****       RRFC: Random Reader Fixed Case Analysis                   *****", 
         "===========================================================================", 
         "(Results apply to the population of readers but only for the cases used in",
         "this study)",
         "These results result from using the OR model, but treating reader as a random",
         "factor and treatment and case as fixed factors. Because case is treated as a fixed",
         "factor, it follows that Cov1 = Cov2 = Cov3 = 0; i.e., there is no correlation",
         "between reader-performance measures (e.g., FOMs) due to reading the same",
         "cases. Thus the OR model reduces to a conventional treatment x reader ANOVA",
         "for the reader-performance outcomes, where reader is a random factor and",
         "treatment is a fixed factor. This is the same as a repeated measures ANOVA",
         "where treatment is the repeated measures factor, i.e., readers provide an",
         "outcome under each treatment.")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nRRFC (a): Test for H0: Treatments have the same FOM\n\n")
  
  df <- OR$RRFC$FTests
  print(format(df, digits = 5, justify = "left"))
  
  if(OR$RRFC$FTests["T","p"] < 0.05){
    x <- c("\nConclusion:", 
           "The treatment AUCs are not equal,",
           sprintf("[F(%1d,%2d) = %7.4f, p = %7.4f].",I-1,(I-1)*(J-1), OR$RRFC$FTests[1,3], OR$RRFC$FTests[1,4]),
           "Note: If there are only 2 treatments, this is equivalent to a paired t-test applied",
           "to the AUCs")
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  } else {
    x <- c("\nConclusion:", 
           "The treatment AUCs are not significantly different,",
           sprintf("[F(%1d,%2d) = %7.4f, p = %7.4f].",I-1,(I-1)*(J-1), OR$RRFC$FTests[1,3], OR$RRFC$FTests[1,4]),
           "Note: If there are only 2 treatments, this is equivalent to a paired t-test applied",
           "to the AUCs")
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  }
  x <- c("\nCOMMENT:", "Error term: MS(TR) \n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  x <- c("\nRRFC (b):", 
         "95% confidence intervals for reader-averaged treatment differences\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$RRFC$ciDiffTrt
  print(format(df, digits = 5, justify = "left"))
  x <- c(
    "\nCOMMENT:",
    "StdErr = sqrt[2/J * MS(T*R)]",
    "DF = df[MS(T*R)] = (I-1)(J-1)",
    "95% CI: Difference +- t(.025;df) * StdErr",
    "Note: If there are only 2 treatments, this is equivalent to a paired t-test applied",
    "to the AUCs\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  x <- c("\nRRFC (c): Single treatment AUC 95% confidence intervals",
         "(Each analysis is based only on data for the specified treatment,",
         "i.e. on the treatment-specific reader ANOVA of AUCs\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- OR$RRFC$ciAvgRdrEachTrt
  df <- cbind(df, as.data.frame(OR$ANOVA$IndividualTrt[,2]))[,c(1,6,2,3,4,5)]
  cnames <- colnames(df);cnames[2] <- "MS"
  colnames(df) <- cnames
  
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENT:",
    "StdErr = sqrt[1/J * MS(R)]",
    "DF = df[MS(R)] = J-1",
    "95% CI: FOM +- t(.025;df) * StdErr",
    "Note: this is the conventional CI, treating the reader AUCs as a random sample.\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)  
  sink()
  return(sucessfulOutput)
}

