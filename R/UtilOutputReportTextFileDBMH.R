OutputTextFileDBMH <- function(dataset,
                               method,
                               methodTxt,
                               ReportFileName,
                               alpha, # decided to restrict to alpha = 0.05 5/21/20
                               FOM,
                               analysisOption,
                               DBM)
{
  sink(ReportFileName)
  Preamble(dataset, FOM, ReportFileName, DBM, methodTxt)
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(dataset$ratings$NL)[3]
  
  x <- c("\n",
         "===========================================================================", 
         "*****        ANOVA Tables (DBM analysis of pseudovalues)              *****", 
         "===========================================================================",
         "                TREATMENT X READER X CASE ANOVA",
         "           Used for global test of equal treatment FOMs and for", 
         "              treatment difference confidence intervals\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- DBM$ANOVA$TRCanova
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\n",
         "===========================================================================", 
         "*****          Reader Case ANOVAs for each treatment                  *****", 
         "===========================================================================",
         "          Used for single treatment confidence intervals in ",
         "                  part (c) of the analysis.",
         "               (msR = mean square Reader, etc.)\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- DBM$ANOVA$IndividualTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENTS:", 
    "The first output is the treatment x reader x case DBM ANOVA table", 
    "used for comparing treatments. The second output is the ",
    "reader x case ANOVA table, one for each treatment, used for computing",
    "single-treatment confidence intervals. For the single-treatment",
    "confidence intervals, only data for the specific treatment are used.\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c("\n",
         "===========================================================================", 
         "*****         Treatment Case ANOVAs for each reader                   *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  df <- DBM$ANOVA$IndividualRdr
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\n",
         "===========================================================================", 
         "*****                   DBM  Variance Components                     *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- DBM$ANOVA$VarCom
  print(format(df, digits = 5, justify = "left"))
  
  df <- UtilDBM2ORVarCom(dim(dataset$ratings$NL)[3], df)
  x <- c("\n",
         "===========================================================================", 
         "*****                   OR  Variance Components                     *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\n",
         "===========================================================================", 
         "*****       RRRC: Random Reader Random Case Analysis                  *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nRRRC (a): Test of NH of no treatment effect\n\n")
  
  df <- DBM$RRRC$FTests
  print(format(df, digits = 5, justify = "left"))
  
  if(DBM$RRRC$FTests["T","p"] < 0.05){
    x <- c("\nCONCLUSION:", 
           "The treatment FOMs are different,",
           sprintf("[F(%1d,%7.4f) = %7.4f, p = %7.4f].",
                   I-1,
                   DBM$RRRC$FTests[2,1], 
                   DBM$RRRC$FTests[1,3], 
                   DBM$RRRC$FTests[1,4]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  } else {
    x <- c("\nCONCLUSION:", 
           "The treatment FOMs are not significantly different,",
           sprintf("[F(%1d,%7.4f) = %7.4f, p = %7.4f].",
                   I-1,
                   DBM$RRRC$FTests[2,1], 
                   DBM$RRRC$FTests[1,3], 
                   DBM$RRRC$FTests[1,4]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  }
  
  x <- c(
    "\nCOMMENTS:",
    "DF = degrees of freedom",
    "MS = means squares",
    "MS(Error): MS(T*R) + max[MS(T*C)-MS(T*R*C),0]",
    "DF(Error) = {MS(T*R) + max[MS(T*C)-MS(T*R*C),0]}**2/{MS(T*R)**2/[(I-1)(J-1)]}",
    "F_obs = MS(Treatment)/MS(Error)",
    "ndf = I - 1, ddf = DF(Error)",
    "F_obs ~ F_{ndf,ddf}\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nRRRC (b): Confidence intervals for reader-averaged \ninter-treatment FOM differences:\n\n")
  
  df <- DBM$RRRC$ciDiffTrt
  print(format(df, digits = 5, justify = "left"))
  x <- c(
    "\nCOMMENTS:",
    "StdErr = sqrt{[1/(J*K)] * [MS(R) + max[MS(C)-MS(R*C),0]]}", 
    "DF same as DF(error term) in (a)",
    "95% CI: Difference +- t(.025;df) * StdErr\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nRRRC (c): 95% treatment confidence intervals based on reader x case ANOVAs for each treatment\n\n")
  
  df <- DBM$RRRC$ciAvgRdrEachTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENTS:",
    "\nError term: MS(R) + max[MS(C) - MS(RC), 0]",
    "StdErr = sqrt{[1/(J*K)] * [MS(R) + max[MS(C)-MS(R*C),0]]}",
    "Df = {MS(R)+ max[MS(C)-MS(R*C),0]}**2/[(MS(R)**2/(J-1)]",
    "The CIs for each treatment are based only on data for that treatment", 
    "This results in a more robust CI, since this CI does not assume equal error",
    "variance and covariances for each treatment.\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c("\n",
         "===========================================================================", 
         "*****       FRRC: Fixed Reader Random Case Analysis                   *****", 
         "===========================================================================\n") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nFRRC (a): Test of NH of no treatment effect\n\n")
  
  df <- DBM$FRRC$FTests
  print(format(df, digits = 5, justify = "left"))
  
  if(DBM$FRRC$FTests["T","p"] < 0.05){
    x <- c("\nCONCLUSION:", 
           "The treatment FOMs are different,",
           sprintf("[F(%1d,%7.4f) = %7.4f, p = %7.4f].",
                   I-1,
                   DBM$FRRC$FTests[2,1], 
                   DBM$FRRC$FTests[1,3], 
                   DBM$FRRC$FTests[1,4]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  } else {
    x <- c("\nCONCLUSION:", 
           "The treatment FOMs are not significantly different,",
           sprintf("[F(%1d,%7.4f) = %7.4f, p = %7.4f].",
                   I-1,
                   DBM$FRRC$FTests[2,1], 
                   DBM$FRRC$FTests[1,3], 
                   DBM$FRRC$FTests[1,4]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  }
  
  x <- c("\nCOMMENTS:", "Error term: MS(TC) \n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nFRRC (b): Confidence intervals for reader-averaged \ninter-treatment FOM differences\n\n")
  
  df <- DBM$FRRC$ciDiffTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\nCOMMENTS:", "Error term: MS(TC) \n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  x <- c("\nFRRC (c): Confidence intervals for reader-averaged treatment FOMs",
         "Individual treatment confidence intervals are based on",
         "reader x case ANOVAs for each treatment (using",
         "only data for the specified treatment)\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- DBM$FRRC$ciAvgRdrEachTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c("\nCOMMENTS:", "Error term: MS(C) \n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("TREATMENT X CASE ANOVAs for each reader\n")
  df <- DBM$ANOVA$IndividualRdr
  print(format(df, digits = 5, justify = "left"))
  
  cat("\n====================")
  x <- c("\nFRRC (d): Confidence intervals",
         "For each reader and treatment pairing reader-averaged treatment FOMs",
         "Treatment-by-case ANOVA CIs for each reader",
         "(each analysis is based only on data for the specified reader)\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- DBM$FRRC$ciDiffTrtEachRdr
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENTS:",
    "TBA: ", 
    "95% CI: Difference +- z(.025) * StdErr\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c("\n",
         "===========================================================================", 
         "*****       RRFC: Random Reader Fixed Case Analysis                   *****", 
         "===========================================================================", 
         "Results apply to the population of readers but only for the cases", 
         "used in this study. Because case is treated as a fixed factor,", 
         "it follows that Cov1 = Cov2 = Cov3 = 0; i.e., there are", 
         "no correlations between reader-performance measures (i.e., FOMs) due", 
         "to reading the same cases. Thus the OR model reduces to a ",
         "conventional treatment x reader ANOVA for the reader-FOMs", 
         "where reader is a random factor and treatment is a fixed factor.") 
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  cat("\nRRFC (a): Test of null hypothesis of no treatment effect\n\n")
  
  df <- DBM$RRFC$FTests
  print(format(df, digits = 5, justify = "left"))
  
  if(DBM$RRFC$FTests["T","p"] < 0.05){
    x <- c("\nCONCLUSION:", 
           "The treatment FOMs are different,",
           sprintf("[F(%1d,%7.4f) = %7.4f, p = %7.4f].",
                   I-1,
                   DBM$RRFC$FTests[2,1], 
                   DBM$RRFC$FTests[1,3], 
                   DBM$RRFC$FTests[1,4]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  } else {
    x <- c("\nCONCLUSION:", 
           "The treatment FOMs are not significantly different,",
           sprintf("[F(%1d,%7.4f) = %7.4f, p = %7.4f].",
                   I-1,
                   DBM$RRFC$FTests[2,1], 
                   DBM$RRFC$FTests[1,3], 
                   DBM$RRFC$FTests[1,4]))
    for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  }
  
  x <- c("\nCOMMENTS:", "Error term: MS(TR) \n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  x <- c("\nRRFC (b):", 
         "Confidence intervals for reader-averaged treatment differences\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- DBM$RRFC$ciDiffTrt
  print(format(df, digits = 5, justify = "left"))
  x <- c(
    "\nCOMMENTS:",
    "StdErr = sqrt[2/J * MS(T*R)]",
    "DF = df[MS(T*R)] = (I-1)(J-1)",
    "95% CI: Difference +- t(.025;df) * StdErr",
    "Note: If there are only 2 treatments, this is equivalent",
    "to a paired t-test applied to the FOMs\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat("\n====================")
  x <- c("\nRRFC (c): Reader-by-case ANOVAs for each treatment",
         "each analysis is based only on data for the", 
         " selected treatment\n\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  df <- DBM$RRFC$ciAvgRdrEachTrt
  print(format(df, digits = 5, justify = "left"))
  
  x <- c(
    "\nCOMMENTS:",
    "StdErr = sqrt[1/J * MS(R)]",
    "DF = df[MS(R)] = J-1",
    "95% CI: AUC +- t(.025;df) * StdErr",
    "Note: this is the conventional CI, treating the reader FOMs",
    "as a random sample\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)
  sink()
  return(sucessfulOutput)
}

