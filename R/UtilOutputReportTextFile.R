OutputTextFile <- function(dataset,
                           method,
                           methodTxt,
                           ReportFileName,
                           alpha,
                           FOM,
                           analysisOption,
                           StResult)
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  datasetName <- deparse(substitute(dataset))
  ciPercent <- 100 * (1 - alpha)
  write(sprintf(c("RJafroc SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR ", 
                  "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, ", 
                  "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE ", 
                  "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER ", 
                  "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ", 
                  "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS ", 
                  "IN THE SOFTWARE.\n", 
                  "================================================================================\n")), 
        ReportFileName)
  write(sprintf(paste("R version:", R.version$version.string)), ReportFileName, append = TRUE)
  write(sprintf(paste("RJafroc version:", packageVersion("RJafroc"))), ReportFileName, append = TRUE)
  dateTime <- paste0("Run date: ", base::format(Sys.time(), "%b %d %Y %a %X %Z"))
  write(sprintf(dateTime), ReportFileName, append = TRUE)
  write(sprintf(" FOM selected         :     %s", FOM), 
        ReportFileName, append = TRUE)
  write(sprintf(" Input  Data          :     %s", datasetName),
        ReportFileName, append = TRUE)
  write(sprintf(" Output Data Filename :     %s\n", ReportFileName), 
        ReportFileName, append = TRUE)
  write(sprintf("================================================================================\n"), 
        ReportFileName, append = TRUE)
  
  NL <- dataset$NL
  LL <- dataset$LL
  lesionID <- dataset$lesionID
  maxNL <- dim(NL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  nLesionPerCase <- rowSums(lesionID != UNINITIALIZED)
  
  
  write(sprintf(" Significance testing method:  %s", methodTxt), ReportFileName, append = TRUE)
  write(sprintf(" Number of Readers          :  %d", J), ReportFileName, append = TRUE)
  write(sprintf(" Number of Treatments       :  %d", I), ReportFileName, append = TRUE)
  write(sprintf(" Number of Normal Cases     :  %d", K1), ReportFileName, append = TRUE)
  write(sprintf(" Number of Abnormal Cases   :  %d", K2), ReportFileName, append = TRUE)
  write(sprintf(" Fraction of Normal Cases   :  %f", K1/K), ReportFileName, append = TRUE)
  
  if (dataType == "FROC") {
    write(sprintf(" Min number of lesions per diseased case   :  %d", 
                  min(nLesionPerCase)), ReportFileName, append = TRUE)
    write(sprintf(" Max number of lesions per diseased case   :  %d", 
                  max(nLesionPerCase)), ReportFileName, append = TRUE)
    write(sprintf(" Mean number of lesions per diseased case  :  %f", 
                  mean(nLesionPerCase)), ReportFileName, append = TRUE)
    write(sprintf(" Total number of lesions                   :  %d", 
                  sum(nLesionPerCase)), ReportFileName, append = TRUE)
    
    nl <- NL[, , (K1 + 1):K, ]
    dim(nl) <- c(I, J, K2, maxNL)
    maxNLRating <- apply(nl, c(1, 2, 3), max)
    maxLLRating <- apply(LL, c(1, 2, 3), max)
    ILF <- sum(maxNLRating > maxLLRating) + 0.5 * sum(maxNLRating == maxLLRating)
    ILF <- ILF/I/J/K2
    write(sprintf(" Inc. Loc. Frac.          :  %f\n\n", ILF), ReportFileName, append = TRUE)
    write(sprintf("================================================================================\n"), 
          ReportFileName, append = TRUE)
    write(sprintf(" Avg. number of non-lesion localization marks per reader on non-diseased cases: %f", 
                  sum(NL[, , 1:K1, ] != UNINITIALIZED)/(I * J * K1)), ReportFileName, append = TRUE)
    write(sprintf(" Avg. number of non-lesion localization marks per reader on diseased cases:  %f", 
                  sum(NL[, , (K1 + 1):K, ] != UNINITIALIZED)/(I * J * K2)), 
          ReportFileName, append = TRUE)
    write(sprintf(" Avg. number of lesion localization marks per reader :  %f\n", 
                  sum(LL != UNINITIALIZED)/(I * J * K2)), 
          ReportFileName, append = TRUE)
  }
  
  write(sprintf("\n================================================================================\n"), 
        ReportFileName, append = TRUE)
  write(sprintf(" Modality IDs in the input file are:  %s", paste(names(modalityID), collapse = ", ")), 
        ReportFileName, append = TRUE)
  write(sprintf(" Modality IDs in the output file are: %s", paste(modalityID, collapse = ", ")), 
        ReportFileName, append = TRUE)
  write(sprintf(" Reader IDs in the input file are:    %s", paste(names(readerID), collapse = ", ")), 
        ReportFileName, append = TRUE)
  write(sprintf(" Reader IDs in the output file are:   %s", paste(readerID, collapse = ", ")), 
        ReportFileName, append = TRUE)
  
  write(c("================================================================================\n", 
          " ====================================================================", 
          " *****                        Overview                          *****", 
          " ====================================================================", 
          " Three analyses are presented: ", 
          " (1) Analysis 1 treats both readers and cases as random samples", 
          "     --results apply to the reader and case populations;", 
          " (2) Analysis 2 treats only cases as a random sample", 
          "     --results apply to the population of cases but only for the", 
          "     readers used in this study; and", 
          " (3) Analysis 3 treats only readers as a random sample", 
          "     --results apply to the population of readers but only for the", 
          "     cases used in this study.\n", 
          " For all three analyses, the null hypothesis of equal treatments is", 
          sprintf(" tested in part (a), treatment difference %d%% confidence intervals",ciPercent), 
          sprintf(" are given in part (b), and treatment %d%% confidence intervals are", ciPercent), 
          " given in part (c).  Parts (a) and (b) are based on the treatment x", 
          " reader x case ANOVA while part (c) is based on the reader x case", 
          " ANOVA for the specified treatment; these ANOVA tables are displayed", 
          " before the analyses.  Different error terms are used as indicated", 
          " for parts (a), (b), and (c) according to whether readers and cases", 
          " are treated as fixed or random factors.  Note that the treatment", 
          " confidence intervals in part (c) are based only on the data for the", 
          " specified treatment, rather than the pooled data.  Treatment", 
          sprintf(" difference %d%% confidence intervals for each reader are presented", ciPercent), 
          " in part (d) of Analysis 2; each interval is based on the treatment", 
          " x case ANOVA table (not included) for the specified reader.\n"), 
        ReportFileName, append = TRUE)
  
  write(sprintf(c(" ===========================================================================", 
                  " *****                            Estimates                            *****", 
                  " ===========================================================================\n", 
                  "                        TREATMENT")), ReportFileName, append = TRUE)
  
  string <- "              "
  for (i in 1:I) {
    string <- paste0(string, "----------")
    if (i < I) {
      string <- paste0(string, "---")
    }
  }
  write(string, ReportFileName, append = TRUE)
  string <- "  READER      "
  for (i in 1:I) {
    string <- paste0(string, sprintf("%-10.10s", dataset$modalityID[i]))
    if (i < I) {
      string <- paste0(string, "   ")
    }
  }
  write(string, ReportFileName, append = TRUE)
  string <- "----------    "
  for (i in 1:I) {
    string <- paste0(string, "----------")
    if (i < I) {
      string <- paste0(string, "   ")
    }
  }
  write(string, ReportFileName, append = TRUE)
  
  for (j in 1:J) {
    string <- sprintf("%-10.10s    ", dataset$readerID[j])
    for (i in 1:I) {
      string <- paste0(string, sprintf("%10.8f", StResult$FOMs$foms[i, j])) 
      # since StSignificanceTesting returned transpose for `foms`
      if (i < I) {
        string <- paste0(string, "   ")
      }
    }
    write(string, ReportFileName, append = TRUE)
  }
  write("\n", ReportFileName, append = TRUE)
  write(c(" TREATMENT MEANS (averaged across readers)", 
          "----------    -----------------------------"), 
        ReportFileName, append = TRUE)
  for (i in 1:I) {
    string <- paste0(sprintf("%-10.10s    %10.8f", 
                             dataset$modalityID[i], mean(StResult$FOMs$foms[i, ])))
    # since StSignificanceTesting returned transpose for `foms`
    write(string, ReportFileName, append = TRUE)
  }
  write("\n\n", ReportFileName, append = TRUE)
  write(c(" TREATMENT MEAN DIFFERENCES", "----------   ----------    -----------"), 
        ReportFileName, append = TRUE)
  for (i in 1:I) {
    if (i < I) {
      for (ip in (i + 1):I) {
        write(sprintf("%-10.10s - %-10.10s    %10.8f", 
                      dataset$modalityID[i], 
                      dataset$modalityID[ip], 
                      mean(StResult$FOMs$foms[i, ]) - mean(StResult$FOMs$foms[ip, ])), 
              # since StSignificanceTesting returned transpose for `foms`
              ReportFileName, append = TRUE)
      }
    }
  }
  write("\n\n\n", ReportFileName, append = TRUE)
  if (method == "DBMH") {
    if (J > 1) {
      write(sprintf(c(" ===========================================================================", 
                      " *****                          ANOVA Tables                           *****", 
                      " ===========================================================================\n", 
                      " TREATMENT X READER X CASE ANOVA\n", 
                      "Source            SS               DF             MS        ", 
                      "------   --------------------    ------   ------------------")), 
            ReportFileName, append = TRUE)
      for (l in 1:7) {
        write(sprintf(" %5s   %20.8f    %6d   %18.8f", 
                      rownames(StResult$FVCA$TRCanovaDBM)[l], 
                      StResult$FVCA$TRCanovaDBM[l,"SS"], 
                      StResult$FVCA$TRCanovaDBM[l,"DF"], 
                      StResult$FVCA$TRCanovaDBM[l,"MS"]), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" %5s   %20.8f    %6d", 
                    rownames(StResult$FVCA$TRCanovaDBM)[8], 
                    StResult$FVCA$TRCanovaDBM[8, "SS"], 
                    StResult$FVCA$TRCanovaDBM[8, "DF"]), 
            ReportFileName, append = TRUE)
      write("\n\n", ReportFileName, append = TRUE)
      write(" TREATMENT X READER X CASE ANOVA", 
            ReportFileName, append = TRUE)
      write("\n\n", ReportFileName, append = TRUE)
      write("                        Mean Squares", 
            ReportFileName, append = TRUE)
      string <- " Source     df   "
      for (i in 1:I) {
        string <- paste0(string, sprintf("%-10.10s", dataset$modalityID[i]))
        if (i < I) {
          string <- paste0(string, "   ")
        }
      }
      write(string, ReportFileName, append = TRUE)
      string <- " ------    ---   "
      for (i in 1:I) {
        string <- paste0(string, "----------   ")
      }
      write(string, ReportFileName, append = TRUE)
      for (l in 1:3) {
        string <- sprintf("     %2s %6d   ", StResult$FVCA$RCanovaDBMSingleTrt[l, 1], StResult$FVCA$RCanovaDBMSingleTrt[l, 2])
        for (i in 1:I) {
          string <- paste0(string, sprintf("%10.8f", StResult$FVCA$RCanovaDBMSingleTrt[l, i + 2]))
          if (i < I) {
            string <- paste0(string, "   ")
          }
        }
        write(string, ReportFileName, append = TRUE)
      }
    }
    
    write(c(" ===========================================================================", 
            " *****                  Variance Components Estimates                  *****", 
            " ===========================================================================\n", 
            " DBM variance component and covariance estimates\n", 
            "     DBM Component             Estimate    ", 
            " -----------------------  ----------------", 
            sprintf(" Var(R)                  %16.8f", StResult$FVCA$DBMVarComp$varR), 
            sprintf(" Var(C)                  %16.8f", StResult$FVCA$DBMVarComp$varC), sprintf(" Var(T*R)                %16.8f", StResult$FVCA$DBMVarComp$varTR), sprintf(" Var(T*C)                %16.8f", StResult$FVCA$DBMVarComp$varTC), 
            sprintf(" Var(R*C)                %16.8f", StResult$FVCA$DBMVarComp$varRC), sprintf(" Var(Error)              %16.8f", StResult$FVCA$DBMVarComp$varErr)), ReportFileName, append = TRUE)
    
  } else {
    write(c(" ===========================================================================", 
            " *****                  Variance Components Estimates                  *****", 
            " ===========================================================================\n", 
            " Obuchowski-Rockette variance component and covariance estimates\n", 
            "     OR Component             Estimate    ", 
            " -----------------------  ----------------", 
            sprintf(" Var(R)                  %16.8f", StResult$FVCA$ORVarComp$varR), 
            sprintf(" Var(T*R)                %16.8f", StResult$FVCA$ORVarComp$varTR), 
            sprintf(" COV1                    %16.8f", StResult$FVCA$ORVarComp$cov1), 
            sprintf(" COV2                    %16.8f", StResult$FVCA$ORVarComp$cov2), 
            sprintf(" COV3                    %16.8f", StResult$FVCA$ORVarComp$cov3), 
            sprintf(" Var(Error)              %16.8f", StResult$FVCA$ORVarComp$var)), 
          ReportFileName, append = TRUE)
  }
  
  smallestDispalyedPval <- 1e-04
  write("\n", ReportFileName, append = TRUE)
  if (J > 1) {
    write(c(" ===========================================================================", 
            " *****           Analysis 1: Random Readers and Random Cases           *****", 
            " ===========================================================================\n", 
            " (Results apply to the population of readers and cases)\n\n"), 
          ReportFileName, append = TRUE)
    
    write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), 
          ReportFileName, append = TRUE)
    write(c(" Source        DF    Mean Square      F value  Pr > F ", 
            " ----------  ------  ---------------  -------  -------"), 
          ReportFileName, append = TRUE)
    if (method == "DBMH") {
      if (StResult$RRRC$FTests$p >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, 
                      StResult$FVCA$TRCanovaDBM["T", "MS"], 
                      StResult$RRRC$FTests$f, 
                      StResult$RRRC$FTests$p), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      I - 1, StResult$FVCA$TRCanovaDBM["T","MS"], 
                      StResult$RRRC$FTests$f, 
                      smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f", 
                    StResult$RRRC$FTests$ddf, 
                    StResult$FVCA$TRCanovaDBM["TR","MS"] + max(StResult$FVCA$TRCanovaDBM["TC","MS"] - StResult$FVCA$TRCanovaDBM["TRC","MS"])), 
            ReportFileName, append = TRUE)
      write(" Error term: MS(TR) + max[MS(TC) - MS(TRC), 0]\n", 
            ReportFileName, append = TRUE)
    } else {
      # method == "ORH"
      if (StResult$RRRC$FTests$p >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, StResult$FVCA$TRanovaOR["T","MS"], StResult$RRRC$FTests$f, StResult$RRRC$FTests$p), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      I - 1, StResult$FVCA$TRanovaOR["T","MS"], StResult$RRRC$FTests$f, smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f\n", 
                    StResult$RRRC$FTests$ddf, 
                    StResult$FVCA$TRanovaOR["TR","MS"] + max(J * (StResult$FVCA$ORVarComp$cov2 - StResult$FVCA$ORVarComp$cov3), 0)), 
            ReportFileName, append = TRUE)
      write(c(" Error term: MS(TR) + J * max[Cov2 - Cov3, 0]", 
              " Df(error term) = [MS(T*R) + r*max(Cov2 - Cov3,0)]**2/{MS(T*R)**2/[(t-1)(r-1)]}",
              " Note: `Error term` is the denominator of the F statistic and is a linear",
              " combination of mean squares, as defined above.  The value of this linear", 
              " combination is given under the `Mean Square` column",
              " Note: Df(error term) is called `ddf_H` in Hillis (2007)\n"),
            ReportFileName, append = TRUE)
    }
    
    if (StResult$RRRC$FTests$p < alpha) {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, I - 1, StResult$RRRC$FTests$ddf, StResult$RRRC$FTests$f, StResult$RRRC$FTests$p), 
            ReportFileName, 
            append = TRUE)
    } else {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, I - 1, StResult$RRRC$FTests$ddf, StResult$RRRC$FTests$f, StResult$RRRC$FTests$p), 
            ReportFileName, append = TRUE)
    }
    write(sprintf("    b) %d%% confidence intervals for treatment differences, H0: the two treatments are equal\n", ciPercent), 
          ReportFileName, append = TRUE)
    write(c(sprintf("Treatment  Estimate   StdErr      DF      t     Pr > t          %d%% CI      ", ciPercent), 
            "----------  --------  --------  -------  ------  -------  -------------------"), 
          ReportFileName, append = TRUE)
    trts <- as.vector(dataset$modalityID)
    ii <- 1
    for (i in 1:I) {
      if (i < I) {
        for (ip in (i + 1):I) {
          write(sprintf("%-10.10s %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f\n",
                        paste0(trts[i]," - ", trts[ip]),
                        StResult$RRRC$ciDiffTrt[ii, 2], 
                        StResult$RRRC$ciDiffTrt[ii, 3], 
                        StResult$RRRC$ciDiffTrt[ii, 4], 
                        StResult$RRRC$ciDiffTrt[ii, 5], 
                        StResult$RRRC$ciDiffTrt[ii, 6], 
                        StResult$RRRC$ciDiffTrt[ii, 7], 
                        StResult$RRRC$ciDiffTrt[ii, 8]), 
                ReportFileName, append = TRUE)
          ii <- ii + 1
        }
      }
    }
    # write("\n", ReportFileName, append = TRUE)
    
    write(c(" StdErr = sqrt{(2/J)*[MS(T*R) + J*max(Cov2 - Cov3,0)]}", 
            " Df same as df(error term) from (a)",
            " 95% CI: Difference +- t(.025;df) * StdErr\n"),
          ReportFileName, append = TRUE)
    
    if (I == 2) {
      write(" H0: the two treatments are equal.", ReportFileName, append = TRUE)
    } else {
      write(c(sprintf(" * H0: the %d treatments are equal.  To control the overall ", I), 
              " type I error rate at .05, we conclude that treatment differences", 
              " with p < .05 are significant only if the global test in ", 
              " (a) is also significant (i.e, p < .05)."), ReportFileName, append = TRUE)
    }
    if (method == "DBMH") {
      write(" Error term: MS(TR) + max[MS(TC) - MS(TRC), 0]\n\n", ReportFileName, append = TRUE)
    } else {
      write(" Error term: MS(TR) + J * max[Cov2 - Cov3, 0]\n\n", ReportFileName, append = TRUE)
    }
    write(c(sprintf("    c) %d%% treatment confidence intervals based on reader x case ANOVAs", ciPercent), 
            "       for each treatment (each analysis is based only on data for the", 
            "       specified treatment\n"), 
          ReportFileName, append = TRUE)
    write(c(sprintf("  Treatment     Area      Std Error     DF     %d%% Confidence Interval ", ciPercent), 
            "  ----------  ----------  ----------  -------  -------------------------"), 
          ReportFileName, append = TRUE)
    for (i in 1:I) {
      write(sprintf("  %-10.10s  %10.8f  %10.8f  %7.2f  (%10.8f , %10.8f)", 
                    StResult$RRRC$ciAvgRdrEachTrt[i, 1], 
                    StResult$RRRC$ciAvgRdrEachTrt[i, 2], 
                    StResult$RRRC$ciAvgRdrEachTrt[i, 3], 
                    StResult$RRRC$ciAvgRdrEachTrt[i,4], 
                    StResult$RRRC$ciAvgRdrEachTrt[i, 5], 
                    StResult$RRRC$ciAvgRdrEachTrt[i, 6]), 
            ReportFileName, append = TRUE)
    }
    write("\n", ReportFileName, append = TRUE)
    
    if (method == "DBMH") {
      write(" Error term: MS(R) + max[MS(C) - MS(RC), 0]\n\n\n", ReportFileName, append = TRUE)
    } else {
      write(c(" StdErr = sqrt{1/J * [MS(R) + J*max(Cov2,0)]}", 
              " Df = [MS(R)+ max(J*cov2,0)]**2/[(MS(R)**2/(J-1)]",
              " Note: Df is called `ddf_H_single` in Hillis (2007)",
              " 95% CI: AUC +- t(.025;df) * StdErr\n"),
            ReportFileName, append = TRUE)
    }
  }
  
  write(c(" ===========================================================================", 
          " *****           Analysis 2: Fixed Readers and Random Cases            *****", 
          " ===========================================================================\n\n", 
          " (Results apply to the population of cases but only for the readers", 
          " used in this study)\n\n"), 
        ReportFileName, append = TRUE)
  
  write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), 
        ReportFileName, append = TRUE)
  write(c(" Source        DF    Mean Square      F value  Pr > F ", 
          " ----------  ------  ---------------  -------  -------"), 
        ReportFileName, append = TRUE)
  if (method == "DBMH") {
    if (StResult$FRRC$FTests$p >= smallestDispalyedPval) {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                    I - 1, 
                    StResult$FVCA$TRCanovaDBM["T", "MS"], 
                    StResult$FRRC$FTests$f, 
                    StResult$FRRC$FTests$p), 
            ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                    I - 1, 
                    StResult$FVCA$TRCanovaDBM["T", "MS"], 
                    StResult$FRRC$FTests$f, 
                    smallestDispalyedPval), 
            ReportFileName, append = TRUE)
    }
    write(sprintf(" Error       %6.2f  %15.8f", 
                  StResult$FRRC$FTests$ddf, StResult$FVCA$TRCanovaDBM["TC", "MS"]), 
          ReportFileName, append = TRUE)
    write("  Error term: MS(TC)\n", ReportFileName, append = TRUE)
  } else {
    # method == "ORH"
    if (StResult$FRRC$FTests$p >= smallestDispalyedPval) {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                    I - 1, 
                    StResult$FVCA$TRanovaOR["TR","MS"], 
                    StResult$FRRC$FTests$f, 
                    StResult$FRRC$FTests$p), 
            ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                    I - 1, 
                    StResult$FVCA$TRanovaOR["TR","MS"], 
                    StResult$FRRC$FTests$f, 
                    smallestDispalyedPval), 
            ReportFileName, append = TRUE)
    }
    if (J > 1) {
      write(sprintf(" Error       %6.2f  %15.8f", 
                    StResult$FRRC$FTests$ddf, 
                    (StResult$FVCA$DBMVarComp$varErr - StResult$FVCA$DBMVarComp$cov1 + (J - 1) * (StResult$FVCA$DBMVarComp[3, 2] - StResult$FVCA$DBMVarComp[4, 2]))), 
            ReportFileName, append = TRUE)
      write(" Error term: Var - Cov1 + (J - 1) * ( Cov2 - Cov3 )\n", 
            ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Error       %6.2f  %15.8f", 
                    StResult$FRRC$FTests$ddf, (StResult$FVCA$DBMVarComp[1, 2] - StResult$FVCA$DBMVarComp[2, 2])), 
            ReportFileName, append = TRUE)
      write(" Error term: Var - Cov1\n", 
            ReportFileName, append = TRUE)
    }
  }
  
  if (StResult$FRRC$FTests$p < alpha) {
    write(sprintf(" Conclusion: The %s FOMs of treatments are significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                  FOM, I - 1, 
                  StResult$FRRC$FTests$ddf, 
                  StResult$FRRC$FTests$f, 
                  StResult$FRRC$FTests$p), 
          ReportFileName, append = TRUE)
  } else {
    write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                  FOM, 
                  I - 1, 
                  StResult$FRRC$FTests$ddf, 
                  StResult$FRRC$FTests$f, 
                  StResult$FRRC$FTests$p), 
          ReportFileName, append = TRUE)
  }
  
  write(sprintf("    b) %d%% confidence intervals for treatment differences\n", ciPercent), 
        ReportFileName, append = TRUE)
  write(c(sprintf("       Treatment         Estimate   StdErr      DF      t     Pr > t          %d%% CI      ", ciPercent), 
          "----------   ----------  --------  --------  -------  ------  -------  -------------------"), 
        ReportFileName, append = TRUE)
  ii <- 1
  for (i in 1:I) {
    if (i < I) {
      for (ip in (i + 1):I) {
        write(sprintf("%-10.10s - %-10.10s  %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f\n", 
                      dataset$modalityID[i], 
                      dataset$modalityID[ip], 
                      StResult$FRRC$ciDiffTrt[ii, 2], 
                      StResult$FRRC$ciDiffTrt[ii, 3], 
                      StResult$FRRC$ciDiffTrt[ii,4], 
                      StResult$FRRC$ciDiffTrt[ii, 5], 
                      StResult$FRRC$ciDiffTrt[ii, 6], 
                      StResult$FRRC$ciDiffTrt[ii, 7], 
                      StResult$FRRC$ciDiffTrt[ii, 8]), 
              ReportFileName, append = TRUE)
        ii <- ii + 1
      }
    }
  }
  write("\n", ReportFileName, append = TRUE)
  if (I == 2) {
    write(" H0: the two treatments are not significantly different.", ReportFileName, append = TRUE)
  } else {
    write(c(sprintf(" * H0: the %d treatments are equal. To control the overall ", I), 
            " Type I error rate at .05, we conclude that treatment differences", 
            " with p < .05 are significant only if the global test in ", 
            " (a) is also significant (i.e, p < .05)."), 
          ReportFileName, append = TRUE)
  }
  if (method == "DBMH") {
    write(" Error term: MS(TC) \n\n", 
          ReportFileName, append = TRUE)
  } else {
    if (J > 1) {
      write(" Error term: Var - Cov1 + (J - 1) * ( Cov2 - Cov3 )\n", 
            ReportFileName, append = TRUE)
    } else {
      write(" Error term: Var - Cov1\n", 
            ReportFileName, append = TRUE)
    }
  }
  
  write(c(sprintf("    c) %d%% treatment confidence intervals based on reader x case ANOVAs", ciPercent), 
          "       for each treatment (each analysis is based only on data for the", 
          "       specified treatment\n"),
        ReportFileName, append = TRUE)
  write(c(sprintf("  Treatment     Area      Std Error     DF     %d%% Confidence Interval ", ciPercent), 
          "  ----------  ----------  ----------  -------  -------------------------"), 
        ReportFileName, append = TRUE)
  for (i in 1:I) {
    write(sprintf("  %-10.10s  %10.8f  %10.8f  %7.2f  (%10.8f , %10.8f)", 
                  StResult$FRRC$ciAvgRdrEachTrt[i, 1], 
                  StResult$FRRC$ciAvgRdrEachTrt[i, 2], 
                  StResult$FRRC$ciAvgRdrEachTrt[i, 3], 
                  StResult$FRRC$ciAvgRdrEachTrt[i, 4], 
                  StResult$FRRC$ciAvgRdrEachTrt[i, 5], 
                  StResult$FRRC$ciAvgRdrEachTrt[i, 6]), 
          ReportFileName, append = TRUE)
  }
  if (method == "DBMH") {
    write(" Error term: MS(C) \n", 
          ReportFileName, append = TRUE)
  } else {
    if (J > 1) {
      write(" Error term: Var - Cov1 + (J - 1) * ( Cov2 - Cov3 )\n", 
            ReportFileName, append = TRUE)
    } else {
      write(" Error term: Var - Cov1\n", 
            ReportFileName, append = TRUE)
    }
  }
  if (method == "DBMH") {
    write(" TREATMENT X CASE ANOVAs for each reader\n", 
          ReportFileName, append = TRUE)
    write("                        Sum of Squares", 
          ReportFileName, append = TRUE)
    string <- " Source     df   "
    for (j in 1:J) string <- paste0(string, sprintf("%-11.11s   ", 
                                                    dataset$readerID[j]))
    write(string, ReportFileName, append = TRUE)
    string <- " ------    ---   "
    for (j in 1:J) string <- paste0(string, sprintf("-----------   ", dataset$readerID[j]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("      T %6d   ", I - 1)
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", StResult$FRRC$ssAnovaEachRdr[1, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("      C %6d   ", K - 1)
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", StResult$FRRC$ssAnovaEachRdr[2, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("     TC %6d   ", (I - 1) * (K - 1))
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", StResult$FRRC$ssAnovaEachRdr[3, j + 2]))
    write(c(string, "\n\n"), ReportFileName, append = TRUE)
    write("                        Mean Squares", ReportFileName, append = TRUE)
    string <- " Source     df   "
    for (j in 1:J) string <- paste0(string, sprintf("%-11.11s   ", dataset$readerID[j]))
    write(string, ReportFileName, append = TRUE)
    string <- " ------    ---   "
    for (j in 1:J) string <- paste0(string, sprintf("-----------   ", dataset$readerID[j]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("      T %6d   ", I - 1)
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", StResult$FRRC$msAnovaEachRdr[1, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("      C %6d   ", K - 1)
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", StResult$FRRC$msAnovaEachRdr[2, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("     TC %6d   ", (I - 1) * (K - 1))
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", StResult$FRRC$msAnovaEachRdr[3, j + 2]))
    write(c(string, "\n\n\n\n"), ReportFileName, append = TRUE)
  }
  
  write("    d) Treatment-by-case ANOVA CIs for each reader ", ReportFileName, append = TRUE)
  write("       (each analysis is based only on data for the specified reader)\n", 
        ReportFileName, append = TRUE)
  write(c(sprintf("  Reader         Treatment        Estimate  StdErr       DF      t     Pr > t          %d%% CI      ", ciPercent), 
          "---------- ---------- ----------  --------  --------  -------  ------  -------  -------------------"), 
        ReportFileName, append = TRUE)
  l <- 1
  for (j in 1:J) {
    for (i in 1:I) {
      if (i < I) {
        for (ip in (i + 1):I) {
          write(sprintf("%-10.10s %-10.10s-%-10.10s  %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f", 
                        dataset$readerID[j], 
                        dataset$modalityID[i], 
                        dataset$modalityID[ip], 
                        StResult$FRRC$ciDiffTrtEachRdr[l, 3], 
                        StResult$FRRC$ciDiffTrtEachRdr[l, 4], 
                        StResult$FRRC$ciDiffTrtEachRdr[l, 5], 
                        StResult$FRRC$ciDiffTrtEachRdr[l, 6], 
                        StResult$FRRC$ciDiffTrtEachRdr[l, 7], 
                        StResult$FRRC$ciDiffTrtEachRdr[l, 8], 
                        StResult$FRRC$ciDiffTrtEachRdr[l, 9]), 
                ReportFileName, append = TRUE)
          l <- l + 1
        }
      }
    }
  }
  if (method == "ORH") {
    string <- "\nReader  Var(Error)     Cov1   \n------  ----------  ----------"
    write(string, ReportFileName, append = TRUE)
    for (j in 1:J) {
      write(sprintf("%-6.6s  %10.8s  %10.8s", 
                    StResult$FRRC$varCovEachRdr[j, 1], 
                    StResult$FRRC$varCovEachRdr[j, 2], 
                    StResult$FRRC$varCovEachRdr[j, 3]), 
            ReportFileName, append = TRUE)
    }
    write(c(
      "StdErr = sqrt[2*(Var(error) - Cov1)]",
      "95% CI: Difference +- z(.025) * StdErr\n"),
      ReportFileName, append = TRUE)
  }
  
  if (J > 1) {
    write(c(" ===========================================================================", 
            " *****           Analysis 3: Random Readers and Fixed Cases            *****", 
            " ===========================================================================", 
            " (Results apply to the population of readers but only for the cases used in this study)\n"), 
          ReportFileName, append = TRUE)
    write(c(" These results result from using the OR model by treating reader as a random", 
            " factor and treatment and case as fixed factors. Because case is treated as a fixed",
            " factor, it follows that Cov1 = Cov2 = Cov3 = 0; i.e., there is no correlation",
            " between reader-performance measures (e.g, AUCs) due to reading the same",
            " cases. Thus the OR model reduces to a conventional treatment x reader ANOVA",
            " for the reader-performance outcomes, where reader is a random factor and",
            " treatment is a fixed factor. This is the same as a repeated measures ANOVA",
            " where treatment is the repeated measures factor, i.e., readers provide an",
            " outcome (e.g., AUC) under each treatment.\n"), 
          ReportFileName, append = TRUE)
    
    write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), 
          ReportFileName, append = TRUE)
    write(c(" Source        DF    Mean Square      F value  Pr > F ", 
            " ----------  ------  ---------------  -------  -------"), 
          ReportFileName, append = TRUE)
    if (method == "DBMH") {
      if (StResult$RRFC$FTests$p >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, 
                      StResult$FVCA$TRCanovaDBM["T", "MS"], 
                      StResult$RRFC$FTests$f, 
                      StResult$RRFC$FTests$p), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      (I - 1)*(J - 1), 
                      StResult$FVCA$TRCanovaDBM["T", "MS"], 
                      StResult$RRFC$FTests$f, 
                      smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f", 
                    StResult$RRFC$FTests$ddf, 
                    StResult$FVCA$TRCanovaDBM["TR", "MS"]), 
            ReportFileName, append = TRUE)
    } else {
      # method = "ORH
      if (StResult$RRFC$FTests$p >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, 
                      StResult$FVCA$TRanovaOR["T","MS"], 
                      StResult$RRFC$FTests$f, 
                      StResult$RRFC$FTests$p), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      I - 1, 
                      StResult$FVCA$TRanovaOR["T","MS"], 
                      StResult$RRFC$FTests$f, 
                      smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" TR       %6.2f  %15.8f", 
                    StResult$RRFC$FTests$ddf, 
                    StResult$FVCA$TRanovaOR["TR","MS"]), 
            ReportFileName, append = TRUE)
    }
    
    if (StResult$RRFC$FTests$p < alpha) {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, 
                    I - 1, 
                    StResult$RRFC$FTests$ddf, 
                    StResult$RRFC$FTests$f, 
                    StResult$RRFC$FTests$p), ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, 
                    I - 1, 
                    StResult$RRFC$FTests$ddf, 
                    StResult$RRFC$FTests$f, 
                    StResult$RRFC$FTests$p), 
            ReportFileName, append = TRUE)
    }
    write(sprintf("    b) %d%% confidence intervals for treatment differences, H0: the two treatments are equal.\n", ciPercent), 
          ReportFileName, append = TRUE)
    write(c(sprintf("Treatment  Estimate   StdErr      DF      t     Pr > t          %d%% CI      ", ciPercent), 
            "----------  --------  --------  -------  ------  -------  -------------------"), 
          ReportFileName, append = TRUE)
    trts <- as.vector(dataset$modalityID)
    ii <- 1
    for (i in 1:I) {
      if (i < I) {
        for (ip in (i + 1):I) {
          write(sprintf("%-10.10s %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f\n",
                        paste0(trts[i]," - ", trts[ip]),
                        StResult$RRFC$ciDiffTrt[ii, 2], 
                        StResult$RRFC$ciDiffTrt[ii, 3], 
                        StResult$RRFC$ciDiffTrt[ii, 4], 
                        StResult$RRFC$ciDiffTrt[ii, 5], 
                        StResult$RRFC$ciDiffTrt[ii, 6], 
                        StResult$RRFC$ciDiffTrt[ii, 7], 
                        StResult$RRFC$ciDiffTrt[ii, 8]), 
                ReportFileName, append = TRUE)
          ii <- ii + 1
        }
      }
    }
    
    write(c(
      " StdErr = sqrt[2/J * MS(T*R)]",
      " DF = df[MS(T*R)] = (I-1)(J-1)",
      " 95% CI: Difference +- t(.025;df) * StdErr\n"),
      ReportFileName, append = TRUE)
    
    if (I == 2) {
      # write(" H0: the two treatments are equal.", ReportFileName, append = TRUE)
    } else {
      write(c(sprintf(" * H0: the %d treatments are equal.  To control the overall ", I), 
              " type I error rate at .05, we conclude that treatment differences", 
              " with p < .05 are significant only if the global test in ", 
              " (a) is also significant (i.e, p < .05).\n"), 
            ReportFileName, append = TRUE)
    }
    
    write(c("    c) Reader-by-case ANOVAs for each treatment (each analysis is based only on data for the", 
            "       selected treatment\n"), 
          ReportFileName, append = TRUE)
    write(c(sprintf("  Treatment     Area      Std Error     DF     %d%% Confidence Interval ", ciPercent), 
            "  ----------  ----------  ----------  -------  -------------------------"), 
          ReportFileName, append = TRUE)
    for (i in 1:I) {
      write(sprintf("  %-10.10s  %10.8f  %10.8f  %7.2f  (%10.8f , %10.8f)", 
                    StResult$RRFC$ciAvgRdrEachTrt[i, 1], 
                    StResult$RRFC$ciAvgRdrEachTrt[i, 2], 
                    StResult$RRFC$ciAvgRdrEachTrt[i, 3], 
                    StResult$RRFC$ciAvgRdrEachTrt[i,4], 
                    StResult$RRFC$ciAvgRdrEachTrt[i, 5], 
                    StResult$RRFC$ciAvgRdrEachTrt[i, 6]), 
            ReportFileName, append = TRUE)
    }
  }
  
  write(c(
    "StdErr = sqrt[1/J * MS(R)]",
    "DF = df[MS(R)] = J-1",
    "95% CI: AUC +- t(.025;df) * StdErr\n"),
    ReportFileName, append = TRUE)
  
  sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)  
  return(sucessfulOutput)
}

