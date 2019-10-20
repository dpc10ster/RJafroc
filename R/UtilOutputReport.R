#' Generate a text formatted report file or an Excel file
#' 
#' @description  Generates a formatted report of the analysis 
#'    and saves it to a text or an Excel file
#' 
#' @param dataset The dataset object to be analyzed (\emph{not the file name}), 
#' see \code{Dataset} in \code{\link{RJafroc-package}}.
#'    
#' @param dataDescription A short string describing the dataset, 
#'    the default is "MyDataDescription: ";
#'    it is inserted, for reference, in the output file.
#'    
#' @param ReportFileBaseName The file base name (sans the extension) for the 
#'    desired report; the default is NULL, in which case the system generates 
#'    a temporary text file, whose name is displayed.  
#'    
#' @param ReportFileExt The report file extension determines the type of output. 
#'    \code{txt}, the default, for 
#'    a text file, \code{xlsx} for an Excel file.
#'    
#' @param method The significance testing method, \code{"ORH"} or 
#'    (the default) \code{"DBMH"}.
#' 
#' @param FOM The figure of merit; see \code{\link{StSignificanceTesting}}.
#' 
#' @param alpha See \code{\link{StSignificanceTesting}}; the default is 0.05.
#' 
#' @param covEstMethod See \code{\link{StSignificanceTesting}}; only needed 
#'     for method = \code{"ORH"}; the default is "Jackknife".
#' 
#' @param nBoots See \code{\link{StSignificanceTesting}}; only needed for 
#'    \code{"ORH"} analysis; the default is 200.
#' 
#' @param sequentialNames A logical variable: if \code{TRUE}, consecutive integers 
#'    (staring from 1) will be used as the treatment and reader IDs in the 
#'    output report. Otherwise, treatment and reader IDs in the original data 
#'    file will be used. This option may be needed for aesthetics. The default
#'    is FALSE.
#'    
#' @param overWrite A \code{logical} variable: if \code{FALSE}, a warning will 
#'    be issued if the report file already exists and the program will wait 
#'    until the user inputs "y" or "n" to determine whether to overwrite the 
#'    existing file. If \code{TRUE}, the existing file will be silently overwritten. 
#'    The default is \code{FALSE}.
#' 
#' 
#' @details
#' A formatted report of the data analysis is written to the output file in 
#'    either text or Excel format.
#' 
#' 
#' @return sigTestResult The object returned by \code{\link{StSignificanceTesting}}.
#' 
#' @examples
#' 
#' \donttest{
#'  # text output is created in a temporary file
#' UtilOutputReport(dataset03, FOM = "Wilcoxon")
#' # Excel output is created in a temporary file
#' UtilOutputReport(dataset03, FOM = "Wilcoxon", ReportFileExt = "xlsx") 
#'
#' }
#'        
#' @importFrom utils packageDescription
#' @importFrom tools file_path_sans_ext file_ext  
#'     
#' @export

UtilOutputReport <- function(dataset, dataDescription = "MyDataDescription: ", 
                             ReportFileBaseName = NULL, ReportFileExt = "txt", 
                             method = "DBMH", FOM, alpha = 0.05, 
                             covEstMethod = "Jackknife", nBoots = 200, 
                             sequentialNames = FALSE, overWrite = FALSE) {
  
  if (!isValidDataset(dataset)) {
    stop("Must specify a valid dataset object.")
  }
  
  if (!isValidFom(dataset, FOM)) {
    stop("Inconsistent dataset - FOM combination")
  }
  
  if (sequentialNames){
    dataset$modalityID <- 1:length(dataset$modalityID)
    dataset$readerID <- 1:length(dataset$readerID)
  }
  
  ReportFileExt <- tolower(ReportFileExt)
  if (!(ReportFileExt %in% c("txt", "xlsx"))) {
    stop("Incorrect file extension specified: must be txt or xlsx")
  }
  
  if (is.null(ReportFileBaseName)) {
    ReportFileName <- tempfile(pattern = "RJafrocUtilOutputReport", fileext = paste0(".", ReportFileExt))
  } else {
    ReportFileName <- paste0(ReportFileBaseName, ".", ReportFileExt)
    if (!overWrite) {
      if (file.exists(ReportFileName)) {
        readInput <- ""
        while (readInput != "y" && readInput != "n") {
          warningMsg <- paste0("WARNING! The file ", ReportFileName, 
                               " already exists. Do you want to replace it? Enter \"y\" to replace or \"n\" to stop.")
          message(warningMsg)
          readInput <- tolower(readline())
        }
        if (readInput == "n") {
          stop("Execution cancelled by user.")
        } 
      }
    }
  }
  cat("\nOutput file name is: \t", ReportFileName, "\n")
  
  if (method == "DBMH") {
    methodTxt <- paste0("DBM-MRMC-HILLIS SIGNIFICANCE TESTING: ", dataDescription)
      sigTestResult <- StSignificanceTesting(dataset, FOM, FPFValue = 0.2, alpha, method)
  } else if (method == "ORH") {
    methodTxt <- paste0("OBUCHOWSKI-ROCKETTE-HILLIS SIGNIFICANCE TESTING: ", dataDescription)
    sigTestResult <- StSignificanceTesting(dataset, FOM, FPFValue = 0.2, alpha, method, covEstMethod, nBoots)
  } else {
    errMsg <- paste0(method, " is not a valid analysis method.")
    stop(errMsg)
  }
  
  if (ReportFileExt == "txt"){
    sucessfulOutput <- OutputTextFile(dataset,
                                      method,
                                      methodTxt,
                                      ReportFileName,
                                      alpha,
                                      FOM,
                                      sigTestResult)
  } else {
    summaryInfo <- data.frame(summaryInfo = c(base::format(Sys.time(), "%b/%d/%Y"), 
                                              basename(ReportFileName)))
    rownames(summaryInfo) <- c("Date", "Output file")
    sucessfulOutput <- OutputExcelFile(dataset,
                                       method,
                                       methodTxt,
                                       ReportFileName,
                                       covEstMethod,
                                       summaryInfo,
                                       alpha,
                                       FOM,
                                       sigTestResult)
  }
  
  return(sigTestResult)
  
} 



OutputTextFile <- function(dataset,
                           method,
                           methodTxt,
                           ReportFileName,
                           alpha,
                           FOM,
                           sigTestResult)
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
  write(sprintf(" Modality IDs in the input file are:  %s\n", paste(names(modalityID), collapse = ", ")), 
        ReportFileName, append = TRUE)
  write(sprintf(" Modality IDs in the output file are: %s\n", paste(modalityID, collapse = ", ")), 
        ReportFileName, append = TRUE)
  write(sprintf(" Reader IDs in the input file are:    %s\n", paste(names(readerID), collapse = ", ")), 
        ReportFileName, append = TRUE)
  write(sprintf(" Reader IDs in the output file are:   %s\n", paste(readerID, collapse = ", ")), 
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
      string <- paste0(string, sprintf("%10.8f", sigTestResult$fomArray[i, j]))
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
                             dataset$modalityID[i], mean(sigTestResult$fomArray[i, ])))
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
                      mean(sigTestResult$fomArray[i, ]) - mean(sigTestResult$fomArray[ip, ])), 
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
                      sigTestResult$anovaY[l, 1], 
                      sigTestResult$anovaY[l, 2], 
                      sigTestResult$anovaY[l, 3], 
                      sigTestResult$anovaY[l, 4]), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" %5s   %20.8f    %6d", 
                    sigTestResult$anovaY[8, 1], 
                    sigTestResult$anovaY[8, 2], 
                    sigTestResult$anovaY[8, 3]), 
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
        string <- sprintf("     %2s %6d   ", sigTestResult$anovaYi[l, 1], sigTestResult$anovaYi[l, 2])
        for (i in 1:I) {
          string <- paste0(string, sprintf("%10.8f", sigTestResult$anovaYi[l, i + 2]))
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
            sprintf(" Var(R)                  %16.8f", sigTestResult$varComp$varComp[1]), 
            sprintf(" Var(C)                  %16.8f", sigTestResult$varComp$varComp[2]), sprintf(" Var(T*R)                %16.8f", sigTestResult$varComp$varComp[3]), sprintf(" Var(T*C)                %16.8f", sigTestResult$varComp$varComp[4]), 
            sprintf(" Var(R*C)                %16.8f", sigTestResult$varComp$varComp[5]), sprintf(" Var(Error)              %16.8f", sigTestResult$varComp$varComp[6])), ReportFileName, append = TRUE)
    
  } else {
    write(c(" ===========================================================================", 
            " *****                  Variance Components Estimates                  *****", 
            " ===========================================================================\n", 
            " Obuchowski-Rockette variance component and covariance estimates\n", 
            "     OR Component             Estimate    ", 
            " -----------------------  ----------------", 
            sprintf(" Var(R)                  %16.8f", 
                    sigTestResult$varComp$varCov[1]), 
            sprintf(" Var(T*R)                %16.8f", 
                    sigTestResult$varComp$varCov[2]), 
            sprintf(" COV1                    %16.8f", 
                    sigTestResult$varComp$varCov[3]), 
            sprintf(" COV2                    %16.8f", 
                    sigTestResult$varComp$varCov[4]), sprintf(" COV3                    %16.8f", sigTestResult$varComp$varCov[5]), 
            sprintf(" Var(Error)              %16.8f", 
                    sigTestResult$varComp$varCov[6])), ReportFileName, append = TRUE)
  }
  
  smallestDispalyedPval <- 1e-04
  write("\n", ReportFileName, append = TRUE)
  if (J > 1) {
    write(c(" ===========================================================================", 
            " *****           Analysis 1: Random Readers and Random Cases           *****", 
            " ===========================================================================\n\n", 
            " (Results apply to the population of readers and cases)\n\n"), 
          ReportFileName, append = TRUE)
    
    write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), 
          ReportFileName, append = TRUE)
    write(c(" Source        DF    Mean Square      F value  Pr > F ", 
            " ----------  ------  ---------------  -------  -------"), 
          ReportFileName, append = TRUE)
    if (method == "DBMH") {
      if (sigTestResult$FTestStatsRRRC$pRRRC >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, 
                      sigTestResult$anovaY[1, 4], 
                      sigTestResult$FTestStatsRRRC$fRRRC, 
                      sigTestResult$FTestStatsRRRC$pRRRC), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      I - 1, sigTestResult$anovaY[1, 4], 
                      sigTestResult$FTestStatsRRRC$fRRRC, 
                      smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f", 
                    sigTestResult$FTestStatsRRRC$ddfRRRC, 
                    sigTestResult$anovaY[4, 4] + max(sigTestResult$anovaY[5, 4] - sigTestResult$anovaY[7, 4])), 
            ReportFileName, append = TRUE)
      write(" Error term: MS(TR) + max[MS(TC) - MS(TRC), 0]\n", 
            ReportFileName, append = TRUE)
    } else {
      if (sigTestResult$FTestStatsRRRC$ddfRRRC >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, sigTestResult$msT, sigTestResult$FTestStatsRRRC$fRRRC, sigTestResult$FTestStatsRRRC$ddfRRRC), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      I - 1, sigTestResult$msT, sigTestResult$FTestStatsRRRC$fRRRC, smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f", 
                    sigTestResult$FTestStatsRRRC$ddfRRRC, 
                    sigTestResult$msTR + max(J * (sigTestResult$varComp[3, 2] - sigTestResult$varComp[4, 2]), 0)), 
            ReportFileName, append = TRUE)
      write(" Error term: MS(TR) + J * max[Cov2 - Cov3, 0]\n", 
            ReportFileName, append = TRUE)
    }
    
    if (sigTestResult$FTestStatsRRRC$ddfRRRC < alpha) {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, I - 1, sigTestResult$FTestStatsRRRC$ddfRRRC, sigTestResult$FTestStatsRRRC$fRRRC, sigTestResult$FTestStatsRRRC$ddfRRRC), 
            ReportFileName, 
            append = TRUE)
    } else {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, I - 1, sigTestResult$FTestStatsRRRC$ddfRRRC, sigTestResult$FTestStatsRRRC$fRRRC, sigTestResult$FTestStatsRRRC$ddfRRRC), 
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
                        sigTestResult$ciDiffTrtRRRC[ii, 2], 
                        sigTestResult$ciDiffTrtRRRC[ii, 3], 
                        sigTestResult$ciDiffTrtRRRC[ii, 4], 
                        sigTestResult$ciDiffTrtRRRC[ii, 5], 
                        sigTestResult$ciDiffTrtRRRC[ii, 6], 
                        sigTestResult$ciDiffTrtRRRC[ii, 7], 
                        sigTestResult$ciDiffTrtRRRC[ii, 8]), 
                ReportFileName, append = TRUE)
          ii <- ii + 1
        }
      }
    }
    write("\n", ReportFileName, append = TRUE)
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
                    sigTestResult$ciAvgRdrEachTrtRRRC[i, 1], 
                    sigTestResult$ciAvgRdrEachTrtRRRC[i, 2], 
                    sigTestResult$ciAvgRdrEachTrtRRRC[i, 3], sigTestResult$ciAvgRdrEachTrtRRRC[i,4], 
                    sigTestResult$ciAvgRdrEachTrtRRRC[i, 5], 
                    sigTestResult$ciAvgRdrEachTrtRRRC[i, 6]), 
            ReportFileName, append = TRUE)
    }
    if (method == "DBMH") {
      write(" Error term: MS(R) + max[MS(C) - MS(RC), 0]\n\n\n", ReportFileName, append = TRUE)
    } else {
      write("\n\n\n", ReportFileName, append = TRUE)
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
    if (sigTestResult$FTestStatsFRRC$ddfFRRC >= smallestDispalyedPval) {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                    I - 1, 
                    sigTestResult$anovaY[1, 4], 
                    sigTestResult$FTestStatsFRRC$fFRRC, 
                    sigTestResult$FTestStatsFRRC$ddfFRRC), 
            ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                    I - 1, 
                    sigTestResult$anovaY[1, 4], 
                    sigTestResult$FTestStatsFRRC$fFRRC, 
                    smallestDispalyedPval), 
            ReportFileName, append = TRUE)
    }
    write(sprintf(" Error       %6.2f  %15.8f", 
                  sigTestResult$FTestStatsFRRC$ddfFRRC, sigTestResult$anovaY[5, 4]), 
          ReportFileName, append = TRUE)
    write("  Error term: MS(TC)\n", ReportFileName, append = TRUE)
  } else {
    if (sigTestResult$FTestStatsFRRC$ddfFRRC >= smallestDispalyedPval) {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                    I - 1, 
                    sigTestResult$msT, 
                    sigTestResult$FTestStatsFRRC$fFRRC, 
                    sigTestResult$FTestStatsFRRC$ddfFRRC), 
            ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                    I - 1, 
                    sigTestResult$msT, 
                    sigTestResult$FTestStatsFRRC$fFRRC, 
                    smallestDispalyedPval), 
            ReportFileName, append = TRUE)
    }
    if (J > 1) {
      write(sprintf(" Error       %6.2f  %15.8f", 
                    sigTestResult$FTestStatsFRRC$ddfFRRC, 
                    (sigTestResult$varComp[1, 2] - sigTestResult$varComp[2, 2] + (J - 1) * (sigTestResult$varComp[3, 2] - sigTestResult$varComp[4, 2]))), 
            ReportFileName, append = TRUE)
      write(" Error term: Var - Cov1 + (J - 1) * ( Cov2 - Cov3 )\n", 
            ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Error       %6.2f  %15.8f", 
                    sigTestResult$FTestStatsFRRC$ddfFRRC, (sigTestResult$varComp[1, 2] - sigTestResult$varComp[2, 2])), 
            ReportFileName, append = TRUE)
      write(" Error term: Var - Cov1\n", 
            ReportFileName, append = TRUE)
    }
  }
  
  if (sigTestResult$FTestStatsFRRC$ddfFRRC < alpha) {
    write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                  FOM, I - 1, 
                  sigTestResult$FTestStatsFRRC$ddfFRRC, 
                  sigTestResult$FTestStatsFRRC$fFRRC, 
                  sigTestResult$FTestStatsFRRC$ddfFRRC), 
          ReportFileName, append = TRUE)
  } else {
    write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                  FOM, 
                  I - 1, 
                  sigTestResult$FTestStatsFRRC$ddfFRRC, 
                  sigTestResult$FTestStatsFRRC$fFRRC, 
                  sigTestResult$FTestStatsFRRC$ddfFRRC), 
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
                      sigTestResult$ciDiffTrtFRRC[ii, 2], 
                      sigTestResult$ciDiffTrtFRRC[ii, 3], 
                      sigTestResult$ciDiffTrtFRRC[ii,4], 
                      sigTestResult$ciDiffTrtFRRC[ii, 5], 
                      sigTestResult$ciDiffTrtFRRC[ii, 6], 
                      sigTestResult$ciDiffTrtFRRC[ii, 7], 
                      sigTestResult$ciDiffTrtFRRC[ii, 8]), 
              ReportFileName, append = TRUE)
        ii <- ii + 1
      }
    }
  }
  write("\n", ReportFileName, append = TRUE)
  if (I == 2) {
    write(" H0: the two treatments are equal.", ReportFileName, append = TRUE)
  } else {
    write(c(sprintf(" * H0: the %d treatments are equal.  To control the overall ", I), 
            " type I error rate at .05, we conclude that treatment differences", 
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
                  sigTestResult$ciAvgRdrEachTrtFRRC[i, 1], 
                  sigTestResult$ciAvgRdrEachTrtFRRC[i, 2], 
                  sigTestResult$ciAvgRdrEachTrtFRRC[i, 3], 
                  sigTestResult$ciAvgRdrEachTrtFRRC[i, 4], 
                  sigTestResult$ciAvgRdrEachTrtFRRC[i, 5], 
                  sigTestResult$ciAvgRdrEachTrtFRRC[i, 6]), 
          ReportFileName, append = TRUE)
  }
  if (method == "DBMH") {
    write(" Error term: MS(C) \n\n\n", 
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
    write(" TREATMENT X CASE ANOVAs for each reader\n\n", 
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
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", sigTestResult$ssAnovaEachRdr[1, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("      C %6d   ", K - 1)
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", sigTestResult$ssAnovaEachRdr[2, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("     TC %6d   ", (I - 1) * (K - 1))
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", sigTestResult$ssAnovaEachRdr[3, j + 2]))
    write(c(string, "\n\n"), ReportFileName, append = TRUE)
    write("                        Mean Squares", ReportFileName, append = TRUE)
    string <- " Source     df   "
    for (j in 1:J) string <- paste0(string, sprintf("%-11.11s   ", dataset$readerID[j]))
    write(string, ReportFileName, append = TRUE)
    string <- " ------    ---   "
    for (j in 1:J) string <- paste0(string, sprintf("-----------   ", dataset$readerID[j]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("      T %6d   ", I - 1)
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", sigTestResult$msAnovaEachRdr[1, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("      C %6d   ", K - 1)
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", sigTestResult$msAnovaEachRdr[2, j + 2]))
    write(string, ReportFileName, append = TRUE)
    string <- sprintf("     TC %6d   ", (I - 1) * (K - 1))
    for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", sigTestResult$msAnovaEachRdr[3, j + 2]))
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
                        sigTestResult$ciDiffTrtEachRdr[l, 3], 
                        sigTestResult$ciDiffTrtEachRdr[l, 4], 
                        sigTestResult$ciDiffTrtEachRdr[l, 5], 
                        sigTestResult$ciDiffTrtEachRdr[l, 6], 
                        sigTestResult$ciDiffTrtEachRdr[l, 7], 
                        sigTestResult$ciDiffTrtEachRdr[l, 8], 
                        sigTestResult$ciDiffTrtEachRdr[l, 9]), 
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
                    sigTestResult$varCovEachRdr[j, 1], 
                    sigTestResult$varCovEachRdr[j, 2], 
                    sigTestResult$varCovEachRdr[j, 3]), 
            ReportFileName, append = TRUE)
    }
  }
  
  write("\n\n", ReportFileName, append = TRUE)
  
  if (J > 1) {
    write(c(" ===========================================================================", 
            " *****           Analysis 3: Random Readers and Fixed Cases            *****", 
            " ===========================================================================", 
            " (Results apply to the population of readers but only for the cases used in this study)\n\n"), 
          ReportFileName, append = TRUE)
    
    write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), 
          ReportFileName, append = TRUE)
    write(c(" Source        DF    Mean Square      F value  Pr > F ", 
            " ----------  ------  ---------------  -------  -------"), 
          ReportFileName, append = TRUE)
    if (method == "DBMH") {
      if (sigTestResult$FTestStatsRRFC$pRRFC >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, 
                      sigTestResult$anovaY[1, 4], 
                      sigTestResult$FTestStatsRRFC$fRRFC, 
                      sigTestResult$FTestStatsRRFC$pRRFC), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      I - 1, 
                      sigTestResult$anovaY[1, 4], 
                      sigTestResult$FTestStatsRRFC$fRRFC, 
                      smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f", 
                    sigTestResult$FTestStatsRRFC$ddfRRFC, 
                    sigTestResult$anovaY[4, 4]), 
            ReportFileName, append = TRUE)
    } else {
      if (sigTestResult$FTestStatsRRFC$pRRFC >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", 
                      I - 1, 
                      sigTestResult$msT, 
                      sigTestResult$FTestStatsRRFC$fRRFC, 
                      sigTestResult$FTestStatsRRFC$pRRFC), 
              ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", 
                      I - 1, 
                      sigTestResult$msT, 
                      sigTestResult$FTestStatsRRFC$fRRFC, 
                      smallestDispalyedPval), 
              ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f", 
                    sigTestResult$FTestStatsRRFC$ddfRRFC, 
                    sigTestResult$msTR), 
            ReportFileName, append = TRUE)
    }
    write(" Error term: MS(TR)\n", 
          ReportFileName, append = TRUE)
    
    if (sigTestResult$FTestStatsRRFC$pRRFC < alpha) {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, 
                    I - 1, 
                    sigTestResult$FTestStatsRRFC$ddfRRFC, 
                    sigTestResult$FTestStatsRRFC$fRRFC, 
                    sigTestResult$FTestStatsRRFC$pRRFC), ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", 
                    FOM, 
                    I - 1, 
                    sigTestResult$FTestStatsRRFC$ddfRRFC, 
                    sigTestResult$FTestStatsRRFC$fRRFC, 
                    sigTestResult$FTestStatsRRFC$pRRFC), 
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
                        sigTestResult$ciDiffTrtRRFC[ii, 2], 
                        sigTestResult$ciDiffTrtRRFC[ii, 3], 
                        sigTestResult$ciDiffTrtRRFC[ii, 4], 
                        sigTestResult$ciDiffTrtRRFC[ii, 5], 
                        sigTestResult$ciDiffTrtRRFC[ii, 6], 
                        sigTestResult$ciDiffTrtRRFC[ii, 7], 
                        sigTestResult$ciDiffTrtRRFC[ii, 8]), 
                ReportFileName, append = TRUE)
          ii <- ii + 1
        }
      }
    }
    
    write("\n", ReportFileName, append = TRUE)
    if (I == 2) {
      write(" H0: the two treatments are equal.", ReportFileName, append = TRUE)
    } else {
      write(c(sprintf(" * H0: the %d treatments are equal.  To control the overall ", I), 
              " type I error rate at .05, we conclude that treatment differences", 
              " with p < .05 are significant only if the global test in ", 
              " (a) is also significant (i.e, p < .05)."), 
            ReportFileName, append = TRUE)
    }
    write("\n\n", ReportFileName, append = TRUE)
    
    write(c("    c) Reader-by-case ANOVAs for each treatment (each analysis is based only on data for the", 
            "       specified treatment\n"), 
          ReportFileName, append = TRUE)
    write(c(sprintf("  Treatment     Area      Std Error     DF     %d%% Confidence Interval ", ciPercent), 
            "  ----------  ----------  ----------  -------  -------------------------"), 
          ReportFileName, append = TRUE)
    for (i in 1:I) {
      write(sprintf("  %-10.10s  %10.8f  %10.8f  %7.2f  (%10.8f , %10.8f)", 
                    sigTestResult$ciAvgRdrEachTrtRRFC[i, 1], 
                    sigTestResult$ciAvgRdrEachTrtRRFC[i, 2], 
                    sigTestResult$ciAvgRdrEachTrtRRFC[i, 3], 
                    sigTestResult$ciAvgRdrEachTrtRRFC[i,4], 
                    sigTestResult$ciAvgRdrEachTrtRRFC[i, 5], 
                    sigTestResult$ciAvgRdrEachTrtRRFC[i, 6]), 
            ReportFileName, append = TRUE)
    }
  }
  
  sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)  
  return(sucessfulOutput)
}

OutputExcelFile <- function(dataset,
                            method,
                            methodTxt,
                            ReportFileName,
                            covEstMethod,
                            summaryInfo,
                            alpha,
                            FOM,
                            sigTestResult)
{
  NL <- dataset$NL
  LL <- dataset$LL
  I <- length(dataset$NL[,1,1,1])
  J <- length(dataset$NL[1,,1,1])
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  #############################################################    
  ## setup up empty excel output file containing Summary worksheet     
  wb <- createWorkbook()
  addWorksheet(wb, "Summary")
  writeData(wb, sheet = "Summary", x = summaryInfo, rowNames = TRUE, colNames = FALSE)
  
  modalityID <- data.frame(output = dataset$modalityID, input = names(dataset$modalityID))
  colnames(modalityID) <- c("Modality ID in output file", "Modality ID in input file")
  writeData(wb, sheet = "Summary", x = modalityID, startRow = 5, colNames = TRUE)
  
  readerID <- data.frame(output = dataset$readerID, input = names(dataset$readerID))
  colnames(readerID) <- c("Reader ID in output file", "Reader ID in input file")
  writeData(wb, sheet = "Summary", x = readerID, startRow = 5, startCol = 3, colNames = TRUE)
  
  if (method == "DBMH"){
    varEstMethod <- "Jackknife"
  }else{
    varEstMethod <- covEstMethod
  }
  
  analysisInfo <- data.frame(info = c(K1, K2, FOM, method, varEstMethod))
  rownames(analysisInfo) <- c("Number of non-diseased cases", 
                              "Number of diseased cases", 
                              "FOM", 
                              "Significance testing", 
                              "Variability estimation method")
  writeData(wb, sheet = "Summary", 
            x = analysisInfo, 
            startRow = 7 + max(I, J), 
            startCol = 1, 
            rowNames = TRUE, 
            colNames = FALSE)
  sty <- createStyle(halign = "center", valign = "center")
  addStyle(wb,  sheet = "Summary", 
           style = sty, rows = seq(1, 11 + max(I, J)), cols = 1:4, gridExpand = TRUE)
  setColWidths(wb, sheet = "Summary", 
               cols = 1:4, widths = "auto", ignoreMergedCells = TRUE)
  
  #############################################################    
  # done with Summary, now create contents of FOMs worksheet    
  addWorksheet(wb, "FOMs")
  setColWidths(wb, sheet = "FOMs", cols = 1:(J + 3), widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "FOMs", cols = 1, widths = 10)
  addStyle(wb,  sheet = "FOMs", style = sty, rows = 1:(I + 2), cols = 1:(J + 3), gridExpand = TRUE)
  fomArray <- as.data.frame(sigTestResult$fomArray)
  if (I == 2){
    fomArray <- cbind(fomArray, apply(fomArray, 1, mean), diff(rev(apply(fomArray, 1, mean))))
  }else{
    fomArray <- cbind(fomArray, apply(fomArray, 1, mean))
  }
  
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  rowNames <- NULL
  for (i in 1:I){
    rowNames <- c(rowNames, paste("Trt", "-", modalityID[i]))
  }
  rownames(fomArray) <- rowNames
  
  colNames <- NULL
  for (j in 1:J){
    colNames <- c(colNames, paste("Rdr", "-", readerID[j]))
  }
  if (I == 2){
    colNames <- c(colNames, "Rdr. Avg.", "Observed effect size")
  }else{
    colNames <- c(colNames, "Rdr. Avg.")
  }
  colnames(fomArray) <- colNames
  
  if (I == 2){
    mergeCells(wb, "FOMs", rows = 1, cols = 1:(J+3))
    mergeCells(wb, "FOMs", rows = 3:(I + 2), cols = (J+3))
  }else{
    mergeCells(wb, "FOMs", rows = 1, cols = 1:(J+2))
  }
  
  writeData(wb, sheet = "FOMs", 
            startRow = 1, 
            x = "FOMs: reader vs. treatment", 
            rowNames = FALSE, colNames = FALSE)
  writeData(wb, sheet = "FOMs", 
            startRow = 2, 
            x = fomArray, 
            rowNames = TRUE, colNames = TRUE)
  
  #############################################################    
  # done with FOMs, now create contents of RRRC worksheet    
  addWorksheet(wb, "RRRC")
  setColWidths(wb, sheet = "RRRC", cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "RRRC", cols = 1, widths = 10)
  testTable <- data.frame(f = sigTestResult$FTestStatsRRRC$fRRRC, ddf = sigTestResult$FTestStatsRRRC$ddfRRRC, p = sigTestResult$FTestStatsRRRC$ddfRRRC)
  names(testTable) <- c("F statistic", "ddf", "P-value")
  writeData(wb, sheet = "RRRC", x = testTable, rowNames = FALSE, colNames = TRUE)
  
  diffTRName <- NULL
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRName <- c(diffTRName, paste(modalityID[i], modalityID[ip], sep = "-"))
    }
  }
  
  diffTable <- sigTestResult$ciDiffTrtRRRC
  diffTable[ , 1] <- diffTRName
  diffTable[ , 2] <- as.numeric(diffTable[ , 2])
  names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr>t",	"Lower",	"Upper")
  writeData(wb, sheet = "RRRC", 
            startRow = 5, 
            x = diffTable, 
            rowNames = FALSE, colNames = TRUE)
  
  mergeCells(wb, "RRRC", rows = 4, cols = 1:8)
  writeData(wb, sheet = "RRRC", startRow = 4, x = "95% CI's FOMs, treatment difference", 
            rowNames = FALSE, colNames = FALSE)
  
  ciTable <- sigTestResult$ciAvgRdrEachTrtRRRC
  ciTable$StdErr <- as.numeric(ciTable$StdErr)
  ciTable$DF <- as.numeric(ciTable$DF)
  ciTable[ , 1] <- modalityID
  names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
  writeData(wb, sheet = "RRRC", 
            startRow = 8 + nrow(diffTable), 
            x = ciTable, rowNames = FALSE, colNames = TRUE)
  addStyle(wb,  sheet = "RRRC", 
           style = sty, 
           rows = 1:(8 + nrow(diffTable) + nrow(ciTable)), 
           cols = 1:(J + 3), gridExpand = TRUE)
  
  writeData(wb, sheet = "RRRC", startRow = 7 + nrow(diffTable), 
            x = "95% CI's FOMs, each treatment", rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "RRRC", rows = 7 + nrow(diffTable), cols = 1:6)
  
  #############################################################    
  # done with RRRC, now create contents of FRRC worksheet    
  addWorksheet(wb, "FRRC")
  setColWidths(wb, sheet = "FRRC", cols = 1:9, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "FRRC", cols = 1, widths = 10)
  testTable <- data.frame(f = sigTestResult$FTestStatsFRRC$fFRRC, ddf = sigTestResult$FTestStatsFRRC$ddfFRRC, p = sigTestResult$FTestStatsFRRC$ddfFRRC)
  if (method == "ORH"){
    testTable$ddf <- "Inf"
  }
  names(testTable) <- c("F statistic", "ddf", "P-value")
  writeData(wb, sheet = "FRRC", x = testTable, rowNames = FALSE, colNames = TRUE)
  
  diffTable <- sigTestResult$ciDiffTrtFRRC
  diffTable[ , 1] <- diffTRName
  diffTable[ , 2] <- as.numeric(diffTable[ , 2])
  if (method == "ORH"){
    diffTable[ , 4] <- "Inf"
  }
  
  names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr > t",	"Lower",	"Upper")
  writeData(wb, sheet = "FRRC", 
            x = diffTable, startRow = 5, 
            rowNames = FALSE, 
            colNames = TRUE)
  
  writeData(wb, sheet = "FRRC", 
            startRow = 4, x = "95% CI's FOMs, treatment difference", 
            rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "FRRC", rows = 4, cols = 1:8)
  
  ciTable <- sigTestResult$ciAvgRdrEachTrtFRRC
  ciTable$StdErr <- as.numeric(ciTable$StdErr)
  ciTable$DF <- as.numeric(ciTable$DF)
  ciTable[ , 1] <- modalityID
  if (method == "ORH"){
    ciTable[ , 4] <- "Inf"
  }
  
  names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
  writeData(wb, sheet = "FRRC", 
            startRow = 8 + nrow(diffTable), 
            x = ciTable, 
            rowNames = FALSE, 
            colNames = TRUE)
  
  writeData(wb, sheet = "FRRC", 
            startRow = 7 + nrow(diffTable), 
            x = "95% CI's FOMs, each treatment", 
            rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "FRRC", rows = 7 + nrow(diffTable), cols = 1:6)
  
  readerNames <- rep(readerID, choose(I, 2))
  trNames <- rep(diffTRName, J)
  diffTableEchR <- sigTestResult$ciDiffTrtEachRdr
  diffTableEchR$Reader <- readerNames
  diffTableEchR$Treatment <- trNames
  if (method == "ORH"){
    diffTableEchR$DF <- "Inf"
  }
  
  names(diffTableEchR)[8:9] <- c("Lower",	"Upper")
  writeData(wb, sheet = "FRRC", 
            startRow = 11 + nrow(diffTable) + nrow(ciTable), 
            x = diffTableEchR, rowNames = FALSE, colNames = TRUE)
  addStyle(wb,  sheet = "FRRC", 
           style = sty, 
           rows = 1:(11 + nrow(diffTable) + nrow(ciTable) + nrow(diffTableEchR)), 
           cols = 1:9, gridExpand = TRUE)
  writeData(wb, sheet = "FRRC", 
            startRow = 10 + nrow(diffTable) + nrow(ciTable), 
            x = "95% CI's FOMs, treatment difference, each reader", 
            rowNames = FALSE, 
            colNames = FALSE)
  mergeCells(wb, "FRRC", rows = 10 + nrow(diffTable) + nrow(ciTable), cols = 1:9)
  
  #############################################################    
  # done with FRRC, now create contents of RRFC worksheet    
  addWorksheet(wb, "RRFC")
  setColWidths(wb, sheet = "RRFC", cols = 1:8, widths = "auto", ignoreMergedCells = TRUE)
  setColWidths(wb, sheet = "RRFC", cols = 1, widths = 10)
  testTable <- data.frame(f = sigTestResult$FTestStatsRRFC$fRRFC, ddf = sigTestResult$FTestStatsRRFC$ddfRRFC, p = sigTestResult$FTestStatsRRFC$pRRFC)
  names(testTable) <- c("F statistic", "ddf", "P-value")
  writeData(wb, sheet = "RRFC", x = testTable, rowNames = FALSE, colNames = TRUE)
  
  diffTable <- sigTestResult$ciDiffTrtRRFC
  diffTable[ , 1] <- diffTRName
  diffTable[ , 2] <- as.numeric(diffTable[ , 2])
  names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr > t",	"Lower",	"Upper")
  writeData(wb, sheet = "RRFC", startRow = 5, 
            x = diffTable, rowNames = FALSE, colNames = TRUE)
  
  writeData(wb, sheet = "RRFC", startRow = 4, 
            x = "95% CI's FOMs, treatment difference", rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "RRFC", rows = 4, cols = 1:8)
  
  ciTable <- sigTestResult$ciAvgRdrEachTrtRRFC
  ciTable$StdErr <- as.numeric(ciTable$StdErr)
  ciTable$DF <- as.numeric(ciTable$DF)
  ciTable[ , 1] <- modalityID
  names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
  writeData(wb, sheet = "RRFC", startRow = 8 + nrow(diffTable), 
            x = ciTable, rowNames = FALSE, colNames = TRUE)
  writeData(wb, sheet = "RRFC", startRow = 7 + nrow(diffTable), 
            x = "95% CI's FOMs, each treatment", rowNames = FALSE, colNames = FALSE)
  mergeCells(wb, "RRFC", rows = 7 + nrow(diffTable), cols = 1:6)
  addStyle(wb,  sheet = "RRFC", style = sty, rows = 1:(8 + nrow(diffTable) + nrow(ciTable)), 
           cols = 1:8, gridExpand = TRUE)
  
  if (method == "DBMH"){
    #############################################################    
    # done with RRFC, now create contents of ANOVA worksheet    
    addWorksheet(wb, "ANOVA")
    setColWidths(wb, sheet = "ANOVA", 
                 cols = 1:9, 
                 widths = "auto", 
                 ignoreMergedCells = TRUE)
    setColWidths(wb, sheet = "ANOVA", 
                 cols = 1, widths = 10)
    writeData(wb, sheet = "ANOVA", 
              x = sigTestResult$varComp, 
              startRow = 2, 
              rowNames = TRUE, 
              colNames = FALSE)
    
    writeData(wb, sheet = "ANOVA", 
              x = "FOM variance components", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 1, cols = 1:2)
    
    writeData(wb, sheet = "ANOVA", 
              x = sigTestResult$anovaY, 
              startRow = 10, 
              rowNames = FALSE, 
              colNames = TRUE)
    writeData(wb, sheet = "ANOVA", 
              startRow = 9,  
              x = "TREATMENT X READER X CASE ANOVA", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 9, cols = 1:4)
    
    colnames(sigTestResult$anovaYi) <- c("Source", "DF", rownames(fomArray))
    writeData(wb, sheet = "ANOVA", 
              x = sigTestResult$anovaYi, 
              startRow = 21, 
              rowNames = FALSE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "ANOVA", 
              startRow = 20,  
              x = "READER X CASE ANOVA for each Trt", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 20, cols = 1:4)
    
    colnames(sigTestResult$msAnovaEachRdr) <- c("Source", "DF", colnames(fomArray)[1:J])
    writeData(wb, sheet = "ANOVA", 
              x = sigTestResult$msAnovaEachRdr, 
              startRow = 27, 
              rowNames = FALSE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "ANOVA", startRow = 26,  
              x = "TREATMENT X CASE ANOVAs (MS) for each reader, assuming fixed reader analysis", 
              rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "ANOVA", rows = 26, cols = 1:(J + 2))
    addStyle(wb,  sheet = "ANOVA", style = sty, rows = 1:30, 
             cols = 1:(2+J), gridExpand = TRUE)
  } else {
    #############################################################    
    # done with RRFC, now create contents of VarComp worksheet    
    addWorksheet(wb, "VarComp")
    setColWidths(wb, sheet = "VarComp", cols = 1:9, widths = "auto", ignoreMergedCells = TRUE)
    setColWidths(wb, sheet = "VarComp", cols = 1, widths = 10)
    
    writeData(wb, sheet = "VarComp", 
              x = sigTestResult$varComp, 
              startRow = 2, 
              rowNames = TRUE, 
              colNames = FALSE)
    writeData(wb, sheet = "VarComp", 
              startRow = 1,  
              x = "OR FOM Variance Covariance Components", 
              rowNames = FALSE, 
              colNames = FALSE)
    mergeCells(wb, "VarComp", rows = 1, cols = 1:2)
    
    sigTestResult$varCovEachRdr[ , 1] <- readerID
    writeData(wb, sheet = "VarComp", 
              x = sigTestResult$varCovEachRdr, 
              startRow = 10, 
              rowNames = FALSE, 
              colNames = TRUE)
    
    writeData(wb, sheet = "VarComp", startRow = 9,  
              x = "OR Variance Covariance Components for each reader, assuming fixed reader analysis", 
              rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "VarComp", rows = 9, cols = 1:3)
    addStyle(wb,  sheet = "VarComp", style = sty, rows = 1:(10 + J), 
             cols = 1:3, gridExpand = TRUE)
  }
  saveWorkbook(wb, ReportFileName, overwrite = TRUE)
  sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)
  return(sucessfulOutput)
}




