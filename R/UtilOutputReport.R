#' Generate a formatted report file
#' 
#' @description  Generate a formatted report of the analysis and save to a text file
#' 
#' @usage UtilOutputReport (dataset, DataFileName, DataFileFormat = "JAFROC", delimiter = ",", dataset, 
#'    dataDescription = deparse(substitute(dataset)), ReportFileName, ReportFileFormat = "txt",
#'    method = "DBMH", FOM = "wJAFROC", alpha = 0.05, covEstMethod = "Jackknife", 
#'    nBoots = 200, renumber = FALSE, overwrite = TRUE)

#' 
#' @param dataset The dataset object to be analyzed (not the file name), \link{RJafroc-package}.
#' @param DataFileName A string variable containing the name of the data file 
#'    to be analyzed, see \link{DfReadDataFile} and "Details".
#' @param DataFileFormat The format of the data specified in \code{DataFileName}: 
#'    see \link{DfReadDataFile} and "Details".
#' @param delimiter See \link{DfReadDataFile}.
#' @param dataDescription Only needed if a dataset is not specified. 
#'    It is a string descriptor of the dataset; the default is the variable 
#'    name of \code{dataset} 
#' @param ReportFileName The file name of the output report file. If this parameter 
#'    is missing, the function will use \code{DataFileName} or \code{dataDescription} 
#'    followed by the underscore separated concatenation of \code{method} 
#'    and \code{FOM} as the output report file.
#' @param ReportFileFormat The format of the output report. The two available formats are 
#'    "txt" and "xlsx", which correspond to a formatted text file and an 
#'    Excel file, respectively. "txt" is the default.
#' @param method The analysis method: \code{"ORH"} or \code{"DBMH"}.
#' @param FOM See \link{StSignificanceTesting}.
#' @param alpha See \link{StSignificanceTesting}.
#' @param covEstMethod See \link{StSignificanceTesting}.
#' @param nBoots See \link{StSignificanceTesting}.
#' @param renumber A logical variable: if \code{TRUE}, consecutive integers 
#'    (staring from 1) will be used as the modality and reader IDs in the 
#'    output report. Otherwise, modality and reader IDs in the original data 
#'    file will be used. This option may be needed for aesthetics.
#' @param overwrite A \code{logical} variable: if \code{FALSE}, a warning will 
#'    be issued if the report file already exists and the program will wait 
#'    until the user inputs "y" or "n" to determine whether to overwrite the 
#'    existing file. If \code{TRUE}, an existing file will be silently overwritten.
#' 
#' 
#' @details
#' At least one of the combinations of \code{DataFileName} and \code{DataFileFormat} 
#'    or \code{dataset} and \code{dataDescription} must be specified. 
#'    If both are specified, the data file \code{DataFileName} is analyzed and 
#'    the \code{dataset} is ignored.
#' 
#' 
#' @return A formatted report of the data analysis, patterned roughly on that of 
#'    OR-DBM MRMC V2.5.
#' 
#' @examples
#' \dontrun{
#' UtilOutputReport(dataset = dataset02, method = "DBMH", FOM = "Wilcoxon", 
#'              dataDescription = "MyROCData", overwrite = TRUE)
#'              
#' UtilOutputReport(dataset = dataset02, method = "DBMH", FOM = "Wilcoxon", 
#' dataDescription = "MyROCData",ReportFileFormat = "xlsx", overwrite = TRUE)
#' 
#' ## Generate a analysis report for a data file.
#' fn <- system.file("extdata", "includedRocData.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' UtilOutputReport(DataFileName = fn, method = "DBMH", FOM = "Wilcoxon",
#'              overwrite = TRUE)
#'              
#' ## Output report for an existing dataset
#' UtilOutputReport(dataset = dataset02, method = "DBMH", FOM = "Wilcoxon", 
#'              ReportFileName = "MyROCDataAnalysis.txt") 
#' UtilOutputReport(dataset = dataset02, method = "ORH", FOM = "Wilcoxon", overwrite = TRUE)
#' ## UtilOutputReport(dataset = dataset05, method = "DBMH", FOM = "Wilcoxon") # ERROR!
#' UtilOutputReport(dataset = dataset05, method = "ORH") # default FOM is wJAFROC
#' UtilOutputReport(dataset = dataset05, method = "DBMH", FOM = "HrAuc")
#' }
#'        
#' @importFrom utils packageDescription
#' @importFrom tools file_path_sans_ext   
#'     
#' @export

UtilOutputReport <- function(dataset, DataFileName, DataFileFormat = "JAFROC", delimiter = ",", 
                             dataDescription = deparse(substitute(dataset)), 
                             ReportFileName, ReportFileFormat = "txt",
                             method = "DBMH", FOM = "wJAFROC", alpha = 0.05, 
                             covEstMethod = "Jackknife", nBoots = 200, 
                             renumber = FALSE, overwrite = TRUE) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  if (missing(dataset) && !missing(DataFileName) && (DataFileFormat %in% c("JAFROC", "MRMC", "iMRMC"))) {
    dataset <- DfReadDataFile(DataFileName, DataFileFormat, delimiter, renumber)
    inputDataList <- FALSE
  } else if (!missing(dataset) && missing(DataFileName) && (DataFileFormat %in% c("JAFROC", "MRMC", "iMRMC"))) {
    inputDataList <- TRUE
    modalityNames <- dataset$modalityID
    readerNames <- dataset$readerID
    if (renumber){
      dataset$modalityID <- 1:length(modalityNames)
      dataset$readerID <- 1:length(readerNames)
    }
    names(dataset$modalityID) <- modalityNames
    names(dataset$readerID) <- readerNames
  } else {
    stop("Please specify a data file or dataset to be analyzed.")
  }
  if (method == "DBMH") {
    methodTxt <- "DBM-MRMC-HILLIS SIGNIFICANCE TESTING"
    result <- StSignificanceTesting(dataset, FOM, alpha, method)
  } else if (method == "ORH") {
    methodTxt <- "OBUCHOWSKI-ROCKETTE-HILLIS SIGNIFICANCE TESTING"
    result <- StSignificanceTesting(dataset, FOM, alpha, method, covEstMethod, nBoots)
  } else {
    errMsg <- paste0(method, " is not a valid analysis method.")
    stop(errMsg)
  }
  
  if (ReportFileFormat == "txt"){
    if (missing(ReportFileName)) {
      if (inputDataList) {
        ReportFileName <- paste0(getwd(), "/", dataDescription, "_", method, "_", FOM, ".txt")
      } else {
        ReportFileName <- paste0(file_path_sans_ext(basename(DataFileName)), "_", method, "_", FOM, ".txt")
      }
    }
    if (!overwrite) {
      if (file.exists(ReportFileName)) {
        readInput <- ""
        while (readInput != "y" && readInput != "n") {
          warningMsg <- paste0("WARNING! The file ", ReportFileName, " already exists. Do you want to replace it? Enter \"y\" to replace or \"n\" to stop.")
          message(warningMsg)
          readInput <- readline()
        }
        if (readInput == "n") {
          stop("Output file exists.")
        }
      }
    }
    ciPercent <- 100 * (1 - alpha)
    write(sprintf(c("RJafroc SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR ", "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, ", "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE ", 
                    "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER ", "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ", "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS ", 
                    "IN THE SOFTWARE.\n", "================================================================================\n")), ReportFileName)
    write(sprintf(paste("R version:", R.version$version.string)), ReportFileName, append = TRUE)
    write(sprintf(paste("RJafroc version:", packageVersion("RJafroc"))), ReportFileName, append = TRUE)
    dateTime <- paste0("Run date: ", base::format(Sys.time(), "%b %d %Y %a %X %Z"))
    write(sprintf(dateTime), ReportFileName, append = TRUE)
    write(sprintf(" FOM selected         :     %s", FOM), ReportFileName, append = TRUE)
    if (inputDataList) {
      write(sprintf(" Input  Data          :     %s", dataDescription), ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Input  Data          :     %s", DataFileName), ReportFileName, append = TRUE)
    }
    write(sprintf(" Output Data Filename :     %s\n", ReportFileName), ReportFileName, append = TRUE)
    write(sprintf("================================================================================\n"), ReportFileName, append = TRUE)
    
    NL <- dataset$NL
    LL <- dataset$LL
    #lesionNum <- dataset$lesionNum
    lesionID <- dataset$lesionID
    #lesionWeight <- dataset$lesionWeight
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
      write(sprintf(" Min number of lesions per diseased case   :  %d", min(nLesionPerCase)), ReportFileName, append = TRUE)
      write(sprintf(" Max number of lesions per diseased case   :  %d", max(nLesionPerCase)), ReportFileName, append = TRUE)
      write(sprintf(" Mean number of lesions per diseased case  :  %f", mean(nLesionPerCase)), ReportFileName, append = TRUE)
      write(sprintf(" Total number of lesions                   :  %d", sum(nLesionPerCase)), ReportFileName, append = TRUE)
      
      nl <- NL[, , (K1 + 1):K, ]
      dim(nl) <- c(I, J, K2, maxNL)
      maxNLRating <- apply(nl, c(1, 2, 3), max)
      maxLLRating <- apply(LL, c(1, 2, 3), max)
      ILF <- sum(maxNLRating > maxLLRating) + 0.5 * sum(maxNLRating == maxLLRating)
      ILF <- ILF/I/J/K2
      write(sprintf(" Inc. Loc. Frac.          :  %f\n\n", ILF), ReportFileName, append = TRUE)
      write(sprintf("================================================================================\n"), ReportFileName, append = TRUE)
      write(sprintf(" Avg. number of non-lesion localization marks per reader on non-diseased cases: %f", sum(NL[, , 1:K1, ] != UNINITIALIZED)/(I * J * K1)), ReportFileName, append = TRUE)
      write(sprintf(" Avg. number of non-lesion localization marks per reader on diseased cases:  %f", sum(NL[, , (K1 + 1):K, ] != UNINITIALIZED)/(I * J * K2)), ReportFileName, append = TRUE)
      write(sprintf(" Avg. number of lesion localization marks per reader :  %f\n", sum(LL != UNINITIALIZED)/(I * J * K2)), ReportFileName, append = TRUE)
    }
    
    write(sprintf("\n================================================================================\n"), ReportFileName, append = TRUE)
    write(sprintf(" Modality IDs in the input file are:  %s\n", paste(names(modalityID), collapse = ", ")), ReportFileName, append = TRUE)
    write(sprintf(" Modality IDs in the output file are: %s\n", paste(modalityID, collapse = ", ")), ReportFileName, append = TRUE)
    write(sprintf(" Reader IDs in the input file are:    %s\n", paste(names(readerID), collapse = ", ")), ReportFileName, append = TRUE)
    write(sprintf(" Reader IDs in the output file are:   %s\n", paste(readerID, collapse = ", ")), ReportFileName, append = TRUE)
    
    write(c("================================================================================\n", " ====================================================================", " *****                        Overview                          *****", 
            " ====================================================================", " Three analyses are presented: ", " (1) Analysis 1 treats both readers and cases as random samples", "     --results apply to the reader and case populations;", 
            " (2) Analysis 2 treats only cases as a random sample", "     --results apply to the population of cases but only for the", "     readers used in this study; and", " (3) Analysis 3 treats only readers as a random sample", 
            "     --results apply to the population of readers but only for the", "     cases used in this study.\n", " For all three analyses, the null hypothesis of equal treatments is", sprintf(" tested in part (a), treatment difference %d%% confidence intervals", 
                                                                                                                                                                                                     ciPercent), sprintf(" are given in part (b), and treatment %d%% confidence intervals are", ciPercent), " given in part (c).  Parts (a) and (b) are based on the treatment x", " reader x case ANOVA while part (c) is based on the reader x case", 
            " ANOVA for the specified treatment; these ANOVA tables are displayed", " before the analyses.  Different error terms are used as indicated", " for parts (a), (b), and (c) according to whether readers and cases", 
            " are treated as fixed or random factors.  Note that the treatment", " confidence intervals in part (c) are based only on the data for the", " specified treatment, rather than the pooled data.  Treatment", 
            sprintf(" difference %d%% confidence intervals for each reader are presented", ciPercent), " in part (d) of Analysis 2; each interval is based on the treatment", " x case ANOVA table (not included) for the specified reader.\n"), 
          ReportFileName, append = TRUE)
    
    write(sprintf(c(" ===========================================================================", " *****                            Estimates                            *****", " ===========================================================================\n", 
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
        string <- paste0(string, sprintf("%10.8f", result$fomArray[i, j]))
        if (i < I) {
          string <- paste0(string, "   ")
        }
      }
      write(string, ReportFileName, append = TRUE)
    }
    write("\n", ReportFileName, append = TRUE)
    write(c(" TREATMENT MEANS (averaged across readers)", "----------    -----------------------------"), ReportFileName, append = TRUE)
    for (i in 1:I) {
      string <- paste0(sprintf("%-10.10s    %10.8f", dataset$modalityID[i], mean(result$fomArray[i, ])))
      write(string, ReportFileName, append = TRUE)
    }
    write("\n\n", ReportFileName, append = TRUE)
    write(c(" TREATMENT MEAN DIFFERENCES", "----------   ----------    -----------"), ReportFileName, append = TRUE)
    for (i in 1:I) {
      if (i < I) {
        for (ip in (i + 1):I) {
          write(sprintf("%-10.10s - %-10.10s    %10.8f", dataset$modalityID[i], dataset$modalityID[ip], mean(result$fomArray[i, ]) - mean(result$fomArray[ip, ])), ReportFileName, append = TRUE)
        }
      }
    }
    write("\n\n\n", ReportFileName, append = TRUE)
    if (method == "DBMH") {
      if (J > 1) {
        write(sprintf(c(" ===========================================================================", " *****                          ANOVA Tables                           *****", " ===========================================================================\n", 
                        " TREATMENT X READER X CASE ANOVA\n", "Source            SS               DF             MS        ", "------   --------------------    ------   ------------------")), ReportFileName, append = TRUE)
        for (l in 1:7) {
          write(sprintf(" %5s   %20.8f    %6d   %18.8f", result$anovaY[l, 1], result$anovaY[l, 2], result$anovaY[l, 3], result$anovaY[l, 4]), ReportFileName, append = TRUE)
        }
        write(sprintf(" %5s   %20.8f    %6d", result$anovaY[8, 1], result$anovaY[8, 2], result$anovaY[8, 3]), ReportFileName, append = TRUE)
        write("\n\n", ReportFileName, append = TRUE)
        write(" TREATMENT X READER X CASE ANOVA", ReportFileName, append = TRUE)
        write("\n\n", ReportFileName, append = TRUE)
        write("                        Mean Squares", ReportFileName, append = TRUE)
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
          string <- sprintf("     %2s %6d   ", result$anovaYi[l, 1], result$anovaYi[l, 2])
          for (i in 1:I) {
            string <- paste0(string, sprintf("%10.8f", result$anovaYi[l, i + 2]))
            if (i < I) {
              string <- paste0(string, "   ")
            }
          }
          write(string, ReportFileName, append = TRUE)
        }
      }
      
      write(c(" ===========================================================================", " *****                  Variance Components Estimates                  *****", " ===========================================================================\n", 
              " DBM variance component and covariance estimates\n", "     DBM Component             Estimate    ", " -----------------------  ----------------", sprintf(" Var(R)                  %16.8f", result$varComp$varComp[1]), 
              sprintf(" Var(C)                  %16.8f", result$varComp$varComp[2]), sprintf(" Var(T*R)                %16.8f", result$varComp$varComp[3]), sprintf(" Var(T*C)                %16.8f", result$varComp$varComp[4]), 
              sprintf(" Var(R*C)                %16.8f", result$varComp$varComp[5]), sprintf(" Var(Error)              %16.8f", result$varComp$varComp[6])), ReportFileName, append = TRUE)
      
    } else {
      write(c(" ===========================================================================", " *****                  Variance Components Estimates                  *****", " ===========================================================================\n", 
              " Obuchowski-Rockette variance component and covariance estimates\n", "     OR Component             Estimate    ", " -----------------------  ----------------", sprintf(" Var(R)                  %16.8f", 
                                                                                                                                                                                        result$varComp$varCov[1]), sprintf(" Var(T*R)                %16.8f", result$varComp$varCov[2]), sprintf(" COV1                    %16.8f", result$varComp$varCov[3]), sprintf(" COV2                    %16.8f", 
                                                                                                                                                                                                                                                                                                                                                                       result$varComp$varCov[4]), sprintf(" COV3                    %16.8f", result$varComp$varCov[5]), sprintf(" Var(Error)              %16.8f", result$varComp$varCov[6])), ReportFileName, append = TRUE)
    }
    
    smallestDispalyedPval <- 1e-04
    write("\n", ReportFileName, append = TRUE)
    if (J > 1) {
      write(c(" ===========================================================================", " *****           Analysis 1: Random Readers and Random Cases           *****", " ===========================================================================\n\n", 
              " (Results apply to the population of readers and cases)\n\n"), ReportFileName, append = TRUE)
      
      write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), ReportFileName, append = TRUE)
      write(c(" Source        DF    Mean Square      F value  Pr > F ", " ----------  ------  ---------------  -------  -------"), ReportFileName, append = TRUE)
      if (method == "DBMH") {
        if (result$pRRRC >= smallestDispalyedPval) {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", I - 1, result$anovaY[1, 4], result$fRRRC, result$pRRRC), ReportFileName, append = TRUE)
        } else {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", I - 1, result$anovaY[1, 4], result$fRRRC, smallestDispalyedPval), ReportFileName, append = TRUE)
        }
        write(sprintf(" Error       %6.2f  %15.8f", result$ddfRRRC, result$anovaY[4, 4] + max(result$anovaY[5, 4] - result$anovaY[7, 4])), ReportFileName, append = TRUE)
        write(" Error term: MS(TR) + max[MS(TC) - MS(TRC), 0]\n", ReportFileName, append = TRUE)
      } else {
        if (result$pRRRC >= smallestDispalyedPval) {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", I - 1, result$msT, result$fRRRC, result$pRRRC), ReportFileName, append = TRUE)
        } else {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", I - 1, result$msT, result$fRRRC, smallestDispalyedPval), ReportFileName, append = TRUE)
        }
        write(sprintf(" Error       %6.2f  %15.8f", result$ddfRRRC, result$msTR + max(J * (result$varComp[3, 2] - result$varComp[4, 2]), 0)), ReportFileName, append = TRUE)
        write(" Error term: MS(TR) + J * max[Cov2 - Cov3, 0]\n", ReportFileName, append = TRUE)
      }
      
      if (result$pRRRC < alpha) {
        write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", FOM, I - 1, result$ddfRRRC, result$fRRRC, result$pRRRC), ReportFileName, 
              append = TRUE)
      } else {
        write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", FOM, I - 1, result$ddfRRRC, result$fRRRC, result$pRRRC), 
              ReportFileName, append = TRUE)
      }
      write(sprintf("    b) %d%% confidence intervals for treatment differences\n", ciPercent), ReportFileName, append = TRUE)
      write(c(sprintf("       Treatment         Estimate   StdErr      DF      t     Pr > t          %d%% CI      ", ciPercent), "----------   ----------  --------  --------  -------  ------  -------  -------------------"), 
            ReportFileName, append = TRUE)
      ii <- 1
      for (i in 1:I) {
        if (i < I) {
          for (ip in (i + 1):I) {
            write(sprintf("%-10.10s - %-10.10s  %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f\n", dataset$modalityID[i], dataset$modalityID[ip], result$ciDiffTrtRRRC[ii, 2], result$ciDiffTrtRRRC[ii, 3], 
                          result$ciDiffTrtRRRC[ii, 4], result$ciDiffTrtRRRC[ii, 5], result$ciDiffTrtRRRC[ii, 6], result$ciDiffTrtRRRC[ii, 7], result$ciDiffTrtRRRC[ii, 8]), ReportFileName, append = TRUE)
            ii <- ii + 1
          }
        }
      }
      write("\n", ReportFileName, append = TRUE)
      if (I == 2) {
        write(" H0: the two treatments are equal.", ReportFileName, append = TRUE)
      } else {
        write(c(sprintf(" * H0: the %d treatments are equal.  To control the overall ", I), " type I error rate at .05, we conclude that treatment differences", " with p < .05 are significant only if the global test in ", 
                " (a) is also significant (i.e, p < .05)."), ReportFileName, append = TRUE)
      }
      if (method == "DBMH") {
        write(" Error term: MS(TR) + max[MS(TC) - MS(TRC), 0]\n\n", ReportFileName, append = TRUE)
      } else {
        write(" Error term: MS(TR) + J * max[Cov2 - Cov3, 0]\n\n", ReportFileName, append = TRUE)
      }
      write(c(sprintf("    c) %d%% treatment confidence intervals based on reader x case ANOVAs", ciPercent), "       for each treatment (each analysis is based only on data for the", "       specified treatment\n"), 
            ReportFileName, append = TRUE)
      write(c(sprintf("  Treatment     Area      Std Error     DF     %d%% Confidence Interval ", ciPercent), "  ----------  ----------  ----------  -------  -------------------------"), ReportFileName, append = TRUE)
      for (i in 1:I) {
        write(sprintf("  %-10.10s  %10.8f  %10.8f  %7.2f  (%10.8f , %10.8f)", result$ciAvgRdrEachTrtRRRC[i, 1], result$ciAvgRdrEachTrtRRRC[i, 2], result$ciAvgRdrEachTrtRRRC[i, 3], result$ciAvgRdrEachTrtRRRC[i, 
                                                                                                                                                                                                               4], result$ciAvgRdrEachTrtRRRC[i, 5], result$ciAvgRdrEachTrtRRRC[i, 6]), ReportFileName, append = TRUE)
      }
      if (method == "DBMH") {
        write(" Error term: MS(R) + max[MS(C) - MS(RC), 0]\n\n\n", ReportFileName, append = TRUE)
      } else {
        write("\n\n\n", ReportFileName, append = TRUE)
      }
    }
    
    
    
    write(c(" ===========================================================================", " *****           Analysis 2: Fixed Readers and Random Cases            *****", " ===========================================================================\n\n", 
            " (Results apply to the population of cases but only for the readers", " used in this study)\n\n"), ReportFileName, append = TRUE)
    
    write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), ReportFileName, append = TRUE)
    write(c(" Source        DF    Mean Square      F value  Pr > F ", " ----------  ------  ---------------  -------  -------"), ReportFileName, append = TRUE)
    if (method == "DBMH") {
      if (result$pFRRC >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", I - 1, result$anovaY[1, 4], result$fFRRC, result$pFRRC), ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", I - 1, result$anovaY[1, 4], result$fFRRC, smallestDispalyedPval), ReportFileName, append = TRUE)
      }
      write(sprintf(" Error       %6.2f  %15.8f", result$ddfFRRC, result$anovaY[5, 4]), ReportFileName, append = TRUE)
      write("  Error term: MS(TC)\n", ReportFileName, append = TRUE)
    } else {
      if (result$pFRRC >= smallestDispalyedPval) {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", I - 1, result$msT, result$fFRRC, result$pFRRC), ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", I - 1, result$msT, result$fFRRC, smallestDispalyedPval), ReportFileName, append = TRUE)
      }
      if (J > 1) {
        write(sprintf(" Error       %6.2f  %15.8f", result$ddfFRRC, (result$varComp[1, 2] - result$varComp[2, 2] + (J - 1) * (result$varComp[3, 2] - result$varComp[4, 2]))), ReportFileName, append = TRUE)
        write(" Error term: Var - Cov1 + (J - 1) * ( Cov2 - Cov3 )\n", ReportFileName, append = TRUE)
      } else {
        write(sprintf(" Error       %6.2f  %15.8f", result$ddfFRRC, (result$varComp[1, 2] - result$varComp[2, 2])), ReportFileName, append = TRUE)
        write(" Error term: Var - Cov1\n", ReportFileName, append = TRUE)
      }
    }
    
    if (result$pFRRC < alpha) {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", FOM, I - 1, result$ddfFRRC, result$fFRRC, result$pFRRC), ReportFileName, append = TRUE)
    } else {
      write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", FOM, I - 1, result$ddfFRRC, result$fFRRC, result$pFRRC), 
            ReportFileName, append = TRUE)
    }
    
    write(sprintf("    b) %d%% confidence intervals for treatment differences\n", ciPercent), ReportFileName, append = TRUE)
    write(c(sprintf("       Treatment         Estimate   StdErr      DF      t     Pr > t          %d%% CI      ", ciPercent), "----------   ----------  --------  --------  -------  ------  -------  -------------------"), 
          ReportFileName, append = TRUE)
    ii <- 1
    for (i in 1:I) {
      if (i < I) {
        for (ip in (i + 1):I) {
          write(sprintf("%-10.10s - %-10.10s  %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f\n", dataset$modalityID[i], dataset$modalityID[ip], result$ciDiffTrtFRRC[ii, 2], result$ciDiffTrtFRRC[ii, 3], result$ciDiffTrtFRRC[ii, 
                                                                                                                                                                                                                                 4], result$ciDiffTrtFRRC[ii, 5], result$ciDiffTrtFRRC[ii, 6], result$ciDiffTrtFRRC[ii, 7], result$ciDiffTrtFRRC[ii, 8]), ReportFileName, append = TRUE)
          ii <- ii + 1
        }
      }
    }
    write("\n", ReportFileName, append = TRUE)
    if (I == 2) {
      write(" H0: the two treatments are equal.", ReportFileName, append = TRUE)
    } else {
      write(c(sprintf(" * H0: the %d treatments are equal.  To control the overall ", I), " type I error rate at .05, we conclude that treatment differences", " with p < .05 are significant only if the global test in ", 
              " (a) is also significant (i.e, p < .05)."), ReportFileName, append = TRUE)
    }
    if (method == "DBMH") {
      write(" Error term: MS(TC) \n\n", ReportFileName, append = TRUE)
    } else {
      if (J > 1) {
        write(" Error term: Var - Cov1 + (J - 1) * ( Cov2 - Cov3 )\n", ReportFileName, append = TRUE)
      } else {
        write(" Error term: Var - Cov1\n", ReportFileName, append = TRUE)
      }
    }
    
    write(c(sprintf("    c) %d%% treatment confidence intervals based on reader x case ANOVAs", ciPercent), "       for each treatment (each analysis is based only on data for the", "       specified treatment\n"), 
          ReportFileName, append = TRUE)
    write(c(sprintf("  Treatment     Area      Std Error     DF     %d%% Confidence Interval ", ciPercent), "  ----------  ----------  ----------  -------  -------------------------"), ReportFileName, append = TRUE)
    for (i in 1:I) {
      write(sprintf("  %-10.10s  %10.8f  %10.8f  %7.2f  (%10.8f , %10.8f)", result$ciAvgRdrEachTrtFRRC[i, 1], result$ciAvgRdrEachTrtFRRC[i, 2], result$ciAvgRdrEachTrtFRRC[i, 3], result$ciAvgRdrEachTrtFRRC[i, 4], 
                    result$ciAvgRdrEachTrtFRRC[i, 5], result$ciAvgRdrEachTrtFRRC[i, 6]), ReportFileName, append = TRUE)
    }
    if (method == "DBMH") {
      write(" Error term: MS(C) \n\n\n", ReportFileName, append = TRUE)
    } else {
      if (J > 1) {
        write(" Error term: Var - Cov1 + (J - 1) * ( Cov2 - Cov3 )\n", ReportFileName, append = TRUE)
      } else {
        write(" Error term: Var - Cov1\n", ReportFileName, append = TRUE)
      }
    }
    if (method == "DBMH") {
      write(" TREATMENT X CASE ANOVAs for each reader\n\n", ReportFileName, append = TRUE)
      write("                        Sum of Squares", ReportFileName, append = TRUE)
      string <- " Source     df   "
      for (j in 1:J) string <- paste0(string, sprintf("%-11.11s   ", dataset$readerID[j]))
      write(string, ReportFileName, append = TRUE)
      string <- " ------    ---   "
      for (j in 1:J) string <- paste0(string, sprintf("-----------   ", dataset$readerID[j]))
      write(string, ReportFileName, append = TRUE)
      string <- sprintf("      T %6d   ", I - 1)
      for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", result$ssAnovaEachRdr[1, j + 2]))
      write(string, ReportFileName, append = TRUE)
      string <- sprintf("      C %6d   ", K - 1)
      for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", result$ssAnovaEachRdr[2, j + 2]))
      write(string, ReportFileName, append = TRUE)
      string <- sprintf("     TC %6d   ", (I - 1) * (K - 1))
      for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", result$ssAnovaEachRdr[3, j + 2]))
      write(c(string, "\n\n"), ReportFileName, append = TRUE)
      write("                        Mean Squares", ReportFileName, append = TRUE)
      string <- " Source     df   "
      for (j in 1:J) string <- paste0(string, sprintf("%-11.11s   ", dataset$readerID[j]))
      write(string, ReportFileName, append = TRUE)
      string <- " ------    ---   "
      for (j in 1:J) string <- paste0(string, sprintf("-----------   ", dataset$readerID[j]))
      write(string, ReportFileName, append = TRUE)
      string <- sprintf("      T %6d   ", I - 1)
      for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", result$msAnovaEachRdr[1, j + 2]))
      write(string, ReportFileName, append = TRUE)
      string <- sprintf("      C %6d   ", K - 1)
      for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", result$msAnovaEachRdr[2, j + 2]))
      write(string, ReportFileName, append = TRUE)
      string <- sprintf("     TC %6d   ", (I - 1) * (K - 1))
      for (j in 1:J) string <- paste0(string, sprintf("%11.7f   ", result$msAnovaEachRdr[3, j + 2]))
      write(c(string, "\n\n\n\n"), ReportFileName, append = TRUE)
    }
    
    
    write("    d) Treatment-by-case ANOVA CIs for each reader ", ReportFileName, append = TRUE)
    write("       (each analysis is based only on data for the specified reader)\n", ReportFileName, append = TRUE)
    write(c(sprintf("  Reader         Treatment        Estimate  StdErr       DF      t     Pr > t          %d%% CI      ", ciPercent), "---------- ---------- ----------  --------  --------  -------  ------  -------  -------------------"), 
          ReportFileName, append = TRUE)
    l <- 1
    for (j in 1:J) {
      for (i in 1:I) {
        if (i < I) {
          for (ip in (i + 1):I) {
            write(sprintf("%-10.10s %-10.10s-%-10.10s  %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f", dataset$readerID[j], dataset$modalityID[i], dataset$modalityID[ip], result$ciDiffTrtEachRdr[l, 3], 
                          result$ciDiffTrtEachRdr[l, 4], result$ciDiffTrtEachRdr[l, 5], result$ciDiffTrtEachRdr[l, 6], result$ciDiffTrtEachRdr[l, 7], result$ciDiffTrtEachRdr[l, 8], result$ciDiffTrtEachRdr[l, 9]), ReportFileName, 
                  append = TRUE)
            l <- l + 1
          }
        }
      }
    }
    if (method == "ORH") {
      string <- "\nReader  Var(Error)     Cov1   \n------  ----------  ----------"
      write(string, ReportFileName, append = TRUE)
      for (j in 1:J) {
        write(sprintf("%-6.6s  %10.8s  %10.8s", result$varCovEachRdr[j, 1], result$varCovEachRdr[j, 2], result$varCovEachRdr[j, 3]), ReportFileName, append = TRUE)
      }
    }
    
    write("\n\n", ReportFileName, append = TRUE)
    
    if (J > 1) {
      write(c(" ===========================================================================", " *****           Analysis 3: Random Readers and Fixed Cases            *****", " ===========================================================================", 
              " (Results apply to the population of readers but only for the cases used in this study)\n\n"), ReportFileName, append = TRUE)
      
      write(sprintf("    a) Test for H0: Treatments have the same %s figure of merit.\n\n", FOM), ReportFileName, append = TRUE)
      write(c(" Source        DF    Mean Square      F value  Pr > F ", " ----------  ------  ---------------  -------  -------"), ReportFileName, append = TRUE)
      if (method == "DBMH") {
        if (result$pRRFC >= smallestDispalyedPval) {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", I - 1, result$anovaY[1, 4], result$fRRFC, result$pRRFC), ReportFileName, append = TRUE)
        } else {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", I - 1, result$anovaY[1, 4], result$fRRFC, smallestDispalyedPval), ReportFileName, append = TRUE)
        }
        write(sprintf(" Error       %6.2f  %15.8f", result$ddfRRFC, result$anovaY[4, 4]), ReportFileName, append = TRUE)
      } else {
        if (result$pRRFC >= smallestDispalyedPval) {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  %7.4f", I - 1, result$msT, result$fRRFC, result$pRRFC), ReportFileName, append = TRUE)
        } else {
          write(sprintf(" Treatment   %6d  %15.8f  %7.2f  <%6.4f", I - 1, result$msT, result$fRRFC, smallestDispalyedPval), ReportFileName, append = TRUE)
        }
        write(sprintf(" Error       %6.2f  %15.8f", result$ddfRRFC, result$msTR), ReportFileName, append = TRUE)
      }
      write(" Error term: MS(TR)\n", ReportFileName, append = TRUE)
      
      if (result$pRRFC < alpha) {
        write(sprintf(" Conclusion: The %s FOMs of treatments are not equal,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", FOM, I - 1, result$ddfRRFC, result$fRRFC, result$pRRFC), ReportFileName, 
              append = TRUE)
      } else {
        write(sprintf(" Conclusion: The %s FOMs of treatments are not significantly different,\n             F(%d,%3.2f) = %3.2f, p = %6.4f.\n\n", FOM, I - 1, result$ddfRRFC, result$fRRFC, result$pRRFC), 
              ReportFileName, append = TRUE)
      }
      write(sprintf("    b) %d%% confidence intervals for treatment differences\n", ciPercent), ReportFileName, append = TRUE)
      write(c(sprintf("       Treatment         Estimate   StdErr      DF      t     Pr > t          %d%% CI      ", ciPercent), "----------   ----------  --------  --------  -------  ------  -------  -------------------"), 
            ReportFileName, append = TRUE)
      ii <- 1
      for (i in 1:I) {
        if (i < I) {
          for (ip in (i + 1):I) {
            write(sprintf("%-10.10s - %-10.10s  %8.5f  %8.5f  %7.2f  %6.2f  %7.4f  %8.5f , %8.5f\n", dataset$modalityID[i], dataset$modalityID[ip], result$ciDiffTrtRRFC[ii, 2], result$ciDiffTrtRRFC[ii, 3], 
                          result$ciDiffTrtRRFC[ii, 4], result$ciDiffTrtRRFC[ii, 5], result$ciDiffTrtRRFC[ii, 6], result$ciDiffTrtRRFC[ii, 7], result$ciDiffTrtRRFC[ii, 8]), ReportFileName, append = TRUE)
            ii <- ii + 1
          }
        }
      }
      
      write("\n", ReportFileName, append = TRUE)
      if (I == 2) {
        write(" H0: the two treatments are equal.", ReportFileName, append = TRUE)
      } else {
        write(c(sprintf(" * H0: the %d treatments are equal.  To control the overall ", I), " type I error rate at .05, we conclude that treatment differences", " with p < .05 are significant only if the global test in ", 
                " (a) is also significant (i.e, p < .05)."), ReportFileName, append = TRUE)
      }
      write("\n\n", ReportFileName, append = TRUE)
      
      write(c("    c) Reader-by-case ANOVAs for each treatment (each analysis is based only on data for the", "       specified treatment\n"), ReportFileName, append = TRUE)
      write(c(sprintf("  Treatment     Area      Std Error     DF     %d%% Confidence Interval ", ciPercent), "  ----------  ----------  ----------  -------  -------------------------"), ReportFileName, append = TRUE)
      for (i in 1:I) {
        write(sprintf("  %-10.10s  %10.8f  %10.8f  %7.2f  (%10.8f , %10.8f)", result$ciAvgRdrEachTrtRRFC[i, 1], result$ciAvgRdrEachTrtRRFC[i, 2], result$ciAvgRdrEachTrtRRFC[i, 3], result$ciAvgRdrEachTrtRRFC[i, 
                                                                                                                                                                                                               4], result$ciAvgRdrEachTrtRRFC[i, 5], result$ciAvgRdrEachTrtRRFC[i, 6]), ReportFileName, append = TRUE)
      }
    }
    
    sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)
    
  }else if (ReportFileFormat == "xlsx"){
    if (missing(ReportFileName)) {
      if (inputDataList) {
        ReportFileName <- paste0(getwd(), "/", dataDescription, "Output", ".xlsx")
        summaryInfo <- data.frame(summaryInfo = c(base::format(Sys.time(), "%b/%d/%Y"), dataDescription, basename(ReportFileName)))
      } else {
        ReportFileName <- paste0(file_path_sans_ext(DataFileName), "Output", ".xlsx")
        summaryInfo <- data.frame(summaryInfo = c(base::format(Sys.time(), "%b/%d/%Y"), basename(DataFileName), basename(ReportFileName)))
      }
    }else{
      if (inputDataList) {
        summaryInfo <- data.frame(summaryInfo = c(base::format(Sys.time(), "%b/%d/%Y"), dataDescription, basename(ReportFileName)))
      } else {
        summaryInfo <- data.frame(summaryInfo = c(base::format(Sys.time(), "%b/%d/%Y"), basename(DataFileName), basename(ReportFileName)))
      }
    }
    rownames(summaryInfo) <- c("Date", "Input file", "Output file")
    if (!overwrite) {
      if (file.exists(ReportFileName)) {
        readInput <- ""
        while (readInput != "y" && readInput != "n") {
          warningMsg <- paste0("WARNING! The file ", ReportFileName, " already exists. Do you want to replace it? Enter \"y\" to replace or \"n\" to stop.")
          message(warningMsg)
          readInput <- readline()
        }
        if (readInput == "n") {
          stop("Output file exists.")
        }
      }
    }
    
    NL <- dataset$NL
    LL <- dataset$LL
    I <- length(dataset$NL[,1,1,1])
    J <- length(dataset$NL[1,,1,1])
    K <- dim(NL)[3]
    K2 <- dim(LL)[3]
    K1 <- K - K2
    
    ## setup up empty excel output file containing Summary worksheet     
    wb <- createWorkbook()
    addWorksheet(wb, "Summary")
    setColWidths(wb, sheet = "Summary", cols = 1:4, widths = "auto")
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
    rownames(analysisInfo) <- c("Number of non-diseased cases", "Number of diseased cases", "FOM", "Significance testing", "Variability estimation method")
    writeData(wb, sheet = "Summary", x = analysisInfo, startRow = 7 + max(I, J), startCol = 1, rowNames = TRUE, colNames = FALSE)
    sty <- createStyle(halign = "center", valign = "center")
    addStyle(wb,  sheet = "Summary", style = sty, rows = seq(1, 11 + max(I, J)), cols = 1:4, gridExpand = TRUE)
    
    # done with Summary, now create contents of FOMs worksheet    
    addWorksheet(wb, "FOMs")
    fomArray <- as.data.frame(result$fomArray)
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
      mergeCells(wb, "FOMs", rows = 3:4, cols = (J+3))
      # addMergedRegion(fomSheet, 1, 1, 1, J + 3)
      # addMergedRegion(fomSheet, 3, 4, J + 3, J + 3)
      # autoSizeColumn(fomSheet, colIndex = 1:(J + 3))
    }else{
      mergeCells(wb, "FOMs", rows = 1, cols = 1:(J+2))
      # addMergedRegion(fomSheet, 1, 1, 1, (J + 2))
      # autoSizeColumn(fomSheet, colIndex = 1:(J + 2))
    }
    
    writeData(wb, sheet = "FOMs", startRow = 1, x = "FOMs: reader vs. treatment", rowNames = FALSE, colNames = FALSE)
    writeData(wb, sheet = "FOMs", startRow = 2, x = fomArray, rowNames = TRUE, colNames = TRUE)
    
    # done with FOMs, now create contents of RRRC worksheet    
    addWorksheet(wb, "RRRC")
    testTable <- data.frame(f = result$fRRRC, ddf = result$ddfRRRC, p = result$pRRRC)
    names(testTable) <- c("F statistic", "ddf", "P-value")
    writeData(wb, sheet = "RRRC", x = testTable, rowNames = FALSE, colNames = TRUE)
    # addDataFrame(testTable, rrrcSheet, row.names = FALSE, colStyle = list("1" = center + numDf, "2" = center + numDf, "3" = center + numDf), colnamesStyle = centerbold)
    
    diffTRName <- NULL
    for (i in 1:I) {
      if (i == I) 
        break
      for (ip in (i + 1):I) {
        diffTRName <- c(diffTRName, paste(modalityID[i], modalityID[ip], sep = " - "))
      }
    }
    
    diffTable <- result$ciDiffTrtRRRC
    diffTable[ , 1] <- diffTRName
    diffTable[ , 2] <- as.numeric(diffTable[ , 2])
    names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr > t",	"Lower",	"Upper")
    writeData(wb, sheet = "RRRC", startRow = 5, x = diffTable, rowNames = FALSE, colNames = TRUE)

    # rows <- createRow(rrrcSheet, rowIndex = 4)
    # cells <- createCell(rows, colIndex = 1) 
    # setCellValue(cells[[1, 1]], "95% CI's FOMs, treatment difference")
    # setCellStyle(cells[[1, 1]], centerbold)
    # addMergedRegion(rrrcSheet, 4, 4, 1, 8)
    mergeCells(wb, "RRRC", rows = 4, cols = 1:8)
    writeData(wb, sheet = "RRRC", startRow = 4, x = "95% CI's FOMs, treatment difference", rowNames = FALSE, colNames = FALSE)
    
    ciTable <- result$ciAvgRdrEachTrtRRRC
    ciTable$StdErr <- as.numeric(ciTable$StdErr)
    ciTable$DF <- as.numeric(ciTable$DF)
    ciTable[ , 1] <- modalityID
    names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
    writeData(wb, sheet = "RRRC", startRow = 8 + nrow(diffTable), x = ciTable, rowNames = FALSE, colNames = TRUE)
    # addDataFrame(ciTable, rrrcSheet, row.names = FALSE, startRow = 8 + nrow(diffTable), colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf, "4" = center + numDf, "5" = center + numDf, 
    #                                                                                                     "6" = center + numDf), colnamesStyle = centerbold)
    # rows <- createRow(rrrcSheet, rowIndex = 7 + nrow(diffTable))
    # cells <- createCell(rows, colIndex = 1) 
    # setCellValue(cells[[1, 1]], "95% CI's FOMs, each treatment")
    writeData(wb, sheet = "RRRC", startRow = 7 + nrow(diffTable), x = "95% CI's FOMs, each treatment", rowNames = FALSE, colNames = FALSE)
    # setCellStyle(cells[[1, 1]], centerbold)
    mergeCells(wb, "RRRC", rows = 7 + nrow(diffTable), cols = 1:6)
    # addMergedRegion(rrrcSheet, 7 + nrow(diffTable), 7 + nrow(diffTable), 1, 6)
    # autoSizeColumn(rrrcSheet, colIndex = 1:8)
    
    # done with RRRC, now create contents of FRRC worksheet    
    addWorksheet(wb, "FRRC")
    testTable <- data.frame(f = result$fFRRC, ddf = result$ddfFRRC, p = result$pFRRC)
    if (method == "ORH"){
      testTable$ddf <- "Inf"
    }
    names(testTable) <- c("F statistic", "ddf", "P-value")
    # addDataFrame(testTable, frrcSheet, row.names = FALSE, colStyle = list("1" = center + numDf, "2" = center + numDf, "3" = center + numDf), colnamesStyle = centerbold)
    writeData(wb, sheet = "FRRC", x = testTable, rowNames = FALSE, colNames = TRUE)

    diffTable <- result$ciDiffTrtFRRC
    diffTable[ , 1] <- diffTRName
    diffTable[ , 2] <- as.numeric(diffTable[ , 2])
    if (method == "ORH"){
      diffTable[ , 4] <- "Inf"
    }
    
    names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr > t",	"Lower",	"Upper")
    # addDataFrame(diffTable, frrcSheet, row.names = FALSE, startRow = 5, colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf, "4" = center + numDf, "5" = center + numDf, 
    #                                                                                     "6" = center + numDf, "7" = center + numDf, "8" = center + numDf), colnamesStyle = centerbold)
    writeData(wb, sheet = "FRRC", x = diffTable, startRow = 5, rowNames = FALSE, colNames = TRUE)
    
    # rows <- createRow(frrcSheet, rowIndex = 4)
    # cells <- createCell(rows, colIndex = 1) 
    # setCellValue(cells[[1, 1]], "95% CI's FOMs, treatment difference")
    writeData(wb, sheet = "FRRC", startRow = 4, x = "95% CI's FOMs, treatment difference", rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "FRRC", rows = 4, cols = 1:8)
    # setCellStyle(cells[[1, 1]], centerbold)
    # addMergedRegion(frrcSheet, 4, 4, 1, 8)
    
    ciTable <- result$ciAvgRdrEachTrtFRRC
    ciTable$StdErr <- as.numeric(ciTable$StdErr)
    ciTable$DF <- as.numeric(ciTable$DF)
    ciTable[ , 1] <- modalityID
    if (method == "ORH"){
      ciTable[ , 4] <- "Inf"
    }
    
    names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
    # addDataFrame(ciTable, frrcSheet, row.names = FALSE, startRow = 8 + nrow(diffTable), 
    # colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf, "4" = center + numDf, "5" = center + numDf, 
    # "6" = center + numDf), colnamesStyle = centerbold)
    writeData(wb, sheet = "FRRC", startRow = 8 + nrow(diffTable), x = ciTable, rowNames = FALSE, colNames = TRUE)
    
    # rows <- createRow(frrcSheet, rowIndex = 7 + nrow(diffTable))
    # cells <- createCell(rows, colIndex = 1) 
    # setCellValue(cells[[1, 1]], "95% CI's FOMs, each treatment")
    # setCellStyle(cells[[1, 1]], centerbold)
    # addMergedRegion(frrcSheet, 7 + nrow(diffTable), 7 + nrow(diffTable), 1, 6)
    writeData(wb, sheet = "FRRC", startRow = 7 + nrow(diffTable), x = "95% CI's FOMs, each treatment", rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "FRRC", rows = 7 + nrow(diffTable), cols = 1:6)
    
    readerNames <- rep(readerID, choose(I, 2))
    trNames <- rep(diffTRName, J)
    diffTableEchR <- result$ciDiffTrtEachRdr
    diffTableEchR$Reader <- readerNames
    diffTableEchR$Treatment <- trNames
    if (method == "ORH"){
      diffTableEchR$DF <- "Inf"
    }
    
    names(diffTableEchR)[8:9] <- c("Lower",	"Upper")
    # addDataFrame(diffTableEchR, frrcSheet, row.names = FALSE, startRow = 11 + nrow(diffTable) + nrow(ciTable), 
    #              colStyle = list("1" = centerbold, "2" = centerbold, "3" = center + numDf, "4" = center + numDf, "5" = center + numDf, 
    #                              "6" = center + numDf, "7" = center + numDf, "8" = center + numDf, "9" = center + numDf), colnamesStyle = centerbold)
    
    writeData(wb, sheet = "FRRC", startRow = 11 + nrow(diffTable) + nrow(ciTable), 
              x = diffTableEchR, rowNames = FALSE, colNames = TRUE)
    # rows <- createRow(frrcSheet, rowIndex = 10 + nrow(diffTable) + nrow(ciTable))
    # cells <- createCell(rows, colIndex = 1) 
    # setCellValue(cells[[1, 1]], "95% CI's FOMs, treatment difference, each reader")
    # setCellStyle(cells[[1, 1]], centerbold)
    # addMergedRegion(frrcSheet, 10 + nrow(diffTable) + nrow(ciTable), 10 + nrow(diffTable) + nrow(ciTable), 1, 9)
    # autoSizeColumn(frrcSheet, colIndex = 1:9)
    writeData(wb, sheet = "FRRC", startRow = 10 + nrow(diffTable) + nrow(ciTable), 
              x = "95% CI's FOMs, treatment difference, each reader", rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "FRRC", rows = 10 + nrow(diffTable) + nrow(ciTable), cols = 1:9)
    #############################################################    
    # done with FRRC, now create contents of RRFC worksheet    
    addWorksheet(wb, "RRFC")
    testTable <- data.frame(f = result$fRRFC, ddf = result$ddfRRFC, p = result$pRRFC)
    names(testTable) <- c("F statistic", "ddf", "P-value")
    # addDataFrame(testTable, rrfcSheet, row.names = FALSE, colStyle = list("1" = center + numDf, "2" = center + numDf, "3" = center + numDf), colnamesStyle = centerbold)
    writeData(wb, sheet = "RRFC", x = testTable, rowNames = FALSE, colNames = TRUE)
    
    diffTable <- result$ciDiffTrtRRFC
    diffTable[ , 1] <- diffTRName
    diffTable[ , 2] <- as.numeric(diffTable[ , 2])
    names(diffTable) <- c("Difference",	"Estimate",	"StdErr",	"DF",	"t",	"Pr > t",	"Lower",	"Upper")
    # addDataFrame(diffTable, rrfcSheet, row.names = FALSE, startRow = 5, colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf, "4" = center + numDf, "5" = center + numDf, 
    #                                                                                     "6" = center + numDf, "7" = center + numDf, "8" = center + numDf), colnamesStyle = centerbold)
    writeData(wb, sheet = "RRFC", startRow = 5, x = diffTable, rowNames = FALSE, colNames = TRUE)
    
    # rows <- createRow(rrfcSheet, rowIndex = 4)
    # cells <- createCell(rows, colIndex = 1) 
    # setCellValue(cells[[1, 1]], "95% CI's FOMs, treatment difference")
    # setCellStyle(cells[[1, 1]], centerbold)
    # addMergedRegion(rrfcSheet, 4, 4, 1, 8)
    writeData(wb, sheet = "RRFC", startRow = 4, x = "95% CI's FOMs, treatment difference", rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "RRFC", rows = 4, cols = 1:8)
    
    ciTable <- result$ciAvgRdrEachTrtRRFC
    ciTable$StdErr <- as.numeric(ciTable$StdErr)
    ciTable$DF <- as.numeric(ciTable$DF)
    ciTable[ , 1] <- modalityID
    names(ciTable) <- c("Treatment",	"Estimate",	"StdErr",	"DF",	"Lower",	"Upper")
    # addDataFrame(ciTable, rrfcSheet, row.names = FALSE, startRow = 8 + nrow(diffTable), colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf, "4" = center + numDf, "5" = center + numDf, 
    #                                                                                                     "6" = center + numDf), colnamesStyle = centerbold)
    writeData(wb, sheet = "RRFC", startRow = 8 + nrow(diffTable), x = ciTable, rowNames = FALSE, colNames = TRUE)
    # rows <- createRow(rrfcSheet, rowIndex = 7 + nrow(diffTable))
    # cells <- createCell(rows, colIndex = 1) 
    # setCellValue(cells[[1, 1]], "95% CI's FOMs, each treatment")
    # setCellStyle(cells[[1, 1]], centerbold)
    # addMergedRegion(rrfcSheet, 7 + nrow(diffTable), 7 + nrow(diffTable), 1, 6)
    # autoSizeColumn(rrfcSheet, colIndex = 1:8)
    writeData(wb, sheet = "RRFC", startRow = 7 + nrow(diffTable), x = "95% CI's FOMs, each treatment", rowNames = FALSE, colNames = FALSE)
    mergeCells(wb, "RRFC", rows = 7 + nrow(diffTable), cols = 1:6)
    
    if (method == "DBMH"){
      #############################################################    
      # done with RRFC, now create contents of ANOVA worksheet    
      addWorksheet(wb, "ANOVA")

      # addDataFrame(result$varComp, anovaSheet, col.names = FALSE, startRow = 2, colStyle = list("1" = center + numDf), rownamesStyle = centerbold)
      writeData(wb, sheet = "ANOVA", x = result$varComp, startRow = 2, rowNames = TRUE, colNames = FALSE)
      
      # rows <- createRow(anovaSheet, rowIndex = 1)
      # cells <- createCell(rows, colIndex = 1) 
      # setCellValue(cells[[1, 1]], "FOM variance components")
      # setCellStyle(cells[[1, 1]], centerbold)
      # addMergedRegion(anovaSheet, 1, 1, 1, 2)
      writeData(wb, sheet = "ANOVA", x = "FOM variance components", rowNames = FALSE, colNames = FALSE)
      mergeCells(wb, "ANOVA", rows = 1, cols = 1:2)
      
      # addDataFrame(result$anovaY, anovaSheet, startRow = 10, row.names = FALSE, colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf, "4" = center + numDf), colnamesStyle = centerbold)
      writeData(wb, sheet = "ANOVA", x = result$anovaY, startRow = 10, rowNames = FALSE, colNames = TRUE)
      
      # rows <- createRow(anovaSheet, rowIndex = 9)
      # cells <- createCell(rows, colIndex = 1) 
      # setCellValue(cells[[1, 1]], "TREATMENT X READER X CASE ANOVA")
      # setCellStyle(cells[[1, 1]], centerbold)
      # addMergedRegion(anovaSheet, 9, 9, 1, 4)
      writeData(wb, sheet = "ANOVA", startRow = 9,  x = "TREATMENT X READER X CASE ANOVA", rowNames = FALSE, colNames = FALSE)
      mergeCells(wb, "ANOVA", rows = 9, cols = 1:4)

      colnames(result$anovaYi) <- c("Source", "DF", rownames(fomArray))
      # addDataFrame(result$anovaYi, anovaSheet, startRow = 21, row.names = FALSE, colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf, "4" = center + numDf), colnamesStyle = centerbold)
      writeData(wb, sheet = "ANOVA", x = result$anovaYi, startRow = 21, rowNames = FALSE, colNames = TRUE)
      
      # rows <- createRow(anovaSheet, rowIndex = 20)
      # cells <- createCell(rows, colIndex = 1) 
      # setCellValue(cells[[1, 1]], "READER X CASE ANOVA for each Trt")
      # setCellStyle(cells[[1, 1]], centerbold)
      # addMergedRegion(anovaSheet, 20, 20, 1, 4)
      writeData(wb, sheet = "ANOVA", startRow = 20,  x = "READER X CASE ANOVA for each Trt", rowNames = FALSE, colNames = FALSE)
      mergeCells(wb, "ANOVA", rows = 20, cols = 1:4)

      colnames(result$msAnovaEachRdr) <- c("Source", "DF", colnames(fomArray)[1:J])
      # colStyle <- c(list(centerbold), list(center + numDf))
      # for (j in 1:J){
      #   colStyle <- c(colStyle, list(center + numDf))
      # }
      # names(colStyle) <- c(1:(J + 2))
      
      # addDataFrame(result$msAnovaEachRdr, anovaSheet, startRow = 27, row.names = FALSE, colStyle = colStyle, colnamesStyle = centerbold)
      writeData(wb, sheet = "ANOVA", x = result$msAnovaEachRdr, startRow = 27, rowNames = FALSE, colNames = TRUE)
      
      # rows <- createRow(anovaSheet, rowIndex = 26)
      # cells <- createCell(rows, colIndex = 1) 
      # setCellValue(cells[[1, 1]], "TREATMENT X CASE ANOVAs (MS) for each reader, assuming fixed reader analysis")
      # setCellStyle(cells[[1, 1]], centerbold)
      # addMergedRegion(anovaSheet, 26, 26, 1, J + 2)
      # autoSizeColumn(anovaSheet, colIndex = 1:(J + 2))
      writeData(wb, sheet = "ANOVA", startRow = 26,  x = "TREATMENT X CASE ANOVAs (MS) for each reader, assuming fixed reader analysis", 
                rowNames = FALSE, colNames = FALSE)
      mergeCells(wb, "ANOVA", rows = 26, cols = 1:(J + 2))
      saveWorkbook(wb, "writeDataExample.xlsx", overwrite = TRUE)
    }else{
      addWorksheet(wb, "VarComp")
      #varSheet <- createSheet(wb, sheetName = "VarComp")
      addDataFrame(result$varComp, varSheet, col.names = FALSE, startRow = 2, colStyle = list("1" = center + numDf), rownamesStyle = centerbold)
      rows <- createRow(varSheet, rowIndex = 1)
      cells <- createCell(rows, colIndex = 1) 
      setCellValue(cells[[1, 1]], "OR Variance Covariance Components")
      setCellStyle(cells[[1, 1]], centerbold)
      addMergedRegion(varSheet, 1, 1, 1, 2)
      
      result$varCovEachRdr[ , 1] <- readerID
      addDataFrame(result$varCovEachRdr, varSheet, startRow = 10, row.names = FALSE, colStyle = list("1" = centerbold, "2" = center + numDf, "3" = center + numDf), colnamesStyle = centerbold)
      rows <- createRow(varSheet, rowIndex = 9)
      cells <- createCell(rows, colIndex = 1) 
      setCellValue(cells[[1, 1]], "OR Variance Covariance Components for each reader, assuming fixed reader analysis")
      setCellStyle(cells[[1, 1]], centerbold)
      addMergedRegion(varSheet, 9, 9, 1, 3)
      
      autoSizeColumn(varSheet, colIndex = 1:3)
    }
    saveWorkbook(wb, ReportFileName)
    sucessfulOutput <- sprintf("The report has been saved to %s.", ReportFileName)
  }
  
  message(sucessfulOutput)
} 
