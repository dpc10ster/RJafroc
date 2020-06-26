#' Generate a text formatted report file or an Excel file
#' 
#' @description  Generates a formatted report of the analysis 
#'    and saves it to a text or an Excel file
#' 
#' @param dataset The dataset object to be analyzed (\emph{not the file name}), 
#' see \code{Dataset} in \code{\link{RJafroc-package}}.
#'    
#' @param ReportFileBaseName \bold{This must be specified by the user.} 
#'    The report file (text or Excel, as specified by option \code{ReportFileExt}) 
#'    is then created \bold{in the user's directory, not the RJafroc directory}. 
#'    See README.md in the 
#'    GitHub directory of this repository, the section on how to install 
#'    the software, on how to create a user directory. This argument specifies the 
#'    report file base name (i.e., without the extension) for the 
#'    desired report; the default is NULL, in which case the system generates 
#'    a temporary text file, whose very long name is displayed. However, the 
#'    temp file
#'    is very hard to locate. This is so that the sofware passes CRAN checks, 
#'    as writing to the project directory, or any of its subdirectories, is 
#'    frowned upon.   
#'    
#' @param ReportFileExt The report file extension determines the type of output. 
#'    \code{txt}, the default, for 
#'    a text file, \code{xlsx} for an Excel file.
#'    
#' @param method The significance testing method, \code{"OR"} or 
#'    (the default) \code{"DBM"}.
#' 
#' @param FOM The figure of merit; see \code{\link{StSignificanceTesting}}.
#' 
#' @param analysisOption "RRRC", "FRRC", "RRFC or "ALL"; see \code{\link{StSignificanceTesting}}.
#' 
#' @param alpha See \code{\link{StSignificanceTesting}}; the default is 0.05.
#' 
#' @param covEstMethod See \code{\link{StSignificanceTesting}}; only needed 
#'     for method = \code{"OR"}; the default is "Jackknife".
#' 
#' @param nBoots See \code{\link{StSignificanceTesting}}; only needed for 
#'    \code{"OR"} analysis; the default is 200.
#' 
#' @param sequentialNames A logical variable: if \code{TRUE}, consecutive integers 
#'    (starting from 1) will be used as the treatment and reader IDs in the 
#'    output report. Otherwise, treatment and reader IDs in the original dataset 
#'    are used. This option is needed for aesthetics, as long names can mess
#'    up the output. The default is \code{FALSE.}
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
#' @return StResult The object returned by \code{\link{StSignificanceTesting}}.
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

UtilOutputReport <- function(dataset, ReportFileBaseName = NULL, ReportFileExt = "txt", 
                             method = "DBM", FOM, alpha = 0.05, 
                             covEstMethod = "jackknife", nBoots = 200, 
                             sequentialNames = FALSE, overWrite = FALSE, analysisOption = "ALL") {
  
  # why is this generating an error in Ch10Vig2? TBA
  if (!isValidDataset(dataset)) {
    stop("Must specify a valid dataset object.")
  }
  
  if (!isValidFom(dataset, FOM)) {
    stop("Inconsistent dataset - FOM combination")
  }
  
  if (sequentialNames){
    dataset$descriptions$modalityID <- 1:length(dataset$descriptions$modalityID)
    dataset$descriptions$readerID <- 1:length(dataset$descriptions$readerID)
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
  
  if (method == "DBM") {
    methodTxt <- "DBM-MRMC-HILLIS SIGNIFICANCE TESTING"
    StResult <- StSignificanceTesting(dataset, FOM, FPFValue = 0.2, alpha, method, analysisOption = analysisOption)
  } else if (method == "OR") {
    methodTxt <- "OBUCHOWSKI-ROCKETTE-HILLIS SIGNIFICANCE TESTING"
    StResult <- StSignificanceTesting(dataset, FOM, FPFValue = 0.2, alpha, method, covEstMethod, nBoots, analysisOption = analysisOption)
  } else {
    errMsg <- paste0(method, " is not a valid analysis method.")
    stop(errMsg)
  }
  
  if (ReportFileExt == "txt"){
    if (method == "DBM") {
      sucessfulOutput <- OutputTextFileDBM(dataset,
                                            method,
                                            methodTxt,
                                            ReportFileName,
                                            alpha,
                                            FOM,
                                            analysisOption,
                                            StResult)
    } else {
      sucessfulOutput <- OutputTextFileORH(dataset,
                                           method,
                                           methodTxt,
                                           ReportFileName,
                                           alpha,
                                           FOM,
                                           analysisOption,
                                           StResult)
    }
  } else if (ReportFileExt == "xlsx") {
    summaryInfo <- data.frame(summaryInfo = 
                                c(base::format(Sys.time(), "%b/%d/%Y"), 
                                  basename(ReportFileName),
                                  dataset$descriptions$name))
    rownames(summaryInfo) <- c("Date", "Output file", "Input Dataset")
    if (method == "DBM") {
      sucessfulOutput <- OutputExcelFileDBMH(dataset,
                                             method,
                                             methodTxt,
                                             ReportFileName,
                                             covEstMethod,
                                             summaryInfo,
                                             alpha,
                                             FOM,
                                             analysisOption,
                                             StResult)
    } else {
      sucessfulOutput <- OutputExcelFileORH(dataset,
                                            method,
                                            methodTxt,
                                            ReportFileName,
                                            covEstMethod,
                                            summaryInfo,
                                            alpha,
                                            FOM,
                                            analysisOption,
                                            StResult)
    }
  } else stop("Incorrect ReportFileExt value")
  
  return(StResult)
  
} 

Preamble <- function(dataset, FOM, ReportFileName, method, methodTxt) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  x <- c("RJafroc IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR ", 
         "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, ", 
         "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE ", 
         "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER ", 
         "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ", 
         "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS ", 
         "IN THE SOFTWARE.", 
         "===============================================================================")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  cat(paste("R version:", R.version$version.string,"\n"))
  cat(paste("RJafroc version:", packageVersion("RJafroc"),"\n"))
  dateTime <- paste0("Run date: ", base::format(Sys.time(), "%b %d %Y %a %X %Z"))
  cat(paste(dateTime, "\n"))
  
  cat(sprintf("FOM selected         :     %s\n", FOM))
  cat(sprintf("Input Data Set       :     %s\n", dataset$descriptions$name))
  cat(sprintf("Output Data Filename :     %s\n", ReportFileName))
  cat(sprintf("===============================================================================\n"))
  
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionID <- dataset$lesions$IDs
  maxNL <- dim(NL)[4]
  dataType <- dataset$descriptions$type
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  nLesionPerCase <- rowSums(lesionID != UNINITIALIZED)
  
  cat(sprintf("Significance testing method:  %s\n", methodTxt))
  cat(sprintf("Number of Readers          :  %d\n", J))
  cat(sprintf("Number of Treatments       :  %d\n", I))
  cat(sprintf("Number of Normal Cases     :  %d\n", K1))
  cat(sprintf("Number of Abnormal Cases   :  %d\n", K2))
  cat(sprintf("Fraction of Normal Cases   :  %f\n", K1/K))
  
  if (dataType == "FROC") {
    cat(sprintf("Min number of lesions per diseased case   :  %d\n", 
                min(nLesionPerCase)))
    cat(sprintf("Max number of lesions per diseased case   :  %d\n", 
                max(nLesionPerCase)))
    cat(sprintf("Mean number of lesions per diseased case  :  %f\n", 
                mean(nLesionPerCase)))
    cat(sprintf("Total number of lesions                   :  %d\n", 
                sum(nLesionPerCase)))
    
    nl <- NL[, , (K1 + 1):K, ]
    dim(nl) <- c(I, J, K2, maxNL)
    maxNLRating <- apply(nl, c(1, 2, 3), max)
    maxLLRating <- apply(LL, c(1, 2, 3), max)
    ILF <- sum(maxNLRating > maxLLRating) + 0.5 * sum(maxNLRating == maxLLRating)
    ILF <- ILF/I/J/K2
    cat(sprintf("Inc. Loc. Frac.          :  %f\n", ILF))
    cat(sprintf("===============================================================================\n"))
    cat(sprintf("Avg. number of non-lesion localization marks per reader on non-diseased cases: %f\n", 
                sum(NL[, , 1:K1, ] != UNINITIALIZED)/(I * J * K1)))
    cat(sprintf("Avg. number of non-lesion localization marks per reader on diseased cases:  %f\n", 
                sum(NL[, , (K1 + 1):K, ] != UNINITIALIZED)/(I * J * K2)))
    cat(sprintf("Avg. number of lesion localization marks per reader :  %f\n", 
                sum(LL != UNINITIALIZED)/(I * J * K2)))
  }
  
  cat(sprintf("\n===============================================================================\n"))
  cat(sprintf("Modality IDs in the input file are:  %s\n", paste(names(modalityID), collapse = ", ")))
  cat(sprintf("Modality IDs in the output file are: %s\n", paste(modalityID, collapse = ", ")))
  cat(sprintf("Reader IDs in the input file are:    %s\n", paste(names(readerID), collapse = ", ")))
  cat(sprintf("Reader IDs in the output file are:   %s\n", paste(readerID, collapse = ", ")))
  
  x <- c("===============================================================================\n\n", 
         "===========================================================================", 
         "*****                           OVERVIEW                              *****", 
         "===========================================================================\n",
         "Three analyses are presented:", 
         "(1) Analysis RRRC treats both readers and cases as random samples", 
         "--results apply to the reader and case populations;", 
         "(2) Analysis FRRC treats only cases as a random sample", 
         "--results apply to the population of cases but only for the", 
         "readers used in the study; and", 
         "(3) Analysis RRFC treats only readers as a random sample", 
         "--results apply to the population of readers but only for the", 
         "cases used in the study.", 
         "\nFor each analysis the null hypothesis of equal treatments is", 
         "tested in part (a), reader-averaged treatment ", 
         "FOM difference(s) 95% confidence intervals", 
         "in part (b), and reader-averaged treatment",
         "FOMs 95% confidence intervals in part (c).  Parts (a) and (b) are",
         "based on the treatment x reader x case ANOVA while part (c)",
         "is based on the reader x case ANOVA for the specified treatment.",
         "Different error terms are used as indicated for parts (a), (b),",
         "and (c) according to whether readers and cases are treated as",
         "fixed or random factors. Note that the treatment confidence",
         "intervals in part (c) are based only on the data for the specified",
         "treatment, rather than the pooled data. Treatment difference 95%",
         "confidence intervals for each reader are in part (d) of",
         "Analysis FRRC; each interval is based on the treatment", 
         "x case ANOVA table for the specified reader.")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
  x <- c("\n",
         "===========================================================================",
         "*****                        FOM Estimates                            *****",
         "===========================================================================\n")
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  cat(c("Individual reader FOMs and the means, and differences of reader-averaged FOMs\n\n"))
  
  df <- method$FOMs$foms
  print(format(df, digits = 5, justify = "left"))
  
  cat(c("\nTREATMENT MEANS (averaged over readers):\n\n"))
  df <- method$FOMs$trtMeans
  print(format(df, digits = 5, justify = "left"))
  
  cat("\n")
  cat(c("TREATMENT MEAN DIFFERENCES (averaged over readers):\n\n"))
  df <- method$FOMs$trtMeanDiffs
  print(format(df, digits = 5, justify = "left"))
}

