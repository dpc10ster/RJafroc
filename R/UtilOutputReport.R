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
#' @param method The significance testing method, \code{"ORH"} or 
#'    (the default) \code{"DBMH"}.
#' 
#' @param FOM The figure of merit; see \code{\link{StSignificanceTesting}}.
#' 
#' @param analysisOption "RRRC", "FRRC", "RRFC or "ALL"; see \code{\link{StSignificanceTesting}}.
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
                             method = "DBMH", FOM, alpha = 0.05, 
                             covEstMethod = "jackknife", nBoots = 200, 
                             sequentialNames = FALSE, overWrite = FALSE, analysisOption = "ALL") {
  
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
    methodTxt <- "DBM-MRMC-HILLIS SIGNIFICANCE TESTING"
    StResult <- StSignificanceTesting(dataset, FOM, FPFValue = 0.2, alpha, method, analysisOption = analysisOption)
  } else if (method == "ORH") {
    methodTxt <- "OBUCHOWSKI-ROCKETTE-HILLIS SIGNIFICANCE TESTING"
    StResult <- StSignificanceTesting(dataset, FOM, FPFValue = 0.2, alpha, method, covEstMethod, nBoots, analysisOption = analysisOption)
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
                                      analysisOption,
                                      StResult)
  } else if (ReportFileExt == "xlsx") {
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
                                       analysisOption,
                                       StResult)
  } else stop("Incorrect ReportFileExt value")
  
  return(StResult)
  
} 

