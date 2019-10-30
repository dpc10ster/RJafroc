#' Read a LROC data file
#'
#' @description Read the Hupse-Karssemeijer LROC data file, a study comparing 
#'    standalone performance of breast CAD vs. radiologists; the study 
#'    actually included radiologists and residents; the following usage 
#'    includes only the radiologists
#'
#' @param RADIOLOGISTS Logical; if TRUE, the default, only radiologists are 
#'    analyzed otherwise all readers are analyzed
#'
#' @details The data format is similar to the JAFROC format (see \code{\link{RJafroc-package}}) 
#'    with the \strong{crucial difference} that there are two types of LL (TP) events: 
#'    those representing correct localizations and those representing incorrect 
#'    localizations. Also, every diseased case has one lesion and NLs are not possible 
#'    on diseased cases. \code{J} is one plus the number of readers. The first treatment 
#'    is CAD, followed by the readers. 
#'
#' @return The \strong{LROC} dataset. 
#' 
#' 
#' @details The return value is a list with the following elements: 
#' \itemize{
#' \item{\code{NL}}{ [1, 1:J, 1:K1, 1] array containing the FP ratings}
#' \item{\code{LLCl}}{ [1, 1:J, 1:K2, 1] array containing the TP correct localization ratings}
#' \item{\code{LLIl}}{ [1, 1:J, 1:K2, 1] array containing the TP incorrect localization ratings}
#' \item{\code{lesionVector}}{ array [1:K2], as in standard JAFROC/ROC format dataset, ones}
#' \item{\code{lesionID}}{ array [1:K2], as in standard JAFROC/ROC format dataset, ones}
#' \item{\code{lesionWeight}}{ array [1:K2], weights (or clinical importances) of lesions}
#' \item{\code{dataType}}{ "LROC", the data type}
#' \item{\code{modalityID}}{ [1:I], treatment labels}
#' \item{\code{readerID}}{ [1:J], reader labels}
#' }
#'
#' @examples
#' radData <- DfReadLrocDataFile()
#' str(radData)
#' allData <- DfReadLrocDataFile(FALSE)
#' str(allData)
#'
#'
#'
#' @references
#' Hupse R, Samulski M, Lobbes M, et al. Standalone computer-aided detection compared 
#' to radiologists' performance
#' for the detection of mammographic masses. Eur Radiol 2013.
#'
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#'
#'
#' @import utils
#' @export
#'
#'
DfReadLrocDataFile <- function (RADIOLOGISTS = TRUE)
{
  residents <- c(6,7,10)
  truthFileName <- system.file("extdata", "jaf_truth.txt", 
                               package = "RJafroc", mustWork = TRUE)
  truth <- read.table(truthFileName, sep = ",")
  CaseID <- truth$V1
  LesionID <- truth$V2
  K1 <- length(LesionID[LesionID == 0])
  K2 <- length(LesionID[LesionID == 1])
  K <- c(K1, K2)
  CaseIDNor <- CaseID[LesionID == 0]
  CaseIDAbn <- CaseID[LesionID == 1]

  findingsFileName <- system.file("extdata", "findings.txt", 
                                  package = "RJafroc", mustWork = TRUE)
  data <- read.table(findingsFileName, sep = "")
  CaseID <- data$V1
  ReaderID <- data$V2
  # increment each rating by one, so it extends from 1 to 101; dpc 12/14/16
  Rating <- data$V13 + 1 #  sum(is.na(Rating)) should give 0
  CL <- as.logical(data$V14)

  ReaderIDArray <- unique(ReaderID)
  temp <- match(residents, ReaderIDArray)
  Radiologists <- ReaderIDArray[-temp]
  if (RADIOLOGISTS) ReaderIDArray <- Radiologists
  J <- length(ReaderIDArray)

  zjkt <- array(dim = c(J, max(c(K1,K2)), 2))
  CLArray <- array(FALSE, dim = c(J, K2))
  for (i in 1:length(CaseID)){
    if ( CaseID[i] %in% CaseIDNor ) t <- 1 else t <- 2
    j <- match(ReaderID[i], ReaderIDArray)
    if (is.na(j)) next
    if (t == 1) k <- match(CaseID[i], CaseIDNor) else k <- match(CaseID[i], CaseIDAbn)
    #if (t == 2) cat("case, j,k,t, =  ", CaseID[i], j, k, t, "\n")
    if (t == 1) {
      if (is.na(zjkt[j,k,t])) zjkt[j,k,t] <- Rating[i] else zjkt[j,k,t] <- max(c(zjkt[j,k,t],Rating[i]))
    } else {
      if (is.na(zjkt[j,k,t])) {
        zjkt[j,k,t] <- Rating[i]
        CLArray[j,k] <- CL[i]
      } else {
        if (Rating[i] > zjkt[j,k,t]) {
          zjkt[j,k,t] <- Rating[i]
          CLArray[j,k] <- CL[i]
        }
      }
    }
  }
  UnMarked <- 0
  zjkt[is.na(zjkt)] <- UnMarked  # sum(is.na(zjkt)) should give zero
  zjk1 <- zjkt[,1:K1,1]
  zjk2Cl <- array(dim = c(J, K2))
  temp1 <- (CLArray == TRUE)
  zjk2 <- zjkt[,1:K2,2]
  zjk2Cl[temp1] <- zjk2[temp1]

  zjk2Il <- array(dim = c(J, K2))
  temp2 <- (CLArray == FALSE)
  zjk2Il[temp2] <- zjk2[temp2]

  # sum(is.na(zjk1)) should give 0
  zjk2Cl[is.na(zjk2Cl)] <- UnMarked
  zjk2Il[is.na(zjk2Il)] <- UnMarked

  # make CAD the first reader
  zjk1 <- zjk1[c(J,1:(J-1)),]
  zjk2Cl <- zjk2Cl[c(J,1:(J-1)),]
  zjk2Il <- zjk2Il[c(J,1:(J-1)),]

  dim(zjk1) <- c(1,J,K1,1)
  dim(zjk2Cl) <- c(1,J,K2,1)
  dim(zjk2Il) <- c(1,J,K2,1)
  if (RADIOLOGISTS) readerID <- as.character(seq(1:10)) else readerID <- as.character(seq(1:13))
  datasetCad <- list (
    NL = zjk1,
    LLCl = zjk2Cl,
    LLIl = zjk2Il,
    lesionVector = rep(1,length(zjk2Cl[1,1,,1])),
    lesionID = rep(1,length(zjk2Cl[1,1,,1])),
    lesionWeight = rep(0,length(zjk2Cl[1,1,,1])),
    dataType = "LROC",
    modalityID = "1",
    readerID = readerID
    )

  return (datasetCad)
}
