#' Save ROC data file in a different format
#' 
#' @description Save ROC data file in a different format so it can be analyzed 
#'    with alternate software
#' 
#' @param dataset {The dataset to be saved in the specified format, 
#'    see \code{\link{RJafroc-package}}}.
#' @param fileName {The file name of the output data file. The extension 
#'    of the data file must match the corresponding format, see \code{\link{RJafroc-package}}}
#' @param format {The format of the data file, which can be \code{"JAFROC"}, 
#'    \code{"MRMC"} or \code{"iMRMC"}, see \code{\link{RJafroc-package}}}.
#' @param dataDescription {An optional string variable describing the data file, the 
#'    default value is the variable name of \code{dataset} The description appears on 
#'    the first line of *.lrc or *imrmc data file. This parameter is not used 
#'    when saving dataset in other formats}.
#' 
#' @examples
#' \donttest{
#' DfSaveDataFile(dataset = dataset05, 
#'    fileName = "rocData2.xlsx", format = "JAFROC")
#' DfSaveDataFile(dataset = dataset02, 
#'    fileName = "rocData2.csv", format = "MRMC")
#' DfSaveDataFile(dataset = dataset02, 
#'    fileName = "rocData2.lrc", format = "MRMC", 
#'    dataDescription = "ExampleROCdata1")
#' DfSaveDataFile(dataset = dataset02, 
#'    fileName = "rocData2.txt", format = "MRMC", 
#'    dataDescription = "ExampleROCdata2")
#' DfSaveDataFile(dataset = dataset02, 
#'    fileName = "dataset05.imrmc", format = "iMRMC", 
#'    dataDescription = "ExampleROCdata3") 
#' }
#'  
#' @importFrom tools file_ext
#' 
#' @export
#' 
DfSaveDataFile <- function(dataset, fileName, format = "JAFROC", dataDescription = paste0(deparse(substitute(dataset)), " Data File")) {
  if (format == "JAFROC") {
    return(SaveJAFROC(dataset, fileName))
  } else if (format == "iMRMC") {
    return(SaveImrmc(dataset, fileName, dataDescription))
  } else if (format == "MRMC") {
    if (file_ext(fileName) == "lrc") {
      return(SaveLrc(dataset, fileName, dataDescription))
    } else {
      return(SaveOrDbmMrmc(dataset, fileName))
    }
  } else {
    errMsg <- sprintf("%s is not an available file format.", format)
    stop(errMsg)
  }
} 



SaveJAFROC <- function(dataset, fileName) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  fileExt <- file_ext(fileName)
  if (!fileExt %in% c("xls", "xlsx")) {
    stop("The extension of JAFROC file name must be *.xls or *.xlsx.")
  }
  NL <- dataset$NL
  LL <- dataset$LL
  lesionVector <- dataset$lesionVector
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  # following several lines assign -2000 as the lowest ROC rating before writing the data file.
  if (dataType == "ROC"){
    NL[ , , 1:K1, ][NL[ , , 1:K1, ] == -Inf] <- -2000
    LL[LL == -Inf][NL[ , , 1:K1, ] == -Inf] <- -2000
  }
  if (dataType == "ROI"){
    NL[ , , 1:K1, ][NL[ , , 1:K1, ] == -Inf] <- -2000
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K2) {
          for (l in 1:lesionVector[k]) {
            if (LL[i, j, k, l] == UNINITIALIZED) {
              LL[i, j, k, l] <- -2000
            }
          }
        }
      }
    }
  }
  
  # overall changes needed for openxlsx package
  wb <- createWorkbook()
  addWorksheet(wb, "TP")
  addWorksheet(wb, "FP")
  addWorksheet(wb, "TRUTH")
  # end overall changes needed for openxlsx package
  
  caseIDs <- c(1:K1, rep(K1 + 1:K2, lesionVector))
  lesionIDs <- as.vector(t(lesionID))
  lesionIDs <- lesionIDs[lesionIDs != UNINITIALIZED]
  lesionIDs <- c(rep(0, K1), lesionIDs)
  lesionWeights <- as.vector(t(lesionWeight))
  lesionWeights <- lesionWeights[lesionWeights != UNINITIALIZED]
  lesionWeights <- c(rep(0, K1), lesionWeights)
  dataSheet <- data.frame(CaseID = as.integer(caseIDs), LesionID = as.integer(lesionIDs), Weight = lesionWeights)
  
  writeData(wb, sheet = "TRUTH", x = dataSheet)
  
  dataSheet <- NULL
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K) {
        for (l in 1:maxNL) {
          if (NL[i, j, k, l] != UNINITIALIZED) {
            dataSheet <- rbind(dataSheet, c(j, i, k, NL[i, j, k, l]))
          }
        }
      }
    }
  }
  dataSheet <- data.frame(ReaderID = readerID[dataSheet[, 1]], ModalityID = modalityID[dataSheet[, 2]], CaseID = as.integer(dataSheet[, 3]), NL_Rating = signif(dataSheet[, 4], 6))
  writeData(wb, sheet = "FP", x = dataSheet)
  
  dataSheet <- NULL
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K2) {
        for (l in 1:lesionVector[k]) {
          if (LL[i, j, k, l] != UNINITIALIZED) {
            dataSheet <- rbind(dataSheet, c(j, i, k + K1, lesionID[k, l], LL[i, j, k, l]))
          }
        }
      }
    }
  }
  dataSheet <- data.frame(ReaderID = readerID[dataSheet[, 1]], ModalityID = modalityID[dataSheet[, 2]], CaseID = as.integer(dataSheet[, 3]), LesionID = as.integer(dataSheet[, 4]), LL_Rating = signif(dataSheet[, 5], 6))
  writeData(wb, sheet = "TP", x = dataSheet) # openxlsx
  saveWorkbook(wb, fileName, overwrite = TRUE)
} 


SaveLrc <- function(dataset, fileName, dataDescription) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  if (dataset$dataType != "ROC") {
    stop("Only ROC data file can be saved as *.lrc format.")
  }
  fileExt <- file_ext(fileName)
  if (fileExt != "lrc") {
    stop("The extension of LRC file name must be *.lrc")
  }
  write(dataDescription, fileName)
  NL <- dataset$NL
  LL <- dataset$LL
  # lesionVector <- dataset$lesionVector
  # lesionID <- dataset$lesionID
  # lesionWeight <- dataset$lesionWeight
  # maxNL <- dim(NL)[4]
  # dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  for (j in 1:J) {
    write(readerID[j], fileName, append = TRUE)
    if (j == 1) {
      string <- ""
      for (i in 1:I) {
        string <- paste0(string, sprintf("   \"%s\"", modalityID[i]))
      }
      write(string, fileName, append = TRUE)
      
      string <- ""
      for (i in 1:I) {
        string <- paste0(string, "       L")
      }
      write(string, fileName, append = TRUE)
    }
    
    for (k in 1:K1) {
      string <- ""
      for (i in 1:I) {
        if (NL[i, j, k, 1] != UNINITIALIZED) {
          string <- paste0(string, sprintf("   %f", NL[i, j, k, 1]))
        } else {
          string <- paste0(string, sprintf("   %f", 0))
        }
      }
      string <- paste0(string, sprintf("   Non-diseased case %d", k))
      write(string, fileName, append = TRUE)
    }
    write("*", fileName, append = TRUE)
    
    for (k in 1:K2) {
      string <- ""
      for (i in 1:I) {
        if (LL[i, j, k, 1] != UNINITIALIZED) {
          string <- paste0(string, sprintf("   %f", LL[i, j, k, 1]))
        } else {
          string <- paste0(string, sprintf("   %f", 0))
        }
      }
      string <- paste0(string, sprintf("   Diseased Case %d", k))
      write(string, fileName, append = TRUE)
    }
    write("*", fileName, append = TRUE)
  }
  write("#", fileName, append = TRUE)
} 



SaveImrmc <- function(dataset, fileName, dataDescription) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  if (dataset$dataType != "ROC") 
    stop("Only ROC data file can be saved as iMRMC format.")
  fileExt <- file_ext(fileName)
  if (fileExt != "imrmc") {
    stop("The extension of iMRMC file name must be *.imrmc.")
  }
  NL <- dataset$NL
  LL <- dataset$LL
  # lesionVector <- dataset$lesionVector
  # lesionID <- dataset$lesionID
  # lesionWeight <- dataset$lesionWeight
  # maxNL <- dim(NL)[4]
  # dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  write(dataDescription, fileName)
  write(sprintf("N0:%d", K1), fileName, append = TRUE)
  write(sprintf("N1:%d", K2), fileName, append = TRUE)
  write(sprintf("NR:%d", J), fileName, append = TRUE)
  write(sprintf("NM:%d", I), fileName, append = TRUE)
  write(sprintf("BEGIN DATA:"), fileName, append = TRUE)
  for (k in 1:K1) {
    write(sprintf("-1,%d,truth,0", k), fileName, append = TRUE)
  }
  for (k in 1:K2) {
    write(sprintf("-1,%d,truth,1", k + K1), fileName, append = TRUE)
  }
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K1) {
        if (NL[i, j, k, 1] != UNINITIALIZED) {
          write(sprintf("%s,%d,%s,%f", dataset$readerID[j], k, dataset$modalityID[i], NL[i, j, k, 1]), fileName, append = TRUE)
        } else {
          write(sprintf("%s,%d,%s,%f", dataset$readerID[j], k, dataset$modalityID[i], -2000), fileName, append = TRUE)
        }
      }
      
      for (k in 1:K2) {
        if (LL[i, j, k, 1] != UNINITIALIZED) {
          write(sprintf("%s,%d,%s,%f", dataset$readerID[j], k + K1, dataset$modalityID[i], LL[i, j, k, 1]), fileName, append = TRUE)
        } else {
          write(sprintf("%s,%d,%s,%f", dataset$readerID[j], k + K1, dataset$modalityID[i], -2000), fileName, append = TRUE)
        }
      }
    }
  }
} 



SaveOrDbmMrmc <- function(dataset, fileName) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  if (dataset$dataType != "ROC") {
    stop("Only ROC data file can be saved in MRMC format.")
  }
  fileExt <- file_ext(fileName)
  if (!fileExt %in% c("csv", "lrc", "txt")) {
    stop("The extension of MRMC file name must be *.txt or *.csv or *.lrc.")
  }
  NL <- dataset$NL
  LL <- dataset$LL
  # lesionVector <- dataset$lesionVector
  # lesionID <- dataset$lesionID
  # lesionWeight <- dataset$lesionWeight
  # maxNL <- dim(NL)[4]
  # dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  write("reader,treatment,case,truth,rating", fileName)
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K1) {
        if (NL[i, j, k, 1] != UNINITIALIZED) {
          write(sprintf("%s,%s,%d,%d,%f", dataset$readerID[j], dataset$modalityID[i], k, 0, NL[i, j, k, 1]), fileName, append = TRUE)
        } else {
          write(sprintf("%s,%s,%d,%d,%f", dataset$readerID[j], dataset$modalityID[i], k, 0, -2000), fileName, append = TRUE)
        }
      }
      
      for (k in 1:K2) {
        if (LL[i, j, k, 1] != UNINITIALIZED) {
          write(sprintf("%s,%s,%d,%d,%f", dataset$readerID[j], dataset$modalityID[i], k + K1, 1, LL[i, j, k, 1]), fileName, append = TRUE)
        } else {
          write(sprintf("%s,%s,%d,%d,%f", dataset$readerID[j], dataset$modalityID[i], k + K1, 1, -2000), fileName, append = TRUE)
        }
      }
    }
  }
} 


SaveLrc <- function(dataset, fileName, dataDscrpt) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  if (dataset$dataType != "ROC") {
    stop("Only ROC data file can be saved as *.lrc format.")
  }
  fileExt <- file_ext(fileName)
  if (fileExt != "lrc") {
    stop("The extension of LRC file name must be *.lrc")
  }
  write(dataDscrpt, fileName)
  NL <- dataset$NL
  LL <- dataset$LL
  # lesionVector <- dataset$lesionVector
  # lesionID <- dataset$lesionID
  # lesionWeight <- dataset$lesionWeight
  # maxNL <- dim(NL)[4]
  # dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  for (j in 1:J) {
    write(readerID[j], fileName, append = TRUE)
    if (j == 1) {
      string <- ""
      for (i in 1:I) {
        string <- paste0(string, sprintf("   \"%s\"", modalityID[i]))
      }
      write(string, fileName, append = TRUE)
      
      string <- ""
      for (i in 1:I) {
        string <- paste0(string, "       L")
      }
      write(string, fileName, append = TRUE)
    }
    
    for (k in 1:K1) {
      string <- ""
      for (i in 1:I) {
        if (NL[i, j, k, 1] != UNINITIALIZED) {
          string <- paste0(string, sprintf("   %f", NL[i, j, k, 1]))
        } else {
          string <- paste0(string, sprintf("   %f", 0))
        }
      }
      string <- paste0(string, sprintf("   Non-diseased case %d", k))
      write(string, fileName, append = TRUE)
    }
    write("*", fileName, append = TRUE)
    
    for (k in 1:K2) {
      string <- ""
      for (i in 1:I) {
        if (LL[i, j, k, 1] != UNINITIALIZED) {
          string <- paste0(string, sprintf("   %f", LL[i, j, k, 1]))
        } else {
          string <- paste0(string, sprintf("   %f", 0))
        }
      }
      string <- paste0(string, sprintf("   Diseased Case %d", k))
      write(string, fileName, append = TRUE)
    }
    write("*", fileName, append = TRUE)
  }
  write("#", fileName, append = TRUE)
} 
