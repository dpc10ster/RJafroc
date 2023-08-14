#' Save dataset object as a JAFROC format Excel file
#' 
#' @description Save a dataset object as a JAFROC format Excel file
#' 
#' @param dataset {The dataset object, see \code{\link{RJafroc-package}}}.
#' @param fileName {The file name to save to; the extension of the data file must be .xlsx}
#' 
#' @examples
#' \donttest{
#' ##DfWriteExcelDataFile(dataset = dataset05, fileName = "rocData2.xlsx")
#' }
#'
#' @import openxlsx  
#' @importFrom tools file_ext
#' 
#' @export
#' 



DfWriteExcelDataFile <- function(dataset, fileName) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  fileExt <- file_ext(fileName)
  if (!fileExt %in% c("xlsx", "xlsx")) {
    stop("The extension of JAFROC file name must be *.xlsx.")
  }
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  lesionVector <- dataset$lesions$perCase
  lesionID <- dataset$lesions$IDs
  lesionWeight <- dataset$lesions$weights
  maxNL <- dim(NL)[4]
  dataType <- dataset$descriptions$type
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
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
  addWorksheet(wb, "LL")
  addWorksheet(wb, "NL")
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
  dataSheet <- data.frame(ReaderID = readerID[dataSheet[, 1]], 
                          ModalityID = modalityID[dataSheet[, 2]], 
                          CaseID = as.integer(dataSheet[, 3]), 
                          NLRating = signif(dataSheet[, 4], 6))
  writeData(wb, sheet = "NL", x = dataSheet)
  
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
  dataSheet <- data.frame(ReaderID = readerID[dataSheet[, 1]], 
                          ModalityID = modalityID[dataSheet[, 2]], 
                          CaseID = as.integer(dataSheet[, 3]), 
                          LesionID = as.integer(dataSheet[, 4]), 
                          LLRating = signif(dataSheet[, 5], 6))
  writeData(wb, sheet = "LL", x = dataSheet) # openxlsx
  saveWorkbook(wb, fileName, overwrite = TRUE)
} 


SaveLrc <- function(dataset, fileName, dataDescription) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  if (dataset$descriptions$type != "ROC") {
    stop("Only ROC data file can be saved as *.lrc format.")
  }
  fileExt <- file_ext(fileName)
  if (fileExt != "lrc") {
    stop("The extension of LRC file name must be *.lrc")
  }
  write(dataDescription, fileName)
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  # lesionVector <- dataset$lesions$perCase
  # lesionID <- dataset$lesions$IDs
  # lesionWeight <- dataset$lesions$weights
  # maxNL <- dim(NL)[4]
  # dataType <- dataset$descriptions$type
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
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



# SaveImrmc <- function(dataset, fileName, dataDescription) {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   if (dataset$descriptions$type != "ROC") 
#     stop("Only ROC data file can be saved as iMRMC format.")
#   fileExt <- file_ext(fileName)
#   if (fileExt != "imrmc") {
#     stop("The extension of iMRMC file name must be *.imrmc.")
#   }
#   NL <- dataset$ratings$NL
#   LL <- dataset$ratings$LL
#   # lesionVector <- dataset$lesions$perCase
#   # lesionID <- dataset$lesions$IDs
#   # lesionWeight <- dataset$lesions$weights
#   # maxNL <- dim(NL)[4]
#   # dataType <- dataset$descriptions$type
#   modalityID <- dataset$descriptions$modalityID
#   readerID <- dataset$descriptions$readerID
#   I <- length(modalityID)
#   J <- length(readerID)
#   K <- dim(NL)[3]
#   K2 <- dim(LL)[3]
#   K1 <- K - K2
#   write(dataDescription, fileName)
#   write(sprintf("N0:%d", K1), fileName, append = TRUE)
#   write(sprintf("N1:%d", K2), fileName, append = TRUE)
#   write(sprintf("NR:%d", J), fileName, append = TRUE)
#   write(sprintf("NM:%d", I), fileName, append = TRUE)
#   write(sprintf("BEGIN DATA:"), fileName, append = TRUE)
#   for (k in 1:K1) {
#     write(sprintf("-1,%d,truth,0", k), fileName, append = TRUE)
#   }
#   for (k in 1:K2) {
#     write(sprintf("-1,%d,truth,1", k + K1), fileName, append = TRUE)
#   }
#   for (i in 1:I) {
#     for (j in 1:J) {
#       for (k in 1:K1) {
#         if (NL[i, j, k, 1] != UNINITIALIZED) {
#           write(sprintf("%s,%d,%s,%f", dataset$descriptions$readerID[j], k, 
#                         dataset$descriptions$modalityID[i], NL[i, j, k, 1]), fileName, append = TRUE)
#         } else {
#           write(sprintf("%s,%d,%s,%f", dataset$descriptions$readerID[j], k, 
#                         dataset$descriptions$modalityID[i], -2000), fileName, append = TRUE)
#         }
#       }
#       
#       for (k in 1:K2) {
#         if (LL[i, j, k, 1] != UNINITIALIZED) {
#           write(sprintf("%s,%d,%s,%f", dataset$descriptions$readerID[j], 
#                         k + K1, dataset$descriptions$modalityID[i], LL[i, j, k, 1]), fileName, append = TRUE)
#         } else {
#           write(sprintf("%s,%d,%s,%f", dataset$descriptions$readerID[j], 
#                         k + K1, dataset$descriptions$modalityID[i], -2000), fileName, append = TRUE)
#         }
#       }
#     }
#   }
# } 
# 
# 
# 
# SaveOrDbmMrmc <- function(dataset, fileName) {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   if (dataset$descriptions$type != "ROC") {
#     stop("Only ROC data file can be saved in MRMC format.")
#   }
#   fileExt <- file_ext(fileName)
#   if (!fileExt %in% c("csv", "lrc", "txt")) {
#     stop("The extension of MRMC file name must be *.txt or *.csv or *.lrc.")
#   }
#   NL <- dataset$ratings$NL
#   LL <- dataset$ratings$LL
#   modalityID <- dataset$descriptions$modalityID
#   readerID <- dataset$descriptions$readerID
#   I <- length(modalityID)
#   J <- length(readerID)
#   K <- dim(NL)[3]
#   K2 <- dim(LL)[3]
#   K1 <- K - K2
#   
#   write("reader,modality,case,truth,rating", fileName)
#   for (i in 1:I) {
#     for (j in 1:J) {
#       for (k in 1:K1) {
#         if (NL[i, j, k, 1] != UNINITIALIZED) {
#           write(sprintf("%s,%s,%d,%d,%f", dataset$descriptions$readerID[j], 
#                         dataset$descriptions$modalityID[i], k, 0, NL[i, j, k, 1]), fileName, append = TRUE)
#         } else {
#           write(sprintf("%s,%s,%d,%d,%f", dataset$descriptions$readerID[j], 
#                         dataset$descriptions$modalityID[i], k, 0, -2000), fileName, append = TRUE)
#         }
#       }
#       
#       for (k in 1:K2) {
#         if (LL[i, j, k, 1] != UNINITIALIZED) {
#           write(sprintf("%s,%s,%d,%d,%f", dataset$descriptions$readerID[j], 
#                         dataset$descriptions$modalityID[i], k + K1, 1, LL[i, j, k, 1]), 
#                 fileName, append = TRUE)
#         } else {
#           write(sprintf("%s,%s,%d,%d,%f", dataset$descriptions$readerID[j], 
#                         dataset$descriptions$modalityID[i], k + K1, 1, -2000), 
#                 fileName, append = TRUE)
#         }
#       }
#     }
#   }
# } 
# 
# 
# SaveLrc <- function(dataset, fileName, dataDscrpt) {
#   UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
#   if (dataset$descriptions$type != "ROC") {
#     stop("Only ROC data file can be saved as *.lrc format.")
#   }
#   fileExt <- file_ext(fileName)
#   if (fileExt != "lrc") {
#     stop("The extension of LRC file name must be *.lrc")
#   }
#   write(dataDscrpt, fileName)
#   NL <- dataset$ratings$NL
#   LL <- dataset$ratings$LL
#   # lesionVector <- dataset$lesions$perCase
#   # lesionID <- dataset$lesions$IDs
#   # lesionWeight <- dataset$lesions$weights
#   # maxNL <- dim(NL)[4]
#   # dataType <- dataset$descriptions$type
#   modalityID <- dataset$descriptions$modalityID
#   readerID <- dataset$descriptions$readerID
#   I <- length(modalityID)
#   J <- length(readerID)
#   K <- dim(NL)[3]
#   K2 <- dim(LL)[3]
#   K1 <- K - K2
#   for (j in 1:J) {
#     write(readerID[j], fileName, append = TRUE)
#     if (j == 1) {
#       string <- ""
#       for (i in 1:I) {
#         string <- paste0(string, sprintf("   \"%s\"", modalityID[i]))
#       }
#       write(string, fileName, append = TRUE)
#       
#       string <- ""
#       for (i in 1:I) {
#         string <- paste0(string, "       L")
#       }
#       write(string, fileName, append = TRUE)
#     }
#     
#     for (k in 1:K1) {
#       string <- ""
#       for (i in 1:I) {
#         if (NL[i, j, k, 1] != UNINITIALIZED) {
#           string <- paste0(string, sprintf("   %f", NL[i, j, k, 1]))
#         } else {
#           string <- paste0(string, sprintf("   %f", 0))
#         }
#       }
#       string <- paste0(string, sprintf("   Non-diseased case %d", k))
#       write(string, fileName, append = TRUE)
#     }
#     write("*", fileName, append = TRUE)
#     
#     for (k in 1:K2) {
#       string <- ""
#       for (i in 1:I) {
#         if (LL[i, j, k, 1] != UNINITIALIZED) {
#           string <- paste0(string, sprintf("   %f", LL[i, j, k, 1]))
#         } else {
#           string <- paste0(string, sprintf("   %f", 0))
#         }
#       }
#       string <- paste0(string, sprintf("   Diseased Case %d", k))
#       write(string, fileName, append = TRUE)
#     }
#     write("*", fileName, append = TRUE)
#   }
#   write("#", fileName, append = TRUE)
# } 
