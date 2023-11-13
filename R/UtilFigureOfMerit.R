#' Calculate empirical figures of merit (FOMs) for factorial dataset, standard 
#'     or cross-modality
#' 
#' @description  Calculate the specified empirical figure of merit for each 
#'     modality-reader combination in a standard or cross-modality dataset
#' 
#' @param dataset The dataset to be analyzed, \code{\link{RJafroc-package}}
#' 
#' @param FOM The figure of merit; the default is \code{"wAFROC"}
#' 
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'    where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return For standard dataset: A \code{c(I, J)} dataframe, where the row names are 
#'    \code{modalityID}'s of the treatments and column names are the 
#'    \code{readerID}'s of the readers.
#'    For cross-modality dataset: Two data frames are returned: 
#'    * \code{c(I2, J)} data frame, FOMs averaged over the first modality, where the row 
#'    names are modality IDS of the second modality 
#'    * \code{c(I1, J)} data frames, FOMs averaged over the second modality, where the row 
#'    names are modality IDs of the first modality, 
#'    * In either case the column names are the \code{readerID}'s.

#' 
#' @details The allowed FOMs depend on the \code{dataType} field of the 
#'    \code{dataset} object. 
#' 
#' 
#'    \strong{For \code{dataset$descriptions$type = "ROC"} only \code{FOM = "Wilcoxon"} is allowed}.
#'    \strong{For \code{dataset$descriptions$type = "FROC"} the following FOMs are allowed}:
#'    \itemize{ 
#'    \item \code{FOM = "AFROC1"} (use only if no normal cases are available)
#'    \item \code{FOM = "AFROC"} 
#'    \item \code{FOM = "wAFROC1"} (use only if no normal cases  are available)
#'    \item \code{FOM = "wAFROC"} (the default) 
#'    \item \code{FOM = "HrAuc"} 
#'    \item \code{FOM = "HrSe"} (example of an end-point based FOM)
#'    \item \code{FOM = "HrSp"} (do:)
#'    \item \code{FOM = "MaxLLF"} (do:)
#'    \item \code{FOM = "MaxNLF"} (do:)
#'    \item \code{FOM = "MaxNLFAllCases"} (do:) 
#'    } 
#'    \code{"MaxLLF"}, \code{"MaxNLF"} and \code{"MaxNLFAllCases"}
#'    correspond to ordinate, and abscissa, respectively, of the highest point 
#'    on the FROC operating characteristic obtained by counting all the marks. 
#'    Given the number of FOMs possible with FROC data, it is appropriate 
#'    to make a recommendation: \strong{it is recommended the wAFROC FOM be used
#'    whenever possible.  One should use the wAFROC1 FOM only if the dataset has 
#'    no non-diseased cases}.
#'    
#'    For \strong{\code{dataType = "ROI"} dataset only \code{FOM = "ROI"} is allowed}.
#'    
#'    For \strong{\code{dataType = "LROC"}} dataset the following FOMs are allowed:
#'    \itemize{
#'    \item \code{FOM = "Wilcoxon"} for ROC data inferred from LROC data 
#'    \item \code{FOM = "PCL"} the probability of correct localization at specified \code{FPFValue}
#'    \item \code{FOM = "ALROC"} the area under the LROC from zero to specified \code{FPFValue} 
#'    }
#'    \code{FPFValue} The FPF at which to evaluate \code{PCL} or \code{ALROC}; 
#'    the default is 0.2; only needed for LROC data.
#'    For cross-modality analysis ROI and LROC datasets are not supported.
#' 
#'
#' @examples
#' res <- UtilFigureOfMerit(dataset02, FOM = "Wilcoxon") # ROC data
#' res <- UtilFigureOfMerit(dataset01) # FROC dataset, default wAFROC FOM
#' res <- UtilFigureOfMerit(datasetXModality, FOM = "wAFROC")
#' 
#' 
#' 
#' @references
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' 
#' 
#' @importFrom dplyr between  
#' @export

UtilFigureOfMerit <- function(dataset, FOM = "wAFROC", FPFValue = 0.2) { 
  
  isValidDataset(dataset, FOM)
  
  if (dataset$descriptions$design == "FCTRL") { 
    # factorial one-treatment dataset 
    
    dataType <- dataset$descriptions$type
    
    if (dataType == "LROC") {
      if (FOM == "Wilcoxon"){
        datasetRoc <- DfLroc2Roc(dataset)
        NL <- datasetRoc$ratings$NL
        LL <- datasetRoc$ratings$LL
      } else if (FOM %in% c("PCL", "ALROC")){
        NL <- dataset$ratings$NL
        LL <- dataset$ratings$LL
      } else stop("incorrect FOM for LROC data")
      
    } else {
      NL <- dataset$ratings$NL
      LL <- dataset$ratings$LL
    }
    
    I <- dim(NL)[1]
    J <- dim(NL)[2]
    K <- dim(NL)[3]
    K2 <- dim(LL)[3]
    K1 <- K - K2  
    
    if ((K1 == 0) && !(FOM %in% c("AFROC1", "wAFROC1"))) {
      errMsg <- paste0("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.")
      stop(errMsg)
    }
    
    design <- dataset$descriptions$design
    t <- dataset$descriptions$truthTableStr
    
    maxNL <- dim(NL)[4]
    maxLL <- dim(LL)[4]
    foms <- array(dim = c(I, J))
    for (i in 1:I) {
      for (j in 1:J) {
        if (design == "FCTRL"){
          nl_ij <- NL[i, j, , ]
          ll_ij <- LL[i, j, , ]
          dim(nl_ij) <- c(K, maxNL)
          dim(ll_ij) <- c(K2, maxLL)
          foms[i, j] <- MyFom_ij(nl_ij, ll_ij, 
                                 dataset$lesions$perCase, 
                                 dataset$lesions$IDs, 
                                 dataset$lesions$weights, 
                                 maxNL, 
                                 maxLL, 
                                 K1, 
                                 K2, 
                                 FOM, 
                                 FPFValue)
        } else stop("Incorrect study design specified: must be `FCTRL`")
      }
    }
    
    modalityID <- dataset$descriptions$modalityID
    readerID <- dataset$descriptions$readerID
    rownames(foms) <- paste("trt", sep = "", modalityID)
    colnames(foms) <- paste("rdr", sep = "", readerID)
    
    return(foms)
    
  } else { 
    # cross-modality factorial dataset, two treatment factors
    
    NL <- dataset$ratings$NL
    LL <- dataset$ratings$LL
    
    I1 <- dim(NL)[1]
    I2 <- dim(NL)[2]
    J <- dim(NL)[3]
    K <- dim(NL)[4]
    K2 <- dim(LL)[4]
    K1 <- K - K2  
    maxNL <- dim(NL)[5]
    maxLL <- dim(LL)[5]
    
    dataType <- dataset$descriptions$type
    
    foms <- array(dim = c(I1, I2, J))
    for (i1 in 1:I1) {
      for (i2 in 1:I2) {
        for (j in 1:J) {
          foms[i1, i2, j] <- MyFom_ij(NL[i1, i2, j, , ], 
                                      LL[i1, i2, j, , ], 
                                      dataset$lesions$perCase, 
                                      dataset$lesions$IDs, 
                                      dataset$lesions$weights, 
                                      maxNL, 
                                      maxLL, 
                                      K1, 
                                      K2, 
                                      FOM)
        }
      }
    }
    
    return(foms)
  }
} 


learnApply <- function(foms){
  
  ave1 <- apply(foms, c(1), mean)
  # [1] 0.8672883 0.8688070
  ave2 <- c(sum(foms[1,,]), sum(foms[2,,]))/4/11
  testthat::expect_equal(ave2, ave1)
  
  ave1 <- apply(foms, c(2), mean)
  # [1] 0.8354075 0.8672539 0.8780768 0.8914524
  ave2 <- c(sum(foms[,1,]), sum(foms[,2,]), sum(foms[,3,]), sum(foms[,4,]))/2/11
  testthat::expect_equal(ave2, ave1)
  
  ave1 <- apply(foms, c(3), mean)
  # [1] 0.8632948 0.8270978 0.9215326 0.9070430 0.9540531 0.8864619 0.8483726 0.7668144 0.8638534 0.8208622 0.8891382
  ave2 <- c(
    sum(foms[,,1]), sum(foms[,,2]), sum(foms[,,3]), sum(foms[,,4]),
    sum(foms[,,5]), sum(foms[,,6]), sum(foms[,,7]), sum(foms[,,8]),
    sum(foms[,,9]), sum(foms[,,10]), sum(foms[,,11]))/2/4
  testthat::expect_equal(ave2, ave1)
  
  ave1 <- apply(foms, c(2,3), mean)
  #           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]     [,11]
  # [1,] 0.7900087 0.7892157 0.8861015 0.8913279 0.9471237 0.8688725 0.8207540 0.7320141 0.8268815 0.8175822 0.8196006
  # [2,] 0.8763336 0.8109141 0.9272275 0.9021410 0.9621540 0.8725490 0.8566176 0.7552984 0.8765499 0.7941176 0.9058896
  # [3,] 0.9107194 0.8480392 0.9226499 0.9012039 0.9485655 0.8840830 0.8643310 0.7869449 0.8512471 0.8239259 0.9171352
  # [4,] 0.8761174 0.8602220 0.9501514 0.9334991 0.9583694 0.9203431 0.8517878 0.7930003 0.9007353 0.8478230 0.9139273
  ave2 <- (foms[1,,] + foms[2,,])/2
  testthat::expect_equal(ave2, ave1)
  
  ave1 <- apply(foms, c(1,3), mean)
  #           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]     [,11]
  # [1,] 0.8758470 0.8313149 0.9143058 0.9029159 0.9537377 0.8811275 0.8298551 0.7573349 0.8723147 0.8274942 0.8939230
  # [2,] 0.8507425 0.8228806 0.9287594 0.9111700 0.9543685 0.8917964 0.8668901 0.7762940 0.8553922 0.8142301 0.8843534
  ave2 <- (foms[,1,] + foms[,2,] + foms[,3,] + foms[,4,])/4
  testthat::expect_equal(ave2, ave1)
  
  ave1 <- apply(foms, c(1,2,3), mean)
  # , , 1
  #
  # [,1]      [,2]      [,3]      [,4]
  # [1,] 0.8365773 0.8652682 0.9189014 0.8826413
  # [2,] 0.7434400 0.8873991 0.9025375 0.8695934
  #  , , 2
  # etc
  # ending with
  # , , 11
  #
  # [,1]      [,2]      [,3]      [,4]
  # [1,] 0.8390283 0.8876874 0.9298587 0.9191176
  # [2,] 0.8001730 0.9240917 0.9044118 0.9087370
  ave2 <- foms
  testthat::expect_equal(ave2, ave1)
  
  
  stop()
}