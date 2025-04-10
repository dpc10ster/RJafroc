#' RSM predicted operating characteristics, ROC pdfs and AUCs
#'
#' @description Visualize RSM predicted ROC, AFROC, wAFROC and FROC curves, and
#'   ROC pdfs, given \bold{equal-length arrays} of search model parameters: mu,
#'   lambda, nu and zeta1.
#'
#' @param mu Array: the RSM mu parameter.
#'
#' @param lambda Array: the RSM lambda parameter.
#'
#' @param nu Array: the RSM nu parameter.
#'
#' @param zeta1 Array, the lowest reporting threshold; if missing the default is
#'   an array of -Inf.
#'
#' @param lesDistr Array: the probability mass function of the lesion
#'   distribution for diseased cases. The default is 1. See \link{UtilLesDistr}.
#'
#' @param relWeights The relative weights of the lesions; a vector of length
#'   equal to \code{length(maxLL)}. The default is zero, in which case equal
#'   weights are assumed.
#'
#' @param  OpChType The type of operating characteristic desired: can be
#'   "\code{ROC}", "\code{AFROC}", "\code{wAFROC}", "\code{FROC}" or
#'   "\code{pdfs}" or "\code{ALL}". The default is "\code{ALL}".
#'
#' @param legendPosition The positioning of the legend: "\code{right}",
#'   "\code{left}", "\code{top}" or "\code{bottom}". Use "\code{none}" to
#'   suppress the legend.
#'
#' @param legendDirection Allows control on the direction of the legend;
#'   \code{"horizontal"}, the default, or \code{"vertical"}
#'
#' @param legendJustification Where to position the legend, default is bottom
#'   right corner c(0,1)
#'
#' @param nlfRange \bold{This applies to FROC plot only}. The x-axis range,
#'   e.g., c(0,2), for FROC plot. Default is "\code{NULL}", which means the
#'   maximum NLF range, as determined by the data.
#'
#' @param llfRange \bold{This applies to FROC plot only}. The y-axis range,
#'   e.g., c(0,1), for FROC plot. Default is "\code{NULL}", which means the
#'   maximum LLF range, as determined by the data.
#'
#' @param nlfAlpha Upper limit of the integrated area under the FROC plot.
#'   Default is "\code{NULL}", which means the maximum NLF range is used (i.e.,
#'   lambda). Attempt to integrate outside the maximum NLF will generate an
#'   error.
#'
#'
#' @return A list containing five \pkg{ggplot2} objects (ROCPlot, AFROCPlot
#'   wAFROCPlot, FROCPlot and PDFPlot) and two area measures (each of which can
#'   have up to two elements), the area under the search model predicted ROC
#'   curves in up to two treatments, the area under the search model predicted
#'   AFROC curves in up to two treatments, the area under the search model
#'   predicted wAFROC curves in up to two treatments, the area under the search
#'   model predicted FROC curves in up to two treatments.
#' \itemize{
#' \item\code{ROCPlot}   The predicted ROC plots
#' \item\code{AFROCPlot}     The predicted AFROC plots
#' \item\code{wAFROCPlot}    The predicted wAFROC plots
#' \item\code{FROCPlot}  The predicted FROC plots
#' \item\code{PDFPlot}   The predicted ROC pdf plots, highest rating generated
#' \item\code{aucROC}    The predicted ROC AUCs, highest rating generated
#' \item\code{aucAFROC}  The predicted AFROC AUCs
#' \item\code{aucwAFROC} The predicted wAFROC AUCs
#' \item\code{aucFROC}   The predicted FROC AUCs
#' }
#'
#' @details RSM is the Radiological Search Model described in the book. This
#'   function is vectorized with respect to the first 4 arguments. For
#'   \code{lesDistr} the sum must be one. To indicate that all dis. cases
#'   contain 4 lesions, set lesDistr = c(0,0,0,1).
#'
#'
#'
#'
#' @import ggplot2
#'
#' @importFrom stats integrate dbinom dnorm pnorm qnorm
#'
#' @references Chakraborty DP (2006) A search model and figure of merit for
#'   observer data acquired according to the free-response paradigm, Phys Med
#'   Biol 51, 3449-3462.
#'
#'   Chakraborty DP (2006) ROC Curves predicted by a model of visual search,
#'   Phys Med Biol 51, 3463--3482.
#'
#'   Chakraborty, DP, Yoon, HJ (2008) Operating characteristics predicted by
#'   models for diagnostic tasks involving lesion localization, Med Phys, 35:2,
#'   435.
#'
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations,
#' Modeling, and Applications with R-Based Examples} (CRC Press, Boca Raton, FL).
#'   \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#'
#' @examples
#' ## Following example is for mu = 2, lambda = 1, nu = 0.6, in one modality and
#' ## mu = 3, lambda = 1.5, nu = 0.8, in the other modality. 20% of the diseased
#' ## cases have a single lesion, 40% have two lesions, 10% have 3 lesions,
#' ## and 30% have 4 lesions.
#'
#' res <- PlotRsmOpCht(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
#'    lesDistr = c(0.2, 0.4, 0.1, 0.3), legendPosition = "bottom")
#'
#' @export
#' 
PlotRsmOpChr <- function(mu, 
                                            lambda, 
                                            nu,
                                            zeta1,
                                            lesDistr = 1, 
                                            relWeights = 0, 
                                            OpChType = "ALL", 
                                            legendPosition = "bottom", 
                                            legendDirection = "horizontal", 
                                            legendJustification = c(0,1),
                                            nlfRange = NULL, 
                                            llfRange = NULL, 
                                            nlfAlpha = NULL){
  
  
  if (missing(zeta1)) zeta1 <- array(-Inf, dim = length(mu))
  
  if (!all(c(length(mu) == length(lambda), length(mu) == length(nu), length(mu) == length(zeta1))))
    stop("Parameters mu, lambda, nu and zeta1 have different lengths.")
  
  lesWghtDistr <- UtilLesWghtsLD(lesDistr, relWeights)
  
  plotStep <- 0.01
  # begin bug fix 12/7/21
  # plotStep <- 0.1 # delete after debug
  zeta <- array(list(), dim = length(mu))
  for (i in 1:length(mu)) {
    if (zeta1[i] == -Inf) temp1 <- -3 else temp1 <- zeta1[i]  
    zeta[[i]] <- seq(from = temp1, to = max(mu)+5, by = plotStep)
  }
  # end bug fix 12/7/21
  
  ROCPlot <- NA
  FROCPlot <- NA
  AFROCPlot <- NA
  wAFROCPlot <- NA
  PDFPlot <- NA
  ROCPoints <- data.frame(FPF = NULL, TPF = NULL, Treatment = NULL, stringsAsFactors = FALSE)
  ROCDashes <- data.frame(FPF = NULL, TPF = NULL, Treatment = NULL, stringsAsFactors = FALSE)
  FROCPoints <- data.frame(NLF = NULL, LLF = NULL, Treatment = NULL, stringsAsFactors = FALSE)
  AFROCPoints <- data.frame(FPF = NULL, LLF= NULL, Treatment = NULL, stringsAsFactors = FALSE)
  AFROCDashes <- data.frame(FPF = NULL, LLF= NULL, Treatment = NULL, stringsAsFactors = FALSE)
  wAFROCPoints <- data.frame(FPF = NULL, wLLF= NULL, Treatment = NULL, stringsAsFactors = FALSE)
  wAFROCDashes <- data.frame(FPF = NULL, wLLF= NULL, Treatment = NULL, stringsAsFactors = FALSE)
  abnPDFPoints <- data.frame(pdf = NULL, highestZSample = NULL, Treatment = NULL, stringsAsFactors = FALSE)
  norPDFPoints <- data.frame(pdf = NULL, highestZSample = NULL, Treatment = NULL, stringsAsFactors = FALSE)
  aucROC <- rep(NA, length(mu));aucAFROC <- aucROC;aucwAFROC <- aucROC;aucFROC <- aucROC
  
  for (i in 1:length(mu)){
    if (mu[i] <= 0 ) stop("mu must be greater than zero")
    if (lambda[i] < 0 ) stop("lambda must be greater than zero")
    if (nu[i] < 0 ) stop("nu must be greater than zero")
    
    FPF <- sapply(zeta[[i]], xROC_cpp, lambda[i]) # C++ function uses lambda
    TPF <- sapply(zeta[[i]], yROC_cpp, mu = mu[i], lambda[i], nu[i], lesDistr)
    NLF <- sapply(zeta[[i]], RSM_NLF, lambda[i])
    LLF <- sapply(zeta[[i]], RSM_LLF, mu = mu[i], nu[i]) # 9/22/22
    # begin bug fix
    # found bug 11/24/21 two places, here and as indicated by # bug fix 11/24/21
    # found ROC AUC did not change with zeta1 as it should   
    #    maxFPF <- xROC_cpp(-20, lambda[i]) # this is wrong
    maxFPF <- xROC_cpp(zeta1[i], lambda[i]) # this is correct
    if( OpChType == "ALL" ||  OpChType == "ROC"){
      ROCPoints <- rbind(ROCPoints, data.frame(FPF = FPF, 
                                               TPF = TPF, 
                                               Treatment = as.character(i), 
                                               stringsAsFactors = FALSE))
      ROCDashes <- rbind(ROCDashes, data.frame(FPF = c(FPF[1], 1), 
                                               TPF = c(TPF[1], 1), 
                                               Treatment = as.character(i), 
                                               stringsAsFactors = FALSE))
      #       maxTPF <- yROC_cpp(-20, mu[i], lambda, nu, lesDistr)
      maxTPF <- yROC_cpp(zeta1[i], mu[i], lambda[i], nu[i], lesDistr)
      # end bug fix
      AUC <- integrate(y_ROC_FPF_cpp, 0, maxFPF, mu = mu[i], lambda[i], nu[i], lesDistr =lesDistr)$value
      aucROC[i] <- AUC + (1 + maxTPF) * (1 - maxFPF) / 2
    }
    
    if( OpChType == "ALL" ||  OpChType == "FROC"){
      FROCPoints <- rbind(FROCPoints, data.frame(NLF = NLF, 
                                                 LLF = LLF, 
                                                 Treatment = as.character(i), 
                                                 stringsAsFactors = FALSE))
      if (is.null(nlfAlpha)){
        maxNLF <- max(NLF)
        aucFROC[i] <- integrate(integrandFROC, 0, maxNLF, mu= mu[i], lambda[i], nu[i])$value
      }else{
        maxNLF <- max(NLF)
        if (nlfAlpha <= maxNLF){
          aucFROC[i] <- integrate(integrandFROC, 0, nlfAlpha, mu= mu[i], lambda[i], nu[i])$value
        }else{
          stop("nlfAlpha cannot be greater than max(NLF).")
        }
      }
    }
    
    if( OpChType == "ALL" ||  OpChType == "AFROC"){
      AFROCPoints <- rbind(AFROCPoints, data.frame(FPF = FPF, 
                                                   LLF = LLF, 
                                                   Treatment = as.character(i), 
                                                   stringsAsFactors = FALSE))
      AFROCDashes <- rbind(AFROCDashes, data.frame(FPF = c(FPF[1], 1), 
                                                   LLF = c(LLF[1], 1), 
                                                   Treatment = as.character(i), 
                                                   stringsAsFactors = FALSE))
      maxLLF <- RSM_LLF(zeta1[i], mu[i], nu[i]) # bug fix 9/22/22
      AUC <- integrate(y_AFROC_FPF, 0, maxFPF, mu = mu[i], lambda[i], nu[i])$value
      aucAFROC[i] <- AUC + (1 + maxLLF) * (1 - maxFPF) / 2
    }
    
    if( OpChType == "ALL" ||  OpChType == "wAFROC"){
      wLLF <- sapply(zeta[[i]], RSM_wLLF_cpp, mu[i], nu[i], lesDistr, lesWghtDistr) # cpp code requires lesWghtDistr
      wAFROCPoints <- rbind(wAFROCPoints, data.frame(FPF = FPF, 
                                                     wLLF = wLLF, 
                                                     Treatment = as.character(i), 
                                                     stringsAsFactors = FALSE))
      wAFROCDashes <- rbind(wAFROCDashes, data.frame(FPF = c(FPF[1], 1), 
                                                     wLLF = c(wLLF[1], 1), 
                                                     Treatment = as.character(i), 
                                                     stringsAsFactors = FALSE))
      maxWLLF <- RSM_wLLF_R(zeta1[i], mu[i], nu[i], lesDistr, relWeights)
      AUC <- integrate(y_wAFROC_FPF, 0, maxFPF, mu = mu[i], lambda[i], nu[i], lesDistr, relWeights)$value
      aucwAFROC[i] <- AUC + (1 + maxWLLF) * (1 - maxFPF) / 2 # 5/3/23 
    }
    
    if( OpChType == "ALL" ||  OpChType == "pdfs"){
      deltaFPF <- FPF[1:(length(FPF) - 1)] - FPF[2:length(FPF)]  
      if( OpChType == "ALL" ||  OpChType == "pdfs"){     
        pdfNor <- deltaFPF / plotStep
        norPDFPoints <- rbind(norPDFPoints, 
                              data.frame(pdf = pdfNor[pdfNor > 1e-6], 
                                         highestZSample = zeta[[i]][-1][pdfNor > 1e-6], 
                                         Treatment = as.character(i), 
                                         class = "non-diseased", 
                                         stringsAsFactors = FALSE))
        deltaTPF <- TPF[1:(length(TPF) - 1)] - TPF[2:length(TPF)]
        pdfAbn <- deltaTPF / plotStep
        abnPDFPoints <- rbind(abnPDFPoints, 
                              data.frame(pdf = pdfAbn[pdfAbn > 1e-6], 
                                         highestZSample = zeta[[i]][-1][pdfAbn > 1e-6], 
                                         Treatment = as.character(i), 
                                         class = "diseased", 
                                         stringsAsFactors = FALSE))
      }
    }
  }
  
  if( OpChType == "ALL" ||  OpChType == "ROC") {
    ROCPlot <- with(ROCPoints, {
      ggplot(data = ROCPoints) + 
        geom_line(aes(x = FPF, y = TPF, color = Treatment))  +       
        geom_line(data = ROCDashes, aes(x = FPF, y = TPF, color = Treatment), linetype = 2) +       
        theme(legend.direction = legendDirection, legend.justification = c(1, 0)) + 
        theme(legend.position = "inside", legend.position.inside = legendPosition)
    })
  }
  
  if( OpChType == "ALL" ||  OpChType == "FROC"){
    FROCPlot <- with(FROCPoints, {
      ggplot(data = FROCPoints) + 
        geom_line(aes(x = NLF, y = LLF, color = Treatment))  +       
        scale_x_continuous(expand = c(0, 0), limits = nlfRange) + 
        scale_y_continuous(expand = c(0, 0), limits = llfRange) + 
        theme(legend.direction = legendDirection, legend.justification = c(1, 0)) +
        theme(legend.position = "inside", legend.position.inside = legendPosition)
    })
  }
  
  if( OpChType == "ALL" ||  OpChType == "AFROC"){
    AFROCPlot <- with(AFROCPoints, {
      ggplot(data = AFROCPoints) + 
        geom_line(aes(x = FPF, y = LLF , color = Treatment)) + 
        geom_line(data = AFROCDashes, aes(x = FPF, y = LLF, color = Treatment), linetype = 2) +       
        theme(legend.direction = legendDirection, legend.justification = c(1, 0)) + 
        theme(legend.position = "inside", legend.position.inside = legendPosition)
    }
    )
  }
  
  if( OpChType == "ALL" ||  OpChType == "wAFROC"){
    wAFROCPlot <- with(wAFROCPoints, {
      ggplot(data = wAFROCPoints) + 
        geom_line(aes(x = FPF, y = wLLF , color = Treatment)) + 
        geom_line(data = wAFROCDashes, aes(x = FPF, y = wLLF, color = Treatment), linetype = 2) +       
        theme(legend.direction = legendDirection, legend.justification = c(1, 0)) + 
        theme(legend.position = "inside", legend.position.inside = legendPosition)
      
    })
  }
  
  if( OpChType == "ALL" ||  OpChType == "pdfs"){
    if (legendPosition == "top" || legendPosition == "bottom"){
      legendDirection = "horizontal"
    }else{
      legendDirection = "vertical"
    }
    PDFPoints <- rbind(norPDFPoints, abnPDFPoints)
    PDFPlot <- with(PDFPoints, {
      ggplot(data = PDFPoints, 
             aes(x = highestZSample, y = pdf, color = Treatment, linetype = class)) + 
        geom_line()  + theme(legend.box = legendDirection) + 
        theme(legend.position = "inside", legend.position.inside = legendPosition)
      labs(x = "Highest Z Sample") 
    })
  }
  
  return(list(
    ROCPlot = ROCPlot,
    AFROCPlot = AFROCPlot,
    wAFROCPlot = wAFROCPlot,
    FROCPlot = FROCPlot,
    PDFPlot = PDFPlot,
    aucROC = aucROC,
    aucAFROC = aucAFROC,
    aucwAFROC = aucwAFROC,
    aucFROC = aucFROC
  ))
}


