#' RSM predicted operating characteristics, ROC highest rating pdfs and FOMs,
#'    for FROC data
#' 
#' @description Visualize RSM predicted ROC, AFROC, wAFROC, FROC and pdf 
#'    (probability density functions of highest ratings curves 
#'    for non-diseased and diseased cases), for sets of search model parameters: 
#'    mu, lambda, nu and zeta1.
#' 
#' @param mu Array: the mean of the Gaussian distribution for the 
#'    ratings of latent LLs (continuous ratings of lesions that are found by the 
#'    observer's search mechanism). The ratings of NLs are distributed as N(0,1).
#' 
#' @param lambda Array: the \emph{intrinsic} Poisson distribution parameter 
#'    which models the random numbers of latent NLs (suspicious regions that do 
#'    not correspond to actual lesions) per case. The corresponding 
#'    \emph{physical} parameter is \code{lambda/mu}. Two conversion functions 
#'    are provided: \code{\link{UtilIntrinsic2PhysicalRSM}} and 
#'    \code{\link{UtilPhysical2IntrinsicRSM}}.
#' 
#' @param nu Array: the \emph{intrinsic} parameter which models the random 
#'    numbers of latent LLs (suspicious regions that correspond to actual 
#'    lesions) per diseased case. The corresponding \emph{physical} parameter is 
#'    \code{1 - exp(nu*mu)}, the success probability of the binomial distribution.
#' 
#' @param zeta1 Array, the lowest reporting threshold; if missing the default 
#'    is -3. [Used to demonstrate continuity of the slope of the ROC at the 
#'    end point; TBA Online Appendix 17.H.3] 
#'
#' @param lesDistr Array: the probability mass function of the 
#'    lesion distribution for diseased cases. See \link{UtilLesionDistr}. 
#' 
#' @param relWeights The relative weights of the lesions; a vector of 
#'    length equal to \code{length(maxLL)}. The default is zero, in which case
#'    equal weights are assumed.
#' 
#' @param  OpChType The type of operating characteristic desired: can be "\code{ROC}", 
#'    "\code{AFROC}", "\code{wAFROC}", "\code{FROC}" or "\code{pdfs}" or "\code{ALL}". 
#'    The default is "\code{ALL}".
#' 
#' @param legendPosition The positioning of the legend: "\code{right}", "\code{left}", 
#'    "\code{top}" or "\code{bottom}". Use "\code{none}" to suppress the legend.
#' 
#' @param legendDirection Allows control on the direction of the legend; 
#'    \code{"horizontal"}, the default, or \code{"vertical"}
#' 
#' @param legendJustification Where to position the legend, default 
#' is bottom right corner c(0,1)
#' 
#' @param nlfRange \bold{This applies to FROC plot only}. The x-axis range, e.g., c(0,2), 
#'    for FROC plot. Default is "\code{NULL}", which means the maximum NLF range, 
#'    as determined by the data.
#' 
#' @param llfRange \bold{This applies to FROC plot only}. The y-axis range, e.g., c(0,1), 
#'    for FROC plot. Default is "\code{NULL}", which means the maximum LLF range,
#'    as determined by the data.
#' 
#' @param nlfAlpha Upper limit of the integrated area under the FROC plot. 
#'    Default is "\code{NULL}", which means the maximum NLF range is used 
#'    (i.e., lambda/mu). Attempt to integrate outside the maximum NLF will 
#'    generate an error.
#' 
#' 
#' @return A list of elements containing five \pkg{ggplot2} objects 
#'    (ROCPlot, AFROCPlot wAFROCPlot, FROCPlot and PDFPlot) and two area measures 
#'    (each of which can have up to two elements), the area under the search 
#'    model predicted ROC curves in up to two treatments, the area under the search 
#'    model predicted AFROC curves in up to two treatments, the area under the 
#'    search model predicted wAFROC curves in up to two treatments, the area under 
#'    the search model predicted FROC curves in up to two treatments.
#' \itemize{
#' \item{\code{ROCPlot}}   {The predicted ROC plots}
#' \item{\code{AFROCPlot}}     {The predicted AFROC plots}
#' \item{\code{wAFROCPlot}}    {The predicted wAFROC plots}
#' \item{\code{FROCPlot}}  {The predicted FROC plots}
#' \item{\code{PDFPlot}}   {The predicted pdf plots}
#' \item{\code{aucROC}}    {The predicted ROC AUCs}
#' \item{\code{aucAFROC}}  {The predicted AFROC AUCs}
#' \item{\code{aucwAFROC}} {The predicted wAFROC AUCs}
#' \item{\code{aucFROC}}   {The predicted FROC AUCs}
#' }
#' 
#' @details RSM is the Radiological Search Model described in the book. This 
#'    function is vectorized with respect to the first 4 arguments. For 
#'    \code{lesDistr} the sum must be one. To indicate that all dis. cases
#'    contain 4 lesions, set lesDistr = c(0,0,0,1).  
#' 
#'   
#' 
#' 
#' @import ggplot2
#' 
#' @importFrom stats integrate dbinom dnorm pnorm qnorm
#' 
#' @references
#' Chakraborty DP (2006) A search model and figure of merit for observer data acquired according to the free-response 
#' paradigm, Phys Med Biol 51, 3449-3462.
#' 
#' Chakraborty DP (2006) ROC Curves predicted by a model of visual search, Phys Med Biol 51, 3463--3482.
#'
#' Chakraborty, DP, Yoon, HJ (2008) Operating characteristics predicted by models for diagnostic tasks involving lesion localization, Med Phys, 35:2, 435.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples} (CRC Press, Boca Raton, FL). 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @examples
#' ## Following example is for mu = 2, lambda = 1, nu = 0.6, in one treatment and   
#' ## mu = 3, lambda = 1.5, nu = 0.8, in the other treatment. 20% of the diseased 
#' ## cases have a single lesion, 40% have two lesions, 10% have 3 lesions, 
#' ## and 30% have 4 lesions.  
#' lesDistr <- c(0.2, 0.4, 0.1, 0.3)
#' 
#' PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
#'    lesDistr = lesDistr, legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))
#' 
#' @export
#' 
PlotRsmOperatingCharacteristics <- function(mu, 
                                            lambda, 
                                            nu,
                                            zeta1,
                                            lesDistr, 
                                            relWeights = 0, 
                                            OpChType = "ALL", 
                                            legendPosition = c(1,0), 
                                            legendDirection = "horizontal", 
                                            legendJustification = c(0,1),
                                            nlfRange = NULL, 
                                            llfRange = NULL, 
                                            nlfAlpha = NULL){
  if (missing(zeta1)) zeta1 <- array(-3, dim = length(mu))
      
  if (!all(c(length(mu) == length(lambda), length(mu) == length(nu), length(mu) == length(zeta1))))
    stop("Parameters mu, lambda, nu and zeta1 have different lengths.")
  
  lesWghtDistr <- UtilLesionWeightsDistr(length(lesDistr), relWeights)
  lesDistr <- UtilLesionDistr(lesDistr)
  
  plotStep <- 0.01
  plotStep <- 0.1 # delete after debug
  zeta <- array(list(), dim = length(mu))
  for (i in 1:length(mu)) zeta[[i]] <- seq(from = zeta1[i], to = max(mu)+5, by = plotStep)
  
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
  aucROC <- rep(NA, length(mu));aucAFROC <- aucROC;aucwAFROC <- aucROC;aucFROC <- aucROC;lambdaP <- lambda
  nuP <- nu
  
  for (i in 1:length(mu)){
    if (mu[i] <= 0 ) stop("mu must be greater than zero")
    if (lambda[i] < 0 ) stop("lambda must be greater than zero")
    if (nu[i] < 0 ) stop("nu must be greater than zero")
    
    lambdaP[i] <- lambda[i] / mu[i]
    if (abs(nu[i] * mu[i]) <= 1e-6 ) nuP[i] <- 1e-6 else nuP[i] <- (1-exp(-nu[i] * mu[i]))
    FPF <- sapply(zeta[[i]], xROC, lambdaP = lambdaP[i]) # C++ function uses lambdaP
    TPF <- sapply(zeta[[i]], yROC, mu = mu[i], lambdaP = lambdaP[i], nuP = nuP[i], lesDistr = lesDistr)
    NLF <- sapply(zeta[[i]], RSM_xFROC, mu = mu[i], lambda = lambda[i])
    LLF <- sapply(zeta[[i]], RSM_yFROC, mu = mu[i], nu = nu[i])
    
    maxFPF <- xROC(-20, lambdaP[i])
    if( OpChType == "ALL" ||  OpChType == "ROC"){
      ROCPoints <- rbind(ROCPoints, data.frame(FPF = FPF, 
                                               TPF = TPF, 
                                               Treatment = as.character(i), 
                                               stringsAsFactors = FALSE))
      ROCDashes <- rbind(ROCDashes, data.frame(FPF = c(FPF[1], 1), 
                                               TPF = c(TPF[1], 1), 
                                               Treatment = as.character(i), 
                                               stringsAsFactors = FALSE))
      maxTPF <- yROC(-20, mu[i], lambdaP[i], nuP[i], lesDistr)
      AUC <- integrate(y_ROC_FPF, 0, maxFPF, mu = mu[i], lambdaP = lambdaP[i], nuP = nuP[i], lesDistr =lesDistr)$value
      aucROC[i] <- AUC + (1 + maxTPF) * (1 - maxFPF) / 2
    }
    
    if( OpChType == "ALL" ||  OpChType == "FROC"){
      FROCPoints <- rbind(FROCPoints, data.frame(NLF = NLF, 
                                                 LLF = LLF, 
                                                 Treatment = as.character(i), 
                                                 stringsAsFactors = FALSE))
      if (is.null(nlfAlpha)){
        maxNLF <- max(NLF)
        aucFROC[i] <- integrate(intFROC, 0, maxNLF, mu= mu[i], lambda = lambda[i], nu = nu[i])$value
      }else{
        maxNLF <- max(NLF)
        if (nlfAlpha <= maxNLF){
          aucFROC[i] <- integrate(intFROC, 0, nlfAlpha, mu= mu[i], lambda = lambda[i], nu = nu[i])$value
        }else{
          stop("nlfAlpha cannot be greater than the maximum of NLF.")
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
      maxLLF <- RSM_yFROC(-20, mu[i], nu[i])
      AUC <- integrate(y_AFROC_FPF, 0, maxFPF, mu = mu[i], lambda = lambda[i], nu = nu[i])$value
      aucAFROC[i] <- AUC + (1 + maxLLF) * (1 - maxFPF) / 2
    }
    
    if( OpChType == "ALL" ||  OpChType == "wAFROC"){
      wLLF <- sapply(zeta[[i]], ywAFROC, mu[i], nuP[i], lesDistr, lesWghtDistr)
      wAFROCPoints <- rbind(wAFROCPoints, data.frame(FPF = FPF, 
                                                     wLLF = wLLF, 
                                                     Treatment = as.character(i), 
                                                     stringsAsFactors = FALSE))
      wAFROCDashes <- rbind(wAFROCDashes, data.frame(FPF = c(FPF[1], 1), 
                                                     wLLF = c(wLLF[1], 1), 
                                                     Treatment = as.character(i), 
                                                     stringsAsFactors = FALSE))
      maxWLLF <- ywAFROC(-20, mu[i], nuP[i], lesDistr, lesWghtDistr) 
      AUC <- integrate(y_wAFROC_FPF, 0, maxFPF, mu = mu[i], lambda = lambda[i], nu = nu[i], lesDistr, lesWghtDistr)$value
      aucwAFROC[i] <- AUC + (1 + maxWLLF) * (1 - maxFPF) / 2
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
        theme(legend.position = legendPosition, legend.direction = legendDirection, legend.justification = c(1, 0)) 
    })
  }
  
  if( OpChType == "ALL" ||  OpChType == "FROC"){
    FROCPlot <- with(FROCPoints, {
      ggplot(data = FROCPoints) + 
        geom_line(aes(x = NLF, y = LLF, color = Treatment))  +       
        scale_x_continuous(expand = c(0, 0), limits = nlfRange) + 
        scale_y_continuous(expand = c(0, 0), limits = llfRange) + 
        theme(legend.position = legendPosition, legend.direction = legendDirection, legend.justification = c(1, 0)) 
    })
  }
  
  if( OpChType == "ALL" ||  OpChType == "AFROC"){
    AFROCPlot <- with(AFROCPoints, {
      ggplot(data = AFROCPoints) + 
        geom_line(aes(x = FPF, y = LLF , color = Treatment)) + 
        geom_line(data = AFROCDashes, aes(x = FPF, y = LLF, color = Treatment), linetype = 2) +       
        theme(legend.position = legendPosition, legend.direction = legendDirection, legend.justification = c(1, 0)) 
    }
    )
  }
  
  if( OpChType == "ALL" ||  OpChType == "wAFROC"){
    wAFROCPlot <- with(wAFROCPoints, {
      ggplot(data = wAFROCPoints) + 
        geom_line(aes(x = FPF, y = wLLF , color = Treatment)) + 
        geom_line(data = wAFROCDashes, aes(x = FPF, y = wLLF, color = Treatment), linetype = 2) +       
         theme(legend.position = legendPosition, legend.direction = legendDirection, legend.justification = c(1, 0)) 
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
        geom_line()  + theme(legend.position = legendPosition, legend.box = legendDirection) + 
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

is.wholenumber <- function(x)  round(x) == x
