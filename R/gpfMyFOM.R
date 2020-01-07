#' @importFrom stats approx

gpfMyFOM <- function(nl, ll, 
                     lesionVector, lesionID, 
                     lesionWeight, maxNL, 
                     maxLL, K1, K2, 
                     FOM, FPFValue = NULL) {
  if (!FOM %in% c("Wilcoxon", "HrAuc", "HrSe", "HrSp", "SongA1", 
                  "SongA2", "AFROC1", "AFROC", "wAFROC1", "wAFROC",
                  "FOM_AFROC1", "FOM_AFROC", "FOM_wAFROC1", "FOM_wAFROC", "FROC", # dpc 
                  "MaxLLF", "MaxNLF", "MaxNLFAllCases", "ExpTrnsfmSp", "ROI",
                  "ALROC", "PCL")){ # dpc 
    errMsg <- paste0(FOM, " is not an available figure of merit.")
    stop(errMsg)
  }
  fom <- NA
  # fom <- wJAFROC_dpc(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight)
  # fom <- FOM_wAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight)
  fom <- switch(FOM,
                "Wilcoxon" = TrapezoidalArea(nl, K1, ll, K2),
                "HrAuc" = HrAuc(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "HrSe" = HrSe(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "HrSp" = HrSp(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "SongA1" = SongA1(K1, K2, maxNL, maxLL, lesionVector, nl, ll),
                "SongA2" = SongA2(K1, K2, maxNL, maxLL, lesionVector, nl, ll),
                "AFROC1" = FOM_AFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL), # dpc
                "FOM_AFROC1" = FOM_AFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "AFROC" = FOM_AFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL), # dpc
                "FOM_AFROC" = FOM_AFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "wAFROC1" = FOM_wAFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight), # dpc
                "FOM_wAFROC1" = FOM_wAFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight),
                "wAFROC" = FOM_wAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight), # dpc
                "FOM_wAFROC" = FOM_wAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight),
                "FROC" = FROC(nl, ll, lesionID, lesionVector, K1, K2),
                "ALROC" = LrocFoms(nl, ll, FPFValue)$ALroc,
                "PCL" = LrocFoms(nl, ll, FPFValue)$PCL,
                "MaxLLF" = MaxLLF(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "MaxNLF" = MaxNLF(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "MaxNLFAllCases" = MaxNLFAllCases(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "ExpTrnsfmSp" = ExpTrnsfmSp(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "ROI" = ROI(K1, K2, maxNL, lesionVector, nl, ll)
  )
  return(fom)
} 


FROC <- function(nl, ll, lesionID, lesionVector, K1, K2){
  nl <- as.vector(nl[nl != -Inf])
  ll <- ll[is.finite(lesionID)]
  sumNumLL <- sum(lesionVector)
  frocFOM <- 0
  for (l in 1:length(nl)){
    frocFOM <- frocFOM + sum(nl[l] < ll) + 0.5 * sum(nl[l] == ll)
  }
  return(frocFOM / (K1 + K2) / sumNumLL)
}

# major changes 10/25/19
# replaced interpolation function (approx) with that shown below
# did search on available R functions - none satisfied my simple need 
# see ChkLrocFoms.xlsx
LrocFoms <- function (zk1, zk2Cl, FPFValue) {
  zk1 <- drop(zk1)
  zk2Cl <- drop(zk2Cl)
  lroc <- LrocOperatingPointsFromRatings( zk1, zk2Cl )
  # this is my simple interpolation code, 
  # in the spirit of empirical values, no smoothing assumptions
  # find values of x (lowerX and upperX) immediately surrounding x = FPFValue
  # then perform linear interpolation
  x <- FPFValue
  lowerX <- max(lroc$FPF[lroc$FPF < x]) # the ordering of the 4 inequalities is critical
  upperX <- min(lroc$FPF[lroc$FPF >= x])
  lowerY <- max(lroc$PCL[lroc$FPF < x])
  upperY <- min(lroc$PCL[lroc$FPF >= x])
  f <- (x - lowerX)/(upperX - lowerX)
  PCL <- f * (upperY - lowerY) + lowerY
  # end my code
  tempFpf <-c(lroc$FPF[lroc$FPF < FPFValue],FPFValue)
  tempPcl <-c(lroc$PCL[lroc$FPF < FPFValue],PCL)
  ALroc <- trapz(tempFpf, tempPcl) # computes trapezoidal area under LROC (0 to FPFValue)
  return (list ( 
    PCL = PCL,
    ALroc = ALroc
  ))  
}



Wilcoxon <- function (zk1, zk2)
{
  
  K1 = length(zk1)
  K2 = length(zk2)
  
  W <- 0
  for (k1 in 1:K1) {
    W <- W + sum(zk1[k1] < zk2)
    W <- W + 0.5 * sum(zk1[k1] == zk2)
  }
  W <- W/K1/K2
  
  return (W)
  
}

#
# copied from caTools; July 5th, 2018, after threatening email that CaTools and my package would be 
# archived, whatever that means; see email from Kurt Hornik <Kurt.Hornik@wu.ac.at> dated 7/5/2018
# 
trapz = function(x, y)
{ ### computes the integral of y with respect to x using trapezoidal integration.
  idx = 2:length(x)
  return (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}


comp_phidpc <- function(a,b)
{
  ret <- 0
  if (a < b) return (1) else if (a == b) return (0.5)
  return(ret)
}


wJAFROC_dpc <- function(nl, ll, n_lesions_per_image, max_cases, max_nl, max_ll, weights)
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  ret <-  0.0
  
  for (na in 1:max_cases[2]) {
    for (nn in 1:max_cases[1]) {
      for (nles in  1:n_lesions_per_image[na]) {
        fp  <- UNINITIALIZED
        for (nor_index in 1:max_nl)
          if (nl[nn, nor_index] > fp ) fp = nl[nn, nor_index] ## this captures the highest value on normal case nn
          ret  <-  ret + weights[na, nles] *  comp_phidpc( fp, ll[na, nles] ) ;
      }
    }
  }
  ret  <- ret / (max_cases[1] * max_cases[2])
  
  return (ret)
}

