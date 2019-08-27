#' @importFrom stats approx

gpfMyFOM <- function(nl, ll, 
                     lesionVector, lesionID, 
                     lesionWeight, maxNL, 
                     maxLL, K1, K2, 
                     FOM, FPFValue = NULL) {
  if (!FOM %in% c("Wilcoxon", "HrAuc", "HrSe", "HrSp", "SongA1", 
                  "SongA2", "AFROC1", "AFROC", "wAFROC1", "wAFROC",
                  "JAFROC1", "JAFROC", "wJAFROC1", "wJAFROC", "FROC", # dpc 
                  "MaxLLF", "MaxNLF", "MaxNLFAllCases", "ExpTrnsfmSp", "ROI",
                  "ALROC", "PCL")){ # dpc 
    errMsg <- paste0(FOM, " is not an available figure of merit.")
    stop(errMsg)
  }
  fom <- NA
  # fom <- wJAFROC_dpc(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight)
  # fom <- wJAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight)
  fom <- switch(FOM,
                "Wilcoxon" = TrapezoidalArea(nl, K1, ll, K2),
                "HrAuc" = HrAuc(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "HrSe" = HrSe(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "HrSp" = HrSp(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "SongA1" = SongA1(K1, K2, maxNL, maxLL, lesionVector, nl, ll),
                "SongA2" = SongA2(K1, K2, maxNL, maxLL, lesionVector, nl, ll),
                "AFROC1" = JAFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL), # dpc
                "JAFROC1" = JAFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "AFROC" = JAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL), # dpc
                "JAFROC" = JAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL),
                "wAFROC1" = wJAFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight), # dpc
                "wJAFROC1" = wJAFROC1(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight),
                "wAFROC" = wJAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight), # dpc
                "wJAFROC" = wJAFROC(nl, ll, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight),
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



LrocFoms <- function (zk1, zk2Cl, FPFValue) {
  zk1 <- drop(zk1)
  zk2Cl <- drop(zk2Cl)
  zk1 <- zk1[zk1 != -Inf]
  lroc <- LrocOperatingPointsFromRatings( zk1, zk2Cl )
  PCL <- (approx(lroc$FPF, lroc$PCL, xout = FPFValue, ties = min))$y # computes PCL @ FPFValue
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

