#' for testing cpp code TBA!!
#' 
#' 
#' @description test cpp code, temporary function, move to tests as they are done and finally delete this function
#' 
#' 
#' @export
#' 
#' 

testCpp <- function() {
  mu <- 2
  nu <- 0.9
  zeta <- seq(from = -3, to = max(mu)+5, by = 0.2)
  
  ds <- RJafroc::dataset06 # fail Magnus; dataset11 # fail Dobbins
  LD <- UtilLesDistr(ds)
  Freq <- LD$Freq
  lesID <- LD$lesID
  W <- UtilLesWghtsDS(ds)
  
  for (i in 1:length(zeta)) {
    ret1 <- RSM_wLLF_cpp  (zeta[i], mu = mu, nu = nu, f_L = Freq, W = W)
    ret2 <- RSM_wLLF_R(zeta[i], mu = mu, nu = nu, lesDistr = Freq, relWeights = 0)
    testthat::expect_equal(ret1, ret2)
  }
  
}
