## ----setup, include = FALSE----------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)
  library(ggplot2)
  library(mvtnorm)

## ----echo=FALSE----------------------------------------------------------
Transform2ab <- function (d_a, c){
  b <- -(c+1)/(c-1)
  a <- (d_a/sqrt(2))*sqrt(1+b^2)
  return( list(
    a = a,
    b = b
  ) )
}

GetLimits <- function (d_a, c)
{
  if (c < 0.0) {
    LL <-  d_a/4/c*sqrt(1+c^2)
    UL <-  10.0
  }
  if (c == 0.0) 
  {
    LL  <-  -10.0
    UL <-  10.0
  }
  if (c > 0.0) 
  {
    LL <-  -10.0
    UL <-  d_a/4/c*sqrt(1+c^2)
  }
  return( list(
    LL = LL,
    UL = UL
  ) )
}

Heaviside <- function (c)
{
  if (c < 0.0) rval <-  0.0 else rval  <-  1.0
  return (rval)
}

FalsePositiveFraction <- function (vc, d_a, c)
{
  
  arg1 <-  -(1-c)*vc-d_a/2*sqrt(1+c^2)
  arg2 <-  -(1-c)*vc+d_a/2/c*sqrt(1+c^2)
  
  rval = pnorm (arg1) + pnorm (arg2) - Heaviside (c)
  
  return (rval)
}

TruePositiveFraction <- function (vc, d_a, c)
{
  arg1 = -(1+c)*vc+d_a/2*sqrt(1+c^2)
  arg2 = -(1+c)*vc+d_a/2/c*sqrt(1+c^2)
  
  rval = pnorm (arg1) + pnorm (arg2) - Heaviside (c)
  
  return (rval)
}

# TontData.R
c1 <- array(dim=c(5,2));d_a1 <- array(dim=c(5,2))

c1[1,1] <- -0.132280361
d_a1[1,1] <- 1.197239295
c1[2,1] <- -0.086965135	
d_a1[2,1] <- 1.771175637
c1[3,1] <- -0.144441852	
d_a1[3,1] <- 1.481934876
c1[4,1] <- 0.080460161	
d_a1[4,1] <- 1.513756914
c1[5,1] <- 0.222558765	
d_a1[5,1] <- 1.740157217

c1[1,2] <- -0.081742476	
d_a1[1,2] <- 0.628125133
c1[2,2] <- 0.049764485	
d_a1[2,2] <- 0.973878556
c1[3,2] <- -0.132612623	
d_a1[3,2] <- 1.155870661
c1[4,2] <- 0.118222633	
d_a1[4,2] <- 1.620175716
c1[5,2] <- 0.078103299	
d_a1[5,2] <- 0.89288159

d_a1 <- t(d_a1)
c1 <- t(c1)

trapz = function(x, y) 
{ ### computes the integral of y with respect to x using trapezoidal integration. 
  idx = 2:length(x)
  return (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}

## ------------------------------------------------------------------------
npts <-  10000
for (i in 1:2) {
  for (j in 1:5) {
    C  <-  c1[i,j]
    da  <-  d_a1[i,j]
    ret <- GetLimits(da,C)
    LL <- ret$LL;UL <- ret$UL
    vc  <-  seq (LL, UL, length.out = npts)
    TPF  <-  TruePositiveFraction (vc, da, C)
    FPF <- FalsePositiveFraction (vc, da, C)
    FPF <- rev(FPF);TPF <- rev(TPF)
    df2 <- data.frame(FPF = FPF, TPF = TPF)
    # do integral numerically
    numAuc <- trapz(FPF, TPF)
    # Implement Eqn. 36 from Metz-Pan paper 
    rho <- -(1-C^2)/(1+C^2);sigma <- rbind(c(1, rho), c(rho, 1))
    lower <- rep(-Inf,2);upper <- c(-da/sqrt(2),0)
    aucProproc <- pnorm(da/sqrt(2)) + 2 * pmvnorm(lower, upper, sigma = sigma)
    aucProproc <-  as.numeric(aucProproc)
    cat("i = ", i,"j = ", j,"C = ", C, ", da = ", da, "aucProproc =", aucProproc, "Norm. Diff. = ", (aucProproc-numAuc)/aucProproc,"\n")
  }
}

