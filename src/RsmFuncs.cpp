#include <Rcpp.h>
#include "CommonFuncs.h"
using namespace Rcpp;

/* This function clashes with with std::erf(double) in math.h (since C++11)
   See <https://en.cppreference.com/w/cpp/numeric/math/erf>
   It does not appear to be called by any of the *.R *.Rmd *.Rd or *.cpp files
   This function could be renamed and call the std:erf, once all CRAN platforms
   are using C++11.
// [[Rcpp::export]]
double erf(double x){
  return 2 * R::pnorm(sqrt(2.0) * x, 0, 1, 1, 0) - 1;
}
*/

// [[Rcpp::export]]
NumericVector erfVect(NumericVector x){
  int l = x.size();
  NumericVector erfx(l);
  for (int il = 0; il < l; il ++){
    erfx[il] = 2 * R::pnorm(sqrt(2.0) * x[il], 0, 1, 1, 0) - 1;
  }
  return erfx;
}

/* Candidate for Rcpp::export, if required */
double erfcpp(double x){
  return 2 * R::pnorm(sqrt(2.0) * x, 0, 1, 1, 0) - 1;
}

// [[Rcpp::export]]
double xROC(double zeta, double lambdaP){
  return 1 - exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2.0)));
}

// [[Rcpp::export]]
NumericVector xROCVect(NumericVector zeta, double lambdaP){
  int l = zeta.size();
  NumericVector FPF(l);
  for (int il = 0; il < l; il ++){
    FPF[il] = 1 - exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta[il] / sqrt(2.0)));
  }
  return FPF;
}

// [[Rcpp::export]]
double yROC(double zeta, double mu, double lambdaP, double nuP, NumericMatrix lesDistr){
  NumericVector fl = lesDistr(_, 1) / sum(lesDistr(_, 1));
  double TPF = 0;
  
  for (int i = 0; i < lesDistr.nrow(); i++){
    TPF = TPF + fl[i] * (1 - pow(1 - nuP/2 + nuP/2  * erfcpp( (zeta - mu) / sqrt(2.0) ) , lesDistr(i, 0)) * exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2.0))));
  }
  return TPF;
}

// [[Rcpp::export]]
NumericVector yROCVect(NumericVector zeta, double mu, double lambdaP, double nuP, NumericMatrix lesDistr){
  NumericVector fl = lesDistr(_, 1) / sum(lesDistr(_, 1));
  int l = zeta.size();
  NumericVector TPF(l);
  
  for (int il = 0; il < l; il ++){
    for (int i = 0; i < lesDistr.nrow(); i++){
      TPF[il] = TPF[il] + fl[i] * (1 - pow(1 - nuP/2 + nuP/2  * erfcpp( (zeta[il] - mu) / sqrt(2.0) ) , lesDistr(i, 0)) * exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta[il] / sqrt(2.0))));
    }
  }
  
  return TPF;
}

// [[Rcpp::export]]
NumericVector yROCVectNuP(double zeta, double mu, double lambdaP, NumericVector nuP, NumericMatrix lesDistr){
  NumericVector fl = lesDistr(_, 1) / sum(lesDistr(_, 1));
  int l = nuP.size();
  NumericVector TPF(l);
  
  for (int il = 0; il < l; il ++){
    for (int i = 0; i < lesDistr.nrow(); i++){
      TPF[il] = TPF[il] + fl[i] * (1 - pow(1 - nuP[il]/2 + nuP[il]/2  * erfcpp( (zeta - mu) / sqrt(2.0) ) , lesDistr(i, 0)) * exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2.0))));
    }
  }
  
  return TPF;
}

// // [[Rcpp::export]]
// double DiffAucSmMinusAucCbm(double mu, double lambdaP, double nuP, NumericMatrix lesDistr, double AUCCbm){
//   return SmAuc(mu, lambdaP, nuP, lesDistr) - AUCCbm;
// }

// // [[Rcpp::export]]
// NumericVector DiffAucSmMinusAucCbmVectMu(NumericVector mu, double lambdaP, double nuP, NumericMatrix lesDistr, double AUCCbm){
//   int l = mu.size();
//   NumericVector aucDiff(l);
//   for (int il = 0; il < l; il++){
//     aucDiff[il] = SmAuc(mu[il], lambdaP, nuP, lesDistr) - AUCCbm;
//   }
//   return aucDiff;
// }

// // [[Rcpp::export]]
// NumericVector DiffAucSmMinusAucCbmVectNuP(double mu, double lambdaP, NumericVector nuP, NumericMatrix lesDistr, double AUCCbm){
//   int l = nuP.size();
//   NumericVector aucDiff(l);
//   for (int il = 0; il < l; il++){
//     aucDiff[il] = SmAuc(mu, lambdaP, nuP[il], lesDistr) - AUCCbm;
//   }
//   return aucDiff;
// }

// [[Rcpp::export]]
double RsmInner(double mu, double lambdaP, double nuP, NumericMatrix lesDistr, NumericVector zeta, NumericVector fb, NumericVector tb){
  NumericVector FPF = xROCVect(zeta, lambdaP);
  NumericVector TPF = yROCVect(zeta, mu, lambdaP, nuP, lesDistr);
  int l = fb.size();
  NumericVector FPFBin(l), TPFBin(l);
  double L = 0;
  FPFBin[0] = 1 - FPF[0]; if (FPFBin[0] < 1e-15) FPFBin[0] = 1e-15;
  TPFBin[0] = 1 - TPF[0]; if (TPFBin[0] < 1e-15) TPFBin[0] = 1e-15;
  FPFBin[l - 1] = FPF[l - 2]; if (FPFBin[l - 1] < 1e-15) FPFBin[l - 1] = 1e-15;
  TPFBin[l - 1] = TPF[l - 2]; if (TPFBin[l - 1] < 1e-15) TPFBin[l - 1] = 1e-15;
  L = L + fb[0] * log(FPFBin[0]) + tb[0] * log(TPFBin[0]) + fb[l - 1] * log(FPFBin[l - 1]) + tb[l - 1] * log(TPFBin[l - 1]);
  for (int il = 1; il < l - 1; il++){
    FPFBin[il] = FPF[il - 1] - FPF[il];
    TPFBin[il] = TPF[il - 1] - TPF[il];
    if (FPFBin[il] < 1e-15) FPFBin[il] = 1e-15;
    if (TPFBin[il] < 1e-15) TPFBin[il] = 1e-15;
    L = L + fb[il] * log(FPFBin[il]) + tb[il] * log(TPFBin[il]);
  }
  return -L;
}

// // [[Rcpp::export]]
// double RsmInnerDebug(double mu, double lambdaP, double nuP, NumericMatrix lesDistr, NumericVector zeta, NumericVector fb, NumericVector tb){
//   NumericVector FPF = xROCVect(zeta, lambdaP);
//   NumericVector TPF = yROCVect(zeta, mu, lambdaP, nuP, lesDistr);
//   int l = fb.size();
//   NumericVector FPFBin(l), TPFBin(l);
//   double L = 0;
//   FPFBin[0] = 1 - FPF[0]; TPFBin[0] = 1 - TPF[0];
//   FPFBin[l - 1] = FPF[l - 2]; TPFBin[l - 1] = TPF[l - 2];
//   L = L + fb[0] * log(FPFBin[0]) + tb[0] * log(TPFBin[0]) + fb[l - 1] * log(FPFBin[l - 1]) + tb[l - 1] * log(TPFBin[l - 1]);
//   for (int il = 1; il < l - 1; il++){
//     FPFBin[il] = FPF[il - 1] - FPF[il];
//     TPFBin[il] = TPF[il - 1] - TPF[il];
//     if (FPFBin[il] < 1e-15) FPFBin[il] = 1e-15;
//     if (TPFBin[il] < 1e-15) TPFBin[il] = 1e-15;
//     L = L + fb[il] * log(FPFBin[il]) + tb[il] * log(TPFBin[il]);
//   }
//   
//   return -L;
// }

// [[Rcpp::export]]
NumericVector intROC(NumericVector FPF, double mu, double lambdaP, double nuP, NumericMatrix lesDistr){
  int l = FPF.size();
  NumericVector zeta(l);
  double temp;
  for (int il = 0; il < l; il++){
    temp = 1 / lambdaP * log(1 - FPF[il]) + 1;
    if (temp <= 0){
      zeta[il] = -20;
    }else{
      zeta[il] = R::qnorm(temp, 0, 1, 1, 0);
    }
  }
  
  // Rcpp::Rcout << "FPF is now " << FPF << std::endl;
  // Rcpp::Rcout << "temp is now " << temp << std::endl;
  // Rcpp::Rcout << "zeta is now " << zeta << std::endl;
  return yROCVect(zeta, mu, lambdaP, nuP, lesDistr);
}
