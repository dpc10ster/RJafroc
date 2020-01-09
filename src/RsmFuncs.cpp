#include <Rcpp.h>
#include "CommonFuncs.h"
using namespace Rcpp;

// [[Rcpp::export]]
double erfcpp(double x){
  return 2 * R::pnorm(sqrt(2.0) * x, 0, 1, 1, 0) - 1;
}

// [[Rcpp::export]]
NumericVector erfVect(NumericVector x){
  int l = x.size();
  NumericVector erfx(l);
  for (int il = 0; il < l; il ++){
    erfx[il] = 2 * R::pnorm(sqrt(2.0) * x[il], 0, 1, 1, 0) - 1;
  }
  return erfx;
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
  NumericVector fl = lesDistr(_, 1) / sum(lesDistr(_, 1)); // unnecessary normalization to unit-sum weights, but OK
  double TPF = 0;
  
  for (int i = 0; i < lesDistr.nrow(); i++){
    // lesDistr(i, 0) contains the nunber of lesions per case
    // fl[i] is the fraction of cases with lesDistr(i, 0) lesions
    // no changes made to code, just convincing myself that it is correct
    TPF = TPF + fl[i] * (1 - 
      pow(1 - nuP/2 + nuP/2  * erfcpp( (zeta - mu) / sqrt(2.0) ) , 
          lesDistr(i, 0)) * exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2.0))));
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



// // [[Rcpp::export]]
// NumericVector yROCVectNuP(double zeta, double mu, double lambdaP, NumericVector nuP, NumericMatrix lesDistr){
//   NumericVector fl = lesDistr(_, 1) / sum(lesDistr(_, 1));
//   int l = nuP.size();
//   NumericVector TPF(l);
//   
//   for (int il = 0; il < l; il ++){
//     for (int i = 0; i < lesDistr.nrow(); i++){
//       TPF[il] = TPF[il] + fl[i] * (1 - pow(1 - nuP[il]/2 + nuP[il]/2  * erfcpp( (zeta - mu) / sqrt(2.0) ) , lesDistr(i, 0)) * exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2.0))));
//     }
//   }
//   
//   return TPF;
// }

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


// double factrl (const int n) {
//   static NumericVector a(171);
//   static bool init = true;
//   if (init) {
//     init = false;
//     a[0] = 1;
//     for (int i = 1; i <171; i++) a[i] = i*a[i-1];
//   }
//   if (n < 0 || n > 170) throw("factrl out of range");
//   return a[n];
// }
// 
// 
// double BinCombFunction(int N, int n) {
//   return factrl(N)/factrl(N-n)/factrl(n);
// }
// 
// 
// double dbinom(int lesLocalizations, int trialSize, double prob) {
//   return BinCombFunction(trialSize, lesLocalizations) * 
//     pow(prob, lesLocalizations)* pow(1-prob, trialSize - lesLocalizations);
// }



// // [[Rcpp::export]]
// NumericVector ywAFROC (NumericVector zeta, double mu, double nuP,
//                        NumericMatrix lesDistr, NumericMatrix lesWghtDistr){
//   int len = zeta.size();
//   NumericVector wLLF(len);
//   NumericVector fl = lesDistr(_, 1) / sum(lesDistr(_, 1));
//   for (int l1 = 0;l1 < len; l1++) {
//     // wLLF[l1] = 0;
//     for (int l = 0;l < lesDistr.nrow();l++) {
//       int nLesPerCase = lesDistr(l, 0);
//       double wLLFTmp = 0;
//       for (int LL = 0; LL < nLesPerCase; LL++){
//         wLLFTmp = wLLFTmp + lesWghtDistr(l, LL+1) * LL *
//           dbinom(LL, nLesPerCase, nuP) * (1 - R::pnorm(zeta[l1] - mu, 0, 1, 1, 0));
//       }
//       wLLF[l1] = wLLF[l1] +  fl[l] * wLLFTmp;
//     }
//   }
//   return wLLF;
// }

// [[Rcpp::export]]
double ywAFROC (double zeta, double mu, double nuP,
                NumericMatrix lesDistr, NumericMatrix lesWghtDistr){
  NumericVector fl = lesDistr(_, 1) / sum(lesDistr(_, 1));
  double wLLF = 0; 
  for (int row = 0;row < lesDistr.nrow();row++) { 
    int nLesPerCase = lesDistr(row, 0);
    double wLLFrow = 0;
    for (int col = 0; col < nLesPerCase; col++){
      wLLFrow += lesWghtDistr(row, col+1) * (col+1) *
        R::dbinom(double(col+1), double(nLesPerCase), nuP, 0) * (1 - R::pnorm(zeta - mu, 0, 1, 1, 0));
// above line was tricky; note difference in col indexing from R code: lesWghtDistr[row, col+1] * col * dbinom(col, nLesPerCase, nuP) * (1 - pnorm(zeta - mu))
// this is due to zero-based C indexing vs. 1 based r indexing
    }
    wLLF += fl[row] * wLLFrow;
  }
  return wLLF;
}

