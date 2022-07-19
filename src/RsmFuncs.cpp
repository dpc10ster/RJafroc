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
double yROC(double zeta, double mu, double lambdaP, double nuP, NumericVector lesDistr){
  double TPF = 0;
  int maxLes = lesDistr.size();
    
  for (int i = 0; i < maxLes; i++){
    TPF = TPF + lesDistr[i] * (1 - 
      pow(1 - nuP/2 + nuP/2  * erfcpp( (zeta - mu) / sqrt(2.0) ) , 
          (i+1)) * exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2.0))));
  }
  return TPF;
}

// [[Rcpp::export]]
NumericVector yROCVect(NumericVector zeta, double mu, double lambdaP, double nuP, NumericVector lesDistr){
  int l = zeta.size();
  NumericVector TPF(l);
  int maxLes= lesDistr.size();
    
  for (int il = 0; il < l; il ++){
    for (int i = 0; i < maxLes; i++){
      TPF[il] = TPF[il] + lesDistr[i] * (1 - pow(1 - nuP/2 + nuP/2  * erfcpp( (zeta[il] - mu) / sqrt(2.0) ) , (i+1)) * exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta[il] / sqrt(2.0))));
    }
  }
  
  return TPF;
}



// [[Rcpp::export]]
double RsmInner(double mu, double lambdaP, double nuP, NumericVector lesDistr, NumericVector zeta, NumericVector fb, NumericVector tb){
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


// [[Rcpp::export]]
NumericVector y_ROC_FPF(NumericVector FPF, double mu, double lambdaP, double nuP, NumericVector lesDistr){
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






// [[Rcpp::export]]
double ywAFROC (double zeta, double mu, double nuP,
                NumericVector lesDistr, NumericMatrix lesWghtDistr){
  double wLLF = 0; 
  int maxLes = lesDistr.size();
  for (int row = 0;row < maxLes;row++) { 
    int nLesPerCase = row+1;
    double wLLFrow = 0;
    for (int col = 0; col < nLesPerCase; col++){
      wLLFrow += lesWghtDistr(row, col+1) * (col+1) *
        R::dbinom(double(col+1), double(nLesPerCase), nuP, 0) * (1 - R::pnorm(zeta - mu, 0, 1, 1, 0));
// above line was tricky; note difference in col indexing from R code: lesWghtDistr[row, col+1] * col * dbinom(col, nLesPerCase, nuP) * (1 - pnorm(zeta - mu))
// this is due to zero-based C indexing vs. 1 based r indexing
    }
    wLLF += lesDistr[row] * wLLFrow;
  }
  return wLLF;
}

