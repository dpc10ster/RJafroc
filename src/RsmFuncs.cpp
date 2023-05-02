#include <Rcpp.h>
#include "CommonFuncs.h"
using namespace Rcpp;

// [[Rcpp::export]]
double erf_cpp(double x){
  return 2 * R::pnorm(sqrt(2.0) * x, 0, 1, 1, 0) - 1;
}

// [[Rcpp::export]]
NumericVector erf_vect_cpp(NumericVector x){
  int l = x.size();
  NumericVector erfx(l);
  for (int il = 0; il < l; il ++){
    // pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
    erfx[il] = 2 * R::pnorm(sqrt(2.0) * x[il], 0, 1, 1, 0) - 1;
  }
  return erfx;
}

// [[Rcpp::export]]
double xROC_cpp(double zeta, double lambda){
  return 1 - exp( (-lambda / 2) + 0.5 * lambda * erf_cpp(zeta / sqrt(2.0)));
}

// [[Rcpp::export]]
NumericVector xROC_vect_cpp(NumericVector zeta, double lambda){
  int l = zeta.size();
  NumericVector FPF(l);
  for (int il = 0; il < l; il ++){
    FPF[il] = 1 - exp( (-lambda / 2) + 0.5 * lambda * erf_cpp(zeta[il] / sqrt(2.0)));
  }
  return FPF;
}

// [[Rcpp::export]]
double yROC_cpp(double zeta, double mu, double lambda, double nu, NumericVector lesDistr){
  double TPF = 0;
  int maxLes = lesDistr.size();
    
  for (int i = 0; i < maxLes; i++){
    TPF = TPF + lesDistr[i] * (1 - 
      pow(1 - nu/2 + nu/2  * erf_cpp( (zeta - mu) / sqrt(2.0) ) , 
          (i+1)) * exp( (-lambda / 2) + 0.5 * lambda * erf_cpp(zeta / sqrt(2.0))));
  }
  return TPF;
}

// [[Rcpp::export]]
NumericVector yROC_vect_cpp(NumericVector zeta, double mu, double lambda, double nu, NumericVector lesDistr){
  int l = zeta.size();
  NumericVector TPF(l);
  int maxLes= lesDistr.size();
    
  for (int il = 0; il < l; il ++){
    for (int i = 0; i < maxLes; i++){
      TPF[il] = TPF[il] + lesDistr[i] * (1 - pow(1 - nu/2 + nu/2  * erf_cpp( (zeta[il] - mu) / sqrt(2.0) ) , (i+1)) * exp( (-lambda / 2) + 0.5 * lambda * erf_cpp(zeta[il] / sqrt(2.0))));
    }
  }
  
  return TPF;
}



// [[Rcpp::export]]
double RsmInner(double mu, double lambda, double nu, NumericVector lesDistr, NumericVector zeta, NumericVector fb, NumericVector tb){
  NumericVector FPF = xROC_vect_cpp(zeta, lambda);
  NumericVector TPF = yROC_vect_cpp(zeta, mu, lambda, nu, lesDistr);
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
NumericVector y_ROC_FPF_cpp(NumericVector FPF, double mu, double lambda, double nu, NumericVector lesDistr){
  int l = FPF.size();
  NumericVector zeta(l);
  double temp;
  for (int il = 0; il < l; il++){
    temp = 1 / lambda * log(1 - FPF[il]) + 1;
    if (temp <= 0){
      zeta[il] = -20;
    }else{
      zeta[il] = R::qnorm(temp, 0, 1, 1, 0);
    }
  }
  
  // Rcpp::Rcout << "FPF is now " << FPF << std::endl;
  // Rcpp::Rcout << "temp is now " << temp << std::endl;
  // Rcpp::Rcout << "zeta is now " << zeta << std::endl;
  return yROC_vect_cpp(zeta, mu, lambda, nu, lesDistr);
}






// [[Rcpp::export]]
double RSM_wLLF_cpp (double zeta, double mu, double nu, NumericVector f_L, NumericMatrix W){
  double wLLF = 0; 
  int maxLes = f_L.size();
  for (int row = 0;row < maxLes;row++) { 
    int L = row+1;
    double wLLF_L = 0;
    for (int col = 0; col < L; col++){
      wLLF_L += W(row, col+1) * (col+1) * R::dbinom(double(col+1), double(L), nu, 0);
// above line was tricky
// Note difference in col indexing from following R code 
// W[row, col+1] * col * dbinom(col, L, nu)
// this is due to zero-based C indexing vs. 1-based r indexing
    }
    wLLF += f_L[row] * wLLF_L;
  }
  wLLF = wLLF * R::pnorm(mu - zeta, 0, 1, 1, 0);
  
  return wLLF;
}

