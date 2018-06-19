#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double ForwardValue(double value, double valueLower, double valueUpper) 
{
  return log(-log((value - valueLower)/(valueUpper - valueLower)));
}

// [[Rcpp::export]]
double InverseValue(double valueFwd, double valueLower, double valueUpper) {
  return valueLower + (valueUpper - valueLower) * (exp(-exp(valueFwd)));
}

// // [[Rcpp::export]]
// NumericVector ForwardZetas(NumericVector zetas){
//   int l = zetas.size();
//   NumericVector zetasFwd(l);
//   zetasFwd[0] = zetas[0];
//   if (l > 1){
//     for (int i = 1; i < l; i++) zetasFwd[i] = log(zetas[i] - zetas[i - 1]);
//   }
//   return zetasFwd;
// }
// 
// // [[Rcpp::export]]
// NumericVector InverseZetas(NumericVector zetasFwd){
//   int l = zetasFwd.size();
//   NumericVector zetas(l);
//   zetas[0] = zetasFwd[0];
//   if (l > 1) {
//     for (int i = 1; i < l; i++) zetas[i] = exp(zetasFwd[i]) + zetas[i - 1];
//   }
//   return zetas;
// }

