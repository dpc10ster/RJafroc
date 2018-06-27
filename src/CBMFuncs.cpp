#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double Qz (double zeta1, double zeta2){
  return stats::pnorm_0(zeta2, 1, 0) - stats::pnorm_0(zeta1, 1, 0);
}

// [[Rcpp::export]]
double Pz (double mu, double alpha, double zeta1, double zeta2){
  return (1 - alpha) * (stats::pnorm_0(zeta2, 1, 0) - stats::pnorm_0(zeta1, 1, 0)) + alpha * (stats::pnorm_1(zeta2, mu, 1, 0) - stats::pnorm_1(zeta1, mu, 1, 0));
}

// [[Rcpp::export]]
double CBMNLLInner (double mu, double alpha, NumericVector zetas, NumericVector fi, NumericVector ti){
  int l = fi.size();
  NumericVector Q(l);
  NumericVector P(l);
  double L = 0;
  for (int i = 0; i < l; i++){
    Q[i] = Qz(zetas[i], zetas[i + 1]);
    P[i] = Pz(mu, alpha, zetas[i], zetas[i + 1]);
    L = L + log(Q[i]) * fi[i] + log(P[i]) * ti[i];
  }
  return L;
}
