#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double BMNLLInner (double a, double b, NumericVector zetas, NumericVector fi, NumericVector ti){
  int l = fi.size();
  NumericVector Q(l);
  NumericVector P(l);
  double L = 0;
  for (int i = 0; i < l; i++){
    Q[i] = stats::pnorm_0(zetas[i + 1], 1, 0) - stats::pnorm_0(zetas[i], 1, 0);
    P[i] = stats::pnorm_0(b * zetas[i + 1] - a, 1, 0) - stats::pnorm_0(b * zetas[i] - a, 1, 0);
    L = L + log(Q[i]) * fi[i] + log(P[i]) * ti[i];
  }
  return L;
}
