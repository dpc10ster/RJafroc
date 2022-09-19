// // [[Rcpp::depends(RcppEigen)]]
// // [[Rcpp::depends(RcppNumerical)]]
// #include <RcppNumerical.h>
// #include "CommonFuncs.h"
// using namespace Numer;
// 
// // P(0.3 < X < 0.8), X ~ Beta(a, b)
// class SmAucIntFunc: public Func
// {
// private:
//   double mu;
//   double lambda;
//   double nu;
//   Rcpp::NumericMatrix lesionDistribution;
// public:
//   SmAucIntFunc(double mu_, double lambdaP_, double nuP_, Rcpp::NumericMatrix lesionDistribution_) : mu(mu_), lambda(lambdaP_), nu(nuP_), lesionDistribution(lesionDistribution_){}
//   
//   double operator()(const double& FPF) const
//   {
//     double temp = 1 / lambda * log(1 - FPF) + 1;
//     double zeta;
//     if (temp <= 0){
//       zeta = -20;
//     }else{
//       zeta = R::qnorm(temp, 0, 1, 1, 0);
//     }
//     // Rcpp::Rcout << "FPF is now " << FPF << std::endl;
//     // Rcpp::Rcout << "temp is now " << temp << std::endl;
//     // Rcpp::Rcout << "zeta is now " << zeta << std::endl;
//     return yROC(zeta, mu, lambda, nu, lesionDistribution);
//   }
// };
// 
// // [[Rcpp::export]]
// double SmAuc(double mu, double lambda, double nu, NumericMatrix lesionDistribution)
// {
//   SmAucIntFunc f(mu, lambda, nu, lesionDistribution);
//   double maxFPF = xROC(R_NegInf, lambda);
//   double maxTPF = yROC(R_NegInf, mu, lambda, nu, lesionDistribution);
//   // Rcpp::Rcout << "maxFPF is now " << maxFPF << std::endl;
//   // Rcpp::Rcout << "maxTPF is now " << maxTPF << std::endl;
//   double err_est;
//   int err_code;
//   double AUC = integrate(f, 0, maxFPF, err_est, err_code, 100, 0.0001220703, 0.0001220703);
//   // Rcpp::Rcout << "int AUC is now " << AUC << std::endl;
//   // return Rcpp::List::create(
//   //   Rcpp::Named("value") = AUC + (1 + maxTPF) * (1 - maxFPF) / 2,
//   //   Rcpp::Named("error_estimate") = err_est,
//   //   Rcpp::Named("error_code") = err_code
//   // );
//   return AUC + (1 + maxTPF) * (1 - maxFPF) / 2;
// }