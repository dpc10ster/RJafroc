#include <Rcpp.h>
using namespace Rcpp;

double yROC(double zeta, double mu, double lambda, double nu, NumericMatrix lesionDistribution);

double xROC(double zeta, double lambda);

double SmAuc(double mu, double lambda, double nu, NumericMatrix lesionDistribution);

double ywAFROC (double zeta, double mu, double nu,
                NumericMatrix lesDistr, NumericMatrix lesWghtDistr);

double erfcpp(double x);

NumericVector erfVect(NumericVector x);

NumericVector xROCVect(NumericVector zeta, double lambda);

NumericVector yROCVect(NumericVector zeta, double mu, double lambda, double nu, NumericMatrix lesDistr);

NumericVector erfVect(NumericVector x);
