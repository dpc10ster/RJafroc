#include <Rcpp.h>
using namespace Rcpp;

double yROC(double zeta, double mu, double lambdaP, double nuP, NumericMatrix lesionDistribution);

double xROC(double zeta, double lambdaP);

double SmAuc(double mu, double lambdaP, double nuP, NumericMatrix lesionDistribution);

double ywAFROC (double zeta, double mu, double nuP,
                NumericMatrix lesDistr, NumericMatrix lesWghtDistr);

double erfcpp(double x);

NumericVector erfVect(NumericVector x);

NumericVector xROCVect(NumericVector zeta, double lambdaP);

NumericVector yROCVect(NumericVector zeta, double mu, double lambdaP, double nuP, NumericMatrix lesDistr);

NumericVector erfVect(NumericVector x);
