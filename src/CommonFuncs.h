#include <Rcpp.h>
using namespace Rcpp;

double yROC(double zeta, double mu, double lambdaP, double nuP, NumericMatrix lesionDistribution);

double xROC(double zeta, double lambdaP);

double SmAuc(double mu, double lambdaP, double nuP, NumericMatrix lesionDistribution);

double ywAFROC (double zeta, double mu, double nuP,
                NumericMatrix lesDistr, NumericMatrix lesWghtDistr);
  