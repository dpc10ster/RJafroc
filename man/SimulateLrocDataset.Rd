% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimulateLrocDataset.R
\name{SimulateLrocDataset}
\alias{SimulateLrocDataset}
\title{Simulates an uncorrelated LROC dataset using the RSM}
\usage{
SimulateLrocDataset(mu, lambda, nu, zeta1, I, J, K1, K2, lesionVector)
}
\arguments{
\item{mu}{The mu parameter of the RSM}

\item{lambda}{The RSM lambda parameter}

\item{nu}{The RSM nu parameter}

\item{zeta1}{The lowest reporting threshold}

\item{I}{The number of treatments}

\item{J}{The number of readers}

\item{K1}{The number of non-diseased cases}

\item{K2}{The number of diseased cases}

\item{lesionVector}{A K2 length array containing the numbers of lesions per diseased case}
}
\value{
An LROC dataset.
}
\description{
Simulates an uncorrelated LROC dataset for specified numbers of
   readers and treatments
}
\details{
See book chapters on the Radiological Search Model (RSM) for details.
   The approach is to first simulate an FROC dataset and then convert it to an Lroc
   dataset. The correlations between FROC ratings on the same case are assumed to be zero.
}
\examples{
  set.seed(1)
  K1 <- 5; K2 <- 5; mu <- 2; lambda <- 1; lesionVector <- rep(1, 5); nu <- 0.8; zeta1 <- -3
  frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, lesionVector)
  lrocData <- DfFroc2Lroc(frocData)

}
\references{
Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
\url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
}
