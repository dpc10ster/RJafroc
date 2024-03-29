% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StOldCode.R
\name{StOldCode}
\alias{StOldCode}
\title{Performs DBM or OR significance testing for factorial dataset Old Code}
\usage{
StOldCode(
  dataset,
  FOM,
  FPFValue = 0.2,
  alpha = 0.05,
  method = "DBM",
  covEstMethod = "jackknife",
  nBoots = 200,
  analysisOption = "ALL"
)
}
\arguments{
\item{dataset}{The dataset to be analyzed.}

\item{FOM}{The figure of merit}

\item{FPFValue}{Only needed for \code{LROC} data.}

\item{alpha}{The significance level of the test; the default is 0.05}

\item{method}{The testing method to be used: \code{"DBM"} (the default),
representing the Dorfman-Berbaum-Metz method or \code{"OR"}, representing
the Obuchowski-Rockette method.}

\item{covEstMethod}{The covariance matrix estimation method}

\item{nBoots}{The number of bootstraps (defaults to 200)}

\item{analysisOption}{Determines which factors are regarded as random vs.
fixed:}
}
\description{
(DBM) or Obuchowski-Rockette (OR) significance testing, for
  specified dataset.
}
\keyword{internal}
