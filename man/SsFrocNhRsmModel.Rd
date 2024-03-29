% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SsFrocSampleSize.R
\name{SsFrocNhRsmModel}
\alias{SsFrocNhRsmModel}
\title{Construct RSM NH model for FROC sample size estimation}
\usage{
SsFrocNhRsmModel(dataset, lesDistr)
}
\arguments{
\item{dataset}{The \strong{pilot} dataset.}

\item{lesDistr}{A 1D array containing the probability mass function of
number of lesions per diseased case in the \strong{pivotal FROC}
study.}
}
\value{
A list containing:
   \itemize{
   \item \code{mu} The RSM mu parameter of the NH model.
   \item \code{lambda} The RSM lambda parameter of the NH model.
   \item \code{nu} The RSM nu parameter of the NH model.
   \item \code{scaleFactor}, the factor by which the ROC effect size
   must by multiplied to get the wAFROC effect size.
   \item \code{R2} The squared correlation of the wAFROC-AUC to ROC-AUC fit.
   
}
}
\description{
Construct RSM NH model for FROC sample size estimation
}
