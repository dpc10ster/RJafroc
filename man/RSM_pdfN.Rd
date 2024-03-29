% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsmFormulae.R
\name{RSM_pdfN}
\alias{RSM_pdfN}
\title{RSM predicted ROC-rating pdf for non-diseased cases}
\usage{
RSM_pdfN(z, lambda)
}
\arguments{
\item{z}{The z-vector at which to evaluate the pdf.}

\item{lambda}{The scalar RSM  lambda parameter.}
}
\value{
pdf, density function for non-diseased cases
}
\description{
RSM predicted ROC-rating pdf for non-diseased cases
}
\examples{
RSM_pdfN(c(1,2),1)

}
