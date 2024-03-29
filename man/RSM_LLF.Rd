% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsmFormulae.R
\name{RSM_LLF}
\alias{RSM_LLF}
\title{RSM predicted FROC ordinate}
\usage{
RSM_LLF(z, mu, nu)
}
\arguments{
\item{z}{The z-vector value at which to evaluate the FROC ordinate.}

\item{mu}{The scalar RSM mu parameter.}

\item{nu}{The scalar RSM nu prime parameter.}
}
\value{
LLF, the ordinate of the FROC curve
}
\description{
RSM predicted FROC ordinate
}
\examples{
RSM_LLF(c(1,2),1,0.5)

}
