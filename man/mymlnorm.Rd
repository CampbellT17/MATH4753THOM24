% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymlnorm.R
\name{mymlnorm}
\alias{mymlnorm}
\title{mymlnorm}
\usage{
mymlnorm(x, mu, sig, lwd, labcex)
}
\arguments{
\item{x}{Sample Vector}

\item{mu}{Mean}

\item{sig}{Standard Deviation}

\item{lwd}{line width}

\item{labcex}{label character expansion factor}
}
\value{
This function makes a maximum likelihood function for two parameters
}
\description{
mymlnorm
}
\examples{
\dontrun{mymlnorm(x=c(10,12,13,15,12,11,10),mu=seq(10,15,length=1000),
sig=seq(0.1,4,length=1000),lwd=2,labcex=1)}

}
