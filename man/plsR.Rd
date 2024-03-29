% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prueba2.R
\name{plsR}
\alias{plsR}
\title{Partial Least Square regression for imputation}
\usage{
plsR(x, y)
}
\arguments{
\item{x}{predictor matrix}

\item{y}{response vector}
}
\value{
a model object that can be used by the \code{\link[imputeR]{impute}} function
}
\description{
minimos cuadra parciales para imputar
}
\examples{
data(parkinson)
missdata <- SimIm(parkinson, 0.1)
\donttest{
impdata <- impute(missdata, lmFun = "plsR")
}
}
\seealso{
\code{\link[pls]{plsr}}
}
