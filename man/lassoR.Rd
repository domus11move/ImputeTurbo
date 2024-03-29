% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prueba2.R
\name{lassoR}
\alias{lassoR}
\title{LASSO for regression}
\usage{
lassoR(x, y)
}
\arguments{
\item{x}{matrix predictora}

\item{y}{vector de respuestA}
}
\value{
a model object that can be used by the \code{\link[imputeR]{impute}} function
}
\description{
metodo para variable continua
}
\examples{
data(parkinson)
missdata <- SimIm(parkinson, 0.1)
\donttest{
impdata <- impute(missdata, lmFun = "lassoR")
}
}
