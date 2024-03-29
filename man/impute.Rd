% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prueba1.R
\name{impute}
\alias{impute}
\title{Estrucutra de programación}
\usage{
impute(
  missdata,
  lmFun = NULL,
  cFun = NULL,
  ini = NULL,
  maxiter = 100,
  verbose = TRUE,
  conv = TRUE
)
}
\arguments{
\item{missdata}{matriz de datos que contiene datos faltantes y reemplazados con NA.}

\item{lmFun}{metodo de seleccion para variable continua}

\item{cFun}{varibale que indica el metodo asociado a data categórica}

\item{ini}{indica el procedimiento del método (mean imputation), "median" (median imputation) or "random"
(random guess), the default is "mean". para categórica puede ser "majority" or "random",
Si la base de datos es mixta se especifica en uin vector los metodos ( mean, majority) como default}

\item{maxiter}{máximo numerod e iteraciones}

\item{verbose}{parametro lógico}

\item{conv}{logical, para True muestra en detalle la convergencia}
}
\value{
if conv = FALSE, it returns a completed data matrix with no
missing values; if TRUE, it rrturns a list of components including:

\item{imp}{genra la data sin valores faltantes}
\item{conv}{estado de convergencia}
}
\description{
mertodo de imputación
}
\details{
metodos de impitacion para dito de datos continuo o categorico
}
\examples{
data(parkinson)
# introduce 10\% valores aleatorios en la matriz
missdata <- SimIm(parkinson, 0.1)
# impute the missing values by LASSO
\donttest{
impdata <- impute(missdata, lmFun = "lassoR")
# calcula el NRMSE
Rmse(impdata$imp, missdata, parkinson, norm = TRUE)
}
}
\seealso{
\code{\link{SimIm}} simulacion de valores faltantes.
}
