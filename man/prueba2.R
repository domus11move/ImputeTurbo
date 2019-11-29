\name{prueba2}
\alias{prueba2}
\title{General Imputation Framework in R}
\usage{
  prueba2(missdata, lmFun = NULL, cFun = NULL, ini = NULL)
}
\arguments{
  \item{missdata}{data matrix with missing values encoded as NA.}

  \item{lmFun}{the variable selection method for continuous data.}

  \item{cFun}{the variable selection method for categorical data.}

  \item{ini}{the method for initilisation. It is a length one character if
    missdata contains only one type of variables only. For continous only data,
    ini can be "mean" (mean imputation), "median" (median imputation) or "random"
    (random guess), the default is "mean". For categorical data, it can be
    either "majority" or "random", the default is "majority". If missdata is
    mixed of continuous and categorical data, then ini has to be a vector of two
    characters, with the first element indicating the method for continous
    variables and the other element for categorical variables, and the default
    is c("mean", "majority".)}


}
\description{
  Impute missing values under the general framework in R
}
\details{
  This function can impute several kinds of data, including continuous-only
  data, categorical-only data and mixed-type data. Many methods can be used, including
  regularisation method like LASSO and ridge regression, tree-based model and dimensionality
  reduction method like PCA and PLS.
}
\examples{
  data(parkinson)
  # introduce 10\% random missing values into the parkinson data
  missdata <- SimIm(parkinson, 0.1)
  # impute the missing values by LASSO
  \donttest{
    impdata <- impute(missdata, lmFun = "lassoR")
    # calculate the normalised RMSE for the imputation
    Rmse(impdata$imp, missdata, parkinson, norm = TRUE)
  }
}
}
