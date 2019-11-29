# JOHANN MOLINA Y ROBERTO MENDOZA
# ESTE CODIGO CONTIENE JALA LOS PAQUETES DE IMPUTACION PÑARA LOS TIPSO DE DATOS: CONTINUOS Y CATEGORICOS.


## Regresion del tipo LASSO

#' @title LASSO for regression
#'
#' @description metodo para variable continua
#' @param x matrix predictora
#' @param y vector de respuestA
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @import glmnet
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "lassoR")
#' }
lassoR <- function(x, y) {
  #Check because glmnet is just in Suggests
  if (!requireNamespace("glmnet", quietly = TRUE))
  {
    stop("Package \"glmnet\" needed for lassoR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"glmnet\")",
         call. = FALSE)
  }
  lamb <- glmnet::cv.glmnet(x, y)$lambda.min
  model <- glmnet::glmnet(x, y, lambda = lamb)
  return(model)
}

##### REGRESION TIPO BOSSTING


#' @title Boosting for regression
#'
#' @description boosting variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @import mboost
#' @importFrom stats AIC
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "glmboostR")
#' }
glmboostR <- function(x, y) {
  #Check because glmnet is just in Suggests
  if (!requireNamespace("mboost", quietly = TRUE))
  {
    stop("Package \"mboost\" needed for glmboostR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"mboost\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  t.model <- mboost::glmboost(y~., data = impdata,
                              control = mboost::boost_control(mstop = 1000))
  ms <- mboost::mstop(AIC(t.model, method = "corrected"))
  model <- t.model[ms]
  return(model)
}

## METODO DE COMPONENTES PRINCIPALES

#' @title Principle component regression for imputation
#'
#' @description método de componentes principales para la imputacion
#' @param x matrix predictora
#' @param y vector de respuesta
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#'
# #' @importFrom pls pcr
#' @seealso \code{\link[pls]{pcr}}
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "pcrR")
#' }
pcrR <- function(x, y) {
  #Check because pls is just in Suggests
  if (!requireNamespace("pls", quietly = TRUE))
  {
    stop("Package \"pls\" needed for pcrR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"pls\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- pls::pcr(y~., data = impdata, ncomp = 2)
  return(model)
}

### IMPUTACION DE MINIMOS CUADRADOS PARCIALES


#' @title Partial Least Square regression for imputation
#'
#' @description minimos cuadra parciales para imputar
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @seealso \code{\link[pls]{plsr}}
#'
# #' @importFrom pls plsr
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "plsR")
#' }
plsR <- function(x, y) {
  #Check because pls is just in Suggests
  if (!requireNamespace("pls", quietly = TRUE))
  {
    stop("Package \"pls\" needed for plsR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"pls\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- pls::plsr(y~., data = impdata, ncomp = 2)
  return(model)
}

#### iMPUTACION DE DATA CATEGORICA POR CLASIFICACIÓN DE ARBOLES


#' @title classification tree for imputation
#'
#' @description classification tree for imputation
#'
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @importFrom rpart rpart
#' @seealso \code{\link[rpart]{rpart}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "rpartC")
#' }
rpartC <- function(x, y) {

  #Check because rpart is just in Suggests
  if (!requireNamespace("rpart", quietly = TRUE))
  {
    stop("Package \"rpart\" needed for rpartC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"rpart\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- rpart::rpart(y~., data = impdata, method = "class")
  return(model)
}



#### Imputacion de data categórica por Discriminación

#' @title regularised LDA method for imputation
#'
#' @description LDA method for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @importFrom rda rda
# #' @importFrom rda rda.cv
#' @seealso \code{\link[rda]{rda}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "rdaC")
#' }
rdaC <- function(x, y) {

  #Check because rda is just in Suggests
  if (!requireNamespace("rda", quietly = TRUE))
  {
    stop("Package \"rda\" needed for rdaC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"rda\")",
         call. = FALSE)
  }
  y <- as.numeric(y)
  x <- t(x)
  fit <- rda::rda(x, y)
  cv <- rda::rda.cv(fit, x, y)
  index <- arrayInd(which.min(cv$cv.err), dim(cv$cv.err))
  cv.alpha <- cv$alpha[index[1]]
  cv.delta <- cv$delta[index[2]]
  return(list(x = x , y = y, fit = fit, alpha = cv.alpha, delta = cv.delta))
}


###### IMPUTACION POR REGRESION LOGISTICA



#' @title logistic regression with lasso for imputation
#'
#' @description regresion logistica y Lasso en la imputación
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @import glmnet
#' @seealso \code{\link[glmnet]{cv.glmnet}} and \code{\link[glmnet]{glmnet}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "lassoC")
#' }
lassoC <- function(x, y) {

  #Check because glmnet is just in Suggests
  if (!requireNamespace("glmnet", quietly = TRUE))
  {
    stop("Package \"glmnet\" needed for lassoC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"glmnet\")",
         call. = FALSE)
  }
  lamb <- glmnet::cv.glmnet(x, y, family = "binomial", type.measure = "class")$lambda.min
  model <- glmnet::glmnet(x, y, family = "binomial", lambda = lamb)
  return(model)
}

##### mETODO DE IMPUTACION POR BSSTING TREE

#' @title boosting tree for imputation
#'
#' @description boosting tree for imputation
#' @param x matriz predicotra
#' @param y vector de respuesta
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' and the best.iter for gbm model.
# #' @importFrom gbm gbm
# #' @importFrom gbm gbm.perf
#' @seealso \code{\link[gbm]{gbm}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "gbmC")
#' }
gbmC <- function(x, y) {

  #Check because gbm is just in Suggests
  if (!requireNamespace("gbm", quietly = TRUE))
  {
    stop("Package \"gbm\" needed for gbmC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"gbm\")",
         call. = FALSE)
  }
  y <- as.numeric(y)
  impdata <- data.frame(cbind(y, x))
  gbm1 <- gbm::gbm(y~., data = impdata, distribution = "adaboost", n.trees = 1000,
                   shrinkage = 0.05, interaction.depth = 1, bag.fraction = 0.5,
                   train.fraction = 0.5, cv.folds = 10)
  best.iter <- gbm::gbm.perf(gbm1, method = "cv")
  return(list(model = gbm1, best = best.iter))
}
