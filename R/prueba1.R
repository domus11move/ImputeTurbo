## curso: mineria de datos

## Profesor: Luis Benites

## Alumnos: Johan Molina y Roberto Mendoza Matos


#' Estrucutra de programación
#'
#' @description mertodo de imputación
#' @details metodos de impitacion para dito de datos continuo o categorico
#'
#' @param missdata matriz de datos que contiene datos faltantes y reemplazados con NA.
#' @param lmFun metodo de seleccion para variable continua
#' @param cFun varibale que indica el metodo asociado a data categórica
#' @param ini indica el procedimiento del método (mean imputation), "median" (median imputation) or "random"
#'  (random guess), the default is "mean". para categórica puede ser "majority" or "random",
#'  Si la base de datos es mixta se especifica en uin vector los metodos ( mean, majority) como default
#' @param maxiter máximo numerod e iteraciones
#' @param verbose parametro lógico
#' @param conv logical, para True muestra en detalle la convergencia
#'
#' @return if conv = FALSE, it returns a completed data matrix with no
#' missing values; if TRUE, it rrturns a list of components including:
#'
#' \item{imp}{genra la data sin valores faltantes}
#' \item{conv}{estado de convergencia}
#'
#' @seealso \code{\link{SimIm}} simulacion de valores faltantes.
#' @importFrom stats na.omit predict
#' @export
#' @examples
#' data(parkinson)
#' # introduce 10% valores aleatorios en la matriz
#' missdata <- SimIm(parkinson, 0.1)
#' # impute the missing values by LASSO
#' \donttest{
#' impdata <- impute(missdata, lmFun = "lassoR")
#' # calcula el NRMSE
#' Rmse(impdata$imp, missdata, parkinson, norm = TRUE)
#' }
impute <- function(missdata, lmFun = NULL, cFun = NULL, ini = NULL,
                   maxiter = 100, verbose = TRUE, conv = TRUE) {
  Type <- Detect(missdata)
  if(all(Type == "numeric")) {
    task <- 1
    if(is.null(ini)) {
      ini = "mean"
    } else {
      stopifnot(ini %in% c("mean", "median", "random"))
    }
    names(task) <- "Regression"
  } else if(all(Type == "character")) {
    task <- 2
    if(is.null(ini)) {
      ini = "majority"
    } else {
      stopifnot(ini %in% c("majority", "random"))
    }
    names(task) <- 'Classification'
  } else {
    task <- 3
    if(is.null(ini)) {
      ini = c("mean", "majority")
    } else {
      if (length(ini) !=2) {
        stop ("Data are mixed-type, provide two initial methods")
      } else {
        if (!(ini[1] %in% c("mean", "median", "random")) ||
            !(ini[2] %in% c("majority", "random"))) {
          stop ("ini has to be a two length character vector, check the help
                about this argument and make sure you provide valid names")
        }
      }
    }
    names(task) <- "Regression and Classification mixed"
  }
  if (verbose) {
    cat("Imputation task is:", names(task), "\n")
  }

  if (task == 1) {
    stopifnot(!is.null(lmFun))
    lmFUN <- match.fun(lmFun)
    cFUN <- NULL
  } else if (task == 2) {
    stopifnot(!is.null(cFun))
    lmFUN <- NULL
    cFUN <- match.fun(cFun)
  } else {
    stopifnot(!is.null(cFun), !is.null(lmFun))
    lmFUN <- match.fun(lmFun)
    cFUN <- match.fun(cFun)
  }
  n <- nrow(missdata)
  p <- ncol(missdata)

  ## remueve valores perdidos
  if (any(apply(is.na(missdata), 2, sum) == n)) {
    ind <- which(apply(is.na(missdata), 2, sum) == n)
    missdata <- missdata[, -ind]
    p <- ncol(missdata)
    cat('removed variable(s)', ind,
        'due to the missingness of all entries\n')
  }
  ## perform initial guess on miss
  ximp <- missdata
  if (task == 1) {
    ximp <- guess(ximp, type = ini)
  } else if (task == 2) {
    ximp <- guess(ximp, type = ini)
  } else {
    for (i in seq_along(Type)) {
      if (Type[i] == "numeric") {
        ximp[, i] <- guess(missdata[, i], type = ini[1])
      } else {
        if (ini[2] == "majority") {
          ximp[, i] <- as.numeric(major(missdata[, i]))
        } else {
          ximp[, i] <- guess(missdata[, i], type = "random")
        }
      }
    }
  }

  # extrae el patron de missing values
  NAloc <- is.na(missdata)
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  sort.j <- order(noNAvar, decreasing = TRUE) # indices of increasing amount of NA in vars
  sort.noNAvar <- noNAvar[sort.j]

  # ready for output
  Ximp <- vector('list', maxiter)

  iter <- 0
  k <- length(unique(Type))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)

  # setup convergence container w.r.t. task types
  if (k == 1) {
    if (unique(Type) == 'numeric'){
      names(convNew) <- c('numeric')
    } else {
      names(convNew) <- c('character')
    }
    Converg <- rep(NA, maxiter)
  } else {
    names(convNew) <- c('numeric', 'character')
    Converg <- matrix(NA, nrow = maxiter, ncol = 2)
  }

  # stopping function for the loop
  stopCriterion <- function(Type, convNew, convOld, iter, maxiter) {
    k <- length(unique(Type))
    if (k == 1) {
      (convNew < convOld) & (iter < maxiter)
    } else {
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
    }
  }

  while (stopCriterion(Type, convNew, convOld, iter, maxiter)) {
    if (iter != 0){
      convOld <- convNew
    }

    if (verbose) {
      if (task == 1) {
        cat("iteration", iter + 1,  "using", lmFun,
            "in progress...")
      } else if (task == 2) {
        cat("iteration", iter + 1,  "using", cFun,
            "in progress...")
      } else {
        cat("iteration", iter + 1,  "using", lmFun, "and", cFun,
            "in progress...")
      }
    }

    ximp.old <- ximp
    for (s in 1:p) {
      varInd <- sort.j[s]
      if (noNAvar[[varInd]] != 0) {
        obsi <- !NAloc[, varInd] # which i's are observed
        misi <- NAloc[, varInd] # which i's are missing
        obsY <- ximp[obsi, varInd] # training response
        obsX <- as.matrix(ximp[obsi, seq(1, p)[-varInd]]) # training variables
        colnames(obsX) <- paste0("x", 1:ncol(obsX))
        misX <- as.matrix(ximp[misi, seq(1, p)[-varInd]]) # predictors

        # as.df can be overwritten if some known functions that require data.frame
        # by their preidction function are called.
        if (task == 2 && (cFun %in%
                          c("rpartC", "gbmC"))) {
          misX <- as.data.frame(misX)
        } else if (task == 3) {
          if ((cFun %in% c("rpartC", "gbmC")))
            misX <- as.data.frame(misX)
        }

        colnames(misX) <- colnames(obsX)
        typeY <- Type[varInd]

        ## train model (with automated variable selction) on observed data
        if (typeY == "numeric") {
          Miss <- lmFUN(x = obsX, y = obsY)
          if (lmFun %in% c("pcrR", "plsR")) {
            misY <- predict(Miss, misX, ncomp = 2, type = "response")
          } else if (lmFun == "glmboostR") {
            misY <- predict(Miss, newdata = as.data.frame(misX))
          } else {
            misY <- predict(Miss, misX)
          }
        } else {
          obsY2 <- factor(obsY)
          summarY <- summary(obsY2)
          if (length(summarY) == 1) {
            # if all values of obsY is the same then using model would be
            # unnecessary
            misY <- factor(rep(names(summarY), sum(misi)))
          } else {
            if (cFun %in% c("rdaC")) {
              obsY <- factor(obsY)
            }
            Miss <- cFUN(x = obsX, y = obsY)
            if (cFun %in% c("lassoC")) {
              misY <- ifelse(predict(Miss, misX, type = "response") < 0.5, 0, 1)
            } else if (cFun %in% c("rpartC")) {
              try <- predict(Miss, misX, type = "prob")
              misY <- ifelse(try[, 1] > try[, 2], 0, 1)
            } else if (cFun == "rdaC") {
              try <- predict(Miss$fit, Miss$x, Miss$y, xnew = t(misX), alpha = Miss$alpha,
                             delta = Miss$delta, type = "posterior")
              misY <- ifelse(try[, 1] > try[, 2], 0, 1)
            } else if (cFun == "gbmC") {
              try <- predict(Miss$model, misX, Miss$best, type = "response")
              misY <- ifelse(try < 0.5, 0, 1)
            } else {
              misY <- predict(Miss, misX)
            }
          }
        }
        ## reemplaza los valores perdidos con la prediccion
        ximp[misi, varInd] <- misY
      }
    }
    if (verbose) {
      cat('done!\n')
    }
    iter <- iter + 1
    Ximp[[iter]] <- ximp
    # pasos iterativos
    # This implementation is really smart and is derived from the brillian MissForest
    # package source.
    t.co2 <- 1
    for (t.type in names(convNew)) {
      t.ind <- which(Type == t.type)
      if (t.type == "numeric") {
        convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, t.ind])^2)/sum(ximp[, t.ind]^2)
      } else {
        dist <- sum(as.character(as.matrix(ximp[, t.ind])) !=
                      as.character(as.matrix(ximp.old[, t.ind])))
        convNew[t.co2] <- dist/(n * sum(Type == "character"))
      }
      t.co2 <- t.co2 + 1
    }
    if (conv) {
      if (k == 1) {
        Converg[iter] <- convNew
      } else {
        Converg[iter, ] <- convNew
      }
    }
    if (verbose) {
      if (task == 3) {
        cat("Difference after iteration", iter,  "is", convNew[1],
            "and", convNew[2], "\n")
      } else {
        cat("Difference after iteration", iter,  "is", convNew, "\n")
      }
    }
  }

  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    ximp = Ximp[[iter]]
  } else {
    ximp = Ximp[[iter - 1]]
  }
  if (conv) {
    Converg <- na.omit(Converg)
    return(list(imp = ximp, conv = Converg))
  }
  return(ximp)
}
