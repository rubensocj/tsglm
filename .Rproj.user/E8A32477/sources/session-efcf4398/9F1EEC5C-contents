#' Generalized linear model for univariate time series forecasting
#'
#' @param y time series object of class \code{ts}
#' @param lags vector of lag indexes to be computed. Use positive
#' (negative) values for get past (future) lags
#' @param xreg optional numerical vector or matrix of external regressors
#' @param offset optional offset
#'
#' @return \code{glm} time series model
#' @export
tsglm <- function(y, lags, xreg = NULL, offset = NULL) {

  # lag matrix
  df_lag <- lag_matrix(y, lags)
  df_lag <- head(df_lag, length(y))

  # check external regressor and set xreg column name
  if(!is.null(xreg)) {
    if(is.null(colnames(xreg))) {
      x_cn <- "x"
    } else {
      x_cn <- colnames(xreg)
    }
  } else {
    x_cn <- NULL
  }

  # check offset and set offset column names
  of_cn <- of_f <- NULL
  if(!is.null(offset)) {
    of_cn <- "offset"
    of_f <- "+offset(offset)"
  }

  # data
  df <- data.frame(cbind(df_lag, xreg, offset))
  colnames(df) <- c(colnames(df_lag), x_cn, of_cn)
  df <- na.exclude(df)

  # formula
  f <- paste0("y~", paste0(colnames(df)[-c(1, ncol(df))], collapse = "+"), of_f)

  # model
  mod <- MASS::glm.nb(f, data = df)

  # additional features
  mod$covariates <- colnames(df)
  mod$y <- y
  mod$lags <- lags

  return(mod)
}

#' Recursive time series forecast for generalized linear model
#'
#' @param object Generalized linear model for univariate time series forecasting
#' @param h forecast horizon
#' @param xreg optional numerical vector or matrix of external regressors
#'
#' @return numeric vector with forecasts
#' @export
predict.tsglm <- function(object, h, xreg = NULL) {

  if(!is.null(xreg)) xreg <- data.frame(xreg)

  pred <- numeric(h)
  covariates <- object$covariates
  y <- object$y
  lags <- object$lags
  maxlag <- max(lags)
  flag <- rev(tail(y, n = maxlag))

  for(i in 1:h) {
    newdata <- flag[lags]
    newdata <- data.frame(matrix(newdata, nrow = 1))
    if(!is.null(xreg)) newdata <- data.frame(cbind(newdata, xreg[i, ]))
    colnames(newdata) <- covariates[-1]

    pred[i] <- predict(object = object, newdata = newdata, type = "response")
    flag <- c(pred[i], flag[-maxlag])
  }
  return(pred)
}

#' Lagged matrix for univariate time series
#'
#' @param y time series object of class \code{ts}
#' @param lags vector of lag indexes to be computed. Use positive
#' (negative) values for get past (future) lags
#' @param label string with a name for the variable
#'
#' @return \code{data.frame}
#' @export
lag_matrix <- function(y, lags, label = "y") {
  cll <- paste0("cbind(y, ", paste0("stats::lag(y, -", lags,")", collapse = ", "), ")")
  df <- eval(parse(text = cll))
  colnames(df) <- c(label, paste0(label, "_",lags))
  return(df)
}
