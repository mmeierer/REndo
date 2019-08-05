#' @export
summary.rendo.ivreg <- function(object, ...){
  # Append diagnostics, then call ivreg's summary (the object's next class is ivreg)
  ans              <- NextMethod(object=object, diagnostics = TRUE, ...)
  return(ans)
}


#' @title Predict method for fitted Regression Models with Internal Instrumental Variables
#' @description Predicted values based on model objects fitted using the instrumental variables regression fitted
#' with IVs generated from the data.
#'
#' @param object Object of class inheriting from "rendo.ivreg"
#' @param newdata An optional data frame without any instrumental variables in which to look for variables with which to predict.
#' If omitted, the fitted values are returned.
#' @param ... ignored, for consistency with the generic function.
#'
#' @return
#' \code{predict.rendo.ivreg} produces a vector of predictions
#'
#' @seealso The model fitting functions \code{\link[REndo:hetErrorsIV]{hetErrorsIV}},
#' \code{\link[REndo:higherMomentsIV]{higherMomentsIV}}.
#'
#' @examples
#' data("dataHetIV")
#'
#' het <- hetErrorsIV(y~X1+X2+P|P|IIV(X1, X2),
#'                    data = dataHetIV)
#'
#' # returns the fitted values
#' predict(het)
#'
#' # using the data used for fitting also for predicting,
#' # correctly results in fitted values
#' all.equal(predict(het, dataHetIV), fitted(het)) # TRUE
#'
#' @importFrom stats delete.response model.frame model.matrix
#' @export
predict.rendo.ivreg <- function(object, newdata, ...){

  if(length(list(...)))
    warning("The arguments in ... are ignored.", call. = FALSE, immediate. = TRUE)

  if(missing(newdata)){
    return(fitted(object))
  }else{
    # main model formula without the IIVs
    mf <- model.frame(formula = delete.response(object$terms$regressors), newdata)
    X  <- model.matrix(object = delete.response(object$terms$regressors), mf)
    return(drop(X %*% coef(object)))
  }
}
