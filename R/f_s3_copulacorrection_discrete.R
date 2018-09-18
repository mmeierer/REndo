
#' @export
#' @importFrom stats fitted
fitted.rendo.copulacorrection.discrete <- function(object, ...){
  return(fitted(object$fitted.lm, ...))
}

#' @export
residuals.rendo.copulacorrection.discrete <- function(object, ...){
  return(residuals(object$fitted.lm, ...))
}

#' @export
#' @importFrom stats nobs
nobs.rendo.copulacorrection.discrete <- function(object, ...){
  return(nobs(object$fitted.lm, ...))
}

#' @export
coef.rendo.copulacorrection.discrete <- function(object, ...){
  return(object$coefficients)
}

#' @export
#' @importFrom stats confint
confint.rendo.copulacorrection.discrete <- function(object, parm, level = 0.95, ...) {
  return(confint(object$fitted.lm, parm = parm, level = level, ...))
}


#' @export
#' @importFrom stats logLik
logLik.rendo.copulacorrection.discrete <- function(object, REML = FALSE, ...){
  return(logLik(object$fitted.lm, REML=REML, ...))
}


#' @export
#' @importFrom stats case.names
case.names.rendo.copulacorrection.discrete <- function(object, full = FALSE, ...){
  return(case.names(object$fitted.lm, full=full, ...))
}

#' @export
labels.rendo.copulacorrection.discrete <- function(object, ...){
  return(labels(object$fitted.lm, ...))
}

#' @export
print.rendo.copulacorrection.discrete <- function(x, ...){
  print(summary(object=x, ...))
  invisible(x)
}

#' @export
#' @importFrom stats vcov
vcov.rendo.copulacorrection.discrete <- function(object, ...){
  return(vcov(object$fitted.lm, ...))
}

#' Calculate summary statistics for result object from copulaCorrectionDiscrete
#' @export
summary.rendo.copulacorrection.discrete <- function(object, ...){

  coefficients           <- cbind(object$coefficients, object$CI)
  colnames(coefficients) <- c("Estimate",colnames(object$CI))

  ans <- list(call = object$call, coefficients = coefficients)
  class(ans) <- "summary.rendo.copulacorrection.discrete"
  return(ans)
}

#' Extract coefficient table from summary
#' @export
coef.summary.rendo.copulacorrection.discrete <- function(object, ...){
 return(object$coefficients)
}


#' Print output from summary function on copulaCorrectionDiscrete output
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.copulacorrection.discrete <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                                          ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = F, signif.stars = signif.stars,...)
  invisible(x)
}
