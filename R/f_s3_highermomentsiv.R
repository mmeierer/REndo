#' @export
#' @importFrom stats coef
coef.rendo.highermomentsiv <- function(object, ...){
  return(object$coefficients)
}

#' @export
#' @importFrom stats confint
confint.rendo.highermomentsiv <- function(object, parm, level = 0.95, ...) {
  return(confint(object$res.ivreg, parm = parm, level = level, ...))
}

#' @export
#' @importFrom stats fitted
fitted.rendo.highermomentsiv <- function(object, ...){
  return(fitted(object$res.ivreg))
}

#' @export
#' @importFrom stats nobs
nobs.rendo.rendo.highermomentsiv <- function(object,...){
  return(nobs(object$res.ivreg))
}

#' @export
print.rendo.highermomentsiv <- function(x, ...){
  print(summary(x))
  return(invisible(x))
}

#' @export
#' @importFrom stats residuals
residuals.rendo.highermomentsiv <- function(object, ...){
  return(residuals(object$res.ivreg))
}

#' @export
#' @importFrom stats vcov
vcov.rendo.highermomentsiv <- function(object, ...){
  return(vcov(object$res.ivreg))
}


#' @export
summary.rendo.highermomentsiv <- function(object, ...){
  ans              <- object[c("call", "res.ivreg")]
  ans$sum          <- summary(object$res.ivreg)
  ans$coefficients <- coef(ans$sum)
  class(ans)       <- "summary.rendo.highermomentsiv"
  return(ans)
}

#' @export
#' @importFrom stats coef
coef.summary.rendo.highermomentsiv <- function(object, ...){
  return(object$coefficients)
}

#' @importFrom stats printCoefmat nobs
#' @export
print.summary.rendo.highermomentsiv <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                            ...){
  # cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  #
  # cat("Coefficients:\n")
  # printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = T, signif.stars = signif.stars,...)
  print(x$sum, digits=digits, signif.stars=signif.stars, ...)
  cat("Number of Observations:", nobs(x$res.ivreg), "\n")

  return(invisible(x))
}

