#' @export
print.rendo.heterrorsiv <- function(x, ...){
  print(summary(x))
  return(invisible(x))
}

#' @export
#' @importFrom stats nobs
nobs.rendo.heterrorsiv <- function(object,...){
  return(nobs(object$res.ivreg))
}

#' @export
#' @importFrom stats coef
coef.rendo.heterrorsiv <- function(object, ...){
  return(object$coefficients)
}

#' @export
#' @importFrom stats residuals
residuals.rendo.heterrorsiv <- function(object, ...){
  return(residuals(object$res.ivreg))
}

#' @export
#' @importFrom stats fitted
fitted.rendo.heterrorsiv <- function(object, ...){
  return(fitted(object$res.ivreg))
}

#' @export
#' @importFrom stats confint
confint.rendo.heterrorsiv <- function(object, parm, level = 0.95, ...) {
  return(confint(object$res.ivreg, parm = parm, level = level, ...))
}

#' @export
#' @importFrom stats case.names
case.names.rendo.heterrorsiv <- function(object, ...){
  return(names(fitted(object$res.ivreg)))
}

#' @export
#' @importFrom Formula as.Formula
labels.rendo.heterrorsiv <- function(object, ...){
  return(labels(terms(formula(as.Formula(object$res.ivreg), rhs=1, lhs=1))))
}


#' @export
summary.rendo.heterrorsiv <- function(object, ...){
  ans <- object[c("call", "res.ivreg")]
  ans$sum          <- summary(object$res.ivreg)
  ans$sum$call     <- ans$call
  ans$coefficients <- coef(ans$sum)
  class(ans)       <- "summary.rendo.heterrorsiv"
  return(ans)
}

#' @export
#' @importFrom stats vcov
vcov.rendo.heterrorsiv <- function(object, ...){
  return(vcov(object$res.ivreg))
}

#' @export
coef.summary.rendo.heterrorsiv <- function(object, ...){
  return(object$coefficients)
}

#' @importFrom stats printCoefmat nobs
#' @export
print.summary.rendo.heterrorsiv <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                            ...){
  # cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  # cat("Coefficients:\n")
  # printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = T, signif.stars = signif.stars,...)

  # cat("\nOveridentification Test:\n")
  # print(x$jtest)
  # cat("\nPartial F-test Statistics for Weak IV Detection:\n")
  # print(x$ftest)
  print(x$sum, digits=digits, signif.stars=signif.stars, ...)
  cat("Number of Observations:", nobs(x$res.ivreg), "\n")

  return(invisible(x))
}


