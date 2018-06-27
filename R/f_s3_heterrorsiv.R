#' @export
print.rendo.heterrorsiv <- function(x, ...){
  print(summary(x))
  return(invisible(x))
}

#' @export
nobs.rendo.heterrorsiv <- function(object,...){
  return(NROW(object$residuals))
}

#' @export
coef.rendo.heterrorsiv <- function(object, ...){
  return(object$coefficients)
}

#' @export
summary.rendo.heterrorsiv <- function(object, ...){
  ans <- object[c("call","coefficients","jtest","ftest", "nobs")]
  class(ans) <- "summary.rendo.heterrorsiv"
  return(ans)
}

#' @export
coef.summary.rendo.heterrorsiv <- function(object, ...){
  return(object$coefficients)
}

#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.heterrorsiv <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                            ...){

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Coefficients:\n")
  printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = T, signif.stars = signif.stars,...)

  cat("\nOveridentification Test:\n")
  print(x$jtest)
  cat("\nPartial F-test Statistics for Weak IV Detection:\n")
  print(x$ftest)
  cat("\nNumber of Observations:", x$nobs, "\n")

  return(invisible(x))
}


