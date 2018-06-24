#' @export
residuals.rendo.copulacorrection.discrete <- function(object, ...){
  object$resid
}

#' @export
coef.rendo.copulacorrection.discrete <- function(object, ...){
  return(object$coefficients)
}


#' @export
print.rendo.copulacorrection.discrete <- function(x, ...){
  print(summary(object=x, ...))
  invisible(x)
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
coef.summary.rendo.copulacorrection.discrete <- function(object, ...){
 return(object$coefficients)
}

#' Print output from summary function on copulaCorrectionDiscrete output
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.copulacorrection.discrete <- function(x, digits=5, #signif.stars = getOption("show.signif.stars"),
                                                          ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = F, ...)
  invisible(x)
}
