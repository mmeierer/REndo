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

  coef.table           <- cbind(object$coefficients, object$CI)
  rownames(coef.table) <- names(object$coefficients)
  colnames(coef.table) <- c("Estimate","Low_95%CI","Up_95%CI")

  ans <- list(call = object$call, coef.table = coef.table)
  class(ans) <- "summary.rendo.copulacorrection.discrete"
  return(ans)
}


#' Print output from summary function on copulaCorrectionDiscrete output
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.copulacorrection.discrete <- function(x, digits=5, #signif.stars = getOption("show.signif.stars"),
                                                          ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  printCoefmat(x$coef.table, digits = digits, na.print = "NA", has.Pvalue = F, ...)
  invisible(x)
}
