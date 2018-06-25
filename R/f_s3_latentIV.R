# residuals <- function()
# @export
# show.rendo.ivlatent <- function(object){
#   print(object)
#   invisible(NULL)
# }

#' @export
print.rendo.ivlatent <- function(x, ...){
  print(summary(object = x))
  invisible(x)
}

#' @export
nobs.rendo.ivlatent <- function(object, ...){
  return(object$nobs)
}

#' @export
logLik.rendo.ivlatent <- function(object, ...){
  return(object$log.likelihood)
}

#' @export
coef.rendo.ivlatent <- function(object,...){
  return(object$coefficients)
}

#' @export
summary.rendo.ivlatent <- function(object, ...){
  # Copy from input object
  res <- object[c("call", "initial.values", "log.likelihood", "AIC", "BIC", "conv.code")]
  t.val <- object$coefficients/(object$coefficients.se/sqrt(nobs(object)))
  p.val <- 2*pt(q=(-abs(t.val)), df = nobs(object)-1)

  res$coefficients <- cbind(object$coefficients,
                            object$coefficients.se,
                            t.val,
                            p.val)

  rownames(res$coefficients) <- names(object$coefficients)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "t-value", "Pr(>|z|)")

  class(res) <- "summary.rendo.ivlatent"
  return(res)
}

#' @export
coef.summary.rendo.ivlatent <- function(object, ...){
  return(object$coefficients)
}

#' @export
#' @importFrom stats printCoefmat
print.summary.rendo.ivlatent <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                         ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Coefficients:\n")
  printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = T, signif.stars = signif.stars,...)
  cat("\n")
  cat("Initial parameter values:", round(x$initial.values,3), "\n")
  cat("The Value of the log likelihood function:", x$log.likelihood,"\n")
  cat("AIC:", x$AIC,", BIC:",x$BIC, "\n")
  cat("Convergence Code: ", x$conv.code, "\n")
  invisible(x)
}





