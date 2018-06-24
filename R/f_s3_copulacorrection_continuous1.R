#' @export
residuals.rendo.copulacorrection.continuous1 <- function(object, ...){
  object$residuals
}


#' @export
coef.rendo.copulacorrection.continuous1 <- function(object, ...){
  return(object$parameter.mean)
}

# Unsure
# #' @export
# extractAIC.rendo.copulacorrection.continuous1 <- function(fit, scale, k=2, ...){
#
# }

# Not ready for comparison
# #' @export
# AIC.rendo.copulacorrection.continuous1 <- function(object, ..., k=2){
#   return(object$AIC)
# }
# #' @export
# BIC.rendo.copulacorrection.continuous1 <- function(object, ...){
#   return(object$BIC)
# }

#' @export
logLik.rendo.copulacorrection.continuous1 <- function(object,...){
  return(object$log.likelihood)
}

#' @export
print.rendo.copulacorrection.continuous1 <- function(x, ...){
  print(summary(object=x, ...))
  invisible(x)
}

#' Calculate summary statistics for result object from copulaCorrectionDiscrete
#' @importFrom stats pt
#' @export
summary.rendo.copulacorrection.continuous1 <- function(object, ...){
  # Copy from input
  res <- object[c("call", "parameter.mean", "log.likelihood", "AIC", "BIC", "conv.code")]

  # Coefficient table --------------------------------------------------------------------
  all.est.params <- coef(object)
  # z-score
  z.val <- all.est.params/object$parameter.sd  # z-score endogenous variable
  # p-values
  p.val <- 2*stats::pt(q=(-abs(z.val)), df=NROW(object$regressors)-1) #(length(object$regressors[,1])-1))

  res$coefficients <- cbind(all.est.params,
                            object$parameter.sd,
                            z.val,
                            p.val)

  rownames(res$coefficients) <- names(object$parameter.mean)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

  class(res) <- "summary.rendo.copulacorrection.continuous1"
  return(res)
}


#' Read coefficients from summary function on copulaCorrectionContinuous1 output
#' @export
coef.summary.rendo.copulacorrection.continuous1 <- function(object, ...){
  return(object$coefficients)
}


#' Print output from summary function on copulaCorrectionContinuous1 output
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.copulacorrection.continuous1 <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                                             ...){

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Coefficients:\n")
  stats::printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = T, ...)
  cat("\n")
  # names(x$param) <- rownames(x$parameter.mean)
  cat("Initial parameter values:", round(x$parameter.mean,3), "\n")
  cat("The Value of the log likelihood function:", x$log.likelihood,"\n")
  cat("AIC:", x$AIC,", BIC:",x$BIC, "\n")
  cat("Convergence Code: ", x$conv.code, "\n")
  invisible(x)
}
