#' @export
residuals.rendo.copulacorrection.continuous1 <- function(object, ...){
  object$residuals
}


#' @export
coef.rendo.copulacorrection.continuous1 <- function(object, ...){
  params <- c(object$coefExoVar, object$coefEndoVar, object$rho, object$sigma)
  names(params) <- rownames(object$seCoefficients)
  return(params)
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
  return(object$logLik)
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
  # Take from input
  res <- object[c("call", "param", "seCoefficients", "logLik", "AIC", "BIC", "convCode")]

  res$all.est.params <- coef(object)
  # add z=score
  res$z_val_table <- res$all.est.params/object$seCoefficients  # z-score endogenous variable
  # add p-values
  res$pval <- 2*stats::pt(q=(-abs(res$z_val_table)), df=(length(object$regressors[,1])-1))

  class(res) <- "summary.rendo.copulacorrection.continuous1"
  return(res)
}

#' Print output from summary function on copulaCorrectionContinuous1 output
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.copulacorrection.continuous1 <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                                             ...){

  coef.table <- cbind(x$all.est.params,
                       x$seCoefficients,
                       x$z_val_table,
                       x$pval)
  rownames(coef.table) <- rownames(x$seCoefficients)
  colnames(coef.table) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Coefficients:\n")
  stats::printCoefmat(coef.table, digits = digits, na.print = "NA", has.Pvalue = T, ...)
  cat("\n")
  names(x$param) <- rownames(x$seCoefficients)
  cat("Initial parameter values:", round(x$param,3), "\n")
  cat("The Value of the log likelihood function:", x$logLik,"\n")
  cat("AIC:", x$AIC,", BIC:",x$BIC, "\n")
  cat("Convergence Code: ", x$convCode, "\n")
  invisible(x)
}
