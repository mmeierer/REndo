#' @export
residuals.rendo.copulacorrection.continuous2 <- function(object, ...){
  return(object$residuals)
}


#' @export
coef.rendo.copulacorrection.continuous2 <- function(object, ...){
  return(object$coefficients)
}

#' When result is not saved or result is just typed, call summary anyways
#' @export
print.rendo.copulacorrection.continuous2 <- function(x, ...){
  print(summary(object=x, ...))
  invisible(x)
}


#' Calculate summary statistics for result object from copulaCorrectionDiscrete
#' @importFrom stats pt pf
#' @export
summary.rendo.copulacorrection.continuous2 <- function(object, ...){

  ans <- object[c("call","coefficients","seCoefficients","lm_stats")]
  # z-score
  ans$z_val_table <- object$coefficients/object$seCoefficients  # z-score endogenous variable

  # p-values (length -> nrow???)
  ans$p.val <- 2*stats::pt(q=(-abs(ans$z_val_table)), df=(length(object$regressors[,1])-1))

  # length(object$regressors[,1]) == nrow(object$regressors) ??
  # compute pvalue for Fstat
  ans$pval_fstats <- pf(length(object$regressors[,1]), as.numeric(object$lm_stats$fstat[2]),
                    as.numeric(object$lm_stats$fstat[3]), lower.tail=F)


  ans$coefficients <- cbind(ans$coefficients, ans$seCoefficients, ans$z_val_table, ans$p.val)
  rownames(ans$coefficients) <- names(object$coefficients)
  colnames(ans$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

  class(ans) <- "summary.rendo.copulacorrection.continuous2"
  return(ans)
}


#' Extract coefficient table from summary output
#' @export
coef.summary.rendo.copulacorrection.continuous2 <- function(object, ...){
  return(object$coefficients)
}


#' Print output from summary function on copulaCorrectionContinuous2 output
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.copulacorrection.continuous2 <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
                                                          ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = T, ...)

  cat("\nResidual standard error: ",x$lm_stats$residSE, "on", x$lm_stats$df[2],"degrees of freedom \n")
  cat("Multiple R-squared:",x$lm_stats$r2, "Adjusted R-squared:", x$lm_stats$adjr2,"\n", sep = " ")

  cat("F-statistic: ",x$lm_stats$fstat[1], "on", x$lm_stats$df[1], "and",  x$lm_stats$df[2], "degrees of freedom, p-value: ", x$pval_fstats,"\n")

  invisible(x)
}
