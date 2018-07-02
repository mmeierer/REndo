#' @export
fitted.rendo.copulacorrection.continuous1 <- function(object, ...){
  return(object$fitted)
}


#' @export
residuals.rendo.copulacorrection.continuous1 <- function(object, ...){
  return(object$residuals)
}

#' @export
nobs.rendo.copulacorrection.continuous1 <- function(object, ...){
  return(NROW(object$residuals))
}

#' @export
#' @importFrom stats nobs
logLik.rendo.copulacorrection.continuous1 <- function(object,...){
  return(structure(object$log.likelihood,class = "logLik",
                   nobs=nobs(object), df=length(coef(object$res.optimx))))
}

#' @export
coef.rendo.copulacorrection.continuous1 <- function(object, ...){
  return(object$coefficients)
}

#' @export
formula.rendo.copulacorrection.continuous1 <- function(x, ...){
  return(x$formula)
}

#' @export
print.rendo.copulacorrection.continuous1 <- function(x, ...){
  print(summary(object=x, ...))
  invisible(x)
}

#' Calculate summary statistics for result object from copulaCorrectionDiscrete
#' @importFrom stats pt AIC BIC
#' @export
summary.rendo.copulacorrection.continuous1 <- function(object, ...){
  # Copy from input
  res <- object[c("call","initial.values", "coefficients", "log.likelihood", "conv.code")]

  # Coefficient table --------------------------------------------------------------------
  all.est.params <- coef(object)
  # z-score
  z.val <- all.est.params/object$parameter.sd  # z-score endogenous variable
  # p-values
  p.val <- 2*pt(q=(-abs(z.val)), df=NROW(object$regressors)-1)

  res$coefficients <- cbind(all.est.params,
                            object$parameter.sd,
                            z.val,
                            p.val)

  rownames(res$coefficients) <- names(object$coefficients)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

  res$AIC  <- AIC(object)
  res$BIC  <- BIC(object)
  res$KKT1 <- object$res.optimx[1, "kkt1"]
  res$KKT2 <- object$res.optimx[1, "kkt2"]
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
  printCoefmat(x$coefficients, digits = digits, na.print = "NA", has.Pvalue = T,signif.stars = signif.stars,...)
  cat("\n")

  cat("Initial parameter values:", paste(names(x$initial.values), sep = "=",round(x$initial.values,3)),"\n")
  cat("The Value of the log likelihood function:", x$log.likelihood,"\n")
  cat("AIC:", x$AIC,", BIC:",x$BIC, "\n")
  cat("KKT1:", x$KKT1, " KKT2:", x$KKT2, "Optimx Convergence Code:", x$conv.code, "\n")
  invisible(x)
}
