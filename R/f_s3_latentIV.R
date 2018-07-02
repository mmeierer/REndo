#' @export
fitted.rendo.ivlatent <- function(object, ...){
  return(object$fitted)
}

#' @export
residuals.rendo.ivlatent <- function(object, ...){
  return(object$residuals)
}

# show.rendo.ivlatent <- function(object){
#   print(object)
#   invisible(NULL)
# }

#' @export
print.rendo.ivlatent <- function(x, ...){
  # Print summary only for the 2 main coefficients
  print(summary(object = x, full=F))
  invisible(x)
}

#' @export
nobs.rendo.ivlatent <- function(object, ...){
  return(NROW(object$residuals))
}

#' @export
#' @importFrom stats nobs
logLik.rendo.ivlatent <- function(object, ...){
  return(structure(object$log.likelihood, class="logLik",
                   nobs=nobs(object), df=length(coef(object$res.optimx))))
}

#' @export
coef.rendo.ivlatent <- function(object,...){
  return(object$coefficients)
}

#' @export
formula.rendo.ivlatent <- function(x,...){
  return(x$formula)
}

#' @export
#' @importFrom stats nobs AIC BIC
summary.rendo.ivlatent <- function(object, full=T,...){
  # Copy from input object
  res <- object[c("call", "initial.values", "log.likelihood", "conv.code")]

  if(full){
    # Report all coefs, incl. group stuff
    relevant.coefficients    <- c(object$coefficients, object$group.means, object$prob.group1)
    relevant.coefficients.se <- c(object$coefficients.se, object$group.means.se, object$prob.group1.se)
    names(relevant.coefficients) <- c(names(object$coefficients), "Group Mean 1", "Group Mean 2", "Prob. Group 1")
  }else{
    # only 2 coefs
    relevant.coefficients    <- object$coefficients
    relevant.coefficients.se <- object$coefficients.se
  }

  z.val <- relevant.coefficients/relevant.coefficients.se
  p.val <- 2*pt(q=(-abs(z.val)), df = nobs(object)-1)

  res$coefficients <- cbind(relevant.coefficients,
                            relevant.coefficients.se,
                            z.val,
                            p.val)

  rownames(res$coefficients) <- names(relevant.coefficients)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

  res$AIC  <- AIC(object)
  res$BIC  <- BIC(object)
  res$KKT1 <- object$res.optimx[1, "kkt1"]
  res$KKT2 <- object$res.optimx[1, "kkt2"]

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
  cat("KKT1:", x$KKT1," KKT2:", x$KKT2, "Optimx Convergence Code:", x$conv.code, "\n")
  invisible(x)
}





