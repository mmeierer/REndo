#' @export
fitted.rendo.latentiv <- function(object, ...){
  return(object$fitted)
}

#' @export
residuals.rendo.latentiv <- function(object, ...){
  return(object$residuals)
}

# show.rendo.latentiv <- function(object){
#   print(object)
#   invisible(NULL)
# }

#' @export
print.rendo.latentiv <- function(x, ...){
  # Print summary only for the 2 main coefficients
  print(summary(object = x, full=F))
  invisible(x)
}

#' @export
nobs.rendo.latentiv <- function(object, ...){
  return(NROW(object$residuals))
}

#' @export
#' @importFrom stats nobs
logLik.rendo.latentiv <- function(object, ...){
  return(structure(nall = nobs(object),
                   object$log.likelihood,
                   nobs=nobs(object),
                   df=length(coef(object$res.optimx)) + 1,
                   class="logLik"))
}

#' @export
coef.rendo.latentiv <- function(object,...){
  return(object$coefficients)
}


#' @export
#' @importFrom stats case.names
case.names.rendo.latentiv <- function(object){
  return(names(residuals(object)))
}

#' @export
labels.rendo.latentiv <- function(object, ...){
  return(labels(terms(object$formula)))
}


#' @export
#' @importFrom stats vcov
#' @importFrom corpcor pseudoinverse
vcov.rendo.latentiv <- function(object, ...){
  if(any(!is.finite(object$hessian)))
    stop("The vcov matrix cannot be calulated because the hessian contains non-finite values!", call. = FALSE)

  m.vcov <- pseudoinverse(-object$hessian)
  rownames(m.vcov) <- colnames(m.vcov) <- colnames(object$hessian)
  return(m.vcov)
}



#' @export
#' @importFrom stats nobs AIC BIC
summary.rendo.latentiv <- function(object, full=T,...){
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

  class(res) <- "summary.rendo.latentiv"
  return(res)
}

#' @export
coef.summary.rendo.latentiv <- function(object, ...){
  return(object$coefficients)
}


#' @export
#' @importFrom stats printCoefmat
print.summary.rendo.latentiv <- function(x, digits=5, signif.stars = getOption("show.signif.stars"),
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





