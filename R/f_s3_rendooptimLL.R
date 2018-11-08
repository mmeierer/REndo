#' @export
fitted.rendo.optim.LL <- function(object, ...){
  return(object$fitted.values)
}


#' @export
residuals.rendo.optim.LL <- function(object, ...){
  return(object$residuals)
}

#' @export
nobs.rendo.optim.LL <- function(object, ...){
  return(NROW(object$residuals))
}


#' @export
#' @importFrom stats nobs
logLik.rendo.optim.LL <- function(object, ...){
  return(structure(nall  = nobs(object),
                   object$log.likelihood,
                   nobs  = nobs(object),
                   df    = length(coef(object$res.optimx)),
                   class ="logLik"))
}

#' @export
#' @importFrom stats case.names
case.names.rendo.optim.LL <- function(object, ...){
  return(names(residuals(object)))
}


#' @export
labels.rendo.optim.LL <- function(object, ...){
  # Because terms() is from model.frame, the labels do not include the specials
  # (contraray to when using labels(terms(formula)))
  return(labels(terms(object$model)))
}

#' @export
coef.rendo.optim.LL <- function(object, ...){
  return(object$estim.params)
}


#' @export
#' @importFrom stats vcov
#' @importFrom corpcor pseudoinverse
vcov.rendo.optim.LL <- function(object, ...){
  if(any(!is.finite(object$hessian)))
    stop("The vcov matrix cannot be calulated because the hessian contains non-finite values!", call. = FALSE)

  m.vcov <- corpcor::pseudoinverse(object$hessian)
  rownames(m.vcov) <- colnames(m.vcov) <- colnames(object$hessian)
  return(m.vcov)
}


#' @export
print.rendo.optim.LL <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  # Short print similar to lm
  # Only print the main model coefs

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Coefficients:\n")
  print.default(format(coef(x)[x$names.main.coefs], digits = digits), print.gap = 2L,
                quote = FALSE)

  invisible(x)
}


#' @importFrom stats pt AIC BIC fitted
#' @export
summary.rendo.optim.LL <- function(object, ...){

  # Copy from input
  res <- object[c("call","start.params", "log.likelihood", "estim.params.se",
                  "names.main.coefs")]

  # Coefficient table --------------------------------------------------------------------
  # Return the full coefficient table. The subset is to relevant rows is done in the
  #   printing

  all.est.params <- object$estim.params # get all coefs
  # z-score
  z.val <- all.est.params/object$estim.params.se
  # p-values
  p.val <- 2*pt(q=(-abs(z.val)), df=NROW(fitted(object))-1)

  res$coefficients <- cbind(all.est.params,
                            object$estim.params.se,
                            z.val,
                            p.val)
  rownames(res$coefficients) <- names(all.est.params)
  colnames(res$coefficients) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")

  # Return object ------------------------------------------------------------------------
  # return NA_ placeholder if cannot calculate vcov
  res$vcov <- tryCatch(vcov(object), error = function(e){
                                      h <- object$hessian
                                      h[,] <- NA_real_
                                      return(h)})
  # optimx conf stuff
  res$AIC       <- AIC(object)
  res$BIC       <- BIC(object)
  res$conv.code <- object$res.optimx$convcode
  res$KKT1      <- object$res.optimx[1, "kkt1"]
  res$KKT2      <- object$res.optimx[1, "kkt2"]
  class(res)    <- "summary.rendo.optim.LL"

  return(res)
}



# Read out full coefficients table from summary, including the non-main model params
#' @export
coef.summary.rendo.optim.LL <- function(object, ...){
  return(object$coefficients)
}

# Read out full vcov matrix
#' @export
vcov.summary.rendo.optim.LL <- function(object, ...){
  return(object$vcov)
}


# Print structure:
#     Call
#     Main model coefficient (incl statistics)
#     Further params used during model fitting
#     Optimality stuff
#' @importFrom stats printCoefmat
#' @export
print.summary.rendo.optim.LL <- function(x, digits=max(3L, getOption("digits")-3L),
                                         signif.stars = getOption("show.signif.stars"), ...){

  # Max width to not exceed the fixed width of some prints (printCoef/call)
  # max.width <- min(max(nchar(deparse(x$call))), 0.9 * getOption("width"))
  max.width <- min(65, 0.9 * getOption("width"))

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  # Main model coefficients
  cat("Coefficients:\n")
  printCoefmat(x$coefficients[x$names.main.coefs, ,drop=FALSE], digits = digits, na.print = "NA",
               has.Pvalue = TRUE, signif.stars = signif.stars,...)

  if(anyNA(x$estim.params.se))
    cat("\n",paste0(strwrap("For some parameters the statistics could not be calculated because the Std. Errors are unavailable.",
                            width = max.width),
                    collapse = "\n"), "\n",sep = "")
  cat("\n")

  # Non main model coefs - only show the values, not the statistcs
  cat("Further parameters estimated during model fitting:\n")

  names.non.main.coefs <- setdiff(rownames(x$coefficients), x$names.main.coefs)
  print.default(format(x$coefficients[names.non.main.coefs, "Estimate"], digits = digits), print.gap = 1L,
                quote = FALSE)
  cat("(see help file for details)\n")

  cat("\n")

  cat("Initial parameter values:\n")
  cat( paste0(strwrap( paste0(paste(names(x$start.params), sep = "=",round(x$start.params,digits = digits)),
                              collapse=" "),
                       width = max.width),
              collapse = "\n"))
  cat("\n\n")

  cat("The value of the log-likelihood function:", x$log.likelihood,"\n")
  cat("AIC:",  x$AIC,", BIC:",x$BIC, "\n")
  cat("KKT1:", x$KKT1, " KKT2:", x$KKT2, "Optimx Convergence Code:", x$conv.code, "\n")
  invisible(x)
}

