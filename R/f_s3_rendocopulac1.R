#' @export
#' @importFrom stats nobs
logLik.rendo.copula.c1 <- function(object, ...){
  return(structure(nall  = nobs(object),
                   object$res.optimx$value,
                   nobs  = nobs(object),
                   df    = ncol(coef(object$res.optimx)),
                   class ="logLik"))
}


#' @title Summarizing Bootstrapped copulaCorrection Model Fits for Single Continuous Endogenous variables
#' @param object an object of class \code{rendo.copula.c1}, a result of a call to \code{copulaCorrection}.
#' @param ... ignored, for consistency with the generic function.
#'
#' @description
#'
#' \code{summary} method for a model of class \code{rendo.copula.c1} resulting from fitting \code{copulaCorrection}
#' with only a single continuous endogenous variable.
#'
#' @details
#' Because the model was fitted using maximum likelihood optimization,
#' related goodness of fit measures and convergence indicators are also reported. 
#' Since the estimation is realized in two steps, first the empirical distribution of the endogenous regressor is
#' obtained and then the likelihood function is built, the standard errors are not correct and need to be bootstrapped. Due to the 
#' non-normality of the bootrapped parameters, the confidence intervals reported are the upper and lower bounds of the 95% bootstrapped confidence interval.
#'
#' @return
#' The function \code{summary.rendo.copula.c1} computes and returns a list of summary statistics
#' which contains the following components:
#' \item{coefficients}{a \code{px4} matrix with columns for the estimated coefficients for the the original data, the standard error of the bootstrapped parameters, 
#' and the lower and upper boundaries of the 95\% bootstrap confidence interval.}
#' \item{num.boots}{the number of bootstraps performed.}
#' \item{names.main.coefs}{a vector specifying which coefficients are from the model. For internal usage.}
#' \item{start.params}{a named vector with the initial set of parameters used to optimize the log-likelihood function.}
#' \item{vcov}{variance covariance matrix derived from the bootstrapped parameters.}
#' \item{AIC}{Akaike's An Information Criterion for the model fitted on the provided data.}
#' \item{BIC}{Schwarz's Bayesian Criterion for the model fitted on the provided data.}
#' \item{KKT1}{first Kuhn, Karush, Tucker optimality condition as returned by optimx.}
#' \item{KKT2}{second Kuhn, Karush, Tucker optimality condition as returned by optimx.}
#' \item{conv.code}{the convergence code as returned by optimx.}
#' \item{log.likelihood}{the value of the log-likelihood function at the found solution for the provided data.}
#'
#' @seealso The model fitting function \code{\link[REndo:copulaCorrection]{copulaCorrection}}
#' @seealso \code{\link[REndo:confint.rendo.boots]{confint}} for how the confidence intervals are derived
#' @seealso \code{\link[REndo:vcov.rendo.boots]{vcov}} for how the variance-covariance matrix is derived
#' @seealso \code{\link[optimx:optimx]{optimx}} for explanations about the returned \code{conv.code} and \code{KKT}.
#' @seealso Function \code{coef} will extract the \code{coefficients} matrix and
#' function \code{vcov} will extract the component \code{vcov} from the returned summary object.
#'
#' @importFrom stats AIC BIC logLik
#' @export
summary.rendo.copula.c1 <- function(object, ...){
# Essentially the exact same summary function as the parent class
#   but also add GOF measures and convergence stuff
  # Get the normal summary from the rendo.boots parent class
  res <- NextMethod()

  # Also add the optimx stuff
  res$start.params   <- object$start.params
  res$log.likelihood <- as.numeric(logLik(object))
  res$AIC            <- AIC(object)
  res$BIC            <- BIC(object)
  res$conv.code      <- object$res.optimx$convcode
  res$KKT1           <- object$res.optimx[1, "kkt1"]
  res$KKT2           <- object$res.optimx[1, "kkt2"]
  # Keep the summary class from rendo.boots to use its print function
  class(res)    <- c("summary.rendo.copula.c1", class(res))
  return(res)
}


#' @export
print.summary.rendo.copula.c1 <- function(x, digits=max(3L, getOption("digits")-3L),
                                          signif.stars = getOption("show.signif.stars"), ...){
  # Print rendo.boots summary first part
  NextMethod()


  # Print copula C1 specific part -------------------------------------------------------------------------
  # Max width to not exceed the fixed width of some prints (printCoef/call)
  max.width <- min(65, 0.9 * getOption("width"))

  cat("\n")

  cat("Initial parameter values:\n")
  cat( paste0(strwrap( paste0(paste(names(x$start.params), sep = "=",round(x$start.params,digits = digits)),
                              collapse=" "),
                       width = max.width),
              collapse = "\n"))

  # Also print the LL infos
  cat("\n\n")
  cat("The value of the log-likelihood function:", x$log.likelihood,"\n")
  cat("AIC:",  x$AIC,", BIC:",x$BIC, "\n")
  cat("KKT1:", x$KKT1, " KKT2:", x$KKT2, "Optimx Convergence Code:", x$conv.code, "\n")

  invisible(x)
}
