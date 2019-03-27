# @title Confidence Interval for copula correction models fitted with augmented OLS.
# @description
# In the case of only discrete endogenous regressors, the model is re-fitted multiple times
# and the confidence interval is obtained for each fitted model. The mean of all simulated confidence
# intervals per parameter is reported.
#
# In all other cases, the standard method for a fitted linear model \code{\link[stats]{lm}} is applied.
#
# @inheritParams stats::confint
# @param num.simulations the number of times the model is re-fitted to obtain confidence intervals in case of discrete endogenous regressors only. Ignored with a warning otherwise.
# @param ... ignored, for consistency with the generic function.
#
# @seealso \code{\link[stats]{confint}} for the standard method for linear models
# @seealso \code{\link{copulaCorrection}} for more information about the model's background
#
# @importFrom stats confint confint.lm
# @export
# confint.rendo.copula.c2 <- function(object, parm, level=0.95, num.simulations=250L, ...){
#
#   # Read out needed stuff -------------------------------------------------------------------------------
#   # All if parm is missing (needed because parm is unknown in lapply)
#   if(missing(parm))
#     parm <- names(coef(object))
#
#   names.vars.continuous <- object$names.vars.continuous
#   names.vars.discrete   <- object$names.vars.discrete
#
#   # Determine case --------------------------------------------------------------------------------------
#   # Forward to lm's confint function for all except "discrete only" which needs simulation
#   if(length(names.vars.discrete)>0 & length(names.vars.continuous) == 0){
#
#     # Simulations: Discrete only case
#
#     # Run simulation ------------------------------------------------------------------------------------
#     # Run .run.simulation.copulacorrection.discrete num.simulation times and extract the coefs and CI
#     # for each regressor (and intercept)
#     l.simulated.coefs.and.confints <-
#       lapply(X = seq(num.simulations), FUN = function(i){
#         res.lm <- copulaCorrection_linearmodel(F.formula             = object$formula,
#                                                data                  = object$original.data,
#                                                names.vars.continuous = names.vars.continuous,
#                                                names.vars.discrete   = names.vars.discrete,
#                                                verbose = FALSE, cl=quote(match.call()))
#         # Be sure to call lm's confint per fit, otherwise Inf loop
#         return(confint.lm(res.lm, parm = parm, level = level))
#       })
#     single.lm.ci <- l.simulated.coefs.and.confints[[1]]
#
#     # If there are no results (ie all removed) cannot do calculations
#     if(nrow(single.lm.ci) == 0)
#       return(single.lm.ci)
#
#     # Separate tables for confints
#     #   return depends on number of parm given. if only 1, the results is not a matrix
#     simulated.confints.lower  <- matrix(sapply(l.simulated.coefs.and.confints, function(x) x[,1]),
#                                         nrow=nrow(single.lm.ci))
#     simulated.confints.higher <- matrix(sapply(l.simulated.coefs.and.confints, function(x) x[,2]),
#                                         nrow=nrow(single.lm.ci))
#
#     # Mean of coefs and confints ------------------------------------------------------------------------
#     confint.mean           <- cbind(rowMeans(simulated.confints.lower),
#                                     rowMeans(simulated.confints.higher))
#     dimnames(confint.mean) <- dimnames(single.lm.ci)
#
#     return(confint.mean)
#   }else{
#
#     if(!missing(num.simulations))
#       warning("The specified parameter for \'num.simulations\' is ignored because this model does not use discrete endogenous regressors only.",
#               call. = FALSE, immediate. = TRUE)
#
#     # No discrete only case: Return rendo.boots's confint
#     return(NextMethod(object = object, parm = parm, level = level,...))
#   }
# }



#' @title Summarizing Bootstrapped copulaCorrection Model Fits
#' @param object an object of class \code{rendo.copula.c2}, a result of a call to \code{copulaCorrection}.
#' @param ... ignored, for consistency with the generic function.
#'
#' @description
#'
#' \code{summary} method for a model of class \code{rendo.copula.c2} resulting from fitting \code{copulaCorrection}
#'
#' @details
#' Some details
# @details
# The for the model fitted.  The reported confidence intervals.
#'
#' @return
#' The function \code{summary.rendo.copula.c2} computes and returns a list of summary statistics
#' which contains the following components:
#' \item{coefficients}{a \code{px4} matrix with columns for the estimated coefficients for the the original data, the standard error of the bootstrapped parameters, and the lower and upper boundaries of the 95\% bootstrap confidence interval.}
#' \item{num.boots}{the number of bootstraps performed.}
#' \item{names.main.coefs}{a vector specifying which coefficients are from the model. For internal usage.}
#' \item{vcov}{variance covariance matrix derived from the bootstrapped parameters.}
#' \item{names.vars.continuous}{The names of the continuous endogenous regressors.}
#' \item{names.vars.discrete}{The names of the discrete endogenous regressors.}
#'
#' @seealso The model fitting function \code{\link[REndo:copulaCorrection]{copulaCorrection}}
#' @seealso \code{\link[REndo:confint.rendo.boots]{confint}} for how the confidence intervals are derived
#' @seealso \code{\link[REndo:vcov.rendo.boots]{vcov}} for how the variance-covariance matrix is derived
#' @seealso Function \code{coef} will extract the \code{coefficients} matrix and
#' function \code{vcov} will extract the component \code{vcov} from the returned summary object.
#'
#' @export
summary.rendo.copula.c2 <- function(object, ...){
# Essentially the exact same summary function as the parent class
#   but also add the names of the continuous and discrete variables

  # Get the normal summary from the rendo.boots parent class
  res <- NextMethod()

  # Also add the names
  res$names.vars.continuous <- c(character(0),object$names.vars.continuous)
  res$names.vars.discrete   <- c(character(0),object$names.vars.discrete)

  # Keep the summary class from rendo.boots to use its print function
  class(res)  <- c("summary.rendo.copula.c2", class(res))
  return(res)
}



#' @export
print.summary.rendo.copula.c2 <- function(x, digits=max(3L, getOption("digits")-3L),
                                          signif.stars = getOption("show.signif.stars"), ...){
  # Print rendo.boots summary first part
  NextMethod()


  # Print copula C2 specific part -------------------------------------------------------------------------
  # Max width to not exceed the fixed width of some prints (printCoef/call)
  max.width <- min(65, 0.9 * getOption("width"))

  cat("\n")

  if(length(x$names.vars.continuous)>0)
    cat("Continuous endogenous variables: ",
        paste0(strwrap( paste0(x$names.vars.continuous, collapse=", "),width = max.width),
               collapse = "\n"),
        "\n")
  if(length(x$names.vars.discrete)>0)
    cat("Discrete endogenous variables: ",
        paste0(strwrap( paste0(x$names.vars.discrete, collapse=", "),width = max.width),
               collapse = "\n"))

  invisible(x)
}







