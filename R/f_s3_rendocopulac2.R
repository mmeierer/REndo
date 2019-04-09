#' @title Summarizing Bootstrapped copulaCorrection Model Fits
#' @param object an object of class \code{rendo.copula.c2}, a result of a call to \code{copulaCorrection}.
#' @param ... ignored, for consistency with the generic function.
#'
#' @description
#'
#' \code{summary} method for a model of class \code{rendo.copula.c2} resulting from fitting \code{copulaCorrection}
#'
#' @details
#' Since the estimation is realized in two steps, the standard errors reported by the ordinary least squares are not correct.
#' Therefore, the standard errors and the confidence intervals are obtained using boostraping with replacement, as described 
#' in Effron (1979). The confidence intervals are the lower and upper bounderies of the 95% boostrapped confidence interval. 
#'
#' @return
#' The function \code{summary.rendo.copula.c2} computes and returns a list of summary statistics
#' which contains the following components:
#' \item{coefficients}{a \code{px4} matrix with columns for the estimated coefficients for the the original data, the standard error of the bootstrapped parameters, 
#' and the lower and upper boundaries of the 95\% bootstrap confidence interval.}
#' \item{num.boots}{the number of bootstraps performed.}
#' \item{names.main.coefs}{a vector specifying which coefficients are from the model. For internal usage.}
#' \item{vcov}{variance covariance matrix derived from the bootstrapped parameters.}
#' \item{names.vars.continuous}{The names of the continuous endogenous regressors.}
#' \item{names.vars.discrete}{The names of the discrete endogenous regressors.}
#' @references
#' Effron, B.(1979). "Bootstrap Methods: Another Look at the Jackknife", The Annals of Statistics, 7(1), 1-26.
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

  cat("\n\n")

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







