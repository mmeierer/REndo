#' @export
#' @importFrom stats nobs
logLik.rendo.copula.correction <- function(object, ...){
  if(object$copula.case != 1)
    stop("No logLik available except for the case of a single continuous endogenous regressor!", call. = FALSE)

  return(structure(nall  = nobs(object),
                   object$res.optimx$value,
                   nobs  = nobs(object),
                   df    = ncol(coef(object$res.optimx)),
                   class ="logLik"))

}



#' @title Summarizing Bootstrapped copulaCorrection Model Fits
#' @param object an object of class \code{rendo.copula.correction}, a result of a call to \code{copulaCorrection}.
#' @param ... ignored, for consistency with the generic function.
#'
#' @description
#'
#' \code{summary} method for a model of class \code{rendo.copula.correction} resulting from fitting \code{copulaCorrection}.
#'
#' @details
#'
#' For a single continuous endogenous regressor, the estimation is realized in two steps by first obtaining the
#' empirical distribution of the endogenous regressor and then the likelihood function is built. Also for all
#' other cases the estimation is realized in two steps and hence the standard errors reported by the
#' fitted OLS model are not correct.
#'
#' The standard errors and the confidence intervals are therefore obtained using bootstrapping with replacement as
#' described in Effron (1979). The reported lower and upper boundaries are from the 95\% bootstrapped percentile
#' confidence interval. If there are too few bootstrapped estimates, no boundaries are reported.
#'
#' For a single continuous endogenous regressor the model was fitted using maximum likelihood optimization. The
#' related goodness of fit measures and convergence indicators are also reported here.
#'
#' @return
#' The function computes and returns a list of summary statistics which contains the following components:
#'
#' \item{coefficients}{a \code{px4} matrix with columns for the estimated coefficients for the the original data, the standard error derived from the bootstrapped parameters,
#' and the lower and upper boundaries of the 95\% bootstrap confidence interval.}
#' \item{num.boots}{the number of bootstraps performed.}
#' \item{names.main.coefs}{a vector specifying which coefficients are from the model. For internal usage.}
#' \item{start.params}{a named vector with the initial set of parameters used to optimize the log-likelihood function.}
#' \item{vcov}{variance covariance matrix derived from the bootstrapped parameters.}
#' \item{names.vars.continuous}{the names of the continuous endogenous regressors.}
#' \item{names.vars.discrete}{the names of the discrete endogenous regressors.}
#'
#' For the case of a single continuous endogenous regressor, also the following components
#' resulting from the log-likelihood optimization are returned:
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
#' @references
#' Effron, B.(1979). "Bootstrap Methods: Another Look at the Jackknife", The Annals of Statistics, 7(1), 1-26.
#'
#' @importFrom stats AIC BIC logLik
#' @export
summary.rendo.copula.correction <- function(object, ...){

  # Essentially the exact same summary function as the parent class
  # Get the normal summary from the rendo.boots parent class
  #   but also add some copula correction stuff
  res <- NextMethod()


  # Add the copulaCorrection part
  res$copula.case           <- object$copula.case # for print
  res$names.vars.continuous <- c(character(0),object$names.vars.continuous)
  res$names.vars.discrete   <- c(character(0),object$names.vars.discrete)

  # Nothing to add for case 2
  if(object$copula.case == 1){
    # Add GOF measures and convergence stuff from optimx
    res$start.params   <- object$start.params
    res$log.likelihood <- as.numeric(logLik(object))
    res$AIC            <- AIC(object)
    res$BIC            <- BIC(object)
    res$conv.code      <- object$res.optimx$convcode
    res$KKT1           <- object$res.optimx[1, "kkt1"]
    res$KKT2           <- object$res.optimx[1, "kkt2"]
  }

  # Keep the summary class from rendo.boots to use its print function
  class(res)    <- c("summary.rendo.copula.correction", class(res))

  return(res)
}


#' @export
print.summary.rendo.copula.correction <- function(x, digits=max(3L, getOption("digits")-3L),
                                                  signif.stars = getOption("show.signif.stars"), ...){
  # Print rendo.boots summary first part
  NextMethod()


  # Print copulaCorrection specific part -------------------------------------------------------------------------
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

  # Sprint LL optimx convergence/GOF stuff
  if(x$copula.case == 1){
    cat("\n")

    # Non main model coefs - only show the values, not the statistics
    #   Only if there are any
    names.non.main.coefs <- setdiff(rownames(x$coefficients), x$names.main.coefs)
    if(length(names.non.main.coefs)>0){
      cat("Further parameters estimated during model fitting:\n")
      # For single non main coef the vec will lose its name, regardless of drop=FALSE. Therefore always add names
      coefs.non.main <- x$coefficients[names.non.main.coefs, "Point Estimate", drop = TRUE]
      names(coefs.non.main) <- names.non.main.coefs
      print.default(format(coefs.non.main, digits = digits), print.gap = 1L, quote = FALSE)
      cat("(see help file for details)\n\n")
    }

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
  }

  invisible(x)
}

#' @title Predict method for Models using the Gaussian Copula Approach
#'
#' @description
#'
#' Predicted values based on linear models with endogenous regressors estimated using
#' the gaussian copula.
#'
#' @param object Object of class inheriting from "rendo.copula.correction"
#' @param newdata An optional data frame in which to look for variables with which to predict.
#' If omitted, the fitted values are returned.
#' @param ... ignored, for consistency with the generic function.
#'
#' @return
#' \code{predict.copula.correction} produces a vector of predictions
#'
#' @seealso The model fitting function \code{\link[REndo:copulaCorrection]{copulaCorrection}}
#'
#' @examples
#' \donttest{
#' \dontrun{
#' data("dataCopCont")
#'
#' c1 <- copulaCorrection(y~X1+X2+P|continuous(P), num.boots=10,
#'                        data=dataCopCont)
#'
#' # returns the fitted values
#' predict(c1)
#'
#' # using the data used for fitting also for predicting,
#' # correctly results in fitted values
#' all.equal(predict(c1, dataCopCont), fitted(c1)) # TRUE
#' }}
#'
#' @importFrom stats model.frame model.matrix coef
#' @export
predict.rendo.copula.correction <- function(object, newdata,...){

  if(length(list(...)))
    warning("The arguments in ... are ignored.", call. = FALSE, immediate. = TRUE)

  if(missing(newdata)){
    return(fitted(object))
  }else{
      # MLE fitted
      F.main <- formula(formula(object), rhs=1, lhs=0)
      mf <- model.frame(formula = F.main, newdata)
      X  <- model.matrix(object = F.main, mf)
      # complete=FALSE - only use main model params w/o rho & sigma
      #   unfortunately, for the c2 case this does not work, because the pstars are always part
      #   of coef (because they should be printed in summary). Therefore, subset the coefs by names.
      coefs <- coef(object, complete = FALSE)[colnames(X)]
      return(drop(X %*% coefs))
  }
}
