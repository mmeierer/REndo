#' @title  Fitting Linear Models Endogeneous Regressors using Gaussian Copula
#' @description
#' Fits linear models with continuous or discrete endogeneous regressors (or a mixture of both) using Gaussian copulas, as presented in Park and Gupta (2012).
#' This is a statistical technique to address the endogeneity problem where no external instrumental variables are needed.
#' The important assumption of the model is that the endogeneous variables should NOT be normally distributed, if continuous, preferably with a skewed distribution.
#'
#' @template template_param_formuladataverbose
#' @param ... Arguments for the log-likelihood optimization function in the case of a single continuous endogenous
#'  regressor. Ignored with a warning otherwise.
#' \describe{
#' \item{start.params}{A named vector containing a set of parameters to use in the first optimization iteration.
#' The names have to correspond exactly to the names of the components specified in the \code{formula} parameter.
#' If not provided, a linear model is fitted to derive them.}
#' \item{num.boots}{Number of bootstrapping iterations. Defaults to 1000.}
#' }
#'
#' @details
#'
#' \subsection{Method}{
#'
#' For the case of a single continuous endogenous regressor maximum likelihood estimation is used.
#' Therefore, the call of the function is: \\
#' \code{copulaCorrection(y ~ X1 + X2 + P | continuous(P), data, start.params, num.boots)}, \\
#'
#' the starting values for \eqn{\rho} and \eqn{\sigma}, where \eqn{\rho} is the correlation between the endogenous regressor and the error, and \eqn{\sigma} is
#' the variance of the model's error.
#'
#' In the case of one discrete endogenous regressor, the call of the function is:\\
#' \code{copulaCorrection(y ~ X1 + X2 + P | discrete(P), data)}\\
#' Here, an alternative model specification is used, still based on Gaussian copula, but using augmented OLS.
#' A note in this case: Since the marginal distribution function of the endogenous regressor is a step function in this case,
#' it is important to also have a look at the confidence interval of the coefficient estimates, by calling the \code{confint()} function.
#'
#' In the case of multiple endogenous regressors of both discrete or continuous distributions, the call of the \code{copulaCorrection()} function is: \\
#' \code{copulaCorrection(y ~ X1 + X2 + P1 + P2 | discrete(P1) + continuous(P2), data)}
#'
#'
#' Something about the additional parameters that are used during optimization
#' Something about the confidence interval for discrete only
#'}
#'
#'\subsection{Formula parameter}{
#' The \code{formula} argument follows a two part notation:
#'
#' A two-sided formula describing the model (e.g. \code{y ~ X1 + X2 + P}) to be estimated and a
#' second right-hand side part in which the endogenous regressors and their distributional
#' assumptions are indicated (e.g. \code{continuous(P)}). These two parts are separated by a single vertical bar (\code{|}).
#' In the second part, the special functions \code{continuous} or \code{discrete}, or a combination
#' of both are used to indicate the endogenous regressors and their respective distribution.
#' Both functions use the following parameter:
#'
#'\describe{ \item{...}{The endogenous regressors with the respective distribution.}}
#'
#' Note that no argument to \code{continuous} or \code{discrete} is to be supplied as character
#' but as symbols without quotation marks.
#'
#' See the example section for illustrations on how to specify the \code{formula} parameter.
#'}
#' @return
#' For the case of a single continuous endogenous regressor, an object of class \code{rendo.optim.LL} is returned.
#' It is a list and contains the following components:
#' \item{formula}{The formula given to specify the model to be fitted.}
#' \item{start.params}{A named vector with the initial set of parameters used to optimize the log-likelihood function.}
#' \item{estim.params}{A named vector of all coefficients used during model fitting.}
#' \item{estim.params.se}{A named vector of the standard error of all coefficients used during model fitting.}
#' \item{names.main.coefs}{A vector specifying which coefficients are from the model.}
#' \item{res.optimx}{The result object returned by the function \code{optimx}.}
#' \item{log.likelihood}{The value of the log-likelihood function corresponding to the optimal parameters.}
#' \item{hessian}{A named, symmetric matrix giving an estimate of the Hessian at the found solution.}
#' \item{fitted.values}{Fitted values at the found solution.}
#' \item{residuals}{The residuals.}
#' \item{model}{The model.frame used for model fitting.}
#' \item{terms}{The terms object used for model fitting.}
#'
#' The function summary can be used to obtain and print a summary of the results.
#' The generic accessor functions \code{coefficients}, \code{fitted.values}, \code{residuals}, \code{vcov}, \code{logLik}, \code{AIC}, \code{BIC}, \code{nobs}, and \code{labels} are available.
#'
#' For all other cases, an object of classes \code{rendo.pstar.lm} and \code{lm} is returned.
#' It extends the object returned from \code{lm} of package \code{stats} to additionally include the
#' following components:
#' \item{original.data}{The original data used to fit the model.}
#' \item{names.vars.continuous}{The names of the continuous endogenous regressors.}
#' \item{names.vars.discrete}{The names of the discrete endogenous regressors.}
#'
#' All generic accessor functions for \code{lm} such as \code{anova}, \code{hatalues}, or \code{vcov} are available.
#'
#' @seealso \code{\link[stats]{lm}}
#' @seealso \code{\link[optimx]{optimx}}
#'
#' @references   Park, S. and Gupta, S., (2012), 'Handling Endogeneous Regressors by Joint Estimation Using Copulas', Marketing Science, 31(4), 567-86.
#'
#' @examples
#' data("dataCopC1")
#' data("dataCopC2")
#' data("dataCopDis")
#' data("dataCopDisCont")
#'
#' \notrun{
#' # Single continuous: log-likelihood optimization
#' c1 <- copulaCorrection(y~X1+X2+P|continuous(P), data=dataCopC1)
#' # same as above, with start.parameters and number of bootstrappings
#' c1 <- copulaCorrection(y~X1+X2+P|continuous(P), num.boots=500, data=dataCopC1,
#'                        start.params = c("(Intercept)"=1, X1=1, X2=-2, P=-1))
#' }
#'
#' # All following examples fit linear model with Gaussian copulas
#'
#' # 2 continuous endogenous regressors
#' c2 <- copulaCorrection(y~X1+X2+P1+P2|continuous(P1, P2), data=dataCopC2)
#' # same as above
#' c2 <- copulaCorrection(y~X1+X2+P1+P2|continuous(P1)+continuous(P2), data=dataCopC2)
#'
#' # single discrete endogenous regressor
#' d1 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1), data=dataCopDis)
#' # two discrete endogenous regressor
#' d2 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+discrete(P2), data=dataCopDis)
#' # same as above
#' d2 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1, P2), data=dataCopDis)
#'
#' # single discrete, single continuous
#' cd <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+continuous(P2), data=dataCopDisCont)
#'
#' @importFrom Formula as.Formula
#' @importFrom methods formalArgs
#' @export
copulaCorrection <- function(formula, data, verbose=TRUE, ...){
  # Catch stuff ------------------------------------------------------------------------------------------------
  cl <- quote(match.call())
  l.ellipsis <- list(...)

  # Input checks -----------------------------------------------------------------------------------------------
  check_err_msg(checkinput_copulacorrection_formula(formula=formula))
  check_err_msg(checkinput_copulacorrection_data(data=data))
  check_err_msg(checkinput_copulacorrection_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_copulacorrection_verbose(verbose=verbose))

  # **?? CHECK ASSUMPTION THAT ENDOGENOUS IS NOT NORMAL ??

  # Read out specials ------------------------------------------------------------------------------------------
  F.formula <- as.Formula(formula)
  names.vars.continuous <- formula_readout_special(F.formula = F.formula, name.special = "continuous",
                                                   from.rhs=2, params.as.chars.only=TRUE)
  names.vars.discrete   <- formula_readout_special(F.formula = F.formula, name.special = "discrete",
                                                   from.rhs=2, params.as.chars.only=TRUE)


  # Determine case
  if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0)
    optimizeLL <- TRUE
  else
    optimizeLL <- FALSE

  # Dispatch to either LL optimization or PStar+lm -------------------------------------------------------------
  if(optimizeLL){
    # Single continuous - copula method 1 (optimize LL)
    res <- do.call(what = copulaCorrection_optimizeLL,
                   args = c(list(F.formula=F.formula, data=data,
                                 name.var.continuous =names.vars.continuous,
                                 verbose=verbose, cl=cl), # cl supplied to create return object
                            l.ellipsis))
  }else{
    # All other cases use pstar data + lm
    res <- do.call(copulaCorrection_linearmodel,
                   c(list(F.formula = F.formula, data = data,verbose=verbose,
                          cl=cl, # cl supplied to create return object
                          names.vars.continuous = names.vars.continuous,
                          names.vars.discrete   = names.vars.discrete),
                   l.ellipsis))
  }

  return(res)
}

