#' @title  Fitting Linear Models with one Endogenous Regressor using Latent Instrumental Variables
#'
#' @description  Fits linear models with one endogenous regressor and no additional explanatory variables using the latent instrumental variable approach
#' presented in Ebbes, P., Wedel, M.,  Böckenholt, U., and Steerneman, A. G. M. (2005). This is a statistical technique to address the endogeneity problem where no external instrumental
#' variables are needed. The important assumption of the model is that the latent variables are discrete with at least two groups with different means and
#' the structural error is normally distributed.
#'
#' @param formula A symbolic description of the model to be fitted. Of class "formula".
#' @param start.params A named vector containing a set of parameters to use in the first optimization iteration.
#' The names have to correspond exactly to the names of the components specified in the formula parameter.
#' If not provided, a linear model is fitted to derive them.
#' @param data A data.frame containing the data of all parts specified in the formula parameter.
#' @param verbose Show details about the running of the function.
#'
#' @details
#'
#' Let's consider the model:
#' \ifelse{html}{\out{<br><center>Y<sub>t</sub>=&beta;<sub>0</sub>+&alpha;P<sub>t</sub>+&epsilon;<sub>t</sub></center>}}{ \deqn{Y_{t} = \beta_{0} + \alpha P_{t} + \epsilon_{t}}}
#' \ifelse{html}{\out{<center>P<sub>t</sub>=&pi;'Z<sub>t</sub>+&nu;<sub>t</sub></center>}}{ \deqn{P_{t}=\pi^{'}Z_{t} + \nu_{t}}}
#'
#' where \eqn{t = 1,..,T} indexes either time or cross-sectional units, \ifelse{html}{\out{Y<sub>t</sub>}}{\eqn{Y_{t}}} is the dependent variable,
#' \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}} is a \code{k x 1} continuous, endogenous regressor,
#' \ifelse{html}{\out{&epsilon;<sub>t</sub>}}{\eqn{\epsilon_{t}}} is a structural error term with mean zero
#' and \ifelse{html}{\out{E(&epsilon;<sup>2</sup>)=&sigma;<sub>&epsilon;</sub><sup>2</sup>}}{\eqn{E(\epsilon^{2})=\sigma^{2}_{\epsilon}}},
#' \eqn{\alpha} and \ifelse{html}{\out{&beta;<sub>0</sub>}}{\eqn{\beta_0}} are model parameters.
#' \ifelse{html}{\out{Z;<sub>t</sub>}}{\eqn{Z_{t}}} is a \code{l x 1} vector of instruments,
#' and \ifelse{html}{\out{&nu;<sub>t</sub>}}{\eqn{\nu_{t}}} is a random error with mean zero and
#' \ifelse{html}{\out{E(&nu;<sup>2</sup>)=&sigma;<sub>&nu;</sub><sup>2</sup>}}{\eqn{E(\nu^{2}) = \sigma^{2}_{\nu}}}.
#' The endogeneity problem arises from the correlation of \eqn{P} and \ifelse{html}{\out{&epsilon;<sub>t</sub>}}{\eqn{\epsilon_{t}}}
#' through \ifelse{html}{\out{E(&epsilon;&nu;)=&sigma;<sub>&epsilon;&nu;</sub>}}{\eqn{E(\epsilon\nu) = \sigma_{\epsilon\nu}}}
#'
#' \code{latentIV} considers \ifelse{html}{\out{Z<sub>t</sub>'}}{\eqn{Z_{t}^{'}}} to be a latent, discrete, exogenous variable with an unknown number of groups \eqn{m} and \eqn{\pi} is a vector of group means.
#' It is assumed that \eqn{Z} is independent of the error terms \eqn{\epsilon} and \eqn{\nu} and that it has at least two groups with different means.
#' The structural and random errors are considered normally distributed with mean zero and variance-covariance matrix \eqn{\Sigma}:
#' \ifelse{html}{\out{<center>&Sigma;=(&sigma;<sub>&epsilon;</sub><sup>2</sup>, &sigma;<sub>0</sub><sup>2</sup>,
#' <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&sigma;<sub>0</sub><sup>2</sup>, &sigma;<sub>&nu;</sub><sup>2</sup>)</center>}}{
#' \deqn{\Sigma = \left(
#' \begin{array}{ccc}
#' \sigma_{\epsilon}^{2} \& \sigma_{\epsilon\nu}\\
#' \sigma_{\epsilon\nu} \& \sigma_{\nu}^{2}
#' \end{array}\right)}}
#'
#' The identification of the model lies in the assumption of the non-normality of
#' \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}}, the discreteness of the unobserved instruments and the existence of
#' at least two groups with different means.
#'
#' The method has been implemented such that the latent variable has two groups. Ebbes et al.(2005) show in a Monte Carlo experiment that
#' even if the true number of the categories of the instrument is larger than two, estimates are approximately consistent. Besides, overfitting in terms
#' of the number of groups/categories reduces the degrees of freedom and leads to efficiency loss. For a model with additional explanatory variables a Bayesian approach is needed, since
#' in a frequentist approach identification issues appear.
#'
#' Identification of the parameters relies on the distributional assumptions of the latent instruments as well as that of
#' the endogenous regressor \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}}.
#' Specifically, the endogenous regressor should have a non-normal distribution while the unobserved instruments, \eqn{Z}, should be discrete and have at least two groups with different means Ebbes, Wedel, and Böckenholt (2009).
#' A continuous distribution for the instruments leads to an unidentified model, while a normal distribution of the endogenous regressor gives rise to inefficient estimates.
#'
#' Additional parameters used during model fitting and printed in \code{summary} are:
#' \describe{
#' \item{pi1}{The instrumental variables \eqn{Z} are assumed to be divided into two groups. \code{pi1} represents the estimated group mean of the first group.}
#' \item{pi2}{The estimated group mean of the second group of the instrumental variables \eqn{Z}.}
#' \item{theta5}{The probability of being in the first group of the instruments.}
#' \item{theta6}{The variance, \ifelse{html}{\out{&sigma;<sub>&epsilon;</sub><sup>2</sup>}}{\eqn{\sigma_{\epsilon}^{2}}}}
#' \item{theta7}{The covariance, \ifelse{html}{\out{&sigma;<sub>&epsilon;&nu;</sub>}}{\eqn{\sigma_{\epsilon\nu}}}}
#' \item{theta8}{The variance, \ifelse{html}{\out{&sigma;<sub>&nu;</sub><sup>2</sup>}}{\eqn{\sigma_{\nu}^{2}}}}
#' }
#'
#'
#' @return An object of class \code{rendo.optim.LL} is returned that is a list and contains the following components:
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
#' @references   Ebbes, P., Wedel,M., Böckenholt, U., and Steerneman, A. G. M. (2005). 'Solving and Testing for Regressor-Error
#' (in)Dependence When no Instrumental Variables are Available: With New Evidence for the Effect of Education on Income'.
#' Quantitative Marketing and Economics, 3:365--392.
#'
#' Ebbes P., Wedel M., Böckenholt U. (2009). “Frugal IV Alternatives to Identify the Parameter for an Endogenous Regressor.” Journal of Applied Econometrics, 24(3), 446–468.
#'
#' @examples
#' data(dataLatentIV)
#'
#' # function call without any initial parameter values
#' l  <- latentIV(y ~ P, data = dataLatentIV)
#' summary(l)
#'
#' # function call with initial parameter values given by the user
#' l1 <- latentIV(y ~ P, start.params = c("(Intercept)"=2.5, P=-0.5),
#'                data = dataLatentIV)
#' summary(l1)
#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame model.matrix sd update setNames
#' @importFrom optimx optimx
#' @importFrom corpcor pseudoinverse
#' @importFrom methods is
#' @export
latentIV <- function(formula, data, start.params=c(), verbose=TRUE){
  cl <- match.call()

  # Input checks ------------------------------------------------------------------------------
  check_err_msg(checkinput_latentIV_data(data=data))
  check_err_msg(checkinput_latentIV_formula(formula=formula))
  check_err_msg(checkinput_latentIV_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_latentIV_startparams(start.params=start.params, formula=formula))
  check_err_msg(checkinput_latentIV_verbose(verbose=verbose))

  # Extract data ------------------------------------------------------------------------------
  F.formula          <- as.Formula(formula)
  mf                 <- model.frame(formula = F.formula, data = data)
  vec.data.y         <- model.response(mf)
  vec.data.endo      <- model.part(object=F.formula, data = mf, lhs=0, rhs=1, drop = TRUE)
  # response (y) + data of model (excl intercept)
  m.data.mvnorm      <- cbind(vec.data.y, vec.data.endo)

  # Start parameter and names ------------------------------------------------------------------
  # Iff the given model includes an intercept, one more paramters is needed
  use.intercept      <- as.logical(attr(terms(F.formula), "intercept"))
  name.intercept     <- "(Intercept)" # convenience, always set

  # Readout / generate start values for the main model params
  if(is.null(start.params)){
    # generate start vals from lm
    if(verbose)
      message("No start parameters were given. The linear model ",deparse(formula(F.formula, lhs=1, rhs=1)),
              " is fitted to derive them.")
    res.lm.start.param    <- lm(F.formula, data)
    if(anyNA(coef(res.lm.start.param)))
      stop("The start parameters could not be derived by fitting a linear model.", call. = FALSE)
    start.vals.main.model <-  coef(res.lm.start.param)
  }else{
    # valid start.params given by user
    start.vals.main.model <- start.params
  }

  # read out names
  name.endo.param  <- setdiff(names(start.vals.main.model), name.intercept)
  names.main.model <- if(use.intercept) c(name.intercept, name.endo.param) else name.endo.param
  # ensure sorting is (Intercept), ENDO
  start.vals.main.model <- setNames(start.vals.main.model[names.main.model], names.main.model)

  start.vals.support.params <- c(pi1 = mean(vec.data.endo),
                                 pi2 = mean(vec.data.endo) + sd(vec.data.endo),
                                 theta5 = 1, theta6 = 1, theta7 = 1,
                                 theta8 = 0) # 0 -> will be 0.5 in LL
  names.support.params      <- names(start.vals.support.params)

  # Append support params to start params
  optimx.start.params  <- c(start.vals.main.model, start.vals.support.params)




  # Optimize LL --------------------------------------------------------------------------------
  # Print start params
  if(verbose){
    str.brakets <- paste0("(", paste(names(optimx.start.params), "=", round(optimx.start.params,3),
                                     collapse = ", ", sep=""), ")")
    message("The start parameters c",str.brakets," are used for optimization.")
  }

  optimx.name.endo.param <- make.names(name.endo.param)
  optimx.name.intercept  <- make.names(name.intercept)

  # Fit LL with optimx
  res.optimx <- tryCatch(expr =
                           optimx(par = optimx.start.params,
                                  fn  = latentIV_LL,
                                  m.data.mvnorm = m.data.mvnorm,
                                  use.intercept = use.intercept,
                                  name.intercept  = name.intercept,
                                  name.endo.param = name.endo.param,
                                  method = "Nelder-Mead",
                                  hessian = TRUE,
                                  itnmax  = 5000,
                                  control = list(trace = 0,
                                                 dowarn = FALSE)),
                         error   = function(e){ return(e)})

  if(is(res.optimx, "error"))
    stop("Failed to optimize the log-likelihood function with error \'", res.optimx$message,
         "\'. Please revise your start parameter and data.", call. = FALSE)


  # Read out params ----------------------------------------------------------------------------
  # Ensure naming and ordering

  # Read out params
  optimx.estimated.params  <- coef(res.optimx)[1,]

  # rename main coefficients in correct order to original parameter names
  all.estimated.params   <- setNames(optimx.estimated.params[c(make.names(names.main.model),names.support.params)],
                                     c(names.main.model, names.support.params))
  estim.param.main.model <- all.estimated.params[names.main.model]
  estim.param.support    <- all.estimated.params[names.support.params]

  # Hessian and SE ------------------------------------------------------------------------------
  # Read out hessian.
  hessian <- extract.hessian(res.optimx = res.optimx, names.hessian = names(all.estimated.params))

  fct.se.warn.error <- function(ew){
                              warning("Hessian cannot be solved for the standard errors: ",
                                      ew$message,". All SEs set to NA.",
                                      call. = FALSE, immediate. = TRUE)
                              return(rep(NA_real_,length(all.estimated.params)))}

  all.param.se <- tryCatch(expr = sqrt(diag(corpcor::pseudoinverse(hessian))),
                       # Return same NAs vector if failed to solve so can still read-out results
                       warning = fct.se.warn.error,
                       error   = fct.se.warn.error)
  names(all.param.se)         <- names(all.estimated.params)

  # Fitted and residuals
  if(use.intercept)
    fitted        <- all.estimated.params[[name.intercept]]*1 +
                        all.estimated.params[[name.endo.param]]*vec.data.endo
  else
    fitted        <- all.estimated.params[[name.endo.param]]*vec.data.endo
  fitted          <- as.vector(fitted)
  names(fitted)   <- names(vec.data.y)

  residuals        <- as.vector(vec.data.y - fitted)
  names(residuals) <- names(vec.data.y)

  vcov.error      <- matrix(c(all.estimated.params["theta5"]^2,
                              all.estimated.params["theta5"]   * all.estimated.params["theta6"],
                              all.estimated.params["theta5"]   * all.estimated.params["theta6"],
                              all.estimated.params["theta6"]^2 + all.estimated.params["theta7"]^2),
                           nrow=2, ncol=2)


  # Put together returns ------------------------------------------------------------------
  res <- new_rendo_optim_LL(call=cl, F.formula=F.formula, mf  = mf,
                            start.params     = optimx.start.params,
                            estim.params     = all.estimated.params,
                            estim.params.se  = all.param.se,
                            names.main.coefs = names.main.model,
                            res.optimx = res.optimx, log.likelihood=res.optimx$value,
                            hessian = hessian, fitted.values=fitted,
                            residuals=residuals, vcov.error=vcov.error)

                   # list(coefficients   = estimated.params[names.original.main.coefs],
                   #      group.means    = estimated.params[c("pi1", "pi2")],
                   #      prob.group1    = estimated.params[["theta8"]],
                   #      coefficients.se= param.se[names.original.main.coefs],
                   #      group.means.se = param.se[c("pi1", "pi2")],
                   #      prob.group1.se = param.se[["theta8"]]))
  return(res)
}
