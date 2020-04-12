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
#' @param optimx.args A named list of arguments which are passed to \code{\link[optimx]{optimx}}. This allows users to tweak optimization settings to their liking.
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
#' @return An object of classes \code{rendo.latent.IV} and \code{rendo.base} is returned which is a list and contains the following components:
#' \item{formula}{The formula given to specify the fitted model.}
#' \item{terms}{The terms object used for model fitting.}
#' \item{model}{The model.frame used for model fitting.}
#' \item{coefficients}{A named vector of all coefficients resulting from model fitting.}
#' \item{names.main.coefs}{a vector specifying which coefficients are from the model. For internal usage.}
#' \item{start.params}{A named vector with the initial set of parameters used to optimize the log-likelihood function.}
#' \item{res.optimx}{The result object returned by the function \code{optimx} after optimizing the log-likelihood function.}
#' \item{hessian}{A named, symmetric matrix giving an estimate of the Hessian at the found solution.}
#' \item{m.delta.diag}{A diagonal matrix needed when deriving the vcov to apply the delta method on theta5 which was transformed during the LL optimization.}
#' \item{fitted.values}{Fitted values at the found optimal solution.}
#' \item{residuals}{The residuals at the found optimal solution.}
#'
#' The function \code{summary} can be used to obtain and print a summary of the results.
#' The generic accessor functions \code{coefficients}, \code{fitted.values}, \code{residuals}, \code{vcov}, \code{confint}, \code{logLik}, \code{AIC}, \code{BIC}, \code{case.names}, and \code{nobs} are available.
#'
#'
#' @seealso \code{\link[REndo:summary.rendo.latent.IV]{summary}} for how fitted models are summarized
#' @seealso \code{\link[optimx]{optimx}} for possible elements of parameter \code{optimx.arg}
#'
#' @references   Ebbes, P., Wedel,M., Böckenholt, U., and Steerneman, A. G. M. (2005). 'Solving and Testing for Regressor-Error
#' (in)Dependence When no Instrumental Variables are Available: With New Evidence for the Effect of Education on Income'.
#' Quantitative Marketing and Economics, 3:365--392.
#'
#' Ebbes P., Wedel M., Böckenholt U. (2009). “Frugal IV Alternatives to Identify the Parameter for an Endogenous Regressor.” Journal of Applied Econometrics, 24(3), 446–468.
#'
#' @examples
#' \donttest{
#' data("dataLatentIV")
#'
#' # function call without any initial parameter values
#' l  <- latentIV(y ~ P, data = dataLatentIV)
#' summary(l)
#'
#' # function call with initial parameter values given by the user
#' l1 <- latentIV(y ~ P, start.params = c("(Intercept)"=2.5, P=-0.5),
#'                data = dataLatentIV)
#' summary(l1)
#'
#' # use own optimization settings (see optimx())
#' # set maximum number of iterations to 50'000
#' l2 <- latentIV(y ~ P, optimx.args = list(itnmax = 50000),
#'                data = dataLatentIV)
#'
#' # print detailed tracing information on progress
#' l3 <- latentIV(y ~ P, optimx.args = list(control = list(trace = 6)),
#'                data = dataLatentIV)
#'
#' # use method L-BFGS-B instead of Nelder-Mead and print report every 50 iterations
#' l4 <- latentIV(y ~ P, optimx.args = list(method = "L-BFGS-B", control=list(trace = 2, REPORT=50)),
#'                data = dataLatentIV)
#'
#' # read out all coefficients, incl auxiliary coefs
#' lat.all.coefs <- coef(l4)
#' # same as above
#' lat.all.coefs <- coef(l4, complete = TRUE)
#' # only main model coefs
#' lat.main.coefs <- coef(l4, complete = FALSE)
#' }
#'
#' @importFrom Formula as.Formula
#' @importFrom stats lm coef model.frame model.matrix sd update setNames
#' @importFrom optimx optimx
#' @importFrom corpcor pseudoinverse
#' @importFrom methods is
#' @importFrom utils modifyList
#' @export
latentIV <- function(formula, data, start.params=c(), optimx.args=list(), verbose=TRUE){
  cl <- match.call()

  # Input checks ------------------------------------------------------------------------------
  check_err_msg(checkinput_latentIV_data(data=data))
  check_err_msg(checkinput_latentIV_formula(formula=formula))
  check_err_msg(checkinput_latentIV_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_latentIV_startparams(start.params=start.params, formula=formula))
  check_err_msg(checkinput_latentIV_optimxargs(optimx.args = optimx.args))
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
                                 theta5 = 0.5, theta6 = 1, theta7 = 0.5,
                                 theta8 = 1)
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

  # Default arguments for optimx
  optimx.default.args <- alist(par = optimx.start.params,
                               fn  = latentIV_LL,
                               m.data.mvnorm = m.data.mvnorm,
                               use.intercept = use.intercept,
                               name.intercept  = name.intercept,
                               name.endo.param = name.endo.param,
                               method = "Nelder-Mead",
                               hessian = TRUE,
                               itnmax  = 10000,
                               control = list(trace = 0,
                                              dowarn = FALSE))
  # Update default args with user given args for optimx
  optimx.call.args <- modifyList(optimx.default.args, val = optimx.args, keep.null = FALSE)

  # Call optimx with assembled args
  res.optimx <- tryCatch(expr = do.call(what = optimx, args = optimx.call.args),
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
  all.estimated.params["theta5"] <- exp(all.estimated.params["theta5"]) / (1 + exp(all.estimated.params["theta5"]))


  # Hessian
  names.hessian <- names(all.estimated.params)
  m.hessian     <- attr(res.optimx, "details")[,"nhatend"][[1]]

  # If optimx failed, single NA is returned as the hessian. Replace it with correctly-sized
  #   matrix of NAs
  if(length(m.hessian)==1 & all(is.na(m.hessian))){
    m.hessian <- matrix(data = NA_real_, nrow = length(names.hessian), ncol = length(names.hessian))
    warning("Hessian could not be derived. Setting all entries to NA.", immediate. = TRUE)
  }

  rownames(m.hessian) <- colnames(m.hessian) <- names.hessian

  # To derive the vcov, the delta method is used for which the diagonal matrix is
  #   already calculated here because the same class is used for copulaCorrection C1 but the diag matrix
  #   depends on the model
  vec.diag        <- rep(1,times=length(all.estimated.params))
  names(vec.diag) <- names(all.estimated.params)

  # use the original coef from fitting the model with optimx because theta5 in all.estimated.params is
  #   already transformed to probabilities (same transformation as in LL)
  opt.t5 <- optimx.estimated.params["theta5"]
  vec.diag["theta5"] <- exp(opt.t5) / ((exp(opt.t5) +1)^2)

  m.delta.diag <- diag(vec.diag)
  rownames(m.delta.diag) <- colnames(m.delta.diag) <- names(vec.diag)

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


  # Put together returns ------------------------------------------------------------------
  res <- new_rendo_latent_IV(call = cl,
                             F.formula=F.formula,
                             mf  = mf,
                             start.params     = optimx.start.params,
                             coefficients     = all.estimated.params,
                             m.delta.diag     = m.delta.diag,
                             names.main.coefs = names.main.model,
                             res.optimx = res.optimx,
                             hessian = m.hessian, fitted.values=fitted,
                             residuals=residuals)
  return(res)
}
