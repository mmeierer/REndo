#' @title  Fitting Linear Models Endogenous Regressors using Gaussian Copula
#' @description
#' Fits linear models with continuous or discrete endogenous regressors (or a mixture of both) using Gaussian copulas, as presented in Park and Gupta (2012).
#' This is a statistical technique to address the endogeneity problem where no external instrumental variables are needed.
#' The important assumption of the model is that the endogenous variables should NOT be normally distributed, if continuous, preferably with a skewed distribution.
#'
#' @template template_param_formuladataverbose
#' @param num.boots Number of bootstrapping iterations. Defaults to 1000.
#' @param ... Arguments for the log-likelihood optimization function in the case of a single continuous endogenous
#'  regressor. Ignored with a warning otherwise.
#' \describe{
#' \item{start.params}{A named vector containing a set of parameters to use in the first optimization iteration.
#' The names have to correspond exactly to the names of the components specified in the \code{formula} parameter.
#' If not provided, a linear model is fitted to derive them.}
#' \item{optimx.args}{A named list of arguments which are passed to \code{\link[optimx]{optimx}}. This allows users to tweak optimization settings to their liking.}
#' }
#'
#' @details
#'
#' \subsection{Method}{
#'
#' The underlying idea of the joint estimation method is that using information contained in the observed data,
#' one selects marginal distributions for the endogenous regressor and the structural error term, respectively.
#' Then, the copula model enables the construction of a flexible multivariate joint distribution allowing a wide range
#' of correlations between the two marginals.
#'
#'
#' Consider the model:
#' \ifelse{html}{\out{<br><center>Y<sub>t</sub>=&beta;<sub>0</sub>+&beta;<sub>1</sub>P<sub>t</sub>+&beta;<sub>2</sub>X<sub>t</sub>+&epsilon;<sub>t</sub></center>}}{\deqn{Y_{t}=\beta_{0}+ \beta_{1} P_{t} + \beta_{2} X_{t} + \epsilon_{t}}}
#'
#' where \eqn{t=1,..,T} indexes either time or cross-sectional units, \ifelse{html}{\out{Y<sub>t</sub>}}{\eqn{Y_{t}}} is a \eqn{1x1} response variable,
#' \ifelse{html}{\out{X<sub>t</sub>}}{\eqn{X_{t}}} is a \eqn{kxn} exogenous regressor,
#' \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}} is a \eqn{kx1} continuous endogenous regressor,
#' \ifelse{html}{\out{&epsilon;<sub>t</sub>}}{\eqn{\epsilon_{t}}} is a normally distributed structural error term with mean zero and
#' \ifelse{html}{\out{E(&epsilon;<sup>2</sup>)=&sigma;<sub>&epsilon;</sub><sup>2</sup>}}{\eqn{E(\epsilon^{2})=\sigma^{2}_{\epsilon}}},
#' \eqn{\alpha} and \eqn{\beta} are model parameters.
#'
#' The marginal distribution of the endogenous regressor \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}} is obtained using the Epanechnikov
#' kernel density estimator (Epanechnikov, 1969), as below:
#' \ifelse{html}{\out{<br><center>h&#770;(p)=1/(T&#183;b) &sum;(K&#183;((p-P<sub>t</sub>)/b))</center>}}{\deqn{\hat{h}(p)=\frac{1}{T\cdot b}\sum_{t=1}^{T}K\left(\frac{p-P_{t}}{b}\right)}}
#'
#' where \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}} is the endogenous regressor,
#' \ifelse{html}{\out{K(x)=0.75&#183;(1-x<sup>2</sup>)&#183;I(|x|<=1)}}{\eqn{K(x)=0.75\cdot(1-x^{2})\cdot I(\|x\|<=1)}}
#' and the bandwidth \eqn{b} is the one proposed by Silverman (1986),
#' and is equal to \ifelse{html}{\out{b=0.9&#183;T<sup>-1.5</sup>&#183;min(s, IQR/1.34)}}{\eqn{b=0.9\cdot T^{-1/5}\cdot min(s,IQR/1.34)}}.
#' \eqn{IQR} is the interquartile range while \eqn{s} is the data sample standard deviation
#' and \eqn{T} is the number of time periods observed in the data.
#' After obtaining the joint distribution of the error term and the continuous endogenous regressor, the model parameters are estimated using
#' maximum likelihood estimation.
#'
#' The additional parameters used during model fitting and printed in \code{summary} hence are:
#' \describe{
#' \item{\code{rho}}{The correlation between the endogenous regressor and the error.}
#' \item{\code{sigma}}{The variance of the model's error.}
#' }
#'
#'
#' With more than one continuous endogenous regressor or an endogenous discrete regressor, an alternative approach to the
#' estimation using Gaussian copula should be applied. This approach is similar to the control function approach (Petrin and Train, 2010).
#' The core idea is to apply OLS estimation on the original set of explanatory variables in the model equation above, plus an additional regressor
#' \ifelse{html}{\out{P<sub>t</sub>&#42;=&Phi;<sup>-1</sup>(H(P<sub>t</sub>))}}{\eqn{P_{t}^{*}=\Phi^{-1}(H(P_{t}))}}.
#' Here, \ifelse{html}{\out{H(P<sub>t</sub>)}}{\eqn{H(P_{t})}} is the marginal distribution of the endogenous regressor \eqn{P}.
#' Including this regressor solves the correlation between the endogenous regressor and the structural error, \eqn{\epsilon},
#' OLS providing consistent parameter estimates. Due to identification problems, the discrete endogenous regressor cannot have a binomial
#' distribution.
#'
#' Hence, only in the case of a single continuous endogenous regressor maximum likelihood estimation is used.
#' In all other cases, augmented OLS based on Gaussian copula is applied. This includes cases of multiple endogenous regressors
#' of both discrete and continuous distributions.
#'
#' In the case of discrete endogenous regressors, a random seed needs to be assigned because the marginal distribution function of
#' the endogenous regressor is a step function in this case. This means that the value of \ifelse{html}{\out{P&#42;}}{\eqn{P^{*}}}
#' lies between 2 values, \ifelse{html}{\out{&Phi;<sup>-1</sup>(H(P<sub>t</sub>-1))}}{\eqn{\Phi^{-1}(H(P_{t}-1))}} and
#' \ifelse{html}{\out{&Phi;<sup>-1</sup>(H(P<sub>t</sub>))}}{\eqn{\Phi^{-1}(H(P_{t}))}}.
#' However, the reported upper and lower bounds of the 95\% bootstrapped confidence interval gives indication of the variance of the estimates.
#'}
#' Since the inference procedure in both cases, augmented OLS and maximum likelihood, occurs in two stages (first the empirical distribution of the endogenous
#' regressor is computed and then used in constructing the likelihood function), the standard errors are not correct. Therefore, in both cases, the standard errors
#' and the confidence intervals are obtained based on the sampling distributions resulted from bootstrapping. Since the distribution of the bootstrapped parameters
#' is highly skewed, we report the percentile confidence intervals. Moreover, the variance-covariance matrix is also computed based on the bootstrapped
#' parameters, and not based on the Hessian.
#'
#'
#'
#'\subsection{Formula parameter}{
#' The \code{formula} argument follows a two part notation:
#'
#' A two-sided formula describing the model (e.g. \code{y ~ X1 + X2 + P}) to be estimated and a
#' second right-hand side part in which the endogenous regressors and their distributional
#' assumptions are indicated (e.g. \code{continuous(P)}). These two parts are separated by a single vertical bar (\code{|}).
#' In the second part, the special functions \code{continuous}, \code{discrete}, or a combination
#' of both, are used to indicate the endogenous regressors and their respective distribution.
#' Both functions use the \code{...} parameter in which the respective endogenous regressors is specified.
#'
#' Note that no argument to \code{continuous} or \code{discrete} is to be supplied as character
#' but as symbols without quotation marks.
#'
#' See the example section for illustrations on how to specify the \code{formula} parameter.
#'}
#'
#' @return
#' For all cases, an object of classes \code{rendo.copula.correction}, \code{rendo.boots}, and \code{rendo.base} is returned
#' which is a list and contains the following components:
#' \item{formula}{The formula given to specify the fitted model.}
#' \item{terms}{The terms object used for model fitting.}
#' \item{model}{The model.frame used for model fitting.}
#' \item{coefficients}{A named vector of all coefficients resulting from model fitting.}
#' \item{names.main.coefs}{a vector specifying which coefficients are from the model. For internal usage.}
#' \item{names.vars.continuous}{The names of the continuous endogenous regressors.}
#' \item{names.vars.discrete}{The names of the discrete endogenous regressors.}
#' \item{fitted.values}{Fitted values at the found solution.}
#' \item{residuals}{The residuals at the found solution.}
#' \item{boots.params}{The bootstrapped coefficients.}
#'
#' For the case of a single continuous endogenous regressor, the returned object further
#' contains the following components:
#' \item{start.params}{A named vector with the initial set of parameters used to optimize the log-likelihood function.}
#' \item{res.optimx}{The result object returned by the function \code{optimx} after optimizing the log-likelihood function.}
#'
#' For all other cases, the returned object further contains the following component:
#' \item{res.lm.real.data}{The linear model fitted on the original data together with generated p.star data.}
#'
#' The function \code{summary} can be used to obtain and print a summary of the results.
#' Depending on the returned object, the generic accessor functions \code{coefficients}, \code{fitted.values},
#' \code{residuals}, \code{vcov}, \code{logLik}, \code{AIC}, \code{BIC}, and \code{nobs} are available.
#'
#' @seealso \code{\link[REndo:summary.rendo.copula.correction]{summary}} for how fitted models are summarized
#' @seealso \code{\link[REndo:vcov.rendo.boots]{vcov}} for how the variance-covariance matrix is derived
#' @seealso \code{\link[REndo:confint.rendo.boots]{confint}} for how confidence intervals are derived
#' @seealso \code{\link[optimx]{optimx}} for possible elements of parameter \code{optimx.arg}
#'
#' @references
#' Park, S. and Gupta, S., (2012), "Handling Endogenous Regressors by Joint Estimation Using Copulas", Marketing Science, 31(4), 567-86.
#'
#' Epanechnikov V (1969). "Nonparametric Estimation of a Multidimensional Probability Density." Teoriya veroyatnostei i ee primeneniya, 14(1), 156–161.
#
#' Silverman B (1986). "Density Estimation for Statistics and Data Analysis". CRC Monographs on Statistics and Applied Probability. London: Chapman & Hall.
#'
#' Petrin A, Train K (2010). "A Control Function Approach to Endogeneity in Consumer Choice Models." Journal of Marketing Research, 47(1), 3–13.
#'
#' @examples
#' data("dataCopCont")
#' data("dataCopCont2")
#' data("dataCopDis")
#' data("dataCopDis2")
#' data("dataCopDisCont")
#'
#' \donttest{
#' \dontrun{
#' # Single continuous: log-likelihood optimization
#' c1 <- copulaCorrection(y~X1+X2+P|continuous(P), num.boots=10, data=dataCopCont)
#' # same as above, with start.parameters and number of bootstrappings
#' c1 <- copulaCorrection(y~X1+X2+P|continuous(P), num.boots=10, data=dataCopCont,
#'                        start.params = c("(Intercept)"=1, X1=1, X2=-2, P=-1))
#'
#' # All following examples fit linear model with Gaussian copulas
#'
#' # 2 continuous endogenous regressors
#' c2 <- copulaCorrection(y~X1+X2+P1+P2|continuous(P1, P2),
#'                         num.boots=10, data=dataCopCont2)
#' # same as above
#' c2 <- copulaCorrection(y~X1+X2+P1+P2|continuous(P1)+continuous(P2),
#'                         num.boots=10, data=dataCopCont2)
#'
#' # single discrete endogenous regressor
#' d1 <- copulaCorrection(y~X1+X2+P|discrete(P), num.boots=10, data=dataCopDis)
#'
#' # two discrete endogenous regressor
#' d2 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+discrete(P2),
#'                         num.boots=10, data=dataCopDis2)
#' # same as above but less bootstrap runs
#' d2 <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1, P2), num.boots = 10,
#'                        data=dataCopDis2)
#'
#' # single discrete, single continuous
#' cd <- copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+continuous(P2),
#'                         num.boots=10, data=dataCopDisCont)
#'
#' # For single continuous only: use own optimization settings (see optimx())
#' # set maximum number of iterations to 50'000
#' res.c1 <- copulaCorrection(y~X1+X2+P|continuous(P),
#'                            optimx.args = list(itnmax = 50000),
#'                             num.boots=10, data=dataCopCont)
#'
#' # print detailed tracing information on progress
#'  res.c1 <- copulaCorrection(y~X1+X2+P|continuous(P),
#'                             optimx.args = list(control = list(trace = 6)),
#'                              num.boots=10, data=dataCopCont)
#'
#' # use method L-BFGS-B instead of Nelder-Mead and print report every 50 iterations
#'  res.c1 <- copulaCorrection(y~X1+X2+P|continuous(P),
#'                             optimx.args = list(method = "L-BFGS-B",
#'                                                control=list(trace = 2, REPORT=50)),
#'                              num.boots=10, data=dataCopCont)
#'
#' # For coef(), the parameter "complete" determines if only the
#' # main model parameters or also the auxiliary coefficients are returned
#'
#' c1.all.coefs <- coef(res.c1) # also returns rho and sigma
#' # same as above
#' c1.all.coefs <- coef(res.c1, complete = TRUE)
#'
#' # only main model coefs
#' c1.main.coefs <- coef(res.c1, complete = FALSE)
#'
#'}}
#'
#' @importFrom Formula as.Formula
#' @export
copulaCorrection <- function(formula, data, num.boots=1000, verbose=TRUE, ...){
  # Catch stuff ------------------------------------------------------------------------------------------------
  cl <- quote(match.call())
  l.ellipsis <- list(...)

  # Input checks -----------------------------------------------------------------------------------------------
  check_err_msg(checkinput_copulacorrection_formula(formula=formula))
  check_err_msg(checkinput_copulacorrection_data(data=data))
  check_err_msg(checkinput_copulacorrection_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_copulacorrection_numboots(num.boots=num.boots))
  check_err_msg(checkinput_copulacorrection_verbose(verbose=verbose))


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
                                 num.boots = num.boots,
                                 name.var.continuous = names.vars.continuous,
                                 verbose=verbose, cl=cl), # cl supplied to create return object
                            l.ellipsis))
  }else{
    # All other cases use pstar data + lm
    res <- do.call(copulaCorrection_linearmodel,
                   c(list(F.formula = F.formula, data = data,verbose=verbose,
                          cl=cl, # cl supplied to create return object
                          num.boots = num.boots,
                          names.vars.continuous = names.vars.continuous,
                          names.vars.discrete   = names.vars.discrete),
                   l.ellipsis))
  }

  return(res)
}

