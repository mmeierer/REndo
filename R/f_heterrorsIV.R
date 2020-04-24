#' @title  Fitting Linear Models with Endogenous Regressors using Heteroskedastic Covariance Restrictions
#' @description  This function estimates the model parameters and associated standard errors for a
#' linear regression model with one endogenous regressor. Identification is achieved
#' through heteroscedastic covariance restrictions within the triangular system as proposed in Lewbel(2012).
#'
#' @template template_param_formuladataverbose
#'
#' @details
#' \subsection{Method}{
#' The method proposed in Lewbel(2012) identifies structural parameters in regression models with endogenous
#' regressors by means of variables that are uncorrelated with the product of heteroskedastic errors.
#' The instruments are constructed as simple functions of the model's data. The method can be applied when
#' no external instruments are available or to supplement external instruments to improve the efficiency of the
#' IV estimator.
#' Consider the model in the equation:
#' \ifelse{html}{\out{<br><center>y<sub>t</sub>=&beta;<sub>0</sub>+&beta;<sub>1</sub>P<sub>t</sub>+&beta;<sub>2</sub>X<sub>t</sub>+&epsilon;<sub>t</sub></center>}}{\deqn{ y_{t}=\beta_{0}+ \beta_{1} P_{t} + \beta_{2} X_{t} + \epsilon_{t}}}
#'
#' where \eqn{t=1,..,T} indexes either time or cross-sectional units.The endogeneity problem arises from the correlation of
#' \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}} and \ifelse{html}{\out{&epsilon;<sub>t</sub>}}{\eqn{\epsilon_{t}}}.
#' As such: \ifelse{html}{\out{P<sub>t</sub> = Z<sub>t</sub>+&nu;<sub>t</sub>,}}{\deqn{P_{t}=\gamma Z_{t}+\nu_{t},}}
#' where \ifelse{html}{\out{Z<sub>t</sub>}}{\eqn{Z_{t}}} is a subset of variables in \ifelse{html}{\out{X<sub>t</sub>}}{\eqn{X_{t}}}.
#'
#' The errors, \ifelse{html}{\out{&epsilon;}}{\eqn{\epsilon}} and \ifelse{html}{\out{&nu;}}{\eqn{\nu}}, may be correlated with each other.
#' Structural parameters are identified by an ordinary two-stage least squares regression of \eqn{Y} on \eqn{X} and \eqn{P}, using \eqn{X} and \eqn{[Z-E(Z)]\nu} as instruments.
#' A vital assumption for identification is that \ifelse{html}{\out{cov(Z,&nu;<sup>2</sup>)&ne;0}}{\eqn{cov(Z,\nu^2) \neq 0}}.
#' The strength of the instrument is proportional to the covariance of \ifelse{html}{\out{(Z-Z&#773;)&nu;}}{\eqn{(Z-\bar{Z}) \nu}} with \eqn{\nu}, which corresponds to
#' the degree of heteroskedasticity of \eqn{\nu} with respect to \eqn{Z} (Lewbel 2012).
#'
#' The assumption that the covariance between \eqn{Z} and the squared error is different from zero can be empirically tested (this is checked in the background when calling the
#' function). If it is zero or close to zero, the instrument is weak, producing imprecise estimates, with large standard errors.
#'}
#'
#'\subsection{Formula parameter}{
#' The \code{formula} argument follows a four part notation:
#'
#' A two-sided formula describing the model (e.g. \code{y ~ X1 + X2 + P}), a single endogenous regressor
#' (e.g. \code{P}), and the exogenous variables from which the internal instrumental variables should
#' be build (e.g. \code{IIV(X1) + IIV(X2)}), each part separated by a single vertical bar (\code{|}).
#'
#' The instrumental variables that should be built are specified as (multiple) functions, one for each
#' instrument. This function is \code{IIV} and uses the following arguments:
#'
#'\describe{
#'\item{\code{...}}{The exogenous regressors to build the internal instruments from.
#'If more than one is given, separate instruments are built for each.}
#'}
#' Note that no argument to \code{IIV} is to be supplied as character but as symbols without quotation marks.
#'
#' Optionally, additional external instrumental variables to also include in the instrumental variable
#' regression can be specified. These external instruments have to be already present in the data
#' and are provided as the fourth right-hand side part of the formula, again separated by a vertical bar.
#'
#' See the example section for illustrations on how to specify the \code{formula} parameter.
#'}
#'
#' @examples
#' data("dataHetIV")
#' # P is the endogenous regressor in all examples
#' # X1 generates a weak instrument but for the examples
#' # this is ignored
#'
#' # 2 IVs, one from X1, one from X2
#' het <- hetErrorsIV(y~X1+X2+P|P|IIV(X1)+IIV(X2), data=dataHetIV)
#' # same as above
#' het <- hetErrorsIV(y~X1+X2+P|P|IIV(X1,X2), data=dataHetIV)
#'
#' # use X2 as an external IV
#' het <- hetErrorsIV(y~X1+P|P|IIV(X1)|X2, data=dataHetIV)
#'
#' summary(het)
#'
#' @template template_text_return_rendoivreg
#'
#' @references Lewbel, A. (2012). Using Heteroskedasticity to Identify and Estimate Mismeasured and Endogenous Regressor Models, Journal of Business & Economic Statistics, 30(1), 67-80.
#' @references Angrist, J. and Pischke, J.S. (2009). Mostly Harmless Econometrics: An Empiricists Companion, Princeton University Press.
#'
#' @importFrom Formula as.Formula
#' @importFrom stats model.frame residuals reformulate update.formula terms
#' @importFrom AER ivreg
#' @export
hetErrorsIV <- function(formula, data, verbose=TRUE){

  cl <- match.call()

  # Input checks ----------------------------------------------------------------------------------------------------
  check_err_msg(checkinput_heterrors_formula(formula=formula))
  check_err_msg(checkinput_heterrors_data(data=data))
  check_err_msg(checkinput_heterrors_formulaVSdata(formula=formula, data=data))
  check_err_msg(checkinput_heterrors_verbose(verbose=verbose))


  # Read out the args from all IIV functions in the formula ---------------------------------------------------------
  F.formula <- Formula::as.Formula(formula)

  # Derive instruments ----------------------------------------------------------------------------------------------

  l.res.IIVs <- hetErrorsIV_IIV(F.formula=F.formula, data=data, verbose = verbose)

  # Read out IV data and desc
  df.data.internal.instr   <- l.res.IIVs$df.IIV
  vec.desc.hetii.s         <- l.res.IIVs$desc.IIV


  # Instrumental variable regression ---------------------------------------------------------------------------------

  # Get formula and pretty model description
  l.F        <- formula_build_ivreg(F.formula=F.formula, df.data.iv=df.data.internal.instr,
                                    vec.desc.IVs = vec.desc.hetii.s)
  F.ivreg    <- l.F[["F.ivreg"]]
  model.desc <- l.F[["model.desc"]]

  # Put data together: user data +instruments
  df.data.ivreg <- cbind(data, df.data.internal.instr)

  if(verbose){
    message("The following internal instruments were built: ", paste0(vec.desc.hetii.s, collapse = ", "),".")
    message("Fitting an instrumental variable regression with model ", model.desc, ".")
  }

  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  return(new_rendo_ivreg(F.formula=F.formula, call = cl,
                         res.ivreg = res.ivreg))
}


# lm for residuals: RHS1 ~ RHS2
# instruments x for x-mean(x): x from RHS3
# IVreg formula: LHS1 ~ RHS1 + RHS2 | RHS2 + instruments (+ RHS4, if present)
