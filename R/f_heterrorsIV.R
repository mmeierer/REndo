#' @title  Fitting Linear Models with Endogenous Regressors using Heteroskedastic Covariance Restictions
#' @description  This function estimates the model parameters and associated standard errors for a
#' linear regression model with one or more endogenous regressors. Identification is achieved
#' through heteroscedastic covariance restrictions within the triangular system as proposed in Lewbel(2012).
#'
#' @template template_param_formuladataverbose
#'
#' @details
#' ?? ANY DETAILS ABOUT THE MATH MODEL ??
#'
#' The \code{formula} argument has the following notation:
#' A two-sided formula object describing the MODEL (??), a single endogenous regressor, and
#' the internal instrumental variables to be built, each part separated by a single vertical bar (\code{|}).
#' The MODEL (??) part consists of the response on the left of the \code{~} operator and the term labels
#' on the right-hand side. The sole endogenous regressor is specified in the second right-hand side part
#' of the formula by separating it with a vertical bar from the MODEL(??). The instrumental variables
#' that should be built are specified as (multiple) functions, one for each instrument, and
#' separated from the endogenous regressor by a vertical bar. The function to build the
#' internal variables is \code{IIV} and uses the following arguments:
#'
#'\describe{
#'\item{\code{...}}{The exogenous regressors to build the internal instruments from.
#'If more than one is given, separate instruments are built for each.}
#'}
#'
#' Note that no argument to \code{IIV} is to be supplied as character but as symbols without quotation marks.
#'
#' Optionally, additional external instrumental variables to be used during the instrumental variable
#' regression and already present in the data can be specified, again separated by a vertical bar,
#' as the fourth right-hand side part.
#'
#' See the example section for illustrations on how to specify the \code{formula} parameter.
#'
#' @examples
#' data("dataHetIV")
#' # P is the endogenous regressor in all examples
#'
#' # 2 IVs, one from X1, one from X2
#' het <- hetErrorsIV(y~X1+X2+P|P|IIV(X1)+IIV(X2), data= dataHetIV)
#' # same as above
#' het <- hetErrorsIV(y~X1+X2+P|P|IIV(X1, X2), data= dataHetIV)
#'
#' # use X2 as external IV
#' het <- hetErrorsIV(y~X1+P|P|IIV(X1) | X2, data= dataHetIV)
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

  formula.rhs.iiv  <- 3
  formula.rhs.eiv  <- 4

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
