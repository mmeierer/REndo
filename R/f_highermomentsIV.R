#' @title  Fitting Linear Models with Endogenous Regressors using Lewbel's Higher Moments Approach
#'
#' @description
#' Fits linear models with one endogenous regressor using internal instruments built using the approach described in
#' Lewbel A. (1997). This is a statistical technique to address the endogeneity problem where no external instrumental
#' variables are needed. The implementation allows the incorporation of external instruments if available.
#' An important assumption for identification is that the endogenous variable has a skewed distribution.
#'
#' @template template_param_formuladataverbose
#'
#' @details
#' Consider the model below:
#' \deqn{ Y_{t} = \beta_{0}+ \gamma^{'}X_{t} + \alpha P_{t}+\epsilon_{t} \hspace{0.3cm} (1) }{Y_t = b0 + gamma' * X_t + alpha * P_t + eps_t}
#' \deqn{ P_{t} = Z_{t}+\nu_{t} \hspace{2.5 cm} (2)}{P_t = Z + nu_t}
#'
#' The observed data consist of \eqn{Y_{t}}{Y}, \eqn{X_{t}}{X} and \eqn{P_{t}}{P_t}, while \eqn{Z_{t}}{Z_t}, \eqn{\epsilon_{t}}{eps_t}, and \eqn{\nu_{t}}{nu_t}
#' are unobserved. The endogeneity problem arises from the correlation of \eqn{P_{t}}{P} with the structural error, \eqn{\epsilon_{t}}{eps_t},
#' since \eqn{E(\epsilon \nu)\neq 0}{E(eps * nu) > 0}.
#' The requirement for the structural and measurement error is to have mean zero, but no restriction is imposed on their distribution.
#'
#' Let \eqn{\bar{S}}{mean(S)} be the sample mean of a variable \eqn{S_{t}}{S_t} and \eqn{G_{t} = G(X_{t})}{G = G(X)} for any given function \eqn{G}{G} that
#' has finite third own and cross moments. Lewbel(1997) proves that the following instruments can be constructed and used with 2SLS to obtain consistent estimates:
#' \deqn{ q_{1t}=(G_{t} - \bar{G})  \hspace{1.6 cm}(3a)}{q1 = G_t - mean(G)}
#' \deqn{ q_{2t}=(G_{t} - \bar{G})(P_{t}-\bar{P}) \hspace{0.3cm} (3b) }{q2 = (G_t - mean(G))(P_t -mean(P))}
#' \deqn{ q_{3t}=(G_{t} - \bar{G})(Y_{t}-\bar{Y}) \hspace{0.3cm} (3c)}{q3 = (G_t - mean(G))(Y_t - mean(Y))}
#' \deqn{ q_{4t}=(Y_{t} - \bar{Y})(P_{t}-\bar{P}) \hspace{0.3cm} (3d)}{q4 = (Y_t - mean(Y))(P_t - mean(P))}
#' \deqn{ q_{5t}=(P_{t}-\bar{P})^{2} \hspace{1.5 cm} (3e) }{q5 = (P_t - mean(P))^2}
#' \deqn{ q_{6t}=(Y_{t} - \bar{Y})^{2}\hspace{1.5 cm} (3f)}{q6 = (Y_t - mean(Y))^2}
#'
#' Instruments in equations \code{3e} and \code{3f} can be used only when the measurement and the structural errors are symmetrically distributed.
#' Otherwise, the use of the instruments does not require any distributional assumptions for the errors. Given that the regressors \eqn{G(X) = X}
#' are included as instruments, \eqn{G(X)} should not be linear in \eqn{X} in equation \code{3a}.
#'
#' Let small letter denote deviation from the sample mean: \eqn{s_{i} = S_{i}-\bar{S}}{s_i = S_i - mean(S)}. Then, using as instruments the variables presented in
#' equations \code{3} together with \code{1} and \eqn{X_{t}}{X}, the two-stage-least-squares estimation will provide consistent estimates for the parameters
#' in equation \code{1} under the assumptions exposed in Lewbel(1997).
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
#' \describe{
#' \item{\code{iiv}}{Which internal instrument to build. One of \code{g, gp, gy, yp, p2, y2} can be choosen.}
#' \item{\code{g}}{Which function \code{g} represents in \code{iiv}.
#' One of \code{x2, x3, lnx, 1/x} can be choosen.
#' Only required if the type of internal instrument demands it.}
#' \item{\code{...}}{The exogenous regressors to build the internal instrument.
#' If more than one is given, separate instruments are built for each.
#' Only required if the type of internal instrument demands it.}
#' }
#'
#' Note that no argument to \code{IIV} is to be supplied as character but as symbols without quotation marks.
#'
#' Optionally, additional external instrumental variables to be used during the instrumental variable
#' regression and already present in the data can be specified, again separated by a vertical bar,
#' as the fourth right-hand side part.
#'
#' See the example section for illustrations on how to specify the \code{formula} parameter.
#'
#' @template template_text_return_rendoivreg
#'
#' @examples
#' data("dataHigherMoments")
#' # P is the endogenous regressor in all examples
#'
#' # 2 IVs with g*p, g=x^2, separately for each regressor X1 and X2.
#' hm <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1, X2),
#'                       data = dataHigherMoments)
#'
#' # same as above
#' hm <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1) + IIV(iiv=gp, g=x2, X2),
#'                       data = dataHigherMoments)
#'
#' # 3 different IVs
#' hm <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2)+IIV(iiv=yp)+IIV(iiv=g,g=x3,X1),
#'                       data = dataHigherMoments)
#'
#' # use X2 as external IV
#' hm <- higherMomentsIV(y~X1+P|P|IIV(iiv=y2)+IIV(iiv=g,g=lnx,X1) | X2,
#'                       data = dataHigherMoments)
#' summary(hm)
#'
#' @importFrom Formula as.Formula
#' @importFrom stats update.formula reformulate model.response
#' @importFrom AER ivreg
#' @export
higherMomentsIV <- function(formula, data, verbose=TRUE){

# *** TODO: fail raluca check: underlying assumptions not satisfied - stop()
  cl <- match.call()

  # Input checks -------------------------------------------------------------------------------------
  check_err_msg(checkinput_highermomentsiv_formula(formula=formula))
  check_err_msg(checkinput_highermomentsiv_data(data=data))
  check_err_msg(checkinput_highermomentsiv_formulaVSdata(formula=formula, data=data))
  check_err_msg(checkinput_highermomentsiv_verbose(verbose=verbose))

  # Read out the args from all IIV functions in the formula ------------------------------------------
  F.formula  <- as.Formula(formula)
  l.IIV.args <- formula_readout_special(F.formula = F.formula, name.special = "IIV",
                                        from.rhs = 3, params.as.chars.only = FALSE)
  # str(l.IIV.args)


  # Execute the IIV function and pass in the read out arguments --------------------------------------

  # check first that iiv and g are not present more than once
  check_err_msg(checkinput_highermomentsiv_docalllist(l.args = l.IIV.args))

  l.res.IIVs <- lapply(l.IIV.args, function(iiv.args){
                        # Add data and formula to the args here instead before to every sublist to avoid copy
                        do.call(what = higherMomentsIV_IIV,
                                args = c(alist(data=data, F.formula=F.formula),
                                         iiv.args))})

  # Read out IV data and desc
  l.data.IIVs   <- lapply(l.res.IIVs, "[[","df.IIV")
  vec.desc.IIVs <- unique(unlist(lapply(l.res.IIVs, "[[","desc.IIV")))

  # Bind results in list together to single data.frame
  df.data.IIVs <- do.call(cbind, l.data.IIVs)


  # Call ivreg ---------------------------------------------------------------------------------------

  # Get formula
  l.F <- formula_build_ivreg(F.formula=F.formula, df.data.iv=df.data.IIVs, vec.desc.IVs = vec.desc.IIVs)
  F.ivreg     <- l.F[["F.ivreg"]]
  model.desc  <- l.F[["model.desc"]]

  # Put data together: user data + internal instruments
  df.data.ivreg <- cbind(data, df.data.IIVs)

  if(verbose){
    # print(head(df.data.ivreg))
    message("The following internal instruments were built: ", paste0(vec.desc.IIVs, collapse = ", "),".")
    message("Fitting an instrumental variable regression with model ", model.desc,".")
  }

  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  return(new_rendo_ivreg(F.formula = F.formula,
                         res.ivreg = res.ivreg,
                         call = cl))
}
