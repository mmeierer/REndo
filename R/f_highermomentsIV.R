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
#'
#' \subsection{Method}{
#'
#' Consider the model:
#' \ifelse{html}{\out{<br><center>Y<sub>t</sub>=&beta;<sub>0</sub> + &beta;<sub>1</sub>X<sub>t</sub>+&alpha;P<sub>t</sub>+&epsilon;<sub>t</sub></center>}}{\deqn{ Y_{t} = \beta_{0}+ \beta_{1}X_{t} + \alpha P_{t}+\epsilon_{t} \hspace{0.3cm} (1) }}
#'
#' \ifelse{html}{\out{<center>P<sub>t</sub>=Z<sub>t</sub>+&nu;<sub>t</sub></center>}}{\deqn{ P_{t} = \gamma Z_{t}+\nu_{t} \hspace{2.5 cm} (2)}}
#'
#' The observed data consist of \ifelse{html}{\out{Y<sub>t</sub>}}{\eqn{Y_{t}}}, \ifelse{html}{\out{X<sub>t</sub>}}{\eqn{X_{t}}} and \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}},
#' while \ifelse{html}{\out{Z<sub>t</sub>}}{\eqn{Z_{t}}}, \ifelse{html}{\out{&epsilon;<sub>t</sub>}}{\eqn{\epsilon_{t}}},
#' and \ifelse{html}{\out{&nu;<sub>t</sub>}}{\eqn{\nu_{t}}} are unobserved. The endogeneity problem arises from
#' the correlation of \ifelse{html}{\out{P<sub>t</sub>}}{\eqn{P_{t}}} with the structural error
#' \ifelse{html}{\out{&epsilon;<sub>t</sub>}}{\eqn{\epsilon_{t}}},
#' since \ifelse{html}{\out{E(&epsilon;&nu;)&ne;0}}{\eqn{E(\epsilon \nu)\neq 0}}.
#' The requirement for the structural and measurement error is to have mean zero, but no restriction is imposed on their distribution.
#'
#' Let \ifelse{html}{\out{S&#773;}}{\eqn{\bar{S}}} be the sample mean of a variable \ifelse{html}{\out{S<sub>t</sub>}}{\eqn{S_{t}}}
#' and \ifelse{html}{\out{G<sub>t</sub>=G(X<sub>t</sub>)}}{\eqn{G_{t} = G(X_{t})}} for any given function \eqn{G} that
#' has finite third own and cross moments. Lewbel(1997) proves that the following instruments can be constructed and used with two-stage least squares to obtain consistent estimates:
#'
#' \ifelse{html}{\out{<div align="center">q<sub>1t</sub>=(G<sub>t</sub>-G&#773;)<span style="float:right;">(3a)</span></div>}}{\deqn{ q_{1t}=(G_{t} - \bar{G})  \hspace{1.6 cm}(3a)}}
#' \ifelse{html}{\out{<div align="center">q<sub>2t</sub>=(G<sub>t</sub>-G&#773;)(P<sub>t</sub>-P&#773;)<span style="float:right;">(3b)</span></div>}}{\deqn{ q_{2t}=(G_{t} - \bar{G})(P_{t}-\bar{P}) \hspace{0.3cm} (3b)}}
#' \ifelse{html}{\out{<div align="center">q<sub>3t</sub>=(G<sub>t</sub>-G&#773;)(Y<sub>t</sub>-Y&#773;)<span style="float:right;">(3c)</span></div>}}{\deqn{ q_{3t}=(G_{t} - \bar{G})(Y_{t}-\bar{Y}) \hspace{0.3cm} (3c)}}
#' \ifelse{html}{\out{<div align="center">q<sub>4t</sub>=(Y<sub>t</sub>-Y&#773;)(P<sub>t</sub>-P&#773;)<span style="float:right;">(3d)</span></div>}}{\deqn{ q_{4t}=(Y_{t} - \bar{Y})(P_{t}-\bar{P}) \hspace{0.3cm} (3d)}}
#' \ifelse{html}{\out{<div align="center">q<sub>5t</sub>=(P<sub>t</sub>-P&#773;)<sup>2</sup><span style="float:right;">(3e)</span></div>}}{\deqn{ q_{5t}=(P_{t}-\bar{P})^{2} \hspace{1.5 cm} (3e)}}
#' \ifelse{html}{\out{<div align="center">q<sub>6t</sub>=(Y<sub>t</sub>-Y&#773;)<sup>2</sup><span style="float:right;">(3f)</span></div>}}{\deqn{ q_{6t}=(Y_{t}-\bar{Y})^{2}\hspace{1.5 cm} (3f)}}
#'
#'
#' Instruments in equations \eqn{3e} and \eqn{3f} can be used only when the measurement and the structural errors are symmetrically distributed.
#' Otherwise, the use of the instruments does not require any distributional assumptions for the errors. Given that the regressors \eqn{G(X) = X}
#' are included as instruments, \eqn{G(X)} should not be linear in \eqn{X} in equation \eqn{3a}.
#'
#' Let small letter denote deviation from the sample mean: \ifelse{html}{\out{s<sub>i</sub> = S<sub>i</sub>-S&#773;}}{\eqn{s_{i} = S_{i}-\bar{S}}}.
#' Then, using as instruments the variables presented in equations \eqn{3} together with \eqn{1} and
#' \ifelse{html}{\out{X<sub>t</sub>}}{\eqn{X_{t}}}, the two-stage-least-squares estimation will provide consistent estimates for the parameters
#' in equation \eqn{1} under the assumptions exposed in Lewbel(1997).
#' }
#'
#' \subsection{Formula parameter}{
#'
#' The \code{formula} argument follows a four part notation:
#'
#' A two-sided formula describing the model (e.g. \code{y ~ X1 + X2 + P}), a single endogenous regressor
#' (e.g. \code{P}), and the exogenous variables from which the internal instrumental variables should
#' be build (e.g. \code{IIV(iiv=y2)}), each part separated by a single vertical bar (\code{|}).
#'
#' The instrumental variables that should be built are specified as (multiple) functions, one for each
#' instrument. This function is \code{IIV} and uses the following arguments:
#'
#' \describe{
#' \item{\code{iiv}}{Which internal instrument to build. One of \code{g, gp, gy, yp, p2, y2} can be chosen.}
#' \item{\code{g}}{Which function \code{g} represents in \code{iiv}.
#' One of \code{x2, x3, lnx, 1/x} can be chosen.
#' Only required if the type of internal instrument demands it.}
#' \item{\code{...}}{
#' The exogenous regressors to build the internal instrument. If more than one is given,
#' separate instruments are built for each. Only required if the type of internal instrument demands it.}
#' }
#'
#' Note that no argument to \code{IIV} is to be supplied as character but as symbols without quotation marks.
#'
#' Optionally, additional external instrumental variables to also include in the instrumental variable
#' regression can be specified. These external instruments have to be already present in the data
#' and are provided as the fourth right-hand side part of the formula, again separated by a vertical bar.
#'
#' See the example section for illustrations on how to specify the \code{formula} parameter.
#' }
#'
#' @template template_text_return_rendoivreg
#'
#' @references Lewbel A (1997). “Constructing Instruments for Regressions with Measurement Error When No Additional Data are Available, With an Application to Patents and R&D.” Econometrica, 65(5), 1201–1213.
#'
#' @examples
#' data("dataHigherMoments")
#' # P is the endogenous regressor in all examples
#'
#' # 2 IVs with g*p, g=x^2, separately for each regressor X1 and X2.
#' hm <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1, X2),
#'                       data = dataHigherMoments)
#' # same as above
#' hm <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1) +
#'                                   IIV(iiv=gp, g=x2, X2),
#'                       data = dataHigherMoments)
#'
#' # 3 different IVs
#' hm <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2) + IIV(iiv=yp) +
#'                                   IIV(iiv=g,g=x3,X1),
#'                       data = dataHigherMoments)
#'
#' # use X2 as external IV
#' hm <- higherMomentsIV(y~X1+P|P|IIV(iiv=y2)+IIV(iiv=g,g=lnx,X1)| X2,
#'                       data = dataHigherMoments)
#' summary(hm)
#'
#' @importFrom Formula as.Formula
#' @importFrom stats update.formula reformulate model.response
#' @importFrom AER ivreg
#' @export
higherMomentsIV <- function(formula, data, verbose=TRUE){

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
    message("The following internal instruments were built: ", paste0(vec.desc.IIVs, collapse = ", "),".")
    message("Fitting an instrumental variable regression with model ", model.desc,".")
  }

  res.ivreg <- ivreg(formula = F.ivreg, data = df.data.ivreg)

  # Return ------------------------------------------------------------------------------------------
  return(new_rendo_ivreg(F.formula = F.formula,
                         res.ivreg = res.ivreg,
                         call = cl))
}
