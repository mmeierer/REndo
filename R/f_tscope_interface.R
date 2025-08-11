#' @title Two-Stage Copula Generated Regressor Approach (2sCOPE)
#' @aliases tscope
#'
#' @description
#' Fits linear models with potentially endogenous regressors by the two-stage copula generated regressor approach of
#' Yang, Qian & Xie (2024).  The procedure (i) transforms each endogenous regressor with its empirical CDF followed by
#' the standard-normal quantile and, if additional exogenous regressors are present, (ii) residualises these transforms
#' on the exogenous part.  A final linear model is then estimated with the transformed / residualised regressors.
#'
#' @template template_param_formuladataverbose
#' @param num.boots Integer.  Number of bootstrap replications for inference (default = 1000).
#'
#' @details
#' \subsection{Method}{
#' \describe{
#'   \item{Stage 1 — Transformation and Residualisation}{
#'     \itemize{
#'       \item Each endogenous regressor \eqn{P} is transformed to \eqn{P^{*}= \Phi^{-1}(\hat F_{P}(P))}.
#'       \item If exogenous regressors \eqn{W} exist the transformed endogenous variables are regressed on the
#'             transformed \eqn{W^{*}} and the residuals are retained.
#'     }
#'   }
#'   \item{Stage 2 — Final model}{A linear model is estimated for the outcome on all main predictors and either
#'         the transformed endogenous variables (no \eqn{W}) or their residuals (with \eqn{W}).}
#' }}
#'
#' \subsection{Inference}{
#' Classical standard errors are invalid because of the generated-regressor problem.
#' The function therefore runs parametric bootstrapping: the data are resampled \code{num.boots} times
#' and the whole two-stage procedure is re-estimated.  Standard errors, confidence intervals, and
#' the variance–covariance matrix are taken from the empirical distribution of the bootstrapped coefficients.
#' }
#'
#' \subsection{Formula parameter}{
#' A two-part formula separated by a single vertical bar (\code{|}).
#' The first part holds the dependent variable and all predictors,
#' the second part lists the subset that is treated as endogenous, e.g.
#' \code{y ~ x1 + x2 + p1 + p2 | p1 + p2}.
#' }
#'
#' @return
#' An object of classes \code{rendo.tscope}, \code{rendo.boots} and \code{rendo.base} containing
#' \describe{
#'   \item{call}{Matched call.}
#'   \item{F.formula}{Formula object used.}
#'   \item{mf}{Model frame.}
#'   \item{coefficients}{Named vector of main-model coefficients.}
#'   \item{names.main.coefs}{Names of the main coefficients.}
#'   \item{fitted.values}{Fitted values from the main model.}
#'   \item{residuals}{Main-model residuals.}
#'   \item{boots.params}{Bootstrapped coefficient matrix.}
#'   \item{tscope_model}{Underlying \code{lm} object from Stage 2.}
#'   \item{details}{Diagnostics and intermediate results.}
#' }
#'
#' @references
#' Yang K., Qian L. & Xie Y. (2024) “Addressing Endogeneity using a Two-Stage Copula Generated Regressor Approach.”
#' \emph{Journal of Business & Economic Statistics}.
#'
#' @seealso \code{\link[Formula]{Formula}}, \code{\link[stats]{lm}},
#'          \code{\link[REndo:copulaCorrection]{copulaCorrection}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' data("dataTscope")
#' 
#' # Fit the 2sCOPE model treating 'p' as endogenous
#' # Formula: y ~ p + w | p
#' tscope_fit <- tscope(y ~ p + w | p, data = dataTscope, num.boots = 100, verbose = TRUE)
#' 
#' # Display summary with bootstrap-based standard errors
#' summary(tscope_fit)
#' 
#' # View estimated coefficients
#' coef(tscope_fit)
#' 
#' # Bootstrap-based confidence intervals
#' confint(tscope_fit)
#' 
#' # Variance-covariance matrix from bootstrap
#' vcov(tscope_fit)
#' 
#' # Diagnostics are in the details slot:
#' tscope_fit$details$corr_endostar_resid
#' tscope_fit$details$sdError
#' }
#'
#' @importFrom Formula as.Formula
#' @importFrom utils txtProgressBar setTxtProgressBar
#' 
#' @export
tscope <- function(formula, data, num.boots = 1000, verbose = TRUE) {
  cl <- match.call()
  
  # Input checks ------------------------------------------------------------------------------
  check_err_msg(checkinput_tscope_formula(formula = formula, data = data))
  check_err_msg(checkinput_tscope_data(data = data))
  check_err_msg(checkinput_tscope_dataVSformula(formula = formula, data = data))
  check_err_msg(checkinput_tscope_verbose(verbose = verbose))
  check_err_msg(checkinput_tscope_numboots(num.boots = num.boots))

  # Fit on the original data ------------------------------------------------------------------------------------
  res.real.data <- tscope_fit(formula = formula, data = data, verbose = verbose)

  # Bootstrap params --------------------------------------------------------------------------------------------
  if(verbose){
    message(paste(strwrap(paste0("Bootstrapping: Running ", num.boots, " bootstrap iterations.")),
                  collapse = "\n"))
    pb <- txtProgressBar(initial = 0, max = num.boots, style = 3)
  }

  boots.params <-
    sapply(seq(num.boots), USE.NAMES = TRUE, function(i){
      if(verbose)
        setTxtProgressBar(pb, i)

      boot.indices <- sample.int(n = NROW(data), replace = TRUE)
      boot.data    <- data[boot.indices, , drop = FALSE]
      return(coef(tscope_fit(formula = formula, data = boot.data, verbose = FALSE)))
    })

  if(verbose)
    close(pb)

  # Return object ------------------------------------------------------------------------------------------------
  result <- new_rendo_tscope_boots(
    call = cl,
    F.formula = res.real.data$formula,
    mf = res.real.data$model,
    coefficients = res.real.data$coefficients,
    names.main.coefs = res.real.data$names.main.coefs,
    fitted.values = res.real.data$fitted.values,
    residuals = res.real.data$residuals,
    boots.params = boots.params,
    tscope_model = res.real.data$tscope_model,
    details = res.real.data$details
  )
  
  return(result)
} 