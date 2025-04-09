#' @title Fitting Linear Models with Endogenous Regressors using Two-Stage Copula (2sCOPE)
#' @aliases tscope
#'
#' @description
#' Fits linear models with potentially endogenous regressors using a two-stage copula-based approach, referred to as 2sCOPE.
#' This method implements the 2sCOPE approach described by Yang, Qian, and Xie (2024) in their paper "Addressing Endogeneity using a Two-stage Copula Generated Regressor Approach".
#' It addresses endogeneity without requiring external instrumental variables. It relies on transforming the endogenous regressors
#' based on their empirical cumulative distribution functions (ECDF) and the quantile function of the standard normal distribution.
#' If additional exogenous regressors are specified (beyond those potentially endogenous), a residualization step is performed in the first stage.
#'
#' @param formula A formula object following the standard two-part format. The first part specifies the dependent variable and all regressors
#'   (exogenous and endogenous, e.g., `y ~ x1 + x2 + p1`), and the second part, separated by `|`, specifies which of these regressors are endogenous (e.g., `| p1`).
#'   Thus, a full formula might look like `y ~ x1 + x2 + p1 + p2 | p1 + p2`. An intercept is included automatically unless explicitly removed.
#' @param data A `data.frame` or `tibble` containing the variables specified in the formula.
#' @param verbose A logical value indicating whether to print messages during the estimation process, including a summary of the final stage model. Defaults to `TRUE`.
#'
#' @details
#' \subsection{Method}{
#' The 2sCOPE method proceeds in two main stages:
#' \enumerate{
#'   \item \strong{Transformation and Residualization (Stage 1):}
#'     \itemize{
#'       \item All specified endogenous regressors (\code{endox}) are transformed using their empirical CDF (ECDF) followed by the inverse standard normal CDF (quantile function, \code{qnorm}). This results in normally distributed variables (\code{endoxstar}) based on the ranks of the original endogenous variables:
#'         \ifelse{html}{\out{<div style="text-align:center">endoxstar<sub>ij</sub> = &Phi;<sup>-1</sup>(ECDF<sub>j</sub>(endox<sub>ij</sub>))</div>}}{\deqn{endoxstar_{ij} = \Phi^{-1}(ECDF_{j}(endox_{ij}))}}
#'         where \eqn{\Phi^{-1}} is the quantile function of the standard normal distribution and \eqn{ECDF_j} is the empirical cumulative distribution function for the j-th endogenous regressor. Values exactly equal to 1 from the ECDF are adjusted slightly downwards (\eqn{n / (n + 1)}) before applying \code{qnorm} to avoid infinite values.
#'       \item If additional exogenous regressors (\code{w}) are present in the first part of the formula (i.e., regressors listed in the first part but not listed as endogenous in the second part), these are also transformed in the same manner (\code{wstar}).
#'       \item The transformed endogenous regressors (\code{endoxstar}) are then regressed on the transformed exogenous regressors (\code{wstar}). The residuals from these regressions (\code{stage1_resid}) are retained. This step aims to purge the transformed endogenous variables of the variation explained by the included exogenous variables.
#'       \item If no additional exogenous regressors (\code{w}) are specified, this residualization step is skipped, and the transformed endogenous regressors (\code{endoxstar}) are used directly in the second stage.
#'     }
#'   \item \strong{Final Regression (Stage 2):}
#'     A linear model (\code{lm}) is fitted for the outcome variable (\code{y}). The predictors in this model are:
#'     \itemize{
#'       \item All regressors specified in the first part of the formula (represented by the matrix \code{x}, including the intercept if present, and any additional exogenous variables \code{w}).
#'       \item The residualized transformed endogenous regressors (\code{stage1_resid}) if \code{w} was present, OR the directly transformed endogenous regressors (\code{endoxstar}) if no \code{w} was present.
#'     }
#' }
#' }
#'
#' \subsection{Diagnostic Correlation (rho)}{
#' As a diagnostic measure, the function also calculates the Pearson correlation coefficient (\code{rho}) between each transformed endogenous regressor (\code{endoxstar}) and the residuals obtained from a simple OLS regression of the dependent variable (\code{y}) solely on the original regressors specified in the first part of the formula (\code{x}). These correlations (\code{rho_...}) are included in the output coefficients and can provide an indication of the degree of endogeneity addressed by the transformation. A value close to zero might suggest that the endogeneity concern related to that variable, as captured by this method, is minimal after accounting for other exogenous predictors.
#' }
#'
#' \subsection{Formula parameter}{
#' The `formula` argument requires a two-part structure separated by a vertical bar (`|`).
#' \itemize{
#'   \item The first part defines the main regression model including all regressors: `dependent_variable ~ exogenous_regressors + endogenous_regressors`.
#'   \item The second part lists only those regressors from the first part that should be treated as endogenous: `| endogenous_regressor1 + endogenous_regressor2`.
#' }
#' For example, in `y ~ x1 + x2 + p1 + p2 | p1 + p2`, `y` is the dependent variable, `x1` and `x2` are treated as strictly exogenous regressors, and `p1` and `p2` are treated as endogenous regressors. The model will include `x1`, `x2`, `p1`, and `p2` in the final regression's coefficient set, but `p1` and `p2` will undergo the transformation (and potentially residualization) process described above before being used as predictors in the second stage `lm` fit.
#' }
#'
#' \subsection{Inference and Standard Errors}{
#' Note that the estimation involves two stages. Standard errors, confidence intervals,
#' and p-values derived directly from the summary of the second-stage linear model
#' (`tscope_model` component or via `summary(object)`) may not be theoretically
#' valid due to the generated regressor problem (i.e., the use of transformed/residualized
#' variables estimated in the first stage).
#'
#' For reliable inference, users are strongly encouraged to employ bootstrapping,
#' as demonstrated in the example code accompanying the original paper by
#' Yang, Qian, and Xie (2024). This involves repeatedly resampling the original data,
#' applying the `tscope` function to each sample, and calculating standard errors
#' or confidence intervals from the distribution of the resulting bootstrapped coefficients.
#' The example section below demonstrates the function call, but not the bootstrap procedure.
#' }
#'
#' @return
#' An object of class `rendo.tscope.IV` (inheriting from `rendo.base`) is returned. This object is a list containing the following components:
#' \item{call}{The original function call.}
#' \item{F.formula}{The processed `Formula` object.}
#' \item{mf}{The model frame used for fitting.}
#' \item{coefficients}{A named vector of estimated coefficients. This includes coefficients for all regressors in the first part of the formula (corresponding to the columns in the initial `x` matrix), the diagnostic correlations (`rho_...` for each endogenous regressor), and the standard deviation of the final model's residuals (`sdError`).}
#' \item{names.main.coefs}{A character vector containing the names of the coefficients corresponding to the regressors in the first part of the formula (matching the columns in the initial `x` matrix, excluding `rho_...` and `sdError`).}
#' \item{fitted.values}{The fitted values from the final (Stage 2) linear model.}
#' \item{residuals}{The residuals from the final (Stage 2) linear model.}
#' \item{tscope_model}{The `lm` object resulting from the final (Stage 2) regression.}
#' \item{details}{A list containing intermediate results:
#'   \describe{
#'     \item{endox}{Matrix of the original endogenous regressors.}
#'     \item{endoxstar}{Matrix of the transformed endogenous regressors.}
#'     \item{w}{Matrix of the additional exogenous regressors (NULL if none).}
#'     \item{stage1_resid}{Matrix of the residualized transformed endogenous regressors from Stage 1 (NULL if no `w` regressors or only constant `w`).}
#'     \item{resid2}{Vector of residuals from the OLS regression of y on all initial x (used for calculating rho).}
#'     \item{corr}{Vector of the calculated rho values (correlations between `endoxstar` and `resid2`).}
#'   }}
#'
#' The generic accessor functions `coef`, `fitted`, `residuals`, `vcov` (derived from the final `lm` model), `nobs`, and `summary` (prints summary of the final `lm` model) are available. Note the caveat regarding standard errors mentioned in the Details section; bootstrapping is recommended for inference, rendering the standard `vcov` and `summary` output potentially misleading for inferential purposes.
#'
#' @references
#' Yang, F., Qian, Y., & Xie, H. (2024). EXPRESS: Addressing Endogeneity Using a Two-stage Copula Generated Regressor Approach. *Journal of Marketing Research*, 0(ja). \doi{10.1177/00222437241296453}
#'
#' Yang, F., Qian, Y., & Xie, H. (2022). *Addressing endogeneity using a two-stage copula generated regressor approach* (Working Paper No. w29708). National Bureau of Economic Research.
#'
#' @seealso \code{\link[Formula]{Formula}} for the formula syntax, \code{\link[stats]{lm}} for the underlying regression function.
#' @seealso \code{copulaCorrection} for another copula-based endogeneity correction method.
#'
#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix model.response ecdf qnorm sd cor setNames coef vcov na.omit reformulate
#'
#' @examples
#' \donttest{
#' # Load Formula package for the formula syntax
#' # Ensure Formula package is installed: install.packages("Formula")
#' library(Formula)
#'
#' # Generate some data with endogeneity
#' set.seed(123)
#' n <- 100
#' # Exogenous variable
#' x1 <- rnorm(n)
#' # Underlying instrument (unobserved)
#' z1 <- rnorm(n)
#' # Error terms
#' nu <- rnorm(n, sd = 0.5) # Error for endogenous regressor
#' eps <- rnorm(n, sd = 0.5) # Structural error
#' # Endogenous regressor (correlated with eps through z1 and nu indirectly)
#' p1 <- 0.5 * x1 + 0.8 * z1 + nu
#' # Dependent variable
#' y <- 1 + 0.8 * x1 - 1.2 * p1 + 0.6 * z1 + eps # z1 induces endogeneity
#'
#' dat <- data.frame(y, x1, p1)
#'
#' # Fit the 2sCOPE model
#' # Treat p1 as endogenous
#' # Formula: y ~ x1 + p1 | p1
#' tscope_fit <- tscope(y ~ x1 + p1 | p1, data = dat, verbose = FALSE)
#'
#' # Display summary (based on the second stage lm - see warning about SEs)
#' summary(tscope_fit)
#'
#' # View estimated coefficients, including rho and sdError
#' coef(tscope_fit)
#'
#' # Compare with a naive OLS model (likely biased)
#' naive_ols <- lm(y ~ x1 + p1, data = dat)
#' summary(naive_ols)
#'
#' # Example with an additional exogenous variable used for residualization
#' x2 <- rnorm(n)
#' dat$x2 <- x2
#' y2 <- 1 + 0.8 * x1 + 0.5 * x2 - 1.2 * p1 + 0.6 * z1 + eps
#' dat$y2 <- y2
#'
#' # Formula: y2 ~ x1 + x2 + p1 | p1
#' tscope_fit2 <- tscope(y2 ~ x1 + x2 + p1 | p1, data = dat, verbose = TRUE)
#' summary(tscope_fit2)
#' coef(tscope_fit2)
#'
#' # Note: For reliable standard errors and inference, bootstrapping is recommended.
#' # See the 'Inference and Standard Errors' section in the Details.
#' # The original authors' code provides an example of a bootstrap procedure.
#'
#' }
#' @export
tscope <- function(formula, data, verbose = TRUE) {
  cl <- match.call()
  
  # Input checks ------------------------------------------------------------------------------
  check_err_msg(checkinput_tscope_formula(formula, data))
  check_err_msg(checkinput_tscope_data(data))
  check_err_msg(checkinput_tscope_dataVSformula(formula, data))
  check_err_msg(checkinput_tscope_verbose(verbose))

  # Convert formula and build model frame -----------------------------------------------------
  F.formula  <- as.Formula(formula)
  rhs_parts <- attr(F.formula, "rhs")
  mf         <- model.frame(formula = F.formula, data = data)
  vec.data.y <- model.response(mf)
  
  # Extract regressors from the first part (including intercept)
  x <- as.matrix(model.matrix(F.formula, data = data, rhs = 1))
  
  # Extract endogenous regressors from the second part (remove intercept)
  endox <- as.matrix(model.matrix(F.formula, data = data, rhs = 2))
  endox <- endox[, setdiff(colnames(endox), "(Intercept)"), drop = FALSE]
  
  n <- nrow(x)
  nendox <- ncol(endox)
  
  # Allow one predictor column (even when no intercept is present)
  if (ncol(x) < 1)
    stop("No predictor variables specified for the outcome.")
  
  # Identify additional exogenous regressors -------------------------------------------------
  exog_names <- setdiff(colnames(x), c("(Intercept)", colnames(endox)))
  w <- if (length(exog_names) > 0) x[, exog_names, drop = FALSE] else NULL
  nw <- if (!is.null(w)) ncol(w) else 0
  
  # Transform endogenous regressors ----------------------------------------------------------
  endoxstar <- matrix(0, n, nendox)
  for (i in 1:nendox) {
    temp <- ecdf(endox[, i])(endox[, i])
    temp[temp == 1] <- n / (n + 1)
    endoxstar[, i] <- qnorm(temp)
  }
  
  # Stage 1: residualize transformed endogenous regressors if exogenous regressors exist ------
  if (nw == 0) {
    if (verbose)
      message("No additional exogenous regressors specified. Using transformed endogenous regressors directly.")
    stage1_resid <- NULL
    final_model <- lm(vec.data.y ~ -1 + x + endoxstar)
  } else {
    # Transform exogenous regressors similarly ------------------------------------------------
    wstar <- matrix(0, n, nw)
    for (i in 1:nw) {
      temp <- ecdf(w[, i])(w[, i])
      temp[temp == 1] <- n / (n + 1)
      wstar[, i] <- qnorm(temp)
    }
    stage1_resid <- matrix(0, n, nendox)
    for (j in 1:nendox) {
      stage1_resid[, j] <- lm(endoxstar[, j] ~ wstar)$resid
    }
    final_model <- lm(vec.data.y ~ -1 + x + stage1_resid)
  }
  
  # Diagnostic: compute correlations between each transformed endogenous regressor and residuals ---
  coef_x <- final_model$coef[seq_len(ncol(x))]
  fitted_x <- as.vector(x %*% coef_x)
  resid2 <- vec.data.y - fitted_x
  corr_xerror <- rep(NA, nendox)
  for (j in 1:nendox) {
    corr_xerror[j] <- cor(endoxstar[, j], resid2)
  }
  
  # Prepare result coefficients ---------------------------------------------------------------
  result_coef <- c(final_model$coef[seq_len(ncol(x))],
                   setNames(corr_xerror, paste0("rho_", colnames(endox))),
                   sdError = sd(resid2))
  
  if (verbose) {
    cat("========================================================\n")
    cat("2sCOPE estimates\n")
    print(summary(final_model))
  }
  
  # Prepare details ----------------------------------------------------------------------------
  details <- list(endox = endox,
                  endoxstar = endoxstar,
                  w = w,
                  stage1_resid = stage1_resid,
                  resid2 = resid2,
                  corr = corr_xerror)
  
  # Return object with custom class -----------------------------------------------------------
  res <- new_rendo_tscope(call = cl,
                                F.formula = F.formula,
                                mf = mf,
                                coefficients = result_coef,
                                names.main.coefs = colnames(x),
                                fitted.values = final_model$fitted.values,
                                residuals = final_model$residuals,
                                tscope_model = final_model,
                                details = details)
  return(res)
}