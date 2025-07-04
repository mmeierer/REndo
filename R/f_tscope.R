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
#' @template template_param_formuladataverbose
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
#' An object of class `rendo.tscope` (inheriting from `rendo.base`) is returned. This object is a list containing the following components:
#' \item{call}{The original function call.}
#' \item{F.formula}{The processed `Formula` object.}
#' \item{mf}{The model frame used for fitting.}
#' \item{coefficients}{A named vector of estimated coefficients. This includes coefficients for all regressors in the first part of the formula, the diagnostic correlations (`rho_...` for each endogenous regressor), and the standard deviation of the final model's residuals (`sdError`).}
#' \item{names.main.coefs}{A character vector containing the names of the coefficients corresponding to the regressors in the first part of the formula (excluding `rho_...` and `sdError`).}
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
#' @seealso \code{copulaCorrection} for another copula-based endogeneity correction method.
#'
#' @examples
#' \donttest{
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
#' # Display summary with diagnostic correlations and SE warning
#' summary(tscope_fit)
#'
#' # View estimated coefficients, including rho and sdError
#' coef(tscope_fit)
#'
#' # View only the main model coefficients (excluding rho and sdError)
#' coef(tscope_fit, complete = FALSE)
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
#'
#' @importFrom Formula as.Formula
#' @importFrom stats lm model.frame model.matrix model.response ecdf qnorm sd cor setNames coef vcov reformulate
#' 
#' @export
tscope <- function(formula, data, verbose = TRUE) {
  cl <- match.call()
  
  if (verbose) {
    message("Starting 2sCOPE estimation...")
  }
  
  # Input checks ------------------------------------------------------------------------------
  check_err_msg(checkinput_tscope_formula(formula=formula, data=data))
  check_err_msg(checkinput_tscope_data(data=data))
  check_err_msg(checkinput_tscope_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_tscope_verbose(verbose=verbose))

  # Convert formula and build model frame (following original implementation) ---------------
  F.formula <- as.Formula(formula)
  
  if (verbose) {
    message("Extracting model matrices...")
  }
  
  # Create model frame first to handle complex formulas properly
  mf <- model.frame(F.formula, data = data)
  
  # Extract all regressors from the first part (including intercept) - following original x matrix
  x <- as.matrix(model.matrix(F.formula, data = mf, rhs = 1))
  
  # Extract dependent variable - following original approach, but from model frame
  y <- model.response(mf)
  
  n <- nrow(x)
  if (ncol(x) <= 1) {
    stop("No predictor variables specified for the outcome")
  }
  
  # Extract endogenous regressors from the second part (remove intercept) - following original
  endox <- as.matrix(model.matrix(F.formula, data = mf, rhs = length(F.formula)[2]))
  endox <- endox[, colnames(endox) != "(Intercept)", drop = FALSE]
  nendox <- ncol(endox)
  
  if (verbose) {
    message(paste("Found", nendox, "endogenous regressor(s):", paste(colnames(endox), collapse=", ")))
  }
  
  # Calculate endoxstar matrix
  endoxstar <- matrix(0, n, nendox)
  
  # Extract exogenous regressors (w) - following original approach
  w <- x[, -which(colnames(x) %in% c("(Intercept)", colnames(endox))), drop = FALSE]
  nw <- ncol(w)
  wstar <- matrix(0, n, nw)
  
  if (verbose) {
    if (nw > 0) {
      message(paste("Found", nw, "exogenous regressor(s) for residualization:", paste(colnames(w), collapse=", ")))
    } else {
      message("No additional exogenous regressors found for residualization.")
    }
  }
  
  # Helper function to transform regressors using ECDF and qnorm (following original) -------
  ecdf_transform_regressors <- function(regressor_matrix) {
    apply(regressor_matrix, 2, function(x) {
      temp <- ecdf(x)(x)
      temp[temp == 1] <- length(x) / (length(x) + 1)
      qnorm(temp)
    })
  }
  
  if (verbose) {
    message("Transforming endogenous regressors using ECDF and qnorm...")
  }
  
  # Calculate endoxstar using helper function
  endoxstar <- ecdf_transform_regressors(endox)
  colnames(endoxstar) <- colnames(endox)
  
  # Main algorithm following original implementation exactly ---------------------------------
  if (nw == 0) {
    # No exogenous regressors case - following original
    if (verbose) {
      message("STAGE 1: No exogenous regressors specified, using copula directly")
    }
    stage1_resid <- NULL
    
    # Following original exactly: lm(y ~ -1 + x + endoxstar)
    combined_matrix <- cbind(x, endoxstar)
    res <- lm(y ~ -1 + ., data = data.frame(y = y, combined_matrix))
    
  } else {
    # With exogenous regressors case - following original
    if (verbose) {
      message("STAGE 1: Transforming exogenous regressors for residualization...")
    }
    
    # Calculate wstar using helper function
    wstar <- ecdf_transform_regressors(w)
    colnames(wstar) <- colnames(w)
    
    if (verbose) {
      message("STAGE 1: Residualizing transformed endogenous regressors...")
      for (endox_name in colnames(endox)) {
        message(paste("  Residualizing", endox_name, "on exogenous regressors..."))
      }
    }
    
    # Calculate stage1 residuals following original
    stage1_resid <- matrix(0, n, nendox)
    for (j in 1:nendox) {
      stage1_resid[, j] <- residuals(lm(endoxstar[, j] ~ wstar))
    }
    colnames(stage1_resid) <- paste0(colnames(endox), "_resid")
    
    # Following original exactly: lm(y ~ -1 + x + stage1_resid) 
    if (verbose) {
      message(paste("STAGE 2: Fitting final model: y ~ (Intercept) +", paste(colnames(w), collapse=" + "), "+", paste(colnames(endox), collapse=" + ")))
    }
    combined_matrix <- cbind(x, stage1_resid)
    res <- lm(y ~ -1 + ., data = data.frame(y = y, combined_matrix))
  }
  
  # Diagnostic: compute correlations between each transformed endogenous regressor and residuals ---
  if (verbose) {
    message("Computing diagnostic correlations...")
  }
  
  # Following original exactly: resid2=y-c(x%*%coef(res)[1:ncol(x)])
  coef_x <- coef(res)[1:ncol(x)]
  resid2 <- y - c(x %*% coef_x)
  corr_xerror_tscope <- apply(endoxstar, 2, function(endox_col) cor(endox_col, resid2))
  
  # Prepare result coefficients exactly following original ----------------------------------
  # Following original: res.tab[1,] = c(t(coef_x),t(corr_xerror_tscope),sd(resid2))
  
  result_coef <- c(coef_x,
                   corr_xerror_tscope,
                   sd(resid2))
  
  # Set proper names matching the original order
  coef_names <- c(colnames(x), 
                  paste0("rho_", colnames(endox)),
                  "sdError")
  names(result_coef) <- coef_names
  
  # Prepare details ----------------------------------------------------------------------------
  details <- list(endox = endox,
                  endoxstar = endoxstar,
                  w = if(nw > 0) w else NULL,
                  stage1_resid = stage1_resid,
                  resid2 = resid2,
                  corr = corr_xerror_tscope)
  
  if (verbose) {
    message("2sCOPE estimation complete.")
  }
  
  # Return object with custom class -----------------------------------------------------------
  result <- new_rendo_tscope(call = cl,
                          F.formula = F.formula,
                          mf = mf,
                          coefficients = result_coef,
                          names.main.coefs = colnames(x),
                          fitted.values = res$fitted.values,
                          residuals = res$residuals,
                          tscope_model = res,
                          details = details)
  return(result)
}