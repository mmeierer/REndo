#' @title Summary Method for 2sCOPE Model Objects
#' @description
#' Summary method for objects of class `rendo.tscope`. Returns the summary of the
#'   second-stage linear model with a warning about the validity of standard errors, and
#'   displays diagnostic correlations between transformed endogenous regressors and OLS residuals.
#'
#' @param object An object of class `rendo.tscope` from the `tscope` function.
#' @param ... Additional arguments passed to `summary.lm`.
#'
#' @return A `summary.lm` object from the second-stage linear model.
#'
#' @export
summary.rendo.tscope <- function(object, ...) {
  # Get summary from the tscope_model (second stage lm)
  lm_summary <- summary(object$tscope_model, ...)
  
  # Add a class to customize print method
  class(lm_summary) <- c("summary.rendo.tscope", class(lm_summary))
  
  # Add diagnostic correlations to the summary object
  lm_summary$diagnostic_correlations <- object$details$corr
  lm_summary$endogenous_vars <- colnames(object$details$endoX)
  
  # Return the enhanced summary object
  return(lm_summary)
}

#' @title Print Method for 2sCOPE Model Summary Objects
#' @description 
#' Print method for objects of class `summary.rendo.tscope`. Displays the standard lm summary
#' output with additional warnings about standard errors and diagnostic correlation information.
#' 
#' @param x An object of class `summary.rendo.tscope` from the `summary.rendo.tscope` function.
#' @param ... Additional arguments passed to the next print method.
#' 
#' @return Invisibly, the input object `x`.
#'
#' @export
print.summary.rendo.tscope <- function(x, ...) {
  cat("\nNote: Standard errors, p-values, and confidence intervals may not be theoretically valid due to the\n")
  cat("      generated regressor problem. Bootstrapping is recommended for inference.\n\n")
  
  # Print diagnostic correlations
  cat("\nEndogeneity diagnostics (correlations between transformed endogenous regressors and residuals):\n")
  corr_df <- data.frame(
    row.names = x$endogenous_vars,
    Correlation = x$diagnostic_correlations
  )
  print(corr_df)
  cat("\n")
  
  # Print the standard lm summary output
  NextMethod()
  
  invisible(x)
}

#' @title Variance-Covariance Matrix for 2sCOPE Model Objects
#' @description
#' Returns the variance-covariance matrix of the coefficients from a 2sCOPE model fit.
#' Issues a warning that standard errors may not be theoretically valid due to the generated
#' regressor problem.
#'
#' @param object An object of class `rendo.tscope` from the `tscope` function.
#' @param ... Additional arguments passed to `vcov.lm`.
#'
#' @return The variance-covariance matrix from the second-stage linear model.
#'
#' @export
vcov.rendo.tscope <- function(object, ...) {
  # Extract vcov from the tscope_model but add a warning
  warning("Standard errors from vcov may not be theoretically valid due to the generated regressor problem.\n",
          "For reliable inference, bootstrapping is recommended.")
  return(vcov(object$tscope_model, ...))
}
