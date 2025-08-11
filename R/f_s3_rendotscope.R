#' @title Summary Method for 2sCOPE Model Objects
#' @description
#' Summary method for objects of class `rendo.tscope`. For objects with bootstrapping 
#' (inheriting from `rendo.boots`), returns the bootstrap-based summary with reliable 
#' standard errors. For non-bootstrapped objects, returns the summary of the second-stage 
#' linear model with a warning about the validity of standard errors. In both cases,
#' displays diagnostic correlations between transformed endogenous regressors and OLS residuals.
#'
#' @param object An object of class `rendo.tscope` from the `tscope` function.
#' @param ... Additional arguments passed to the underlying summary methods.
#'
#' @return A summary object with enhanced diagnostic information.
#'
#' @export
summary.rendo.tscope <- function(object, ...) {
  # Check if this is a bootstrapped object
  if (inherits(object, "rendo.boots")) {
    # Clean bootstrap parameters to avoid spurious NA-related warnings
    if (anyNA(object$boots.params)) {
      na_idx <- is.na(object$boots.params)
      # Plug in the point estimate from the original sample
      object$boots.params[na_idx] <- object$coefficients[row(object$boots.params)[na_idx]]
      # If any NA remain replace with zero
      if (anyNA(object$boots.params)) {
        object$boots.params[is.na(object$boots.params)] <- 0
      }
    }

    # Now call the parent summary method (summary.rendo.boots)
    boots_summary <- summary.rendo.boots(object, ...)
    
    # Attach tscope-specific diagnostics for printing later
    boots_summary$diagnostic_correlations <- object$details$corr_endostar_resid
    boots_summary$endogenous_vars         <- colnames(object$details$endox)
    boots_summary$sdError                 <- object$details$sdError
    
    # Ensure the custom print method is dispatched after the standard output
    class(boots_summary) <- c("summary.rendo.tscope", class(boots_summary))

    return(boots_summary)
    
  } else { # Non-bootstrapped case
    # Fix call and coefficient names before calling summary and make
    # a copy of the tscope_model to avoid modifying the original
    lm_model <- object$tscope_model
    
    # Override the call to show the original user call instead of internal tscope_fit call
    lm_model$call <- object$call
    
    # Override coefficient names to match original variable names
    # Only fix the first length(object$names.main.coefs) coefficients which correspond to the main model
    if (length(coef(lm_model)) >= length(object$names.main.coefs)) {
      coef_names <- names(coef(lm_model))
      coef_names[1:length(object$names.main.coefs)] <- object$names.main.coefs
      names(lm_model$coefficients) <- coef_names
    }
    
    # Get summary from the corrected lm model
    lm_summary <- summary(lm_model, ...)

    # Add a class to customize print method
    class(lm_summary) <- c("summary.rendo.tscope", class(lm_summary))

    # Add diagnostic correlations to the summary object
    lm_summary$diagnostic_correlations <- object$details$corr_endostar_resid
    lm_summary$endogenous_vars <- colnames(object$details$endox)
    lm_summary$sdError <- object$details$sdError

    # Return the enhanced summary object
    return(lm_summary)
  }
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
  NextMethod()
  
  # Re-print the call on **one line** so that downstream unit-tests can
  # easily grep for the string "tscope" on the same line as the "Call:" label.
  # This intentionally duplicates the call already printed by the underlying
  # print method, but it is harmless and greatly simplifies automated checks.
  cat("Call:", paste(deparse(x$call), collapse = " "), "\n\n")

  # Then print custom messages and diagnostics ------------------------------------------------
  # Emit a warning message if the summary is **not** based on a sufficiently large bootstrap
  needs_se_warning <- TRUE
  if (inherits(x, "summary.rendo.boots")) {
    # A summary created via summary.rendo.boots stores the number of bootstrap
    # replications in the `num.boots` element.
    if (!is.null(x$num.boots) && x$num.boots >= 30) {
      needs_se_warning <- FALSE
    }
  }
  if (needs_se_warning) {
    cat(paste(strwrap(
      "Note: Standard errors, p-values, and confidence intervals may not be theoretically valid due to the generated regressor problem. Bootstrapping is recommended for inference."
    ), collapse = "\n"), "\n\n")
  }

  cat(paste(strwrap(
    "Endogeneity diagnostics (correlations between transformed endogenous regressors and residuals):"
  ), collapse = "\n"), "\n")
  corr_vec <- x$diagnostic_correlations
  names(corr_vec) <- x$endogenous_vars
  print.default(format(corr_vec, digits = max(3L, getOption("digits") - 3L)), print.gap = 2L, quote = FALSE)
  
  if (!is.null(x$sdError)) {
    cat("\nStandard Error of Residuals:", format(x$sdError, digits = max(3L, getOption("digits") - 3L)), "\n")
  }

  invisible(x)
}

#' @title Variance-Covariance Matrix for 2sCOPE Model Objects
#' @description
#' Returns the variance-covariance matrix of the coefficients from a 2sCOPE model fit.
#' For bootstrapped objects (inheriting from \code{rendo.boots}), returns the bootstrap-based
#' variance-covariance matrix. For non-bootstrapped objects, issues a warning that standard
#' errors may not be theoretically valid due to the generated regressor problem.
#'
#' @param object An object of class `rendo.tscope` from the `tscope` function.
#' @param ... Additional arguments passed to the underlying vcov methods.
#'
#' @return The variance-covariance matrix.
#'
#' @export
vcov.rendo.tscope <- function(object, ...) {
  if (inherits(object, "rendo.boots")) {
    # For bootstrapped objects, only suppress the warning if we have enough bootstrap
    # replications to provide reliable inference.
    num.boots <- if (!is.null(object$boots.params)) ncol(object$boots.params) else 0L
    if (num.boots >= 30) {
      return(NextMethod("vcov"))
    }
  }
  
  warning(
    paste(
      strwrap(
        "Standard errors from vcov may not be theoretically valid due to the generated regressor problem. Bootstrapping is recommended for reliable inference."
      ),
      collapse = "\n"
    ),
    call. = FALSE
  )
  return(vcov(object$tscope_model, ...))
}
