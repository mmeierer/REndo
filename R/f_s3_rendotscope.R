# Helper function
make_safe_names <- function(original_names) {
  safe_names <- make.names(original_names, unique = TRUE)
  names(safe_names) <- original_names
  return(safe_names)
}

# coef method
#' @title Extract Coefficients for 2sCOPE Objects
#' @export
coef.rendo.tscope <- function(object, complete = TRUE, ...) {
  if (complete) {
    coefs <- object$coefficients
    # Append sdError if missing
    if (!"sdError" %in% names(coefs)) {
      if (!is.null(object$tscope_model) && inherits(object$tscope_model, "lm")) {
         sd_err <- summary(object$tscope_model)$sigma
         coefs <- c(coefs, sdError = sd_err)
      } else {
         coefs <- c(coefs, sdError = NA_real_)
      }
    }
    return(coefs)
  } else {
    # More explicit extraction for special character names
    names_to_extract <- object$names.main.coefs
    if (length(names_to_extract) > 0) {
      main_coefs <- numeric(length(names_to_extract))
      for (i in seq_along(names_to_extract)) {
        name <- names_to_extract[i]
        if (name %in% names(object$coefficients)) {
          main_coefs[i] <- object$coefficients[name]
        } else {
          main_coefs[i] <- NA_real_
        }
      }
      names(main_coefs) <- names_to_extract
      return(main_coefs)
    } else {
      return(numeric(0))
    }
  }
}

# fitted method (with NULL check and name preservation)
#' @title Extract Fitted Values for 2sCOPE Objects
#' @export
fitted.rendo.tscope <- function(object, ...) {
  if (is.null(object$tscope_model) || !inherits(object$tscope_model, "lm")) {
    stop("Internal lm object ('tscope_model') is missing or invalid in the rendo.tscope object.")
  }
  vals <- stats::fitted(object$tscope_model)
  model_rownames <- rownames(object$model)
  if (!is.null(model_rownames) && length(vals) == length(model_rownames)) {
      names(vals) <- model_rownames
  }
  return(vals)
}

# residuals method (NULL check, NA handling, and name preservation)
#' @title Extract Residuals for 2sCOPE Objects
#' @export
residuals.rendo.tscope <- function(object, type = "response", ...) {
  model_rownames <- rownames(object$model)
  
  get_residuals <- function(obj) {
    if (is.null(obj$tscope_model) || !inherits(obj$tscope_model, "lm")) {
      stop("Internal lm object ('tscope_model') is missing or invalid in the rendo.tscope object.")
    }
    res <- stats::residuals(obj$tscope_model)
    if (!is.null(model_rownames) && length(res) == length(model_rownames)) {
        names(res) <- model_rownames
    }
    return(res)
  }
  
  if (type == "response") {
    return(get_residuals(object))
  } else if (type == "diagnostic") {
    if (is.null(object$details$resid2)) {
        warning("Diagnostic residuals (details$resid2) not available.")
        n <- nrow(object$model)  # Use model rows directly instead of nobs()
        if (!is.na(n) && n > 0) {
           res_na <- rep(NA_real_, n)
           if (!is.null(model_rownames) && length(res_na) == length(model_rownames)) {
               names(res_na) <- model_rownames
           }
           return(res_na)
        } else {
           return(stats::setNames(NA_real_, if (!is.null(model_rownames)) model_rownames[1] else "NA"))
        }
    }
    # Return diagnostic residuals without modifying names
    return(object$details$resid2)
  } else {
    warning("Only type = 'response' or 'diagnostic' is currently supported for residuals.rendo.tscope. Defaulting to 'response'.")
    return(get_residuals(object))
  }
}

# summary method (modified for robust check and proper coefficient table)
#' @title Summary for 2sCOPE Model Fits
#' @export
summary.rendo.tscope <- function(object, ...) {
  if (is.null(object)) {
    # Remove warning for NULL input
    res <- list(null = TRUE)
    class(res) <- "summary.rendo.tscope"
    return(res)
  }
  
  res <- list()
  res$call <- object$call
  res$formula <- object$formula

  lm_summary <- tryCatch(summary(object$tscope_model), error = function(e) NULL)
  if (is.null(lm_summary) || !inherits(lm_summary, "summary.lm")) {
      warning("Could not generate summary from internal lm object (missing, invalid, or summary failed).")
      res <- list(null = TRUE)
      class(res) <- "summary.rendo.tscope"
      return(res)
  }
  
  lm_coef_table <- lm_summary$coefficients
  
  if (is.null(object$names.copula.coefs.clean)) {
      original_main <- object$names.main.coefs
      # Remove a leading 'x' from the lm table rownames if present
      adjusted_names <- sub("^x", "", rownames(lm_coef_table))
      main_indices <- which(adjusted_names %in% original_main)
      if (length(main_indices) == length(original_main)) {
         display_coef_table <- lm_coef_table[main_indices, , drop = FALSE]
         # Ensure that the rows are ordered in the same order as original_main.
         new_order <- match(original_main, sub("^x", "", rownames(display_coef_table)))
         if (!anyNA(new_order) && length(new_order) == length(original_main)) {
           display_coef_table <- display_coef_table[new_order, , drop = FALSE]
           rownames(display_coef_table) <- original_main
         } else {
           # fallback if ordering fails: only assign names if dimensions match
           if (nrow(display_coef_table) == length(original_main)) {
             rownames(display_coef_table) <- original_main
           }
         }
      } else {
         # fallback: if we cannot reliably extract only the main coefficients,
         # use the full table.
         display_coef_table <- lm_coef_table
      }
  } else {
      # When copula coefficients exist, use the mapping as before.
      original_to_clean_map <- make_safe_names(object$names.main.coefs)  # mapping: original -> safe
      main_coef_indices_clean <- stats::na.omit(match(original_to_clean_map, rownames(lm_coef_table)))
      copula_coef_indices_clean <- stats::na.omit(match(object$names.copula.coefs.clean, rownames(lm_coef_table)))
      display_coef_table <- lm_coef_table
      main_names_in_table <- rownames(display_coef_table)[main_coef_indices_clean]
      valid_map_indices <- which(main_names_in_table %in% original_to_clean_map)
      if (length(valid_map_indices) > 0) {
         rownames(display_coef_table)[main_coef_indices_clean[valid_map_indices]] <-
              names(original_to_clean_map)[match(main_names_in_table[valid_map_indices], original_to_clean_map)]
      }
  }
  
  res$coefficients <- display_coef_table
  all_coefs <- object$coefficients
  res$rho <- all_coefs[grep("^rho_", names(all_coefs))]
  res$sdError <- all_coefs["sdError"]

  res$sigma <- lm_summary$sigma
  res$r.squared <- lm_summary$r.squared
  res$adj.r.squared <- lm_summary$adj.r.squared
  res$fstatistic <- lm_summary$fstatistic

  res$AIC <- tryCatch(stats::AIC(object), error = function(e) NA)
  res$BIC <- tryCatch(stats::BIC(object), error = function(e) NA)
  res$logLik <- tryCatch(stats::logLik(object), error = function(e) NA)
  res$df.residual <- stats::df.residual(object$tscope_model)

  class(res) <- "summary.rendo.tscope"
  return(res)
}

# print.summary method (modified for NULL input)
#' @title Print Method for 2sCOPE Model Summaries
#' @export
print.summary.rendo.tscope <- function(x, digits = max(3L, getOption("digits") - 3L),
                                         signif.stars = getOption("show.signif.stars"), ...) {
  if (is.null(x) || (is.list(x) && !is.null(x$null) && x$null)) {
    cat("Summary object is NULL.\n")
    return(invisible(x))
  }
  cat("\nCall:\n", paste(deparse(x$call), collapse = "\n"), "\n\n", sep = "")
  cat("Coefficients (Final Model):\n")
  stats::printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
                      na.print = "NA", P.values = TRUE, has.Pvalue = TRUE, ...)
  
  cat("\n--- Diagnostics ---\n")
  cat("Endogeneity Diagnostics (rho):\n")
  if (length(x$rho) > 0) {
      print.default(format(x$rho, digits = digits), print.gap = 2L, quote = FALSE)
  } else {
      cat(" (No rho values calculated or found)\n")
  }
  
  if (!is.null(x$sdError) && !is.na(x$sdError)) {
      cat("\nInitial OLS Residual Std. Error (sdError):", format(x$sdError, digits = digits), "\n")
  } else {
      cat("\nInitial OLS Residual Std. Error (sdError): (Not available)\n")
  }
  
  cat("\n--- Final Model Fit ---\n")
  if (!is.null(x$sigma) && !is.null(x$df.residual)) {
      cat("Residual standard error:", format(x$sigma, digits = digits), "on", x$df.residual, "degrees of freedom\n")
  } else {
      cat("Residual standard error: (Not available)\n")
  }
  
  if (!is.null(x$r.squared)) {
      cat("Multiple R-squared:", formatC(x$r.squared, format = "f", digits = digits),
          "\tAdjusted R-squared:", formatC(x$adj.r.squared, format = "f", digits = digits), "\n")
  }
  
  if (!is.null(x$fstatistic) && is.numeric(x$fstatistic) &&
      length(x$fstatistic) == 3 && all(is.finite(x$fstatistic))) {
      fstat <- x$fstatistic
      cat("F-statistic:", formatC(fstat[1L], format = "f", digits = digits), "on",
          fstat[2L], "and", fstat[3L], "DF,  p-value:",
          format.pval(stats::pf(fstat[1L], fstat[2L], fstat[3L], lower.tail = FALSE), digits = digits), "\n")
  }
  
  aic_val <- if (!is.null(x$AIC) && !is.na(x$AIC)) formatC(x$AIC, format = "f", digits = digits) else "(Not available)"
  bic_val <- if (!is.null(x$BIC) && !is.na(x$BIC)) formatC(x$BIC, format = "f", digits = digits) else "(Not available)"
  logLik_val <- if (!is.null(x$logLik) && !is.na(x$logLik)) formatC(as.numeric(x$logLik), format = "f", digits = digits) else "(Not available)"
  
  cat("AIC:", aic_val,
      "\tBIC:", bic_val,
      "\tLog-Likelihood:", logLik_val, "\n")
  
  invisible(x)
}

# predict method (modified for endogenous variable type consistency and error handling)
#' @title Predict for 2sCOPE Objects
#' @details Uses original data ECDFs and re-estimates stage 1 on transformed newdata.
#' @export
predict.rendo.tscope <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(fitted(object))
  }
  if (!is.data.frame(newdata)) stop("'newdata' must be a data.frame")
  
  F.formula <- object$formula
  final_lm_obj <- object$tscope_model
  mf_orig <- object$model
  if (is.null(mf_orig)) stop("Original model frame (`object$model`) is missing.")
  n_orig <- nrow(mf_orig)
  
  if (is.null(final_lm_obj) || !inherits(final_lm_obj, "lm")) {
    stop("Internal lm object ('tscope_model') is missing or invalid in the rendo.tscope object.")
  }
  
  full_terms <- tryCatch(stats::terms(F.formula),
                         error = function(e) stop("Invalid formula found in object: ", F.formula))
  predictor_terms <- stats::delete.response(full_terms)
  
  mf_new <- tryCatch(stats::model.frame(predictor_terms, data = newdata, na.action = na.fail),
                     error = function(e) {
                       if (grepl("contrasts can be applied only to factors", e$message)) {
                           stop("Failed to create model frame from newdata: factor variable has invalid levels.")
                       }
                       vars_needed <- all.vars(predictor_terms)
                       stop("Failed to create model frame from newdata using predictor terms: ",
                            paste(vars_needed, collapse = ", "),
                            ". Check variable names and types in newdata.\n Original error: ", e$message)
                     })
  if (nrow(mf_new) == 0) return(stats::setNames(numeric(0), character(0)))
  
  form_obj <- Formula::Formula(F.formula)
  terms_rhs2_formula <- stats::formula(form_obj, rhs = 2, lhs = 0)
  endox_orig_colnames <- tryCatch(all.vars(terms_rhs2_formula),
                                  error = function(e) stop("Could not extract original endogenous variable names from formula RHS part 2."))
  
  for (var in endox_orig_colnames) {
    if (var %in% names(mf_orig) && is.factor(mf_orig[[var]])) {
      mf_new[[var]] <- factor(mf_new[[var]], levels = levels(mf_orig[[var]]))
    }
  }
  
  if (!all(endox_orig_colnames %in% names(mf_new))) {
      stop("Not all endogenous variables found in newdata model frame: missing ",
           paste(setdiff(endox_orig_colnames, names(mf_new)), collapse = ", "))
  }
  
  terms_rhs1 <- stats::delete.response(stats::terms(F.formula, rhs = 1))
  x_new_orig_names <- stats::model.matrix(terms_rhs1, data = mf_new)
  
  endox_model_formula <- stats::reformulate(termlabels = endox_orig_colnames, intercept = FALSE)
  endox_new_orig_names <- tryCatch({
      stats::model.matrix(endox_model_formula, data = mf_new)
    },
    error = function(e) {
      if (grepl("contrasts", e$message)) {
         stop("Failed to create endogenous variable matrix from newdata: factor variable has invalid levels.")
      } else {
         stop("Failed to create endogenous variable matrix from newdata. Check variable types.\n Original error: ", e$message)
      }
    }
  )
  
  if (ncol(endox_new_orig_names) == length(endox_orig_colnames)) {
       colnames(endox_new_orig_names) <- endox_orig_colnames
  } else {
       stop("Mismatch in number of endogenous variable columns generated from newdata (",
            ncol(endox_new_orig_names), ") versus expected (", length(endox_orig_colnames), ").")
  }
  
  n_new <- nrow(x_new_orig_names)
  nendox_new <- ncol(endox_new_orig_names)
  
  endoxstar_new <- matrix(NA_real_, nrow = n_new, ncol = nendox_new)
  colnames(endoxstar_new) <- paste0("star.", endox_orig_colnames)
  
  for (i in 1:nendox_new) {
    var_name <- endox_orig_colnames[i]
    if (!var_name %in% colnames(object$details$endox)) {
         if (is.null(object$details) || is.null(object$details$endox)) stop("object$details$endox is missing.")
         if (is.null(colnames(object$details$endox))) stop("object$details$endox has no column names.")
         stop("Original endogenous variable '", var_name, "' not found in object$details$endox columns: ", 
              paste(colnames(object$details$endox), collapse = ", "))
    }
    original_p_col <- object$details$endox[, var_name]
    original_ecdf <- stats::ecdf(original_p_col)
    temp <- original_ecdf(endox_new_orig_names[, var_name])
    temp[temp >= 1] <- n_orig / (n_orig + 1)
    temp[temp <= 0] <- 1 / (n_orig + 1)
    endoxstar_new[, i] <- stats::qnorm(temp)
  }
  
  # Ensure names.copula.coefs.clean is defined
  copula_clean_names <- object$names.copula.coefs.clean
  if (is.null(copula_clean_names) || length(copula_clean_names) == 0) {
      copula_clean_names <- paste0("star.", make.names(endox_orig_colnames))
      object$names.copula.coefs.clean <- copula_clean_names
  }
  
  star_names <- grep("^star\\.", copula_clean_names, value = TRUE)
  expected_star_names <- paste0("star.", make.names(endox_orig_colnames))
  matched_star_names <- intersect(star_names, expected_star_names)
  
  if (length(matched_star_names) == nendox_new) {
      name_map <- setNames(expected_star_names, endox_orig_colnames)
      colnames(endoxstar_new) <- name_map[endox_orig_colnames]
  } else if (length(star_names) == nendox_new) {
      warning("Could not reliably match 'star.*' names in object$names.copula.coefs.clean to endogenous variables. Assigning based on order found.")
      colnames(endoxstar_new) <- star_names
  } else {
      warning("Mismatch between number of 'star.*' names in object$names.copula.coefs.clean (", 
              length(star_names), ") and number of endogenous variables (", nendox_new, "). Assigning generic names.")
      colnames(endoxstar_new) <- paste0("star.P", 1:nendox_new)
  }
  
  # Construct final design matrix
  if (is.null(object$names.main.coefs)) stop("object$names.main.coefs is NULL.")
  main_orig_names <- object$names.main.coefs
  main_clean_map <- make_safe_names(main_orig_names)
  x_new_clean_names <- x_new_orig_names
  if (!all(colnames(x_new_orig_names) %in% names(main_clean_map))) {
       stop("Mismatch between column names of newdata's structural matrix ('",
            paste(colnames(x_new_orig_names), collapse = ","),
            "') and names expected from original main coefficients ('",
            paste(names(main_clean_map), collapse = ","), "').")
  }
  colnames(x_new_clean_names) <- main_clean_map[colnames(x_new_orig_names)]
  
  final_design_newdata_clean <- cbind(x_new_clean_names, endoxstar_new)  # or generated regressors if applicable
  model_coef_names_clean <- names(stats::coef(final_lm_obj))
  
  if (!all(model_coef_names_clean %in% colnames(final_design_newdata_clean))) {
      missing_clean_cols <- setdiff(model_coef_names_clean, colnames(final_design_newdata_clean))
      stop("Internal error: Could not construct all necessary columns (clean names) for prediction matrix from newdata. ",
           "Missing columns: ", paste(missing_clean_cols, collapse = ", "), ". ",
           "Available columns: ", paste(colnames(final_design_newdata_clean), collapse = ", "), ". ",
           "Check consistency between tscope object's internal model coefficients and generated regressor names (star.* or res.*).")
  }
  
  final_design_newdata_clean <- final_design_newdata_clean[, model_coef_names_clean, drop = FALSE]
  
  pred_data_frame <- as.data.frame(final_design_newdata_clean)
  predictions <- tryCatch(stats::predict.lm(final_lm_obj, newdata = pred_data_frame, ...),
                            error = function(e) {
                                stop("stats::predict.lm failed. Check newdata structure and values after internal processing.\n Original error: ", e$message)
                            })
  
  mf_new_rownames <- rownames(mf_new)
  if (!is.null(mf_new_rownames) && length(predictions) == length(mf_new_rownames)) {
      names(predictions) <- mf_new_rownames
  }
  
  return(predictions)
}

# logLik method
#' @export
logLik.rendo.tscope <- function(object, ...) {
  if (!is.null(object$tscope_model) && inherits(object$tscope_model, "lm")) {
    ll <- stats::logLik(object$tscope_model, ...)
    nobs_val <- tryCatch(stats::nobs(object$tscope_model), error = function(e) nrow(object$model))
    if (is.null(attr(ll, "nobs")) && !is.null(nobs_val)) attr(ll, "nobs") <- nobs_val
    return(ll)
  } else {
    warning("Final lm model object not found or invalid in rendo.tscope object.")
    structure(NA_real_, df = NA_integer_, nobs = NA_integer_, class = "logLik")
  }
}

# vcov method
#' @export
vcov.rendo.tscope <- function(object, ...) {
   if (!is.null(object$tscope_model) && inherits(object$tscope_model, "lm")) {
    return(stats::vcov(object$tscope_model, ...))
  } else {
    warning("Final lm model object not found or invalid in rendo.tscope object.")
    return(NA_real_)
  }
}

# AIC method
#' @export
AIC.rendo.tscope <- function(object, ..., k = 2) {
  ll <- logLik(object)
  if (!is.na(ll[1]) && !is.null(attr(ll, "df")) && !is.na(attr(ll, "df"))) {
      return(stats::AIC(ll, ..., k = k))
  } else {
      return(NA_real_)
  }
}

# BIC method
#' @export
BIC.rendo.tscope <- function(object, ...) {
  ll <- logLik(object)
  if (!is.na(ll[1]) && !is.null(attr(ll, "df")) && !is.na(attr(ll, "df")) &&
      !is.null(attr(ll, "nobs")) && !is.na(attr(ll, "nobs"))) {
      return(stats::BIC(ll, ...))
  } else {
      return(NA_real_)
  }
}