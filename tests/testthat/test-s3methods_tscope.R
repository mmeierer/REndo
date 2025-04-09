# filepath: /workspace/REndo/tests/testthat/test-s3methods_tscope.R
data("dataTscope")

context("S3 Methods for rendo.tscope objects")

# Enhanced mock object creation with better alignment to real tscope objects
create_mock_tscope <- function(include_clean_names = TRUE, valid_model = TRUE) {
  # Mock formula
  f <- y ~ w | p
  
  # Create a model with basic structure required by S3 methods
  model <- structure(list(), class = "rendo.tscope")
  model$call <- call("tscope", formula = f, data = quote(dataTscope))
  model$formula <- f
  
  # Set up model data with proper dimensions - keep it small for testing
  model_data <- head(dataTscope, 100)
  model$model <- model_data
  
  # Use exact names that would be used by the real function
  safe_names <- c("X.Intercept.", "w")
  orig_names <- c("(Intercept)", "w")
  
  # Create coefficients with both main and rho values
  model$coefficients <- c(2.5, 1.2, 0.5, 0.8)
  names(model$coefficients) <- c("(Intercept)", "w", "rho_p", "sdError")
  
  # Store original names and the mapping
  model$names.main.coefs <- orig_names
  model$orig_to_safe_map <- setNames(safe_names, orig_names)
  
  # Internal lm model - essential for many methods
  if (valid_model) {
    # Create with exactly the right dimensions and structure
    model$tscope_model <- lm(y ~ w, data = model_data)
  } else {
    model$tscope_model <- NULL
  }
  
  # Details required by predict and residuals methods
  model$details <- list(
    endox = data.frame(p = model_data$p),
    resid2 = rnorm(nrow(model_data))  # Diagnostic residuals with correct length
  )
  
  # Add clean names conditionally
  if (include_clean_names) {
    model$names.copula.coefs.clean <- c("star.p")
  }
  
  return(model)
}

# Tests for make_safe_names helper
test_that("make_safe_names works correctly", {
  original <- c("(Intercept)", "X 1", "X-1", "X.1", "w", "p")
  expected_safe <- c("X.Intercept.", "X.1.1", "X.1.2", "X.1", "w", "p")
  
  # Import the make_safe_names function from the package
  make_safe_names <- REndo:::make_safe_names
  
  safe <- make_safe_names(original)
  expect_equal(unname(safe), expected_safe) 
  expect_equal(names(safe), original)
})

# Tests for coef.rendo.tscope
test_that("coef.rendo.tscope returns complete coefficients correctly", {
  model <- create_mock_tscope()
  
  # Test with complete = TRUE (default)
  coefs_all <- coef(model)
  expect_equal(coefs_all, model$coefficients)
  expect_true(all(c(model$names.main.coefs, "sdError") %in% names(coefs_all)))
  expect_true("rho_p" %in% names(coefs_all))
  
  # Test with complete = FALSE
  coefs_main <- coef(model, complete = FALSE)
  # Direct indexing with original names to ensure proper retrieval
  expected_main_coefs <- model$coefficients[model$names.main.coefs]
  expect_equal(coefs_main, expected_main_coefs)
  expect_equal(names(coefs_main), model$names.main.coefs)
  
  # Test coefficients alias function
  expect_equal(coefficients(model), coef(model))
  expect_equal(coefficients(model, complete = FALSE), coef(model, complete = FALSE))
})

test_that("coef.rendo.tscope handles missing sdError", {
  model <- create_mock_tscope()
  model$coefficients <- model$coefficients[!names(model$coefficients) %in% "sdError"]
  
  coefs_all <- coef(model)
  expect_true("sdError" %in% names(coefs_all))
  # Since the model has a valid tscope_model, sdError will be computed from it
  expect_false(is.na(coefs_all["sdError"]))
})

# Tests for fitted.rendo.tscope
test_that("fitted.rendo.tscope works correctly", {
  model <- create_mock_tscope()
  
  fitted_vals <- fitted(model)
  expected_vals <- fitted(model$tscope_model)
  expect_equal(fitted_vals, expected_vals)
  expect_length(fitted_vals, nrow(model$model))
  expect_type(fitted_vals, "double")
  
  # Test names are preserved
  expect_equal(names(fitted_vals), rownames(model$model))
  
  # Test alias
  expect_equal(fitted.values(model), fitted(model))
})

test_that("fitted.rendo.tscope handles invalid model", {
  model <- create_mock_tscope(valid_model = FALSE)
  
  expect_error(fitted(model), "Internal lm object \\('tscope_model'\\) is missing or invalid")
})

# Tests for residuals.rendo.tscope
test_that("residuals.rendo.tscope works with type='response'", {
  model <- create_mock_tscope()
  
  resid_resp <- residuals(model)
  expected_resid <- residuals(model$tscope_model)
  expect_equal(resid_resp, expected_resid)
  expect_length(resid_resp, nrow(model$model))
  expect_type(resid_resp, "double")
  
  # Test alias
  expect_equal(resid(model), residuals(model))
})

test_that("residuals.rendo.tscope works with type='diagnostic'", {
  model <- create_mock_tscope()
  
  resid_diag <- residuals(model, type = "diagnostic")
  expect_equal(resid_diag, model$details$resid2)
  expect_length(resid_diag, nrow(model$model))
  expect_type(resid_diag, "double")
})

test_that("residuals.rendo.tscope handles missing diagnostic residuals", {
  model <- create_mock_tscope()
  model$details$resid2 <- NULL
  
  expect_warning(resid_diag <- residuals(model, type = "diagnostic"), 
                 "Diagnostic residuals \\(details\\$resid2\\) not available")
  # Should create a vector of NAs with length matching the model data
  expect_true(all(is.na(resid_diag)))
  expect_length(resid_diag, nrow(model$model))
})

test_that("residuals.rendo.tscope handles invalid type", {
  model <- create_mock_tscope()
  
  expect_warning(resid_invalid <- residuals(model, type = "invalid"),
                "Only type = 'response' or 'diagnostic' is currently supported")
  expect_equal(resid_invalid, residuals(model))
})

test_that("residuals.rendo.tscope handles invalid model", {
  model <- create_mock_tscope(valid_model = FALSE)
  
  expect_error(residuals(model), "Internal lm object \\('tscope_model'\\) is missing or invalid")
})

# Tests for summary.rendo.tscope
test_that("summary.rendo.tscope creates valid summary object", {
  model <- create_mock_tscope()
  
  sum_obj <- summary(model)
  expect_s3_class(sum_obj, "summary.rendo.tscope")
  
  # Check key components
  expect_equal(sum_obj$call, model$call)
  expect_equal(sum_obj$formula, model$formula)
  expect_true(is.matrix(sum_obj$coefficients))
  expect_equal(ncol(sum_obj$coefficients), 4)  # Standard coefficient table columns
  expect_equal(sum_obj$rho, model$coefficients["rho_p"])
  expect_equal(sum_obj$sdError, model$coefficients["sdError"])
  
  # Check stats from internal model
  expect_false(is.null(sum_obj$sigma))
  expect_false(is.null(sum_obj$r.squared))
  expect_false(is.null(sum_obj$adj.r.squared))
  expect_false(is.null(sum_obj$fstatistic))
  
  # Check model fit stats
  expect_false(is.na(sum_obj$AIC))
  expect_false(is.na(sum_obj$BIC))
  expect_false(is.na(sum_obj$logLik))
})

test_that("summary.rendo.tscope handles NULL input", {
  # We expect no warning here
  expect_silent(sum_null <- summary.rendo.tscope(NULL))
  expect_true(is.list(sum_null))
  expect_true(sum_null$null)
  expect_s3_class(sum_null, "summary.rendo.tscope")
})

test_that("summary.rendo.tscope handles missing internal model", {
  model <- create_mock_tscope(valid_model = FALSE)
  
  expect_warning(sum_obj <- summary(model), 
                "Could not generate summary from internal lm object")
  expect_true(is.list(sum_obj))
  expect_true(sum_obj$null)
})

test_that("summary.rendo.tscope handles missing copula names", {
  model <- create_mock_tscope(include_clean_names = FALSE)
  
  # This should still work without errors, using fallback approach
  expect_silent(sum_obj <- summary(model))
  expect_s3_class(sum_obj, "summary.rendo.tscope")
  expect_true(is.matrix(sum_obj$coefficients))
})

# Tests for print.summary.rendo.tscope
test_that("print.summary.rendo.tscope prints summary correctly", {
  model <- create_mock_tscope()
  sum_obj <- summary(model)
  
  expect_output(ret <- print(sum_obj), "Call:")
  expect_output(print(sum_obj), "Coefficients \\(Final Model\\):")
  expect_output(print(sum_obj), "--- Diagnostics ---")
  expect_output(print(sum_obj), "Endogeneity Diagnostics \\(rho\\):")
  expect_output(print(sum_obj), "Initial OLS Residual Std\\. Error \\(sdError\\):")
  expect_output(print(sum_obj), "--- Final Model Fit ---")
  expect_output(print(sum_obj), "Residual standard error:")
  expect_output(print(sum_obj), "Multiple R-squared:")
  expect_output(print(sum_obj), "F-statistic:")
  expect_output(print(sum_obj), "AIC:")
  
  expect_equal(ret, sum_obj)  # Should return invisibly
})

test_that("print.summary.rendo.tscope handles NULL summary", {
  null_sum <- structure(list(null = TRUE), class = "summary.rendo.tscope")
  
  expect_output(ret <- print(null_sum), "Summary object is NULL")
  expect_equal(ret, null_sum)  # Should return invisibly
})

test_that("print.summary.rendo.tscope handles missing elements gracefully", {
  model <- create_mock_tscope()
  sum_obj <- summary(model)
  
  # Test missing values handling
  incomplete_sum <- sum_obj
  incomplete_sum$rho <- NULL
  incomplete_sum$sdError <- NULL
  incomplete_sum$sigma <- NULL
  incomplete_sum$r.squared <- NULL
  incomplete_sum$fstatistic <- NULL
  incomplete_sum$AIC <- NULL
  incomplete_sum$BIC <- NULL
  incomplete_sum$logLik <- NULL
  
  # Should print without errors and show "Not available" for missing components
  expect_output(print(incomplete_sum), "\\(Not available\\)")
})

# Tests for predict.rendo.tscope
test_that("predict.rendo.tscope with newdata=NULL returns fitted values", {
  model <- create_mock_tscope()
  
  pred_null <- predict(model)
  expect_equal(pred_null, fitted(model))
})

test_that("predict.rendo.tscope requires newdata to be a data.frame", {
  model <- create_mock_tscope()
  
  expect_error(predict(model, newdata = list(w = 1, p = 2)), 
               "'newdata' must be a data.frame")
})

test_that("predict.rendo.tscope handles invalid model", {
  model <- create_mock_tscope(valid_model = FALSE)
  newdata <- data.frame(w = 1:3, p = 4:6)
  
  expect_error(predict(model, newdata = newdata), 
               "Internal lm object \\('tscope_model'\\) is missing or invalid")
})

# Tests for logLik, vcov, AIC, BIC
test_that("logLik.rendo.tscope works correctly", {
  model <- create_mock_tscope()
  
  ll <- logLik(model)
  expect_s3_class(ll, "logLik")
  expect_false(is.na(ll[1]))
  expect_false(is.null(attr(ll, "df")))
  expect_false(is.null(attr(ll, "nobs")))
})

test_that("logLik.rendo.tscope handles invalid model", {
  model <- create_mock_tscope(valid_model = FALSE)
  
  expect_warning(ll <- logLik(model), 
                "Final lm model object not found or invalid")
  expect_true(is.na(ll[1]))
  expect_s3_class(ll, "logLik")
})

test_that("vcov.rendo.tscope works correctly", {
  model <- create_mock_tscope()
  
  v <- vcov(model)
  expect_true(is.matrix(v))
  expect_equal(dim(v)[1], dim(v)[2])  # Square matrix
})

test_that("vcov.rendo.tscope handles invalid model", {
  model <- create_mock_tscope(valid_model = FALSE)
  
  expect_warning(v <- vcov(model), 
                "Final lm model object not found or invalid")
  expect_true(identical(v, NA_real_))
})

test_that("AIC.rendo.tscope works correctly", {
  model <- create_mock_tscope()
  
  aic_val <- AIC(model)
  expect_type(aic_val, "double")
  expect_false(is.na(aic_val))
})

test_that("AIC.rendo.tscope handles invalid model", {
  model <- create_mock_tscope(valid_model = FALSE)
  
  expect_warning(aic_val <- AIC(model), 
                "Final lm model object not found or invalid")
  expect_true(is.na(aic_val))
})

test_that("BIC.rendo.tscope works correctly", {
  model <- create_mock_tscope()
  
  bic_val <- BIC(model)
  expect_type(bic_val, "double")
  expect_false(is.na(bic_val))
})

test_that("BIC.rendo.tscope handles invalid model", {
  model <- create_mock_tscope(valid_model = FALSE)
  
  expect_warning(bic_val <- BIC(model), 
                "Final lm model object not found or invalid")
  expect_true(is.na(bic_val))
})