# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataTscope")

# Major test case -------------------------------------------------------------------------------------------------------------------
context("Correctness - tscope - Test Against Original")

# Test to ensure that the tscope function in the REndo package produces results
# consistent with a known, pre-calculated set of reference coefficients.
# This acts as a regression test to catch unintended changes in the core logic.
test_that("REndo 2sCOPE matches original tscope output", {
  tscope_reference <- c(
    "(Intercept)" = 0.9868224,
    "p"           = 1.0192808,
    "w"           = -0.9853235
  )
  tscope_model <- tscope(
    formula = y ~ p + w | p,
    data = dataTscope,
    verbose = FALSE
  )
  rendo_output <- coef(tscope_model)
  # Only compare the main coefficients
  expect_equal(rendo_output[names(tscope_reference)], tscope_reference, tolerance = 1e-6)
  # Check diagnostics in details
  expect_true(abs(tscope_model$details$corr_endostar_resid[1] - 0.4837118) < 1e-6)
  expect_true(abs(tscope_model$details$sdError - 0.9857568) < 1e-6)
})

# Input checks (Basic structural checks for tscope() function itself) ---------------------------------------------------------------
context("Correctness - tscope - Input Validation")

test_that("tscope() handles incorrect formula structures", {
  # Not a formula object
  expect_error(tscope(formula = "y ~ p | p", data = dataTscope))
  # One part formul
  expect_error(tscope(formula = y ~ p, data = dataTscope))
  # Wrong separato
  expect_error(tscope(formula = y ~ p + p, data = dataTscope))
  # Formula class but only one part (via Formula::as.Formula)
  expect_error(tscope(formula = Formula::as.Formula(y~p), data=dataTscope))
  # Outcome part does not include at least an outcome and one predictor
  expect_error(tscope(formula = y ~ y | w, data = dataTscope))
  # No endogenous regressor provided in the second part
  expect_error(tscope(formula = y ~ p | 1, data = dataTscope))
})

test_that("tscope() handles missing data in essential variables", {
  # Missing data in the outcome variable (y)
  data_missing_p <- dataTscope
  data_missing_p$p[1] <- NA
  expect_error(tscope(formula = y ~ p + w | p, data = data_missing_p))

  # Missing data in the endogenous variable (p)
  data_missing_y <- dataTscope
  data_missing_y$y[1] <- NA
  expect_error(tscope(formula = y ~ p + w | p, data = data_missing_y))

  # Missing data in the exogenous variable (w)
  data_missing_w <- dataTscope
  data_missing_w$w[1] <- NA
  expect_error(tscope(formula = y ~ p + w | p, data = data_missing_w))
})

test_that("tscope() handles incorrect data types for variables in formula", {
  # Endogenous variable 'p' is converted to character type.
  data_char_p <- dataTscope
  data_char_p$p <- as.character(data_char_p$p)
  expect_error(tscope(formula = y ~ p | p, data = data_char_p))
  
  # Outcome variable 'y' is converted to character type.
  data_char_y <- dataTscope
  data_char_y$y <- as.character(data_char_y$y)
  expect_error(tscope(formula = y ~ p | p, data = data_char_y))

  # Exogenous variable 'w' is converted to character type.
  data_char_w <- dataTscope
  data_char_w$w <- as.character(data_char_w$w)
  expect_error(tscope(formula = y ~ p + w | p, data = data_char_w))
})

test_that("tscope() handles variables not found in data", {
  # Test if tscope() throws an error when an exogenous variable ('Z') specified in the formula
  # is not present in the 'dataTscope' dataset.
  expect_error(tscope(formula = y ~ p + Z | p, data = dataTscope))
  
  # Test if tscope() throws an error when an endogenous variable ('Z') specified in the formula
  # (after the '|' separator) is not present in the 'dataTscope' dataset.
  expect_error(tscope(formula = y ~ p | Z, data = dataTscope))
})


# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - tscope - Formula transformations")

test_that("Transformations are correct for outcome variable (LHS)", {
  # Create a copy of the original data to modify
  data_altered_y <- dataTscope
  
  # Transform the outcome variable 'y' to ensure all its values are positive.
  # This is a prerequisite for applying a log transformation in the next step.
  min_y <- min(data_altered_y$y, na.rm = TRUE)
  data_altered_y$y_positive <- data_altered_y$y - min_y + 1 # Shift and add 1 to ensure positivity
  expect_true(all(data_altered_y$y_positive > 0)) # Verify all values are positive

  res_trans_lhs <- NULL; res_data_trans_lhs <- NULL

  # Test case 1: Apply log transformation directly within the formula (LHS)
  # tscope should handle the I() function for inline transformation.
  expect_silent(res_trans_lhs <- tscope(formula = I(log(y_positive)) ~ p + w | p, data = data_altered_y, verbose = FALSE))

  # Test case 2: Pre-transform the outcome variable and then use it in the formula
  data_trans <- data_altered_y
  data_trans$y_logged <- log(data_trans$y_positive) # Manually create the logged variable
  expect_silent(res_data_trans_lhs <- tscope(formula = y_logged ~ p + w | p, data = data_trans, verbose = FALSE))

  # Compare the results from both approaches
  # The coefficients for p, w, rho_p, and sdError should be identical if the LHS transformation is handled correctly.
  if (!is.null(res_trans_lhs) && !is.null(res_data_trans_lhs)) {
    coef_trans <- coef(res_trans_lhs, complete = TRUE) # Coefficients from inline transformation
    coef_data <- coef(res_data_trans_lhs, complete = TRUE)  # Coefficients from pre-transformed data
    
    # Check if the main predictor variables are correctly identified in both models
    expect_true(all(c("p", "w") %in% res_trans_lhs$names.main.coefs))
    expect_true(all(c("p", "w") %in% res_data_trans_lhs$names.main.coefs))

    # Compare the key coefficients: predictors, endogeneity parameter, and error term SD.
    # These should be equal (within tolerance) if the transformation was handled equivalently.
    expect_equal(coef_trans["p"], coef_data["p"], tolerance = 1e-6)
    expect_equal(coef_trans["w"], coef_data["w"], tolerance = 1e-6)
    # Check diagnostics in details
    expect_true(abs(res_trans_lhs$details$corr_endostar_resid[1] - res_data_trans_lhs$details$corr_endostar_resid[1]) < 1e-6)
    expect_true(abs(res_trans_lhs$details$sdError - res_data_trans_lhs$details$sdError) < 1e-6)
  } else {
    warning("Skipping LHS transformation comparison as object creation failed.")
  }
})

test_that("Transformations are correct for endogenous variable (RHS - both parts)", {
  res_trans_rhs <- NULL; res_data_trans_rhs <- NULL

  # Test case 1: Apply transformation directly within the formula for the endogenous variable
  # The endogenous variable 'p' is transformed to 'p*2 + 5' using I()
  # This transformation is applied in both the main model part (LHS of |) and the endogenous part (RHS of |)
  expect_silent(res_trans_rhs <- tscope(formula = y ~ I(p*2 + 5) + w | I(p*2 + 5), data = dataTscope, verbose = FALSE))
  
  # Test case 2: Pre-transform the endogenous variable in the data and then use it in the formula
  data_trans <- dataTscope
  data_trans$p_trans <- data_trans$p * 2 + 5 # Manually create the transformed variable
  expect_silent(res_data_trans_rhs <- tscope(formula = y ~ p_trans + w | p_trans, data = data_trans, verbose = FALSE))
  
  # Compare the results from both approaches if both models were successfully created
  if (!is.null(res_trans_rhs) && !is.null(res_data_trans_rhs)) {
    coef_trans <- coef(res_trans_rhs, complete = TRUE)
    coef_data  <- coef(res_data_trans_rhs, complete = TRUE)

    # Standardize coefficient names for comparison
    # When using I() in the formula, lm (and thus tscope) names the coefficient "I(p * 2 + 5)"
    # and the corresponding rho parameter "rho_I(p * 2 + 5)".
    # When using a pre-transformed variable 'p_trans', the names would be "p_trans" and "rho_p_trans".
    # We rename the coefficients from the pre-transformed data model to match the inline model for direct comparison.
    names(coef_data)[names(coef_data) == "p_trans"] <- "I(p * 2 + 5)"
    names(coef_data)[names(coef_data) == "rho_p_trans"] <- "rho_I(p * 2 + 5)"
     
    # Compare the key coefficients: Intercept, transformed endogenous variable, exogenous variable 'w',
    # the endogeneity parameter rho for the transformed variable, and the standard deviation of the error term.
    # These should be equal (within tolerance) if the transformation was handled equivalently.
    expect_equal(coef_trans["(Intercept)"], coef_data["(Intercept)"], tolerance=1e-6)
    expect_equal(coef_trans["I(p * 2 + 5)"], coef_data["I(p * 2 + 5)"], tolerance=1e-6)
    expect_equal(coef_trans["w"], coef_data["w"], tolerance=1e-6)
    # Check diagnostics in details
    expect_true(abs(res_trans_rhs$details$corr_endostar_resid[1] - res_data_trans_rhs$details$corr_endostar_resid[1]) < 1e-6)
    expect_true(abs(res_trans_rhs$details$sdError - res_data_trans_rhs$details$sdError) < 1e-6)

    # Verify that the transformed variable names are correctly identified in the model's internal structures
    expect_true("I(p * 2 + 5)" %in% res_trans_rhs$names.main.coefs)
    expect_true("p_trans" %in% res_data_trans_rhs$names.main.coefs)
  } else {
    warning("Skipping RHS endogenous transformation comparison as object creation failed.")
  }
})

test_that("Transformations are correct for exogenous variable (RHS - first part)", {
  res_trans_rhs <- NULL; res_data_trans_rhs <- NULL

  # Test case 1: Apply transformation directly within the formula for an exogenous variable
  # The exogenous variable 'w' is transformed to 'w^2' using I() in the main model part.
  # The endogenous variable 'p' remains unchanged.
  expect_silent(res_trans_rhs <- tscope(formula = y ~ p + I(w^2) | p, data = dataTscope, verbose = FALSE))
  
  # Test case 2: Pre-transform the exogenous variable in the data and then use it in the formula
  data_trans <- dataTscope
  data_trans$w_trans <- data_trans$w^2
  expect_silent(res_data_trans_rhs <- tscope(formula = y ~ p + w_trans | p, data = data_trans, verbose = FALSE))
  
  # Compare the results from both approaches if both models were successfully created
  if (!is.null(res_trans_rhs) && !is.null(res_data_trans_rhs)) {
    coef_trans <- coef(res_trans_rhs, complete = TRUE)
    coef_data  <- coef(res_data_trans_rhs, complete = TRUE)

    # Standardize coefficient names for comparison.
    # When using I() in the formula, the coefficient for the transformed 'w' is named "I(w^2)".
    # When using a pre-transformed variable 'w_trans', the coefficient is named "w_trans".
    # We rename the coefficient from the pre-transformed data model to match the inline model for direct comparison.
    names(coef_data)[names(coef_data) == "w_trans"] <- "I(w^2)"
    
    # Compare the key coefficients: Intercept, endogenous variable 'p', transformed exogenous variable 'I(w^2)',
    # the endogeneity parameter rho_p, and the standard deviation of the error term.
    # These should be equal (within tolerance) if the transformation was handled equivalently.
    expect_equal(coef_trans["(Intercept)"], coef_data["(Intercept)"], tolerance=1e-6)
    expect_equal(coef_trans["p"], coef_data["p"], tolerance=1e-6)
    expect_equal(coef_trans["I(w^2)"], coef_data["I(w^2)"], tolerance=1e-6)
    # Check diagnostics in details
    expect_true(abs(res_trans_rhs$details$corr_endostar_resid[1] - res_data_trans_rhs$details$corr_endostar_resid[1]) < 1e-6)
    expect_true(abs(res_trans_rhs$details$sdError - res_data_trans_rhs$details$sdError) < 1e-6)

    # Verify that the transformed variable names are correctly identified in the model's internal structures.
    # res_trans_rhs$names.main.coefs should contain "I(w^2)"
    # res_data_trans_rhs$names.main.coefs should contain "w_trans"
    expect_true("I(w^2)" %in% res_trans_rhs$names.main.coefs)
    expect_true("w_trans" %in% res_data_trans_rhs$names.main.coefs)

  } else { 
    warning("Skipping RHS exogenous transformation comparison as object creation failed.") 
  }
})

# Data sorting ------------------------------------------------------------------------------------------------
context("Correctness - tscope - Data sorting")

test_that("Results are independent of data row order", {
  formula_val <- y ~ p + w | p
  
  res_original <- tscope(formula = formula_val, data = dataTscope, verbose = FALSE)
  coefs_original <- coef(res_original, complete = TRUE)

  fitted_original <- fitted(res_original)
  # If original data has rownames, ensure fitted values are named accordingly for consistent comparison later
  # The names(fitted()) by default are row numbers of the data used in lm, which might be character numbers "1", "2", ...
  # This maps them back to original rownames if they exist.
  if(!is.null(rownames(dataTscope))) names(fitted_original) <- rownames(dataTscope)[as.numeric(names(fitted_original))]
  
  residuals_original <- residuals(res_original)
  if(!is.null(rownames(dataTscope))) names(residuals_original) <- rownames(dataTscope)[as.numeric(names(residuals_original))]

  # Set a seed for reproducibility of shuffling
  set.seed(456)
  # Create a shuffled version of the data
  shuffled_indices <- sample(nrow(dataTscope))
  data_shuffled <- dataTscope[shuffled_indices, ]
  
  # Run tscope with the shuffled data
  res_shuffled <- tscope(formula = formula_val, data = data_shuffled, verbose = FALSE)
  coefs_shuffled <- coef(res_shuffled, complete = TRUE)
  
  # Extract fitted values from the model run on shuffled data
  # Note: These will be in the shuffled order and their names will correspond to the row numbers of data_shuffled
  fitted_shuffled <- fitted(res_shuffled)

  residuals_shuffled <- residuals(res_shuffled)

  # Compare coefficients: they should be identical regardless of data order.
  # Sort names before comparison to ensure element-wise comparison is correct even if internal order differs.
  expect_equal(coefs_original[sort(names(coefs_original))], coefs_shuffled[sort(names(coefs_shuffled))])
  
  original_order_names <- rownames(dataTscope)
  
  reorder_back_to_original <- match(seq_len(nrow(dataTscope)), shuffled_indices)
  
  expect_equal(fitted_original, fitted_shuffled[reorder_back_to_original], tolerance=1e-7, ignore_attr = TRUE)
  expect_equal(residuals_original, residuals_shuffled[reorder_back_to_original], tolerance=1e-7, ignore_attr = TRUE)
})

# Core Logic Variations -----------------------------------------------------------------------------------------
context("Correctness - tscope - Core Logic Variations")

test_that("works correctly with single endogenous variable, no exogenous regressors (w)", {
  data_simple <- dataTscope[, c("y", "p"), drop = FALSE]
  res_simple <- NULL

  # Run tscope with a formula where 'y' is regressed on 'p', and 'p' is specified as endogenous.
  # No other exogenous regressors (like 'w') are included in the model.
  # expect_silent checks that the tscope function runs without producing any errors or warnings.
  expect_silent(res_simple <- tscope(formula = y ~ p | p, data = data_simple, verbose = FALSE))
  
  if (!is.null(res_simple)) {
    all_coefs <- coef(res_simple, complete = TRUE)

    # Verify the presence of the intercept term. It might be named "(Intercept)" or "x(Intercept)".
    expect_true("(Intercept)" %in% names(all_coefs) || "x(Intercept)" %in% names(all_coefs))
    # Verify the presence of the coefficient for the endogenous variable 'p'. It might be named "p" or "xp".
    expect_true("p" %in% names(all_coefs) || "xp" %in% names(all_coefs))
    # Check diagnostics in details
    expect_true(length(res_simple$details$corr_endostar_resid) == 1)
    expect_true(is.numeric(res_simple$details$sdError))
    # 'w' (matrix of exogenous regressors) should be NULL as none were specified in the formula.
    expect_null(res_simple$details$w)
    # 'stage1_resid' (residuals from the first stage regression) should be NULL as no exogenous regressors were used.
    expect_null(res_simple$details$stage1_resid)
    # 'endox' (matrix of endogenous variables) should contain one column named "p".
    expect_equal(colnames(res_simple$details$endox), "p")
    # 'endoxstar' (matrix of transformed endogenous variables, p + second-stage residuals) should have one column.
    expect_equal(ncol(res_simple$details$endoxstar), 1)
    # 'resid2' (residuals from the final model estimation) should be a numeric vector.
    expect_true(is.numeric(res_simple$details$resid2))
    # 'corr' (vector of estimated correlation parameters, rho) should have one element for 'rho_p'.
    expect_length(res_simple$details$corr_endostar_resid, 1)
    # The number of fitted values should match the number of rows in the input data.
    expect_length(fitted(res_simple), nrow(data_simple))
    # The number of residuals should match the number of rows in the input data.
    expect_length(residuals(res_simple), nrow(data_simple))
  } else {
    warning("Skipping checks as res_simple object not created.")
  }
})

# New test: single endogenous variable, no intercept ------------------------------------------------------------

context("Correctness - tscope - Single endogenous, no intercept")

test_that("works correctly with single endogenous variable and no intercept", {
  data_no_intercept <- dataTscope[, c("y", "p"), drop = FALSE]
  res_no_intercept <- NULL

  # Formula: y ~ p - 1 | p  remove intercept in main model
  expect_silent(res_no_intercept <- tscope(formula = y ~ p - 1 | p, data = data_no_intercept, verbose = FALSE))

  if (!is.null(res_no_intercept)) {
    all_coefs <- coef(res_no_intercept, complete = TRUE)
    # Intercept should NOT be present.
    expect_false(any(c("(Intercept)", "x(Intercept)") %in% names(all_coefs)))
    # Coefficient for the endogenous variable 'p' should be present.
    expect_true(any(c("p", "xp") %in% names(all_coefs)))
    # Diagnostics
    expect_length(res_no_intercept$details$corr_endostar_resid, 1)
    expect_true(is.numeric(res_no_intercept$details$sdError))
  } else {
    warning("Skipping checks as res_no_intercept object not created.")
  }
})

test_that("works correctly with single endogenous (p) and single exogenous (w)", {
  data_single_exo <- dataTscope[, c("y", "p", "w"), drop = FALSE]
  res_single_exo <- NULL

  # Run tscope with one endogenous (p) and one exogenous (w) variable.
  expect_silent(res_single_exo <- tscope(formula = y ~ p + w | p, data = data_single_exo, verbose = FALSE))
  
  if (!is.null(res_single_exo)) {
    all_coefs <- coef(res_single_exo, complete = TRUE)
    # Verify the presence of the intercept term.
    expect_true(any(c("(Intercept)", "x(Intercept)") %in% names(all_coefs)))
    # Verify the presence of the coefficient for the endogenous variable 'p'.
    expect_true(any(c("p", "xp") %in% names(all_coefs)))
    # Verify the presence of the coefficient for the exogenous variable 'w'.
    expect_true(any(c("w", "xw") %in% names(all_coefs)))
    # Check diagnostics in details
    expect_true(length(res_single_exo$details$corr_endostar_resid) == 1)
    expect_true(is.numeric(res_single_exo$details$sdError))
    # The number of estimated main coefficients should be 3 (Intercept, p, w).
    expect_length(all_coefs, 3)
    # 'w' (matrix of exogenous regressors) should not be NULL as 'w' was specified.
    expect_false(is.null(res_single_exo$details$w))
    # The column name in the exogenous regressors matrix should be "w".
    expect_equal(colnames(res_single_exo$details$w), "w")
    # 'stage1_resid' (residuals from the first stage regression) should not be NULL
    # because 'p' is residualized against 'w' in the first stage.
    expect_false(is.null(res_single_exo$details$stage1_resid))
    # There should be one column of residuals, corresponding to the single endogenous variable 'p'.
    expect_equal(ncol(res_single_exo$details$stage1_resid), 1)
    # The number of fitted values should match the number of rows in the input data.
    expect_length(fitted(res_single_exo), nrow(data_single_exo))
    # The number of residuals should match the number of rows in the input data.
    expect_length(residuals(res_single_exo), nrow(data_single_exo))
  } else {
    warning("Skipping checks as res_single_exo object not created.") 
  }
})

test_that("works with multiple endogenous (p, p2) and multiple exogenous (w, w2) variables", {
  data_multi <- dataTscope
  set.seed(123) # for reproducibility
  # Create a second endogenous variable p2, correlated with p
  data_multi$p2 <- data_multi$p + rnorm(nrow(data_multi), 0, 0.5)
  # Create a second exogenous variable w2, correlated with w
  data_multi$w2 <- data_multi$w + rnorm(nrow(data_multi), 0, 0.5)

  res_multi <- NULL
  # Run tscope with two endogenous variables (p, p2) and two exogenous variables (w, w2).
  # The formula specifies y ~ p + w + p2 + w2 as the main model, and p + p2 as the endogenous variables.
  expect_silent(res_multi <- tscope(formula = y ~ p + w + p2 + w2 | p + p2, data = data_multi, verbose = FALSE))
  
  if(!is.null(res_multi)){
    all_coefs <- coef(res_multi, complete = TRUE)

    # Verify the presence of coefficients for the first endogenous variable (p or xp).
    expect_true(any(c("p", "xp") %in% names(all_coefs)))
    # Verify the presence of coefficients for the first exogenous variable (w or xw).
    expect_true(any(c("w", "xw") %in% names(all_coefs)))
    # Verify the presence of coefficients for the second endogenous variable (p2 or xp2).
    expect_true(any(c("p2", "xp2") %in% names(all_coefs)))
    # Verify the presence of coefficients for the second exogenous variable (w2 or xw2).
    expect_true(any(c("w2", "xw2") %in% names(all_coefs)))
    # Check diagnostics in details
    expect_true(length(res_multi$details$corr_endostar_resid) == 2)
    expect_true(is.numeric(res_multi$details$sdError))
    # The number of estimated main coefficients should be 5 (Intercept, p, w, p2, w2).
    expect_length(all_coefs, 5)
    # 'endox' (matrix of original endogenous variables) should contain columns "p" and "p2".
    expect_equal(sort(colnames(res_multi$details$endox)), sort(c("p", "p2")))
    # 'endoxstar' (matrix of transformed endogenous variables) should have 2 columns (for p and p2).
    expect_equal(ncol(res_multi$details$endoxstar), 2)
    # 'w' (matrix of exogenous regressors) should not be NULL.
    expect_false(is.null(res_multi$details$w))
    # The exogenous regressors matrix should contain columns "w" and "w2".
    expect_equal(sort(colnames(res_multi$details$w)), sort(c("w", "w2")))
    # 'stage1_resid' (residuals from the first stage regression) should not be NULL.
    expect_false(is.null(res_multi$details$stage1_resid))
    # There should be 2 columns of residuals in stage1_resid, one for each endogenous variable (p and p2).
    expect_equal(ncol(res_multi$details$stage1_resid), 2)
  } else {
    warning("Skipping multi-variable test as tscope object not created. This might indicate tscope() does not support this scenario yet.")
  }
})

test_that("works correctly without an intercept in the main model", {
  res_no_intercept <- NULL
  # Run tscope with a formula that explicitly removes the intercept term (-1).
  # The model is y ~ p + w - 1, with 'p' as the endogenous variable.
  expect_silent(res_no_intercept <- tscope(formula = y ~ p + w - 1 | p, data = dataTscope, verbose = FALSE))
  
  if (!is.null(res_no_intercept)) {
    all_coefs <- coef(res_no_intercept, complete = TRUE)
    # Verify the absence of the intercept term in the coefficient names.
    expect_false("(Intercept)" %in% names(all_coefs))
    expect_false("x(Intercept)" %in% names(all_coefs))
    # Verify the presence of the coefficient for the endogenous variable 'p'.
    expect_true(any(c("p", "xp") %in% names(all_coefs)))
    # Verify the presence of the coefficient for the exogenous variable 'w'.
    expect_true(any(c("w", "xw") %in% names(all_coefs)))
    # Check diagnostics in details
    expect_true(length(res_no_intercept$details$corr_endostar_resid) == 1)
    expect_true(is.numeric(res_no_intercept$details$sdError))
    # The number of estimated main coefficients should be 2 (p, w).
    expect_length(all_coefs, 2)
    # 'endox' (matrix of endogenous variables) should contain one column named "p".
    expect_equal(colnames(res_no_intercept$details$endox), "p")
    # 'w' (matrix of exogenous regressors) should contain one column named "w".
    # Even without an intercept in the main model, 'w' is still part of the model.
    expect_equal(colnames(res_no_intercept$details$w), "w")
  } else { 
    warning("Skipping no-intercept test as object not created.") 
  }
})

test_that("Handles interaction terms correctly (e.g., p*w)", {
  res_interact <- NULL

  # Test 1: Interaction between an endogenous variable (p) and an exogenous variable (w)
  # The formula y ~ p * w | p specifies 'p' as endogenous and includes an interaction term p:w.
  expect_silent(res_interact <- tscope(formula = y ~ p * w | p, data = dataTscope, verbose = FALSE))
  
  if(!is.null(res_interact)){
    all_coefs <- coef(res_interact, complete = TRUE)
    # Verify that an interaction term "p:w" is present in the coefficient names.
    expect_true(any(grepl("p:w", names(all_coefs), fixed = TRUE)))
    # Check diagnostics in details
    expect_true(length(res_interact$details$corr_endostar_resid) == 1)
    expect_true(is.numeric(res_interact$details$sdError))
    # The number of estimated main coefficients should be 4 (Intercept, p, w, p:w).
    expect_length(all_coefs, 4)
    # Verify that the endogenous variable matrix 'endox' contains only "p".
    expect_equal(colnames(res_interact$details$endox), "p")
    # Verify that the exogenous variable matrix 'w' contains "w".
    expect_true("w" %in% colnames(res_interact$details$w)) 
    # Verify that "p:w" is listed among the main model coefficients' names.
    expect_true("p:w" %in% res_interact$names.main.coefs)
  } else { 
    warning("Skipping interaction test as object not created.") 
  }

  data_multi_interact <- dataTscope
  set.seed(789) # for reproducibility
  # Create a second endogenous variable 'p2', correlated with 'p'.
  data_multi_interact$p2 <- data_multi_interact$p + rnorm(nrow(data_multi_interact), 0, 0.5)
  
  res_multi_interact <- NULL

  # Test 2: Interaction between two endogenous variables (p, p2), plus an exogenous variable (w).
  # The formula y ~ p * p2 + w | p + p2 specifies 'p' and 'p2' as endogenous, and includes an interaction term p:p2.
  expect_silent(res_multi_interact <- tscope(formula = y ~ p * p2 + w | p + p2, data = data_multi_interact, verbose = FALSE))

  if(!is.null(res_multi_interact)){
    all_coefs_mi <- coef(res_multi_interact, complete = TRUE)
    # Verify that an interaction term "p:p2" is present in the coefficient names.
    expect_true(any(grepl("p:p2", names(all_coefs_mi), fixed = TRUE)))
    # Check diagnostics in details
    expect_true(length(res_multi_interact$details$corr_endostar_resid) == 2)
    expect_true(is.numeric(res_multi_interact$details$sdError))
  } else { 
    warning("Skipping multi-interaction test as object not created.") 
  }
})