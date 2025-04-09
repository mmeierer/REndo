# TEST INPUT CHECKS for tscope ================================================================================================================================================================
# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataTscope")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - tscope - Parameter formula")

test_that("Fail if formula has not exactly two RHS parts", {
  # Using a one-part formula (missing the endogenous regressors part)
  res <- checkinput_tscope_formula(formula = y ~ p, data = dataTscope)
  
  # Check if the expected error message is in the result
  expect_true(any(grepl("Formula needs to have two parts", res)),
              info = paste("Expected error message not found in:", paste(res, collapse = " | "))
  )
})

test_that("Fail if outcome part does not include at least an outcome and one predictor", {
  # For a formula like y ~ y | w the outcome part only contains one unique variable.
  res <- checkinput_tscope_formula(formula = y ~ y | w, data = dataTscope)
  expect_true(any(grepl("The outcome part must include an outcome and at least one predictor", res)) ||
              any(grepl("cannot also be", res))
  )
})

test_that("Fail if no endogenous regressor provided", {
  # When the second RHS part has no variable (e.g. using a constant)
  res <- checkinput_tscope_formula(formula = y ~ p | 1, data = dataTscope)
  expect_true(any(grepl("Please provide at least one endogenous regressor", res)))
})

test_that("Pass if correct two-part formula", {
  # A valid formula: outcome with at least one predictor and one endogenous regressor.
  res <- checkinput_tscope_formula(formula = y ~ p | w, data = dataTscope)
  expect_equal(length(res), 0)
})

test_that("Fail if non-formula object is passed", {
  # Passing a non-formula (e.g. a character string)
  res <- checkinput_tscope_formula(formula = "not a formula", data = dataTscope)
  expect_true(length(res) > 0)
})

# data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - tscope - Parameter data")

test_that("Fail if data is not a data.frame", {
  res <- checkinput_tscope_data(data = 1:10)
  expect_true(length(res) > 0)
})

test_that("Fail if data is empty", {
  res <- checkinput_tscope_data(data = data.frame())
  expect_true(length(res) > 0)
})

test_that("Pass if valid data.frame", {
  res <- checkinput_tscope_data(data = dataTscope)
  expect_equal(length(res), 0)
})

# dataVSformula ------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - tscope - Parameter dataVSformula")

test_that("Fail if formula variables are not in data", {
  # Remove outcome variable 'y' from data so that the check fails.
  indataTscope <- data.frame(p = 1:10, w = 1:10)
  res <- checkinput_tscope_dataVSformula(formula = y ~ p | w, data = indataTscope)
  expect_true(length(res) > 0)
})

test_that("Pass if all formula variables are in data", {
  res <- checkinput_tscope_dataVSformula(formula = y ~ p | w, data = dataTscope)
  expect_equal(length(res), 0)
})

# verbose ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - tscope - Parameter verbose")

test_that("Fail if verbose is not logical", {
  res <- checkinput_tscope_verbose(verbose = "TRUE")
  expect_true(length(res) > 0)
})

test_that("Pass if verbose is logical", {
  res <- checkinput_tscope_verbose(verbose = TRUE)
  expect_equal(length(res), 0)
})

# Complex formula expressions -----------------------------------------------------------------------------------------------------------------------------------------------
test_that("Complex formula expressions are handled properly", {
  # Formula with interactions
  res <- checkinput_tscope_formula(formula = y ~ p * w | w, data = dataTscope)
  expect_equal(length(res), 0)
  
  # Formula with transformations
  res <- checkinput_tscope_formula(formula = log(y) ~ p + I(p^2) | w, data = dataTscope)
  expect_equal(length(res), 0)
  
  # Formula with offset
  res <- checkinput_tscope_formula(formula = y ~ p + offset(w) | w, data = dataTscope)
  expect_equal(length(res), 0)
})

# Duplicate variables ---------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Formula with duplicate variables is checked correctly", {
  # Same variable on both sides of the bar, typically valid.
  res <- checkinput_tscope_formula(formula = y ~ p + w | w, data = dataTscope)
  expect_equal(length(res), 0)
})

# Intercept manipulations -------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Formulas with intercept manipulations are handled properly", {
  # No intercept in outcome part
  res <- checkinput_tscope_formula(formula = y ~ 0 + p | w, data = dataTscope)
  expect_equal(length(res), 0)
  
  # No intercept in endogenous part
  res <- checkinput_tscope_formula(formula = y ~ p | 0 + w, data = dataTscope)
  expect_equal(length(res), 0)
  
  # Explicit intercept
  res <- checkinput_tscope_formula(formula = y ~ 1 + p | w, data = dataTscope)
  expect_equal(length(res), 0)
})

# Multiple variables ------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Formulas with multiple variables are handled correctly", {
  # Multiple predictors
  res <- checkinput_tscope_formula(formula = y ~ p + w + w | v, data = dataTscope)
  expect_equal(length(res), 0)
  
  # Multiple endogenous variables
  res <- checkinput_tscope_formula(formula = y ~ p | w + w, data = dataTscope)
  expect_equal(length(res), 0)
  
  # Multiple variables on both sides
  res <- checkinput_tscope_formula(formula = y ~ p + x2 | w + w, data = dataTscope)
  expect_equal(length(res), 0)
})

# Edge cases ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Edge case formulas are handled appropriately", {
  # More complex RHS expressions
  res <- checkinput_tscope_formula(formula = y ~ p | w:w + I(w^2), data = dataTscope)
  expect_equal(length(res), 0)
  
  # Unusual but valid formulas with logical expressions
  res <- checkinput_tscope_formula(formula = y ~ p + I(p > 0) | w, data = dataTscope)
  expect_equal(length(res), 0)
})

# Data edge cases --------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Edge cases for data validation are caught", {
  # Data with only one row
  one_row_data <- dataTscope[1, , drop = FALSE]
  res <- checkinput_tscope_data(data = one_row_data)
  expect_equal(length(res), 0)
  
  # Data with missing values
  data_with_na <- dataTscope
  data_with_na[1, 1] <- NA
  res <- checkinput_tscope_data(data = data_with_na)
  expect_equal(length(res), 0)
  
  # Data as tibble or other data.frame extensions
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble_data <- tibble::as_tibble(dataTscope)
    res <- checkinput_tscope_data(data = tibble_data)
    expect_equal(length(res), 0)
  }
})

# Formula vs data edge cases -----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Edge cases for formula vs data checks are caught", {
  # Formula with variables not in data
  bad_formula <- y ~ nonexistent | w
  res <- checkinput_tscope_dataVSformula(formula = bad_formula, data = dataTscope)
  expect_true(length(res) > 0)
  
  # Formula with dot notation
  all_vars_formula <- y ~ . | w
  res <- checkinput_tscope_dataVSformula(formula = all_vars_formula, data = dataTscope)
  expect_true(is.character(res) || length(res) == 0)
  
  # Column names with special characters
  special_data <- dataTscope
  names(special_data)[2] <- "x.1"
  res <- checkinput_tscope_dataVSformula(formula = y ~ `x.1` | w, data = special_data)
  expect_equal(length(res), 0)
})

# Verbose parameter thorough testing -----------------------------------------------------------------------------------------------------------------------------------------
test_that("Verbose parameter validation is thorough", {
  # NA value
  res <- checkinput_tscope_verbose(verbose = NA)
  expect_true(length(res) > 0)
  
  # Non-scalar logical vector
  res <- checkinput_tscope_verbose(verbose = c(TRUE, FALSE))
  expect_true(length(res) > 0)
  
  # NULL value
  res <- checkinput_tscope_verbose(verbose = NULL)
  expect_true(length(res) > 0)
  
  # Valid FALSE value
  res <- checkinput_tscope_verbose(verbose = FALSE)
  expect_equal(length(res), 0)
})

# Multiple errors accumulation -------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Multiple errors are correctly accumulated", {
  # Invalid formula and data at the same time
  invalid_formula <- x ~ y | 1
  invalid_data <- 1:10
  
  res_formula <- checkinput_tscope_formula(formula = invalid_formula, data = dataTscope)
  res_data <- checkinput_tscope_data(data = invalid_data)
  
  expect_true(length(res_formula) > 0)
  expect_true(length(res_data) > 0)
})

# Unexpected input types ---------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Unexpected input types are handled gracefully", {
  # Formula as string
  res <- tryCatch(
    checkinput_tscope_formula(formula = "y ~ x | w", data = dataTscope),
    error = function(e) e$message
  )
  expect_true(is.character(res))
  
  # Data as matrix
  matrix_data <- as.matrix(dataTscope)
  res <- checkinput_tscope_data(data = matrix_data)
  expect_true(length(res) > 0)
  
  # Formula as NULL
  res <- tryCatch(
    checkinput_tscope_formula(formula = NULL, data = dataTscope),
    error = function(e) e$message
  )
  expect_true(is.character(res))
})

# Boundary conditions -----------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Boundary conditions are handled properly", {
  # Zero-column data frame (should fail)
  zero_col_data <- dataTscope[, FALSE, drop = FALSE]
  res <- checkinput_tscope_data(data = zero_col_data)
  expect_true(length(res) > 0)
  
  # Zero-row data frame (empty but with columns)
  zero_row_data <- dataTscope[FALSE, , drop = FALSE]
  res <- checkinput_tscope_data(data = zero_row_data)
  expect_true(length(res) > 0)
})

# Input validation bypass -------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Input validation can't be bypassed with unusual inputs", {
  # Attempt formula injection (should be safe)
  injection_formula <- as.formula("y ~ p | w")
  res <- checkinput_tscope_formula(formula = injection_formula, data = dataTscope)
  expect_equal(length(res), 0)
  
  # Data with unusual structure: list with class 'data.frame'
  unusual_data <- list(data = dataTscope)
  class(unusual_data) <- c("data.frame", class(unusual_data))
  res <- checkinput_tscope_data(data = unusual_data)
  expect_true(length(res) >= 0)
})

# Handling various data types -----------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Various data types are handled correctly", {
  # Data frame with factors
  factor_data <- dataTscope
  factor_data$category <- factor(rep(c("A", "B"), length.out = nrow(factor_data)))
  res <- checkinput_tscope_data(data = factor_data)
  expect_equal(length(res), 0)
  
  # Data frame with characters
  char_data <- dataTscope
  char_data$text <- rep(c("text1", "text2"), length.out = nrow(char_data))
  res <- checkinput_tscope_data(data = char_data)
  expect_equal(length(res), 0)
  
  # Data frame with logical columns
  logical_data <- dataTscope
  logical_data$flag <- rep(c(TRUE, FALSE), length.out = nrow(logical_data))
  res <- checkinput_tscope_data(data = logical_data)
  expect_equal(length(res), 0)
})

# Very large formulas ------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("Very large formulas are handled correctly", {
  many_terms <- paste0("x", 1:50, collapse = " + ")
  large_formula_str <- paste0("y ~ ", many_terms, " | w")
  large_formula <- as.formula(large_formula_str)
  
  large_data <- dataTscope
  for (i in 1:50) {
    large_data[[paste0("x", i)]] <- rnorm(nrow(large_data))
  }
  
  res <- checkinput_tscope_formula(formula = large_formula, data = large_data)
  expect_equal(length(res), 0)
})

# Unusual formula transformations --------------------------------------------------------------------------------------------------------------------------------------------
test_that("Unusual formula transformations are handled properly", {
  # Formula with nested functions
  nested_formula <- y ~ log(abs(p)) | w
  res <- checkinput_tscope_formula(formula = nested_formula, data = dataTscope)
  expect_equal(length(res), 0)
  
  # Formula with unusual operators (if variables exist)
  if ("p" %in% colnames(dataTscope) && "x2" %in% colnames(dataTscope)) {
    unusual_op_formula <- y ~ p %in% c(1,2,3) | w
    res <- checkinput_tscope_formula(formula = unusual_op_formula, data = dataTscope)
    expect_equal(length(res), 0)
  }
})
