# Helper functions for S3-method unit tests of the 2sCOPE implementation
#
# This helper file is auto-loaded by testthat because its name starts with the
# conventional `helper-` prefix. It provides the helper used across the
# S3-method tests in one central place to avoid code duplication.

#' Structural checks for `summary.rendo.tscope` objects
#'
#' @param res.tscope An object returned by `tscope()` (either bootstrapped or
#'   not).
#'
#' @keywords internal
fct.helper.check.tscope.summary <- function(res.tscope) {
  testthat::test_that("summary returns correct class and structure", {
    testthat::expect_silent(res.sum <- summary(res.tscope))
    testthat::expect_s3_class(res.sum, "summary.rendo.tscope")
    testthat::expect_true(is.list(res.sum))
    testthat::expect_true("diagnostic_correlations" %in% names(res.sum))
    testthat::expect_true("endogenous_vars" %in% names(res.sum))
    testthat::expect_true("coefficients" %in% names(res.sum))
    testthat::expect_true(is.numeric(res.sum$diagnostic_correlations))
    testthat::expect_true(is.character(res.sum$endogenous_vars))
    testthat::expect_true(all(res.sum$endogenous_vars %in% colnames(res.tscope$details$endox)))
  })

  testthat::test_that("print.summary.rendo.tscope outputs diagnostics and (if applicable) SE warning", {
    testthat::expect_silent(res.sum <- summary(res.tscope))
    testthat::expect_output(print(res.sum), regexp = "Endogeneity diagnostics")
    
    output <- capture.output(print(res.sum))
    output_str <- paste(output, collapse = " ")
    
    if (!inherits(res.sum, "summary.rendo.boots") || (inherits(res.sum, "summary.rendo.boots") && !is.null(res.sum$num.boots) && res.sum$num.boots < 30)) {
      # For non-bootstrapped summaries or summaries based on very few bootstraps,
      # the warning about potentially invalid SEs must be shown.
      testthat::expect_true(grepl("Standard errors, p-values, and confidence intervals may not be", output_str, fixed = TRUE))
      testthat::expect_true(grepl("theoretically valid", output_str, fixed = TRUE))
    } else {
      # For sufficiently bootstrapped summaries the warning should be absent.
      testthat::expect_false(grepl("Standard errors, p-values, and confidence intervals may not be", output_str, fixed = TRUE))
    }
  })

  testthat::test_that("summary does not produce an R warning", {
    testthat::expect_silent(summary(res.tscope))
  })

  testthat::test_that("vcov.rendo.tscope warning behaviour", {
    if (!inherits(res.tscope, "rendo.boots") || ncol(res.tscope$boots.params) < 30) {
      testthat::expect_warning(vcov(res.tscope), regexp = "Standard errors from vcov may not be theoretically valid")
    } else {
      testthat::expect_silent(vcov(res.tscope))
    }
  })

  testthat::test_that("summary call and coefficient names are correct (addresses PR review)", {
    # This test ensures that the call output shows the original user call
    # and coefficient names show original variable names, not internal transformed names
    testthat::expect_silent(res.sum <- summary(res.tscope))

    # Check that the call field matches the original call, not the internal tscope_fit call
    testthat::expect_is(res.sum$call, "call")
    testthat::expect_identical(res.sum$call, res.tscope$call)

    # Check that coefficient names match the original variable names (stored in names.main.coefs)
    coef_names <- rownames(res.sum$coefficients)
    main_coef_names <- coef_names[1:length(res.tscope$names.main.coefs)]
    testthat::expect_equal(main_coef_names, res.tscope$names.main.coefs)
    
    # Ensure internal transformation indicators like ".Star" are not present in main coefficient names
    testthat::expect_false(any(grepl("\\.Star", main_coef_names)))

    # Check that the call output in print does not contain "tscope_fit" (internal function)
    output <- capture.output(print(res.sum))
    call_line <- output[grepl("Call:", output)]
    testthat::expect_false(any(grepl("tscope_fit", call_line)))

    # Check that the call line contains "tscope" (the user-facing function)
    testthat::expect_true(any(grepl("tscope", call_line)))
  })
}


