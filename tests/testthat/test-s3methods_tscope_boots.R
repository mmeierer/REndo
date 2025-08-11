# Bootstrapped S3 methods tests for tscope

context("S3methods - tscope (bootstrapped)")

data("dataTscope")

boot_fit <- tscope(y ~ p + w | p, data = dataTscope, num.boots = 30, verbose = FALSE)

# Helper for structural checks is auto-loaded from `helper-s3methods_tscope.R`

fct.helper.check.tscope.summary(boot_fit)

# Additional expectations specific to bootstrapped objects
test_that("print.summary for bootstrapped object does not show invalid SE warning", {
  res.sum <- summary(boot_fit)
  output  <- capture.output(print(res.sum))
  expect_false(any(grepl("Standard errors, p-values, and confidence intervals may not be", output, fixed = TRUE)))
})


test_that("vcov.rendo.tscope does not warn for bootstrapped object", {
  expect_silent(vcov(boot_fit))
})
