skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")

# Works out of the box --------------------------------------------------------------
# this also checks running with verbose=T
test_that("Works out of the box", {
  expect_no_failure(capture_output(
    copulaIMA(
      formula = y ~ X + P - 1 | continuous(P),
      data = dataCopIMAContExo
    )
  ))
})


# Specifications for smoke tests ----------------------------------------------------
# Specs vary along three dimensions:
# - Intercept: with vs without (including minimal no-intercept-no-exo model)
# - Exogenous regressors: continuous, binary, or none
# - Endogenous regressors: single vs multiple
# Each spec is crossed with all 4 cdf options

specs <- list(
  "no intercept, continuous exo, single endo" = list(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  ),
  "with intercept, continuous exo, single endo" = list(
    formula = y ~ X + P | continuous(P),
    data = dataCopIMAContExo
  ),
  "no intercept, binary exo, single endo" = list(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMABinExo
  ),
  "with intercept, no exo, multiple endo" = list(
    formula = y ~ P1 + P2 | continuous(P1, P2),
    data = dataCopIMAMultiEndo
  ),
  "with intercept, no exo, single endo" = list(
    formula = y ~ P1 | continuous(P1),
    data = dataCopIMAMultiEndo
  ),
  "no intercept, no exo, single endo (minimal)" = list(
    formula = y ~ P1 - 1 | continuous(P1),
    data = dataCopIMAMultiEndo
  )
)

cdfs <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")

expect_no_error_ignore_boot_warn <- function(expr) {
  expect_no_error(
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (grepl("recommended to run 1000", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  )
}

# specs x cdf ----------------------------------------------------------------------
test_that("copulaIMA runs across specs and cdf options", {
  for (spec_name in names(specs)) {
    for (cdf in cdfs) {
      res <- expect_no_error_ignore_boot_warn(
        copulaIMA(
          formula = specs[[spec_name]]$formula,
          data = specs[[spec_name]]$data,
          cdf = cdf,
          verbose = FALSE,
          num.boots = 10
        )
      )
      expect_false(anyNA(fitted(res)))
      expect_false(anyNA(residuals(res)))
      expect_false(anyNA(coef(res)))
      expect_false(anyNA(vcov(res)))
      expect_false(anyNA(res$boots.params))
    }
  }
})

# small sample ---------------------------------------------------------------------
test_that("copulaIMA runs on small sample", {
  small_data <- dataCopIMAContExo[1:50, ]
  expect_no_error_ignore_boot_warn(
    copulaIMA(
      formula = y ~ X + P - 1 | continuous(P),
      data = small_data,
      cdf = "adj.ecdf",
      verbose = FALSE,
      num.boots = 10
    )
  )
})


# S3 methods -----------------------------------------------------------------------
test_that("S3 methods print and summary run without error", {
  res <- expect_warning(
    copulaIMA(
      formula = y ~ X + P - 1 | continuous(P),
      data = dataCopIMAContExo,
      verbose = FALSE,
      num.boots = 10
    ),
    regexp = "recommended to run 1000"
  )

  expect_no_error(capture_output(print(res)))
  expect_no_error(capture_output(print(summary(res))))
})
