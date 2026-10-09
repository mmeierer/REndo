skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")

# Works out of the box --------------------------------------------------------------
# this also checks running with verbose=T
test_that("Works out of the box", {
  res <- expect_message(copulaIMA(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  ))
  check_clean_fit(res)
})


# Specifications for smoke tests ----------------------------------------------------
# Specs vary along three dimensions:
# - Intercept: with vs without (including minimal no-intercept-no-exo model)
# - Exogenous regressors: continuous, binary, or none
# - Endogenous regressors: single vs multiple
# Each spec is crossed with all 4 cdf options

specs <- list(
  "no intercept, continuous exo, single endo" = list(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  ),
  "with intercept, continuous exo, single endo" = list(
    formula = y ~ X + P | P,
    data = dataCopIMAContExo
  ),
  "no intercept, binary exo, single endo" = list(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMABinExo
  ),
  "with intercept, no exo, multiple endo" = list(
    formula = y ~ P1 + P2 | P1 + P2,
    data = dataCopIMAMultiEndo
  ),
  "with intercept, no exo, single endo" = list(
    formula = y ~ P1 | P1,
    data = dataCopIMAMultiEndo
  ),
  "no intercept, no exo, single endo (minimal)" = list(
    formula = y ~ P1 - 1 | P1,
    data = dataCopIMAMultiEndo
  )
)

cdfs <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")

# specs x cdf ----------------------------------------------------------------------
test_that("copulaIMA runs across specs and cdf options", {
  for (cdf in cdfs) {
    run_cases(
      fn = fit_ima_fast,
      base.args = list(cdf = cdf),
      cases = specs,
      check = check_clean_fit
    )
  }
})

# Tolerate irrelevant data ----------------------------------------------------------
test_that("Works with NA in irrelevant col", {
  df <- dataCopIMAContExo
  df$abc <- rnorm(nrow(df))
  df$abc[1] <- NA
  res <- fit_ima_fast(formula = y ~ X + P | P, data = df)
  check_clean_fit(res)
})


# small sample ---------------------------------------------------------------------
test_that("copulaIMA runs on small sample", {
  small_data <- dataCopIMAContExo[1:50, ]
  res <- fit_ima_fast(
    formula = y ~ X + P - 1 | P,
    data = small_data
  )
  check_clean_fit(res)
})


# S3 methods -----------------------------------------------------------------------
test_that("S3 methods print and summary run without error", {
  res <- fit_ima_fast(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  )

  expect_no_error(capture_output(print(res)))
  expect_no_error(capture_output(print(summary(res))))
})
