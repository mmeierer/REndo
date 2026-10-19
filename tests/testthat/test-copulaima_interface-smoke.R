skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")

# Works out of the box --------------------------------------------------------------
# this also checks running with verbose=T
test_that("Works out of the box", {
  expect_message(res <- copulaIMA(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  ))
  check_clean_fit(res)
})


# Specifications for smoke tests ----------------------------------------------------
# Specs vary along three dimensions:
# - Intercept: with vs without
# - Exogenous regressors: continuous, or binary
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
  "with intercept, continuous exo, multiple endo" = list(
    formula = y ~ X + P1 + P2 | P1 + P2,
    data = dataCopIMAMultiEndo
  ),
  "no intercept, continuous exo, multiple endo" = list(
    formula = y ~ X + P1 + P2 - 1 | P1 + P2,
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

# Formula -------------------------------------------------------------------------

test_that("Works for formula edge-cases, incl basic s3 (print, summary)", {
  #   Do s3 methods work correctly
  results <- run_cases(
    fn = fit_ima_fast,
    cases = fixture_ima_formula_edge_cases(),
    check = check_clean_fit
  )

  # S3 methods on each edge case
  for (nm in names(results)) {
    check_print_summary(results[[nm]])
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
