skip_on_cran()
set.seed(42)

data("dataCopBMW")
data("dataCopBMWMultiEndo")

all.cdfs <- c("ecdf", "adj.ecdf", "resc.ecdf", "kde")


# Out of the box --------------------------------------------------------------------
test_that("Works out of the box", {
  res <- expect_message(copulaBMW(
    formula = y ~ X + P | P,
    data = dataCopBMW
  ))
  check_clean_fit(res)
})


# specs x cdf ----------------------------------------------------------------------
# Specs vary along:
# - intercept: w/ and w/o
# - endo: 1 vs multiple
#
# All combinations: cross each spec with all 4 cdf options
test_that("Works for all specs and cdf combinations", {
  specs <- list(
    "with intercept, single endo" = list(
      formula = y ~ X + P | P,
      data = dataCopBMW
    ),
    "no intercept, single endo" = list(
      formula = y ~ X + P - 1 | P,
      data = dataCopBMW
    ),
    "with intercept, multiple endo" = list(
      formula = y ~ X + P1 + P2 | P1 + P2,
      data = dataCopBMWMultiEndo
    ),
    "no intercept, multiple endo" = list(
      formula = y ~ X + P1 + P2 - 1 | P1 + P2,
      data = dataCopBMWMultiEndo
    )
  )

  for (cdf in all.cdfs) {
    run_cases(
      fn = fit_bmw_fast,
      base.args = list(cdf = cdf),
      cases = specs,
      check = check_clean_fit
    )
  }
})


# Formula -------------------------------------------------------------------------
test_that("Works for formula edge-cases, incl basic s3 (print, summary)", {
  results <- run_cases(
    fn = fit_bmw_fast,
    cases = fixture_bmw_formula_edge_cases(),
    check = check_clean_fit
  )

  # S3 methods on each edge case
  for (nm in names(results)) {
    expect_no_error(capture_output(print(results[[nm]])))
    expect_no_error(capture_output(print(summary(results[[nm]]))))
  }
})


# Tolerate irrelevant data ----------------------------------------------------------
test_that("Works with NA in irrelevant col", {
  df <- dataCopBMW
  df$abc <- rnorm(nrow(df))
  df$abc[1] <- NA
  res <- fit_bmw_fast(formula = y ~ X + P | P, data = df)
  check_clean_fit(res)
})


# Small sample ----------------------------------------------------------------------
test_that("Works on small sample", {
  small <- dataCopBMW[1:50, ]
  res <- fit_bmw_fast(formula = y ~ X + P | P, data = small)
  check_clean_fit(res)
})
