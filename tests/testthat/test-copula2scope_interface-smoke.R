skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")

# Works out of the box --------------------------------------------------------------
# also checks verbose=T
test_that("Works out of the box", {
  expect_no_error(capture_output(
    copula2sCOPE(
      formula = y ~ P + X | P,
      data = dataCopula2sCOPECase1
    )
  ))
})


# Specifications for smoke tests ----------------------------------------------------
# Specs vary along three dimensions:
# - Intercept: w/ vs w/o
# - Exo: present vs absent
# - Endo: single vs multiple (X is not actually endo)
# Each spec is crossed with all 4 cdf options
#
# warn:
# - warn = NULL: no warning allowed
# - warn = "regexp": warning must appear

specs <- list(
  "with intercept, with exo, single endo" = list(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1,
    warn = NULL
  ),
  "no intercept, with exo, single endo" = list(
    formula = y ~ P + X - 1 | P,
    data = dataCopula2sCOPECase1,
    warn = NULL
  ),
  "with intercept, no exo, single endo" = list(
    formula = y ~ P | P,
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "no intercept, no exo, single endo" = list(
    formula = y ~ P - 1 | P,
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "with intercept, no exo, multiple endo" = list(
    formula = y ~ P + X | P + X,
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "no intercept, no exo, multiple endo" = list(
    formula = y ~ P + X - 1 | P + X,
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "with intercept, with exo, multiple endo" = list(
    formula = y ~ P + X + X2 | P + X,
    data = cbind(dataCopula2sCOPECase1, X2 = rnorm(nrow(dataCopula2sCOPECase1))),
    warn = NULL
  ),
  "no intercept, with exo, multiple endo" = list(
    formula = y ~ P + X + X2 - 1 | P + X,
    data = cbind(dataCopula2sCOPECase1, X2 = rnorm(nrow(dataCopula2sCOPECase1))),
    warn = NULL
  )
)

cdfs <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")


# specs x cdf ----------------------------------------------------------------------
test_that("copula2sCOPE runs across specs and cdf options", {
  for (spec_name in names(specs)) {
    for (cdf in cdfs) {
      spec <- specs[[spec_name]]

      if (is.null(spec$warn)) {
        res <- expect_no_warning(
          fit_2scope_fast(formula = spec$formula, data = spec$data, cdf = cdf)
        )
      } else {
        res <- expect_warning(
          fit_2scope_fast(formula = spec$formula, data = spec$data, cdf = cdf),
          regexp = spec$warn
        )
      }

      expect_false(anyNA(fitted(res)))
      expect_false(anyNA(residuals(res)))
      expect_false(anyNA(coef(res)))
      expect_false(anyNA(vcov(res)))
      expect_false(anyNA(res$boots.params))
    }
  }
})

# small sample ---------------------------------------------------------------------
test_that("copula2sCOPE runs on small sample", {
  small_data <- dataCopula2sCOPECase1[1:50, ]

  res <- fit_2scope_fast(
    formula = y ~ P + X | P,
    data = small_data
  )
  check_clean_fit(res)
})


test_that("copula2sCOPE allows NA in non-relevant column", {
  bad_data <- dataCopula2sCOPECase1
  bad_data$Q <- rnorm(nrow(dataCopula2sCOPECase1))
  bad_data$Q[1] <- NA
  res <- fit_2scope_fast(
    formula = y ~ P + X | P,
    data = bad_data
  )
  check_clean_fit(res)
})

# Formula -------------------------------------------------------------------------
test_that("Works for formula edge-cases, incl basic s3 (print, summary)", {
  results <- run_cases(
    fn = fit_2scope_fast,
    cases = fixture_2scope_formula_edge_cases(),
    check = check_clean_fit
  )

  # S3 methods on each edge case
  for (nm in names(results)) {
    expect_no_error(capture_output(print(results[[nm]])))
    expect_no_error(capture_output(print(summary(results[[nm]]))))
  }
})

})
