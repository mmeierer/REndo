skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")
data("dataCopula2sCOPECase2")
data("dataCopula2sCOPECase3")

# Setup ----------------------------------------------------------------------------
fit_copula2sCOPE_lowboots <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 10,
  verbose = FALSE
) {
  return(suppress_lowboots_warning(
    copula2sCOPE(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    )
  ))
}

# Data sorting ----------------------------------------------------------------------
test_that("Differently sorted data produces same results", {
  data_sorted <- dataCopula2sCOPECase1[order(dataCopula2sCOPECase1$y), ]
  data_rev <- dataCopula2sCOPECase1[rev(order(dataCopula2sCOPECase1$y)), ]

  res_sorted <- copula2sCOPE(
    formula = y ~ P + X | P,
    data = data_sorted,
    verbose = FALSE
  )
  res_rev <- copula2sCOPE(
    formula = y ~ P + X | P,
    data = data_rev,
    verbose = FALSE
  )

  expect_equal(object = coef(res_sorted), expected = coef(res_rev))
})

# Formula specifications ------------------------------------------------------
test_that("Intercept in formula is correctly respected", {
  res_with <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1
  )

  res_without <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X - 1 | P,
    data = dataCopula2sCOPECase1
  )

  expect_true("(Intercept)" %in% names(coef(res_with)))
  expect_false("(Intercept)" %in% names(coef(res_without)))
  expect_equal(
    object = length(coef(res_with)),
    expected = length(coef(res_without)) + 1L
  )
})

test_that("Regressor order in formula does not affect results", {
  res_px <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1
  )

  res_xp <- fit_copula2sCOPE_lowboots(
    formula = y ~ X + P | P,
    data = dataCopula2sCOPECase1
  )

  expect_equal(
    object = sort(coef(res_px)[names(coef(res_px))]),
    expected = sort(coef(res_xp)[names(coef(res_xp))])
  )
})

test_that("Duplicate regressors are handled correctly", {
  res_normal <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1
  )

  res_dup <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X + X | P,
    data = dataCopula2sCOPECase1
  )

  expect_equal(object = coef(res_dup), expected = coef(res_normal))
})

# Parameter recovery -------------------------------------------------------

copula2sCOPE_param_recovery <- function(formula, data, true_vals) {
  res <- copula2sCOPE(
    formula = formula,
    data = data,
    cdf = "adj.ecdf",
    num.boots = 1000,
    verbose = FALSE
  )
  check_param_recovery(res = res, true_vals = true_vals)
}

test_that("Parameter recovery: dataCopula2sCOPECase1", {
  copula2sCOPE_param_recovery(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1,
    true_vals = c("(Intercept)" = 1, P = 1, X = -1)
  )
})

test_that("Parameter recovery: dataCopula2sCOPECase2", {
  copula2sCOPE_param_recovery(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase2,
    true_vals = c("(Intercept)" = 1, P = 1, X = -1)
  )
})

test_that("Parameter recovery: dataCopula2sCOPECase3", {
  copula2sCOPE_param_recovery(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase3,
    true_vals = c("(Intercept)" = 1, P = 1, X = -1)
  )
})


# Parameter transformations ----------------------------------------------------------

test_that("Parameter transformations (exo): Recovery", {
  dataCopula2sCOPECase1_trans <- dataCopula2sCOPECase1
  dataCopula2sCOPECase1_trans$X <- exp(dataCopula2sCOPECase1_trans$X)

  copula2sCOPE_param_recovery(
    formula = y ~ P + log(X) | P,
    data = dataCopula2sCOPECase1_trans,
    true_vals = c("(Intercept)" = 1, P = 1, "log(X)" = -1)
  )
})


test_that("Parameter transformations (endo): Recovery", {
  dataCopula2sCOPECase1_trans <- dataCopula2sCOPECase1
  dataCopula2sCOPECase1_trans$P <- exp(dataCopula2sCOPECase1_trans$P)

  copula2sCOPE_param_recovery(
    formula = y ~ log(P) + X | log(P),
    data = dataCopula2sCOPECase1_trans,
    true_vals = c("(Intercept)" = 1, "log(P)" = 1, X = -1)
  )
})


# Return values calculated correctly -------------------------------------------------

test_that("structural residuals & fitted values are calculated correctly", {
  res <- expect_warning(
    fit_copula2sCOPE_lowboots(
      formula = y ~ P + X | P + X,
      data = dataCopula2sCOPECase1
    ),
    regexp = "No exogenous regressors"
  )
  check_struct_residuals(res = res, aux.names = c("P_cop", "X_cop"))
})

# Single endo + 0 exo: Collapses to copulaCorrection case 1 (continuous only) ---------
# (equivalent according to the paper)
# data case 3: need non-normally distributed exogenous

run_2scope_parkgupta_equivalent <- function(data) {
  # Need SE: With boots=1000
  expect_warning(
    res.2scope <- copula2sCOPE(
      formula = y ~ P | P,
      data = data,
      verbose = FALSE,
      num.boots = 1000
    ),
    regexp = "No exogenous regressors found"
  )

  res.cc <- fit_copulacorrection_fast(
    formula = y ~ P | P,
    data = data
  )

  check_param_recovery(
    res = res.2scope,
    true_vals = coef(res.cc)[c("(Intercept)", "P")]
  )
}

test_that("Single endo + NO exo: Same result as PG copulaCorrection (LL case 1) - data case 1", {
  run_2scope_parkgupta_equivalent(dataCopula2sCOPECase1)
})

test_that("Single endo + NO exo: Same result as PG copulaCorrection (LL case 1) - data case 2", {
  run_2scope_parkgupta_equivalent(dataCopula2sCOPECase2)
})
