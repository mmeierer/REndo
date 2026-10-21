skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopBMW")
data("dataCopBMWMultiEndo")

# data from copulaCorrection for tests vs P&G
data("dataCopCont")


# Setup --------------------------------------------------------------------------------
setup.correctness <- list(
  method = copulaBMW,
  fn = fit_bmw_fast,
  data = dataCopBMW,
  formula = y ~ X + P | P,
  label = "copulaBMW"
)

# Invariance ---------------------------------------------------------------------------
test_correctness_invariance_row_order(setup = setup.correctness)

test_correctness_invariance_regressor_order(
  setup = setup.correctness,
  formula.reordered = y ~ P + X | P
)

test_correctness_invariance_duplicate_regressor(
  setup = setup.correctness,
  formula.duplicate = y ~ X + X + P | P
)

test_correctness_invariance_dot(
  setup = setup.correctness,
  formula.dot = y ~ . | P
)

# Formula specifications ------------------------------------------------------
test_correctness_formula_intercept(setup = setup.correctness)


# Internal consistency --------------------------------------------------------------
test_correctness_formula_edge_cases(
  setup = setup.correctness,
  cases = fixture_bmw_formula_edge_cases()
)

test_that("Internals have correct shape", {
  res <- fit_bmw_fast(formula = y ~ X + P | P, data = dataCopBMW, num.boots = 10)
  n <- nrow(res$model)
  expect_length(fitted(res), n)
  expect_length(residuals(res), n)
  expect_equal(rownames(res$boots.params), names(coef(res)))
  expect_equal(ncol(res$boots.params), 10)
  expect_equal(rownames(vcov(res)), names(coef(res)))
  expect_equal(colnames(vcov(res)), names(coef(res)))
})


# Parameter cdf ----------------------------------------------------------------------
test_correctness_all_cdfs_different_result(setup = setup.correctness)


# Parameter recovery -------------------------------------------------------------------
test_that("Recovery: single endo (dataCopBMW)", {
  set.seed(42)
  res <- fit_bmw_defaults(formula = y ~ X + P | P, data = dataCopBMW)
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, X = -1, P = 1)
  )
})

test_that("Recovery: multiple endo (dataCopBMWMultiEndo)", {
  set.seed(42)
  res <- fit_bmw_defaults(
    formula = y ~ X + P1 + P2 | P1 + P2,
    data = dataCopBMWMultiEndo
  )
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, X = -1, P1 = 1, P2 = 1)
  )
})


# Parameter transformations ------------------------------------------------------------
test_that("Recovery: Parameter transformations in endo", {
  set.seed(42)
  df.trans <- dataCopBMW
  df.trans$P <- exp(df.trans$P)

  res <- fit_bmw_defaults(formula = y ~ X + log(P) | log(P), data = df.trans)
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, X = -1, "log(P)" = 1)
  )
})

test_that("Recovery: Parameter transformations in exo", {
  set.seed(42)
  df.trans <- dataCopBMW
  df.trans$X <- exp(df.trans$X)

  res <- fit_bmw_defaults(formula = y ~ log(X) + P | P, data = df.trans)
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, "log(X)" = -1, P = 1)
  )
})


# Consistent with Park and Gupta -------------------------------------------------------
test_that("Collapses to P&G (when P independent of X)", {
  set.seed(42)

  res <- fit_bmw_defaults(formula = y ~ X1 + X2 + P | P, data = dataCopCont)

  res.cc <- fit_copulacorrection_fast(
    formula = y ~ X1 + X2 + P | continuous(P),
    data = dataCopCont
  )

  params.to.compare <- c("(Intercept)", "X1", "X2", "P")
  check_param_recovery(
    res = res,
    true_vals = coef(res.cc)[params.to.compare]
  )
})
