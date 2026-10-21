skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")
data("dataCopula2sCOPECase2")
data("dataCopula2sCOPECase3")

# Setup --------------------------------------------------------------------------------
setup.correctness <- list(
  fn = fit_2scope_fast,
  data = dataCopula2sCOPECase1,
  formula = y ~ P + X | P,
  label = "copula2sCOPE"
)

# Invariance ---------------------------------------------------------------------------
test_correctness_invariance_row_order(setup = setup.correctness)

test_correctness_invariance_regressor_order(
  setup = setup.correctness,
  formula.reordered = y ~ X + P | P
)

test_correctness_invariance_duplicate_regressor(
  setup = setup.correctness,
  formula.duplicate = y ~ P + X + X | P
)

test_correctness_invariance_dot(
  setup = setup.correctness,
  formula.dot = y ~ . | P
)

# Formula specifications ------------------------------------------------------
test_correctness_formula_intercept(setup = setup.correctness)

# Parameter recovery -------------------------------------------------------

test_that("Parameter recovery: dataCopula2sCOPECase1", {
  set.seed(42)
  res <- fit_2scope_defaults(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1
  )
  check_param_recovery(res = res, true_vals = c("(Intercept)" = 1, P = 1, X = -1))
})

test_that("Parameter recovery: dataCopula2sCOPECase2", {
  set.seed(42)
  res <- fit_2scope_defaults(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase2
  )
  check_param_recovery(res = res, true_vals = c("(Intercept)" = 1, P = 1, X = -1))
})

test_that("Parameter recovery: dataCopula2sCOPECase3", {
  set.seed(42)
  res <- fit_2scope_defaults(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase3
  )
  check_param_recovery(res = res, true_vals = c("(Intercept)" = 1, P = 1, X = -1))
})


# Parameter transformations ----------------------------------------------------------

test_that("Parameter transformations (exo): Recovery", {
  dataCopula2sCOPECase1_trans <- dataCopula2sCOPECase1
  dataCopula2sCOPECase1_trans$X <- exp(dataCopula2sCOPECase1_trans$X)

  res <- fit_2scope_defaults(
    formula = y ~ P + log(X) | P,
    data = dataCopula2sCOPECase1_trans
  )
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, P = 1, "log(X)" = -1)
  )
})


test_that("Parameter transformations (endo): Recovery", {
  dataCopula2sCOPECase1_trans <- dataCopula2sCOPECase1
  dataCopula2sCOPECase1_trans$P <- exp(dataCopula2sCOPECase1_trans$P)

  res <- fit_2scope_defaults(
    formula = y ~ log(P) + X | log(P),
    data = dataCopula2sCOPECase1_trans
  )
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, "log(P)" = 1, X = -1)
  )
})


# Internal consistency --------------------------------------------------------------

test_correctness_formula_edge_cases(
  setup = setup.correctness,
  cases = fixture_2scope_formula_edge_cases()
)

# Single endo + 0 exo: Collapses to copulaCorrection case 1 (continuous only) ---------
# (equivalent according to the paper)
# data case 3: need non-normally distributed exogenous

run_2scope_parkgupta_equivalent <- function(data) {
  # Need SE: With boots=1000
  res.2scope <- fit_2scope_defaults(
    formula = y ~ P | P,
    data = data
  )

  res.cc <- fit_copulacorrection_fast(
    formula = y ~ P | continuous(P),
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
