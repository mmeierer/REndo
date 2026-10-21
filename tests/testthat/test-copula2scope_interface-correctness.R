skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")
data("dataCopula2sCOPECase2")
data("dataCopula2sCOPECase3")
data("dataCopCont")
data("dataCopCont2")

# Setup --------------------------------------------------------------------------------
setup.correctness <- list(
  method = copula2sCOPE,
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

test_correctness_formula_transformations_endo_exo(setup = setup.correctness)

# Parameter cdf ------------------------------------------------------------------------
test_correctness_all_cdfs_different_result(setup = setup.correctness)

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


# Internal consistency --------------------------------------------------------------

test_correctness_formula_edge_cases(
  setup = setup.correctness,
  cases = fixture_2scope_formula_edge_cases()
)

# Consistent with Park and Gupta -------------------------------------------------------

# . With exo: recovers true values ----------------------------------------------------
# P&G continuous endo data: 2sCOPE contains PG, therefore can also recover its params
test_that("Recovery on P&G (copulaCorrection) data: With exo, single endo (dataCopCont)", {
  set.seed(42)
  res <- fit_2scope_defaults(
    formula = y ~ X1 + X2 + P | P,
    data = dataCopCont
  )
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 2, X1 = 1.5, X2 = -3, P = -1)
  )
})

test_that("Recovery on P&G (copulaCorrection) data: With exo, multi endo (dataCopCont2)", {
  set.seed(42)
  res <- fit_2scope_defaults(
    formula = y ~ X1 + X2 + P1 + P2 | P1 + P2,
    data = dataCopCont2
  )
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 2, X1 = 1.5, X2 = -3, P1 = -1, P2 = 0.8)
  )
})

# . No exo: Collapses to copulaCorrection --------------------------------------------------
# Collapses to P&G according to the paper = same result as copulaCorrection!
# Here is not about parameter recovery but to get same results through different
# estimator
# - single endo: compare vs MLE
# - multiple endo: both are augmented OLS but
#
# Data:
# - from copulaCorrection
# - from 2sCOPE (but 3rd case not ok for PG)

run_2scope_parkgupta_equivalent <- function(data, formula.2scope, formula.cc, params) {
  set.seed(42)
  # Need SE: With boots=1000
  res.2scope <- fit_2scope_defaults(formula = formula.2scope, data = data)

  res.cc <- fit_copulacorrection_fast(formula = formula.cc, data = data)

  check_param_recovery(res = res.2scope, true_vals = coef(res.cc)[params])
}

test_that("No exo: Same result as P&G implementation (copulaCorrection) - single endo (dataCopula2sCOPECase1)", {
  run_2scope_parkgupta_equivalent(
    data = dataCopula2sCOPECase1,
    formula.2scope = y ~ P | P,
    formula.cc = y ~ P | continuous(P),
    params = c("(Intercept)", "P")
  )
})

test_that("No exo: Same result as P&G implementation (copulaCorrection) - single endo (dataCopula2sCOPECase2)", {
  run_2scope_parkgupta_equivalent(
    data = dataCopula2sCOPECase2,
    formula.2scope = y ~ P | P,
    formula.cc = y ~ P | continuous(P),
    params = c("(Intercept)", "P")
  )
})

test_that("No exo: Same result as P&G implementation (copulaCorrection) - single endo (dataCopCont)", {
  run_2scope_parkgupta_equivalent(
    data = dataCopCont,
    formula.2scope = y ~ P | P,
    formula.cc = y ~ P | continuous(P),
    params = c("(Intercept)", "P")
  )
})

test_that("No exo: Same result as P&G implementation (copulaCorrection) - multi endo (dataCopCont2)", {
  run_2scope_parkgupta_equivalent(
    data = dataCopCont2,
    formula.2scope = y ~ P1 + P2 | P1 + P2,
    formula.cc = y ~ P1 + P2 | continuous(P1, P2),
    params = c("(Intercept)", "P1", "P2")
  )
})
