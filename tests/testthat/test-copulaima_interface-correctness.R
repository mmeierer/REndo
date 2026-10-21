skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")


# Setup --------------------------------------------------------------------------------
# IMA tests without intercept
setup.correctness <- list(
  method = copulaIMA,
  fn = fit_ima_fast,
  data = dataCopIMAContExo,
  formula = y ~ X + P - 1 | P,
  label = "copulaIMA"
)

# Invariance ---------------------------------------------------------------------------
test_correctness_invariance_row_order(setup = setup.correctness)

test_correctness_invariance_regressor_order(
  setup = setup.correctness,
  formula.reordered = y ~ P + X - 1 | P
)

test_correctness_invariance_duplicate_regressor(
  setup = setup.correctness,
  formula.duplicate = y ~ X + X + P - 1 | P
)

test_correctness_invariance_dot(
  setup = setup.correctness,
  formula.dot = y ~ . - 1 | P
)

# Formula specifications ------------------------------------------------------

test_correctness_formula_intercept(setup = setup.correctness)

# Parameter cdf ------------------------------------------------------------------------
test_correctness_all_cdfs_different_result(setup = setup.correctness)

# Parameter recovery -------------------------------------------------------

test_that("Parameter recovery: dataCopIMAContExo", {
  res <- fit_ima_defaults(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  )

  check_param_recovery(res = res, true_vals = c(X = 1, P = 1))
})

test_that("Parameter recovery: dataCopIMAMultiEndo", {
  res <- fit_ima_defaults(
    formula = y ~ X + P1 + P2 | P1 + P2,
    data = dataCopIMAMultiEndo
  )

  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 10, X = 1, P1 = 1, P2 = -1)
  )
})

test_that("Parameter recovery: dataCopIMABinExo", {
  res <- fit_ima_defaults(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMABinExo
  )
  check_param_recovery(res = res, true_vals = c(X = 1, P = 1))
})

# Internal consistency --------------------------------------------------------------

test_correctness_formula_edge_cases(
  setup = setup.correctness,
  cases = fixture_ima_formula_edge_cases()
)

# Bootstrap: Degenerate cases ---------------------------------------------------

test_that("fit() stops on constant data (required for bootstrapping)", {
  df.const.x <- dataCopIMAContExo
  df.const.x$X <- 1
  df.const.p <- dataCopIMAContExo
  df.const.p$P <- 1

  run_cases_error(
    fn = copulaima_fit,
    base.args = list(
      F.formula = as.Formula(y ~ X + P | P),
      labels.endo = "P",
      labels.exo = "X",
      cdf = "ecdf"
    ),
    cases = list(
      "constant exo" = list(data = df.const.x),
      "constant endo" = list(data = df.const.p)
    ),
    regexp = "must not be constant"
  )
})
