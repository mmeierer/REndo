skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")


# Data sorting ----------------------------------------------------------------------
test_that("Differently sorted data produces same results", {
  data_sorted <- dataCopIMAContExo[order(dataCopIMAContExo$y), ]
  data_rev <- dataCopIMAContExo[rev(order(dataCopIMAContExo$y)), ]

  res_sorted <- fit_ima_fast(
    formula = y ~ X + P - 1 | P,
    data = data_sorted
  )
  res_rev <- fit_ima_fast(
    formula = y ~ X + P - 1 | P,
    data = data_rev
  )

  expect_equal(object = coef(res_sorted), expected = coef(res_rev))
})

# Formula specifications ------------------------------------------------------

test_that("Intercept in formula is correctly respected", {
  res_with <- fit_ima_fast(
    formula = y ~ X + P | P,
    data = dataCopIMAContExo
  )

  res_without <- fit_ima_fast(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  )

  expect_true("(Intercept)" %in% names(coef(res_with)))
  expect_false("(Intercept)" %in% names(coef(res_without)))
  expect_equal(
    object = length(coef(res_with)),
    expected = length(coef(res_without)) + 1L
  )
})

test_that("Regressor order in formula does not affect results", {
  res_xy <- fit_ima_fast(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  )

  res_yx <- fit_ima_fast(
    formula = y ~ P + X - 1 | P,
    data = dataCopIMAContExo
  )

  expect_equal(object = sort(coef(res_xy)), expected = sort(coef(res_yx)))
})

test_that("Duplicate regressors are handled correctly", {
  res_normal <- fit_ima_fast(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  )

  res_dup <- fit_ima_fast(
    formula = y ~ X + X + P - 1 | P,
    data = dataCopIMAContExo
  )

  expect_equal(object = coef(res_dup), expected = coef(res_normal))
})

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

  check_param_recovery(res = res, true_vals = c("(Intercept)" = 10, X = 1, P1 = 1, P2 = -1))
})

test_that("Parameter recovery: dataCopIMABinExo", {
  res <- fit_ima_defaults(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMABinExo
  )
  check_param_recovery(res = res, true_vals = c(X = 1, P = 1))
})

# Internal consistency --------------------------------------------------------------

test_that("Formula edge cases", {
  # Are labels etc are correctly preserved
  run_cases(
    fn = fit_ima_fast,
    cases = fixture_ima_formula_edge_cases(),
    check = function(res) {
      check_labelled_consistently(res)
      check_struct_residuals(res = res, aux.names = res$labels.pcop)
    }
  )
})
