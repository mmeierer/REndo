skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")

# Setup ----------------------------------------------------------------------------
fit_copulaima_lowboots <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 10,
  verbose = FALSE
) {
  return(suppress_lowboots_warning(
    copulaIMA(
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
  data_sorted <- dataCopIMAContExo[order(dataCopIMAContExo$y), ]
  data_rev <- dataCopIMAContExo[rev(order(dataCopIMAContExo$y)), ]

  res_sorted <- copulaIMA(
    formula = y ~ X + P - 1 | continuous(P),
    data = data_sorted,
    verbose = FALSE
  )
  res_rev <- copulaIMA(
    formula = y ~ X + P - 1 | continuous(P),
    data = data_rev,
    verbose = FALSE
  )

  expect_equal(object = coef(res_sorted), expected = coef(res_rev))
})

# Formula specifications ------------------------------------------------------
test_that("Equivalent continuous() specifications produce same results", {
  res_combined <- fit_copulaima_lowboots(
    formula = y ~ P1 + P2 | continuous(P1, P2),
    data = dataCopIMAMultiEndo
  )

  res_separate <- fit_copulaima_lowboots(
    formula = y ~ P1 + P2 | continuous(P1) + continuous(P2),
    data = dataCopIMAMultiEndo
  )

  expect_equal(coef(res_combined), coef(res_separate))
})


test_that("Intercept in formula is correctly respected", {
  res_with <- fit_copulaima_lowboots(
    formula = y ~ X + P | continuous(P),
    data = dataCopIMAContExo
  )

  res_without <- fit_copulaima_lowboots(
    formula = y ~ X + P - 1 | continuous(P),
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
  res_xy <- fit_copulaima_lowboots(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  res_yx <- fit_copulaima_lowboots(
    formula = y ~ P + X - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  expect_equal(
    object = sort(coef(res_xy)[names(coef(res_xy))]),
    expected = sort(coef(res_yx)[names(coef(res_yx))])
  )
})

test_that("Duplicate regressors are handled correctly", {
  res_normal <- fit_copulaima_lowboots(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  res_dup <- fit_copulaima_lowboots(
    formula = y ~ X + X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  expect_equal(object = coef(res_dup), expected = coef(res_normal))
})

# Parameter recovery -------------------------------------------------------

copulaima_param_recovery <- function(formula, data, true_vals) {
  res <- copulaIMA(
    formula = formula,
    data = data,
    cdf = "adj.ecdf",
    num.boots = 1000,
    verbose = FALSE
  )
  check_param_recovery(res = res, true_vals = true_vals)
}

test_that("Parameter recovery: dataCopIMAContExo", {
  copulaima_param_recovery(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo,
    true_vals = c(X = 1, P = 1)
  )
})

test_that("Parameter recovery: dataCopIMAMultiEndo", {
  copulaima_param_recovery(
    formula = y ~ P1 + P2 | continuous(P1, P2),
    data = dataCopIMAMultiEndo,
    true_vals = c("(Intercept)" = 10, P1 = 1, P2 = 1)
  )
})

test_that("Parameter recovery: dataCopIMABinExo", {
  copulaima_param_recovery(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMABinExo,
    true_vals = c(X = 1, P = 1)
  )
})

# Return values calculated correctly -------------------------------------------------

test_that("structural residuals & fitted values are calculated correctly", {
  res <- fit_copulaima_lowboots(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )
  check_struct_residuals(res = res, aux.names = "P_cop")
})
