skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")

# Setup ----------------------------------------------------------------------------
fit_copulaIMA_lowboots <- function(
  formula,
  data,
  cdf = "adj.ecdf",
  num.boots = 10,
  verbose = FALSE
) {
  return(withCallingHandlers(
    copulaIMA(
      formula = formula,
      data = data,
      cdf = cdf,
      num.boots = num.boots,
      verbose = verbose
    ),
    warning = function(w) {
      if (grepl("recommended to run 1000", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
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
  res_combined <- fit_copulaIMA_lowboots(
    formula = y ~ P1 + P2 | continuous(P1, P2),
    data = dataCopIMAMultiEndo
  )

  res_separate <- fit_copulaIMA_lowboots(
    formula = y ~ P1 + P2 | continuous(P1) + continuous(P2),
    data = dataCopIMAMultiEndo
  )

  expect_equal(coef(res_combined), coef(res_separate))
})


test_that("Intercept in formula is correctly respected", {
  res_with <- fit_copulaIMA_lowboots(
    formula = y ~ X + P | continuous(P),
    data = dataCopIMAContExo
  )

  res_without <- fit_copulaIMA_lowboots(
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
  res_xy <- fit_copulaIMA_lowboots(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  res_yx <- fit_copulaIMA_lowboots(
    formula = y ~ P + X - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  expect_equal(
    object = sort(coef(res_xy)[names(coef(res_xy))]),
    expected = sort(coef(res_yx)[names(coef(res_yx))])
  )
})

test_that("Duplicate regressors are handled correctly", {
  res_normal <- fit_copulaIMA_lowboots(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  res_dup <- fit_copulaIMA_lowboots(
    formula = y ~ X + X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )

  expect_equal(object = coef(res_dup), expected = coef(res_normal))
})

# Parameter recovery -------------------------------------------------------
expect_param_recovery <- function(res, true_vals) {
  coefs <- coef(res)
  ses <- sqrt(diag(vcov(res)))
  for (nm in names(true_vals)) {
    diff <- abs(coefs[nm] - true_vals[nm])
    expect_true(
      object = diff < 2 * ses[nm],
      info = sprintf(
        "%s: est=%.3f, true=%.3f, 2*SE=%.3f",
        nm,
        coefs[nm],
        true_vals[nm],
        2 * ses[nm]
      )
    )
  }
}

copulaIMA_param_recovery <- function(formula, data, true_vals) {
  res <- copulaIMA(
    formula = formula,
    data = data,
    cdf = "adj.ecdf",
    num.boots = 1000,
    verbose = FALSE
  )
  expect_param_recovery(res = res, true_vals = true_vals)
}

test_that("Parameter recovery: dataCopIMAContExo", {
  copulaIMA_param_recovery(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo,
    true_vals = c(X = 1, P = 1)
  )
})

test_that("Parameter recovery: dataCopIMAMultiEndo", {
  copulaIMA_param_recovery(
    formula = y ~ P1 + P2 | continuous(P1, P2),
    data = dataCopIMAMultiEndo,
    true_vals = c("(Intercept)" = 10, P1 = 1, P2 = 1)
  )
})

test_that("Parameter recovery: dataCopIMABinExo", {
  copulaIMA_param_recovery(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMABinExo,
    true_vals = c(X = 1, P = 1)
  )
})

# Return values calculated correctly -------------------------------------------------

test_that("structural residuals & fitted values are calculated correctly", {
  res <- fit_copulaIMA_lowboots(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo
  )
  res.lm <- res$res.lm.augmented

  # Alternative route: Remove cop contribution from augmented fit
  names.coefs.cop <- c("P_cop")
  pcop.coefs <- coef(res.lm)[names.coefs.cop]
  cop.matrix <- model.matrix(res.lm)[, names.coefs.cop, drop = FALSE]

  residuals.alt <- drop(residuals(res.lm) + cop.matrix %*% pcop.coefs)
  fitted.alt <- drop(fitted(res.lm) - cop.matrix %*% pcop.coefs)

  expect_equal(residuals(res), residuals.alt)
  expect_equal(fitted.values(res), fitted.alt)
})
