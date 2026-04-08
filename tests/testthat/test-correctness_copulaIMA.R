# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")

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
    formula = y ~ X + P | continuous(P),
    data = dataCopIMABinExo,
    true_vals = c(X = 1, P = 1)
  )
})
