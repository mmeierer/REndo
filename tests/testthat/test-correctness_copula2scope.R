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
  return(withCallingHandlers(
    copula2sCOPE(
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
  data_sorted <- dataCopula2sCOPECase1[order(dataCopula2sCOPECase1$y), ]
  data_rev <- dataCopula2sCOPECase1[rev(order(dataCopula2sCOPECase1$y)), ]

  res_sorted <- copula2sCOPE(
    formula = y ~ P + X | continuous(P),
    data = data_sorted,
    verbose = FALSE
  )
  res_rev <- copula2sCOPE(
    formula = y ~ P + X | continuous(P),
    data = data_rev,
    verbose = FALSE
  )

  expect_equal(object = coef(res_sorted), expected = coef(res_rev))
})

# Formula specifications ------------------------------------------------------
test_that("Intercept in formula is correctly respected", {
  res_with <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase1
  )

  res_without <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X - 1 | continuous(P),
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
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase1
  )

  res_xp <- fit_copula2sCOPE_lowboots(
    formula = y ~ X + P | continuous(P),
    data = dataCopula2sCOPECase1
  )

  expect_equal(
    object = sort(coef(res_px)[names(coef(res_px))]),
    expected = sort(coef(res_xp)[names(coef(res_xp))])
  )
})

test_that("Duplicate regressors are handled correctly", {
  res_normal <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase1
  )

  res_dup <- fit_copula2sCOPE_lowboots(
    formula = y ~ P + X + X | continuous(P),
    data = dataCopula2sCOPECase1
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

copula2sCOPE_param_recovery <- function(formula, data, true_vals) {
  res <- copula2sCOPE(
    formula = formula,
    data = data,
    cdf = "adj.ecdf",
    num.boots = 1000,
    verbose = FALSE
  )
  expect_param_recovery(res = res, true_vals = true_vals)
}

test_that("Parameter recovery: dataCopula2sCOPECase1", {
  copula2sCOPE_param_recovery(
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase1,
    true_vals = c("(Intercept)" = 1, P = 1, X = -1)
  )
})

test_that("Parameter recovery: dataCopula2sCOPECase2", {
  copula2sCOPE_param_recovery(
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase2,
    true_vals = c("(Intercept)" = 1, P = 1, X = -1)
  )
})

test_that("Parameter recovery: dataCopula2sCOPECase3", {
  copula2sCOPE_param_recovery(
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase3,
    true_vals = c("(Intercept)" = 1, P = 1, X = -1)
  )
})


# Parameter transformations ----------------------------------------------------------

test_that("Parameter transformations (exo): Recovery", {
  dataCopula2sCOPECase1_trans <- dataCopula2sCOPECase1
  dataCopula2sCOPECase1_trans$X <- exp(dataCopula2sCOPECase1_trans$X)

  copula2sCOPE_param_recovery(
    formula = y ~ P + log(X) | continuous(P),
    data = dataCopula2sCOPECase1_trans,
    true_vals = c("(Intercept)" = 1, P = 1, "log(X)" = -1)
  )
})


test_that("Parameter transformations (endo): Recovery", {
  dataCopula2sCOPECase1_trans <- dataCopula2sCOPECase1
  dataCopula2sCOPECase1_trans$P <- exp(dataCopula2sCOPECase1_trans$P)

  copula2sCOPE_param_recovery(
    formula = y ~ log(P) + X | continuous(log(P)),
    data = dataCopula2sCOPECase1_trans,
    true_vals = c("(Intercept)" = 1, "log(P)" = 1, X = -1)
  )
})


# Return values calculated correctly -------------------------------------------------

test_that("structural residuals & fitted values are calculated correctly", {
  res <- expect_warning(
    fit_copula2sCOPE_lowboots(
      formula = y ~ P + X | continuous(P, X),
      data = dataCopula2sCOPECase1
    ),
    regexp = "No exogenous regressors"
  )
  res.lm <- res$res.lm.augmented

  # Alternative route: Remove cop contribution from augmented fit
  names.coefs.cop <- c("P_cop", "X_cop")
  pcop.coefs <- coef(res.lm)[names.coefs.cop]
  cop.matrix <- model.matrix(res.lm)[, names.coefs.cop, drop = FALSE]

  residuals.alt <- drop(residuals(res.lm) + cop.matrix %*% pcop.coefs)
  fitted.alt <- drop(fitted(res.lm) - cop.matrix %*% pcop.coefs)

  expect_equal(residuals(res), residuals.alt)
  expect_equal(fitted.values(res), fitted.alt)
})

# Single endo + 0 exo: Collapses to copulaCorrection case 1 (continuous only) ---------
# data case 3: need non-normally distributed exogenous

run_parkgupta_equivalent <- function(data) {
  # Need SE: With boots=1000
  expect_warning(
    res.2scope <- copula2sCOPE(
      formula = y ~ P | continuous(P),
      data = data,
      verbose = FALSE,
      num.boots = 1000
    ),
    regexp = "No exogenous regressors found"
  )

  expect_warning(
    res.cc <- copulaCorrection(
      formula = y ~ P | continuous(P),
      data = data,
      verbose = FALSE,
      num.boots = 3
    ),
    regexp = "It is recommended to run"
  )

  nms <- c("(Intercept)", "P")
  diff <- abs(coef(res.2scope)[nms] - coef(res.cc)[nms])
  expect_true(all(diff < sqrt(diag(vcov(res.2scope)))[nms]))
}

test_that("Single endo + NO exo: Same result as PG copulaCorrection (LL case 1) - data case 1", {
  run_parkgupta_equivalent(dataCopula2sCOPECase1)
})

test_that("Single endo + NO exo: Same result as PG copulaCorrection (LL case 1) - data case 2", {
  run_parkgupta_equivalent(dataCopula2sCOPECase2)
})
