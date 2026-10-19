set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopBMW")
data("dataCopBMWMultiEndo")

# data from copulaCorrection for tests vs P&G
data("dataCopCont")


# Invariance ---------------------------------------------------------------------------
test_that("Differently sorted data produces same coefs", {
  res.sorted <- fit_bmw_fast(
    formula = y ~ X + P | P,
    data = dataCopBMW[order(dataCopBMW$y), ]
  )
  res.rev <- fit_bmw_fast(
    formula = y ~ X + P | P,
    data = dataCopBMW[rev(order(dataCopBMW$y)), ]
  )
  expect_equal(coef(res.sorted), coef(res.rev))
})

test_that("Formula order does not impact coefs", {
  res.px <- fit_bmw_fast(formula = y ~ P + X | P, data = dataCopBMW)
  res.xp <- fit_bmw_fast(formula = y ~ X + P | P, data = dataCopBMW)
  expect_equal(sort(coef(res.px)), sort(coef(res.xp)))
})

test_that("Dot yields same result as explicit", {
  res.dot <- fit_bmw_fast(formula = y ~ . | P, data = dataCopBMW)
  res.explicit <- fit_bmw_fast(formula = y ~ X + P | P, data = dataCopBMW)
  expect_equal(coef(res.dot), coef(res.explicit))
})

test_that("Duplicate regressor yields same result as single", {
  res.normal <- fit_bmw_fast(formula = y ~ X + P | P, data = dataCopBMW)
  res.dup <- fit_bmw_fast(formula = y ~ X + X + P | P, data = dataCopBMW)
  expect_equal(coef(res.dup), coef(res.normal))
})

test_that("Intercept is respected", {
  res.with <- fit_bmw_fast(formula = y ~ X + P | P, data = dataCopBMW)
  res.without <- fit_bmw_fast(formula = y ~ X + P - 1 | P, data = dataCopBMW)
  expect_true("(Intercept)" %in% names(coef(res.with)))
  expect_false("(Intercept)" %in% names(coef(res.without)))
  expect_equal(length(coef(res.with)), length(coef(res.without)) + 1)
})


# Internal consistency --------------------------------------------------------------
test_that("Formula edge cases", {
  # Are labels etc correctly preserved
  run_cases(
    fn = fit_bmw_fast,
    cases = fixture_bmw_formula_edge_cases(),
    check = function(res) {
      check_labelled_consistently(res)
      check_struct_residuals(res = res, aux.names = res$labels.pcop)
    }
  )
})

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
test_that("Different cdf produce different result", {
  skip_on_cran()
  f.all <- y ~ X + P1 + P2 | P1 + P2
  res.adj <- fit_bmw_fast(formula = f.all, data = dataCopBMWMultiEndo, cdf = "adj.ecdf")
  res.ecdf <- fit_bmw_fast(formula = f.all, data = dataCopBMWMultiEndo, cdf = "ecdf")
  res.resc <- fit_bmw_fast(
    formula = f.all,
    data = dataCopBMWMultiEndo,
    cdf = "resc.ecdf"
  )
  res.kde <- fit_bmw_fast(formula = f.all, data = dataCopBMWMultiEndo, cdf = "kde")
  expect_false(isTRUE(all.equal(coef(res.adj), coef(res.ecdf))))
  expect_false(isTRUE(all.equal(coef(res.ecdf), coef(res.resc))))
  expect_false(isTRUE(all.equal(coef(res.resc), coef(res.kde))))
})


# Parameter recovery -------------------------------------------------------------------
test_that("Recovery: single endo (dataCopBMW)", {
  skip_on_cran()
  res <- fit_bmw_defaults(formula = y ~ X + P | P, data = dataCopBMW)
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, X = -1, P = 1)
  )
})

test_that("Recovery: multiple endo (dataCopBMWMultiEndo)", {
  skip_on_cran()
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
  skip_on_cran()
  df.trans <- dataCopBMW
  df.trans$P <- exp(df.trans$P)

  res <- fit_bmw_defaults(formula = y ~ X + log(P) | log(P), data = df.trans)
  check_param_recovery(
    res = res,
    true_vals = c("(Intercept)" = 1, X = -1, "log(P)" = 1)
  )
})

test_that("Recovery: Parameter transformations in exo", {
  skip_on_cran()
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
  skip_on_cran()

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
