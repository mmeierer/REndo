skip_on_cran()
set.seed(42)

data("dataCopula2sCOPEnpCont")
data("dataCopula2sCOPEnpBi")
data("dataCopula2sCOPEnpMulti")

# data from copulaCorrection for tests vs P&G
data("dataCopCont")


data.cont.pos <- local({
  df <- dataCopula2sCOPEnpCont
  df$P.pos <- abs(df$P) + 1
  df$X.pos <- abs(df$X) + 1
  return(df)
})

check_labelled_consistently_2scopenp <- function(res) {
  check_labelled_consistently(res)

  # one bw per endo, named after endo
  expect_setequal(names(res$bws), res$labels.endo)
}


# Setup --------------------------------------------------------------------------------
setup.correctness <- list(
  fn = fit_2scopenp_fast,
  data = dataCopula2sCOPEnpCont,
  formula = y ~ P + X | P,
  label = "copula2sCOPEnp"
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


# Internal consistency --------------------------------------------------------------

test_correctness_formula_edge_cases(
  setup = setup.correctness,
  cases = fixture_2scopenp_formula_edge_cases(),
  check = function(res) {
    check_labelled_consistently_2scopenp(res)
    check_struct_residuals(res = res)
  }
)


test_that("Internals have correct shape", {
  res <- fit_2scopenp_fast(formula = y ~ P + X | P, data = data.cont.pos)
  n <- nrow(res$model)
  expect_length(fitted(res), n)
  expect_length(residuals(res), n)
  expect_equal(rownames(vcov(res)), names(coef(res)))
  expect_equal(colnames(vcov(res)), names(coef(res)))
})


test_that("Ordered and factors in endo / exo", {
  # Also check if formula can make ordered and factor
  df.small <- dataCopula2sCOPEnpMulti[1:250, ]
  df.small$P2.char <- as.character(df.small$P2)
  df.small$X3.char <- as.character(df.small$X3)

  # fmt: skip
  res <- fit_2scopenp_fast(
    formula = y ~ P1 + ordered(P2.char) + X1 + X2 + factor(X3.char) |
      P1 + ordered(P2.char),
    npcdistbw.args = list(
      nmulti = 1,
      tol = 1,
      ftol = 1
    ),
    data = df.small
  )
  check_labelled_consistently_2scopenp(res)
  check_struct_residuals(res = res)

  # correct param names
  expect_named(
    coef(res),
    expected = c(
      "(Intercept)",
      "X1",
      "X2",
      "factor(X3.char)low",
      "factor(X3.char)medium",
      "P1",
      "ordered(P2.char).L",
      "ordered(P2.char).Q",
      "ordered(P2.char).C",
      "P1_cop",
      "`ordered(P2.char)_cop`"
    ),
    ignore.order = TRUE
  )

  # made to correct type
  res.s <- summary(res)
  df.table <- res.s$bws.summaries[[2]]$bandwidths
  expect_true(df.table[df.table$name == "ordered(P2.char)", "type"] == "ordered")
  expect_true(df.table[df.table$name == "factor(X3.char)", "type"] == "unordered")
})


# bws is used ----------------------------------------------------------------

test_that("Parameter bws is used as-is", {
  # When using heuristic, the result would be the same anyways. Therefore change it and
  # check that returns a different one
  res1 <- fit_2scopenp_fast(formula = y ~ P + X | P, data = data.cont.pos)

  bws.mod <- res1$bws
  for (nm in names(bws.mod)) {
    bws.mod[[nm]]$xbw <- bws.mod[[nm]]$xbw * 3
    bws.mod[[nm]]$ybw <- bws.mod[[nm]]$ybw * 3
  }

  res.perturbed <- fit_2scopenp_fast(
    formula = y ~ P + X | P,
    data = data.cont.pos,
    bws = bws.mod,
    npcdistbw.args = list(bandwidth.compute = FALSE)
  )

  # if bws were ignored the coefficients would be unchanged
  expect_false(isTRUE(all.equal(coef(res.perturbed), coef(res1))))
})

# bws call ------------------------------------------------------------------------

test_that("Call in bws has no data inlined and a small env", {
  npcdistbw.args <- list(bwmethod = "normal-reference", nmulti = 2)
  res <- fit_2scopenp_fast(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPEnpCont,
    npcdistbw.args = npcdistbw.args
  )
  bw.call <- res$bws$P$call
  expected.args <- c("xdat", "ydat", names(npcdistbw.args))

  # all args passed as symbols, incl the ones which np collects in `...`
  call.args <- as.list(bw.call)[-1]
  call.args <- c(call.args[names(call.args) != "..."], as.list(call.args[["..."]]))
  expect_setequal(names(call.args), expected.args)
  for (nm in names(call.args)) {
    expect_true(is.name(call.args[[nm]]), info = nm)
  }

  # env contains only the args, parent is np
  env <- environment(bw.call)
  expect_setequal(ls(env, all.names = TRUE), expected.args)
  expect_identical(parent.env(env), asNamespace("np"))

  # downstream usage in np without data still works
  expect_no_error(predict(res$bws$P))
  dist <- expect_no_error(np::npcdist(bws = res$bws$P))
  expect_no_error(fitted(dist))
  expect_no_error(predict(dist))
})

# Consistency with Park & Gupta ------------------------------------------------
# According to Kimberly:
# When the endogenous regressor \eqn{P} is independent of all exogenous regressors \eqn{X},
# the conditional CDF collapses to the marginal CDF, \eqn{\hat{F})(P|X) = \hat{F}(P)} and the
# method then reduces to the Park and Gupta (2012) copula correction.

run_2scopenp_parkgupta_equivalent <- function(
  formula.np,
  formula.pg,
  data,
  params.to.compare
) {
  res.np <- suppress_lowboots_warning(
    copula2sCOPEnp(
      formula = formula.np,
      data = data,
      npcdistbw.args = list(
        nmulti = 1,
        tol = 0.1,
        ftol = 0.1
      ),
      verbose = FALSE,
      num.boots = 100
    )
  )

  res.cc <- fit_copulacorrection_fast(formula = formula.pg, data = data)

  check_param_recovery(
    res = res.np,
    true_vals = coef(res.cc)[params.to.compare]
  )
}

test_that("Collapses to P&G - single continuous", {
  skip_on_os(c("windows", "mac")) # slow on Windows (np ca 6x slower) and macOS runners, still runs on linux

  run_2scopenp_parkgupta_equivalent(
    formula.np = y ~ X1 + P | P,
    formula.pg = y ~ X1 + P | continuous(P),
    data = dataCopCont,
    params.to.compare = c("(Intercept)", "X1", "P")
  )
})

test_that("Collapses to P&G - single continuous, single discrete", {
  skip_on_os(c("windows", "mac")) # slow on Windows (np ca 6x slower) and macOS runners, still runs on linux

  run_2scopenp_parkgupta_equivalent(
    formula.np = y ~ X1 + X2 + P1 + P2 | P1 + P2,
    formula.pg = y ~ X1 + X2 + P1 + P2 | discrete(P1) + continuous(P2),
    data = dataCopDisCont,
    # discrete P1 does not match and neither does intercept
    params.to.compare = c("X1", "X2", "P2")
  )
})

# Params recovery ------------------------------------------------------------------

test_that("Recovery: Continuous endo (dataCopula2sCOPEnpCont)", {
  skip_on_os(c("windows", "mac")) # slow on Windows (np ca 6x slower) and macOS runners, still runs on linux
  res <- suppress_lowboots_warning(copula2sCOPEnp(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPEnpCont,
    npcdistbw.args = list(nmulti = 1, tol = 0.1, ftol = 0.1),
    num.boots = 100,
    verbose = FALSE
  ))
  check_param_recovery(res = res, true_vals = c("(Intercept)" = 1, P = 1, X = 2))
})

test_that("Recovery: Binary endo (dataCopula2sCOPEnpBi)", {
  skip_on_os(c("windows", "mac")) # slow on Windows (np ca 6x slower) and macOS runners, still runs on linux
  res <- suppress_lowboots_warning(copula2sCOPEnp(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPEnpBi,
    npcdistbw.args = list(nmulti = 1, tol = 0.1, ftol = 0.1),
    num.boots = 100,
    verbose = FALSE
  ))
  check_param_recovery(res = res, true_vals = c("(Intercept)" = 0, P = 1, X = 2))
})

test_that("Recovery: multiple endo (dataCopula2sCOPEnpMulti)", {
  skip_on_os(c("windows", "mac")) # slow on Windows (np ca 6x slower) and macOS runners, still runs on linux
  res <- suppress_lowboots_warning(copula2sCOPEnp(
    formula = y ~ . | P1 + P2,
    data = dataCopula2sCOPEnpMulti,
    npcdistbw.args = list(nmulti = 1, tol = 0.5, ftol = 0.5),
    num.boots = 100,
    verbose = FALSE
  ))
  # check continuous numeric params only
  # Kimberly: "The intercept absorbs the mean contribution from P2 and X3 (the 2 ordered factors).
  # P2 levels coded: 1, 2,3 and 4 would give a mean of around 2.5 and X3 levels
  # coded 1, 2 and 3 would give a mean of around 2, the expected intercept shift would
  # approximately be alpha2 * mean(P2_n) + beta3 * mean(X3_n) = 1 *2.5 + 1 *2 = 4.5,
  # which would then be added to the true parameter of the intercept (1)"
  check_param_recovery(
    res = res,
    # "(Intercept)" = 1 + 4.5, vs 5.328 at 2*se = 0.086
    true_vals = c(P1 = -1, X1 = 2, X2 = 0.5)
  )

  # check expanded factors
  coefs <- coef(res)
  expect_true(all(c("P2.L", "P2.Q", "P2.C") %in% names(coefs)))
  expect_true(all(c("X3.L", "X3.Q") %in% names(coefs)))
  expect_true(coefs["P2.L"] > 0)
  expect_true(coefs["X3.L"] > 0)
})
