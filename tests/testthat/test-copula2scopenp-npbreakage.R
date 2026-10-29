# Early warning for breaking changes in package np ------------------------------------
# Runs ON PURPOSE on CRAN (no skip_on_cran()): CRAN runs these tests in the reverse
# dependency checks of new np versions, so breaking np changes are spotted before
# np is released.
#
# Only tests the touchpoints to package np (calls, args, variable types,
# bws paths, object fields, call/env).
#
# Redundant by design: Everything is also verified in the other test files
# (skipped on CRAN). If too slow on CRAN, disable by adding skip_on_cran() here.
# This only loses the early warning but no verification.

set.seed(42)

data("dataCopula2sCOPEnpCont")
data("dataCopula2sCOPEnpMulti")

# small data to keep runtime on CRAN low
data.small <- dataCopula2sCOPEnpCont[1:200, ]


# Calls --------------------------------------------------------------------------------
# np accepts REndo's calls with model frame data:
#  - npcdistbw(xdat=, ydat=) for the bandwidths
#  - npcdist(bws=, txdat=, tydat=) in the fit and the bootstrap
test_that("np accepts REndo's calls of npcdistbw() and npcdist()", {
  res <- fit_2scopenp_fast(formula = y ~ P + X | P, data = data.small)
  check_clean_fit(res)
})


# Args ---------------------------------------------------------------------------------
# REndo's input checks use the right argument names from npcdistbw (by using getS3method("npcdistbw", "default")).
# If np renames the method or the args documented in REndo, the check warns
# about valid user input (or fails)
test_that("np still has the npcdistbw() args documented in REndo", {
  expect_no_warning(expect_null(checkinput_copula2scopenp_npcdistbwargs(
    npcdistbw.args = list(
      bwmethod = "cv.ls",
      nmulti = 1,
      itmax = 100,
      ftol = 1,
      tol = 1,
      bandwidth.compute = TRUE
    )
  )))
})

# np accepts the documented args when passed through to npcdistbw()
test_that("np accepts the npcdistbw.args documented in REndos docu", {
  res <- expect_no_warning(fit_2scopenp_fast(
    formula = y ~ P + X | P,
    data = data.small,
    npcdistbw.args = list(
      bwmethod = "cv.ls",
      nmulti = 1,
      itmax = 10,
      ftol = 1,
      tol = 1
    )
  ))
  check_clean_fit(res)
})


# Data types ----------------------------------------------------------------
# np relies on the class of the data (continuous, ordered, unordered) to choose
# the kernel

# P2 and X3 are ordered
data.types <- dataCopula2sCOPEnpMulti[1:200, ]
data.types$X4 <- factor(sample(c("a", "b", "c"), nrow(data.types), replace = TRUE))

test_that("np accepts all variable types of exo (normal-reference)", {
  # normal-reference does not work for ordered endo
  res <- fit_2scopenp_fast(
    formula = y ~ P1 + X1 + X3 + X4 | P1,
    data = data.types,
    npcdistbw.args = list(bwmethod = "normal-reference")
  )
  check_clean_fit(res)
})

test_that("np accepts all variable types of endo and exo (cv.ls)", {
  res <- fit_2scopenp_fast(
    formula = y ~ P1 + P2 + X1 + X3 + X4 | P1 + P2,
    data = data.types,
    npcdistbw.args = list(
      bwmethod = "cv.ls",
      nmulti = 1,
      tol = 1,
      ftol = 1,
      itmax = 100
    )
  )
  check_clean_fit(res)
})


# bws paths ----------------------------------------------------------------------------
# np takes a different path (npcdistbw.condbandwidth()) when a condbandwidth object is
# passed as bws. REndo relies on two behaviours of this path:
#  - bandwidth.compute = FALSE: the bandwidths in bws are returned as-is
#  - bws given: np uses the method of bws and ignores bwmethod (basis of REndo's input
#    check checkinput_copula2scopenp_bws_npcdistbwargs())
# Whether np really starts the search from bws (refinement) is not observable.

bandwidth_small <- function(data, bws, npcdistbw.args) {
  return(copula2scopenp_bandwidth(
    F.formula = Formula::as.Formula(y ~ P + X | P),
    data = data,
    verbose = FALSE,
    labels.endo = "P",
    labels.exo = "X",
    bws = bws,
    npcdistbw.args = npcdistbw.args
  ))
}

test_that("np returns bws as-is for bandwidth.compute = FALSE", {
  bws.subset <- bandwidth_small(
    data = data.small[1:100, ],
    bws = NULL,
    npcdistbw.args = list(bwmethod = "normal-reference")
  )

  # run with previous fitted bws as input but different data (100 vs 250)
  bws <- bandwidth_small(
    data = data.small,
    bws = bws.subset,
    npcdistbw.args = list(bandwidth.compute = FALSE)
  )
  expect_identical(bws$P$xbw, bws.subset$P$xbw)
  expect_identical(bws$P$ybw, bws.subset$P$ybw)
})

test_that("np uses the method of bws and ignores bwmethod", {
  bws.nr <- bandwidth_small(
    data = data.small,
    bws = NULL,
    npcdistbw.args = list(bwmethod = "normal-reference")
  )
  bws <- bandwidth_small(
    data = data.small,
    bws = bws.nr,
    npcdistbw.args = list(bwmethod = "cv.ls", nmulti = 1, tol = 1, ftol = 1, itmax = 20)
  )
  expect_identical(bws$P$method, "normal-reference")
})


# Object fields ------------------------------------------------------------------------
# REndo reads undocumented fields of np's condbandwidth object directly:
#  - input checks and fit: method, xnames, ynames
#  - summary(): type flags (ixcon, ixuno, iycon, iyuno), bandwidth (raw bandwidth or
#    lambda), sumNum (scale factor or lambda max), pmethod, ptype, fval, fval.history
# If np renames or restructures them, the checks break or summary() prints wrong values
# silently (eg ifelse() on the type flags). Therefore check existence and structure.
#
# All variable types and cv.ls (fval only exists for cv.ls). Tested via
# copula2scopenp_bandwidth() because only the bw object is needed.
test_that("np's condbandwidth object has the fields read by REndo", {
  bws <- copula2scopenp_bandwidth(
    F.formula = Formula::as.Formula(y ~ P1 + P2 + X1 + X3 + X4 | P1 + P2),
    data = data.types,
    verbose = FALSE,
    labels.endo = c("P1", "P2"),
    labels.exo = c("X1", "X3", "X4"),
    bws = NULL,
    npcdistbw.args = list(bwmethod = "cv.ls", nmulti = 2, tol = 1, ftol = 1, itmax = 20)
  )

  # P1 continuous, P2 ordered
  for (p in c("P1", "P2")) {
    bw <- bws[[p]]

    # input checks and fit
    expect_identical(bw$method, "cv.ls", info = p)
    expect_identical(bw$ynames, p, info = p)
    expect_identical(bw$xnames, c("X1", "X3", "X4"), info = p)

    # summary(): type flags, per variable
    expect_identical(unname(bw$iycon), p == "P1", info = p)
    expect_identical(unname(bw$iyuno), FALSE, info = p)
    # X1 continuous, X3 ordered, X4 unordered
    expect_identical(unname(bw$ixcon), c(TRUE, FALSE, FALSE), info = p)
    expect_identical(unname(bw$ixuno), c(FALSE, FALSE, TRUE), info = p)

    # summary(): bandwidth / lambda and scale factor / lambda max, per variable
    expect_true(is.numeric(bw$bandwidth$y) && length(bw$bandwidth$y) == 1, info = p)
    expect_true(is.numeric(bw$bandwidth$x) && length(bw$bandwidth$x) == 3, info = p)
    expect_true(is.numeric(bw$sumNum$y) && length(bw$sumNum$y) == 1, info = p)
    expect_true(is.numeric(bw$sumNum$x) && length(bw$sumNum$x) == 3, info = p)

    # summary(): method and bandwidth type labels
    expect_true(is.character(bw$pmethod) && length(bw$pmethod) == 1, info = p)
    expect_true(is.character(bw$ptype) && length(bw$ptype) == 1, info = p)

    # summary(): objective function, one value per restart
    expect_true(is.numeric(bw$fval) && length(bw$fval) == 1, info = p)
    expect_true(is.numeric(bw$fval.history) && length(bw$fval.history) == 2, info = p)
  }
})


# Call/env -----------------------------------------------------------------------------
# REndo calls npcdistbw() with args as symbols in a small env (see
# copula2scopenp_bandwidth()). This relies on np storing match.call() with its
# parent.frame() in bw$call and evaluating xdat/ydat from there for downstream usage
# without data. If np changes this, users' downstream usage of res$bws breaks.
#
# Tested via copula2scopenp_bandwidth() because only the bw object is needed.
test_that("np stores the call with symbols and a small env, usable without data", {
  npcdistbw.args <- list(bwmethod = "normal-reference", nmulti = 2)
  bws <- bandwidth_small(data = data.small, bws = NULL, npcdistbw.args = npcdistbw.args)
  bw.call <- bws$P$call
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
  expect_no_error(predict(bws$P))
  dist <- expect_no_error(np::npcdist(bws = bws$P))
  expect_no_error(fitted(dist))
  expect_no_error(predict(dist))
})
