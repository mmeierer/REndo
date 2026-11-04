# tests only for the internal check helpers in f_copula2scopenp_checkinput.R
# these do 2sopeNP-specific input checks

# checkinput_copula2scopenp_npcdistbwargs -------------------------------------------
test_that("npcdistbwargs fails for invalid input", {
  cases <- list(
    "not a list" = list(npcdistbw.args = c(bwmethod = "cv.ls")),
    "unnamed" = list(npcdistbw.args = list("cv.ls")),
    "partially named" = list(npcdistbw.args = list(bwmethod = "cv.ls", "x")),
    "reserved xdat" = list(npcdistbw.args = list(xdat = 1)),
    "reserved ydat" = list(npcdistbw.args = list(ydat = 1))
  )
  run_cases_errmsg(
    fn = checkinput_copula2scopenp_npcdistbwargs,
    cases = cases,
    regexp = c("plain list", "must be named", "must be named", "reserved", "reserved")
  )
})

test_that("npcdistbwargs warns for unknown arg name", {
  expect_warning(
    object = checkinput_copula2scopenp_npcdistbwargs(
      npcdistbw.args = list(abc = 1)
    ),
    regexp = "Unknown argument"
  )
})

test_that("npcdistbwargs works with defaults and legals args", {
  expect_null(checkinput_copula2scopenp_npcdistbwargs(npcdistbw.args = list()))
  expect_null(checkinput_copula2scopenp_npcdistbwargs(
    npcdistbw.args = list(bwmethod = "cv.ls", nmulti = 2)
  ))
})

# checkinput_copula2scopenp_bws -----------------------------------------------------
test_that("bws fails for illegal inputs", {
  bw <- structure(list(), class = "condbandwidth")
  cases <- list(
    "not a list" = list(bws = "x", labels.endo = "P"),
    "unnamed list" = list(bws = list(bw), labels.endo = "P"),
    "duplicate names" = list(bws = list(P = bw, P = bw), labels.endo = c("P", "Q")),
    "names not endo" = list(bws = list(Q = bw), labels.endo = "P"),
    "names not endo (multi)" = list(
      list(Q1 = bw, Q2 = bw),
      labels.endo = c("Q1", "P2")
    ),
    "wrong class" = list(bws = list(P = 1), labels.endo = "P")
  )
  run_cases_errmsg(
    fn = checkinput_copula2scopenp_bws,
    cases = cases,
    regexp = c(
      "must be a list",
      "named list",
      "duplicate names",
      "named after",
      "named after",
      "condbandwidth"
    )
  )
})

test_that("bws works for NULL and valid list", {
  bw <- structure(list(ynames = "P", xnames = "X"), class = "condbandwidth")
  expect_null(checkinput_copula2scopenp_bws(
    bws = NULL,
    labels.endo = "P",
    labels.exo = "X"
  ))
  expect_null(checkinput_copula2scopenp_bws(
    bws = setNames(list(bw), "P"),
    labels.endo = "P",
    labels.exo = "X"
  ))
})

test_that("bws fails if variables differ from the mf cols (incl order)", {
  bw.P <- structure(list(ynames = "P", xnames = c("X1", "X2")), class = "condbandwidth")
  bw.X21 <- structure(
    list(ynames = "P", xnames = c("X2", "X1")),
    class = "condbandwidth"
  )
  bw.X1 <- structure(list(ynames = "P", xnames = "X1"), class = "condbandwidth")
  bw.X123 <- structure(
    list(ynames = "P", xnames = c("X1", "X2", "X3")),
    class = "condbandwidth"
  )
  cases <- list(
    "exo different order" = list(bws = list(P = bw.X21), labels.endo = "P"),
    "exo missing" = list(bws = list(P = bw.X1), labels.endo = "P"),
    "exo additional" = list(bws = list(P = bw.X123), labels.endo = "P"),
    "endo different (transformed)" = list(
      bws = list("log(P)" = bw.P),
      labels.endo = "log(P)"
    )
  )
  run_cases_errmsg(
    fn = checkinput_copula2scopenp_bws,
    base.args = list(labels.exo = c("X1", "X2")),
    cases = cases,
    regexp = c(
      "exogenous regressors 'X2, X1' but must be for 'X1, X2' \\(same order required\\)",
      "exogenous regressors 'X1' but must be for 'X1, X2'",
      "exogenous regressors 'X1, X2, X3' but must be for 'X1, X2'",
      "endogenous regressor 'P' but must be for 'log\\(P\\)'"
    )
  )
})

test_that("bws works if variables match model frame columns (transformed, non-syntactic)", {
  bw.P1 <- structure(
    list(ynames = "P 1", xnames = c("log(X1)", "X 2")),
    class = "condbandwidth"
  )
  bw.logP2 <- structure(
    list(ynames = "log(P2)", xnames = c("log(X1)", "X 2")),
    class = "condbandwidth"
  )
  cases <- list(
    "single endo" = list(bws = list("`P 1`" = bw.P1), labels.endo = "`P 1`"),
    "multi endo" = list(
      bws = list("`P 1`" = bw.P1, "log(P2)" = bw.logP2),
      labels.endo = c("`P 1`", "log(P2)")
    )
  )
  run_cases_errmsg_empty(
    fn = checkinput_copula2scopenp_bws,
    base.args = list(labels.exo = c("log(X1)", "`X 2`")),
    cases = cases
  )
})

# checkinput_copula2scopenp_bws_npcdistbwargs ----------------------------------------
test_that("bws_npcdistbwargs fails if bwmethod differs from method of bws", {
  bw.nr <- structure(list(method = "normal-reference"), class = "condbandwidth")
  bw.cv <- structure(list(method = "cv.ls"), class = "condbandwidth")
  cases <- list(
    "nr bws, cv.ls" = list(
      bws = list(P = bw.nr),
      npcdistbw.args = list(bwmethod = "cv.ls")
    ),
    "cv bws, nr" = list(
      bws = list(P = bw.cv),
      npcdistbw.args = list(bwmethod = "normal-reference")
    ),
    "multi endo, only one differs" = list(
      bws = list(P1 = bw.cv, P2 = bw.nr),
      npcdistbw.args = list(bwmethod = "cv.ls")
    )
  )
  run_cases_errmsg(
    fn = checkinput_copula2scopenp_bws_npcdistbwargs,
    cases = cases,
    regexp = c(
      "Element 'P' of parameter 'bws' was estimated with bwmethod 'normal-reference'",
      "Element 'P' of parameter 'bws' was estimated with bwmethod 'cv.ls'",
      "^Element 'P2'"
    )
  )
})

test_that("bws_npcdistbwargs fails if no fitted bws and dont want to compute (bandwidth.compute=FALSE)", {
  cases <- list(
    "no bws" = list(bws = NULL, npcdistbw.args = list(bandwidth.compute = FALSE))
  )
  run_cases_errmsg(
    fn = checkinput_copula2scopenp_bws_npcdistbwargs,
    cases = cases,
    regexp = "Parameter 'bws' is required"
  )
})

test_that("bws_npcdistbwargs works if no conflict or nothing to check", {
  bw.nr <- structure(list(method = "normal-reference"), class = "condbandwidth")
  bw.cv <- structure(list(method = "cv.ls"), class = "condbandwidth")
  cases <- list(
    "no bws" = list(bws = NULL, npcdistbw.args = list(bwmethod = "cv.ls")),
    "same method" = list(
      bws = list(P = bw.cv),
      npcdistbw.args = list(bwmethod = "cv.ls")
    ),
    "no bwmethod" = list(bws = list(P = bw.nr), npcdistbw.args = list(nmulti = 2)),
    "no bandwidth search" = list(
      bws = list(P = bw.nr),
      npcdistbw.args = list(bwmethod = "cv.ls", bandwidth.compute = FALSE)
    )
  )
  run_cases_errmsg_empty(
    fn = checkinput_copula2scopenp_bws_npcdistbwargs,
    cases = cases
  )
})
