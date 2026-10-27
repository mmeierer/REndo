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
  bw <- structure(list(), class = "condbandwidth")
  expect_null(checkinput_copula2scopenp_bws(bws = NULL, labels.endo = "P"))
  expect_null(checkinput_copula2scopenp_bws(
    bws = setNames(list(bw), "P"),
    labels.endo = "P"
  ))
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
