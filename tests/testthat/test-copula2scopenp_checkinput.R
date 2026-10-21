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
