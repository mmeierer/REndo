skip_on_cran()
set.seed(42)

data("dataCopula2sCOPEnpCont")
data("dataCopula2sCOPEnpBi")
data("dataCopula2sCOPEnpMulti")


# Setup --------------------------------------------------------------------------------
setup.smoke <- list(
  method = copula2sCOPEnp,
  fn = fit_2scopenp_fast,
  data = dataCopula2sCOPEnpCont,
  formula = y ~ P + X | P,
  label = "copula2sCOPEnp"
)

# Out of the box --------------------------------------------------------------------
test_smoke_out_of_the_box(setup = setup.smoke)


# Formula -------------------------------------------------------------------------
test_smoke_formula_edge_cases(
  setup = setup.smoke,
  cases = fixture_2scopenp_formula_edge_cases()
)


# Discrete endo ------------------------------------------------------------
test_that("works with a discrete endo", {
  res <- fit_2scopenp_fast(formula = y ~ P + X | P, data = dataCopula2sCOPEnpBi)
  check_clean_fit(res)
})

# Factor data -------------------------------------------------------------------------
test_that("works with ordered factors in in endo and exo", {
  res <- fit_2scopenp_fast(
    formula = y ~ P1 + P2 + X1 + X2 + X3 | P1 + P2,
    data = dataCopula2sCOPEnpMulti[1:250, ],
    # cannot run with heuristic
    npcdistbw.args = list(nmulti = 1, tol = 1, ftol = 1)
  )
  check_clean_fit(res)
  check_print_summary(res)
})


# Tolerates irrelevant data ----------------------------------------------------------
test_smoke_na_irrelevant_col(setup = setup.smoke)


# Small sample ----------------------------------------------------------------------
test_smoke_small_sample(setup = setup.smoke)


# npcdistbw.args + bws: iterative refinement -----------------------------------------

test_that("works with a supplied bws and via iterative refinement", {
  # refinement starting with normal-reference does not work! It just uses
  # normal-reference again (uses bws$method and ignores bwmethod)!
  res_loose <- suppress_lowboots_warning(
    copula2sCOPEnp(
      formula = y ~ P + X | P,
      data = dataCopula2sCOPEnpCont,
      npcdistbw.args = list(bwmethod = "cv.ls", nmulti = 1, ftol = 1, tol = 1),
      num.boots = 2,
      verbose = FALSE
    )
  )
  check_clean_fit(res_loose)

  # refine: pass previous bandwidths back as starting points
  res_refined <- suppress_lowboots_warning(
    copula2sCOPEnp(
      formula = y ~ P + X | P,
      data = dataCopula2sCOPEnpCont,
      bws = res_loose$bws,
      npcdistbw.args = list(nmulti = 1, ftol = 1, tol = 1),
      num.boots = 2,
      verbose = FALSE
    )
  )
  check_clean_fit(res_refined)
})

test_that("works with a supplied bws without recomputing them (bandwidth.compute = FALSE)", {
  # fit bws on subsample: recomputation on the full data would give other values
  res_subset <- fit_2scopenp_fast(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPEnpCont[1:100, ]
  )

  res <- fit_2scopenp_fast(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPEnpCont,
    bws = res_subset$bws,
    npcdistbw.args = list(bandwidth.compute = FALSE)
  )
  check_clean_fit(res)

  # bws kept as supplied
  expect_equal(res$bws$P$ybw, res_subset$bws$P$ybw)
  expect_equal(res$bws$P$xbw, res_subset$bws$P$xbw)
})


test_that("works with a supplied bws estimated with the np formula interface", {
  bws <- list(
    P = np::npcdistbw(
      formula = P ~ X,
      data = dataCopula2sCOPEnpCont,
      bwmethod = "normal-reference"
    )
  )

  res <- fit_2scopenp_fast(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPEnpCont,
    bws = bws,
    npcdistbw.args = list(bandwidth.compute = FALSE)
  )
  check_clean_fit(res)
})


# S3: Summary & print ------------------------------------------------------------
# They use nps internal (bw object) which differ by bw method and variable type

df.types <- dataCopula2sCOPEnpMulti[1:250, ]
# unordered exo (X3 is ordered)
df.types$X4 <- factor(sample(c("a", "b", "c"), nrow(df.types), replace = TRUE))


check_summary_bws_types <- function(res) {
  # print() and summary()
  check_print_summary(res)

  # bw table has each variable type
  out <- capture_output(print(summary(res)))
  for (type in c("continuous", "ordered", "unordered")) {
    # word boundary: 'ordered' is part of 'unordered'
    expect_match(out, paste0("\\b", type, "\\b"))
  }
}

test_that("summary and print work for normal-reference bws (for all types of exo)", {
  # normal-reference does not work for ordered endo
  res <- fit_2scopenp_fast(
    formula = y ~ P1 + X1 + X3 + X4 | P1,
    data = df.types,
    npcdistbw.args = list(bwmethod = "normal-reference")
  )
  check_clean_fit(res)
  check_summary_bws_types(res)
})

test_that("summary and print work for CV bws (for all types of endo and exo)", {
  res <- fit_2scopenp_fast(
    formula = y ~ P1 + P2 + X1 + X3 + X4 | P1 + P2,
    data = df.types,
    npcdistbw.args = list(bwmethod = "cv.ls", nmulti = 1, tol = 1, ftol = 1)
  )
  check_clean_fit(res)
  check_summary_bws_types(res)
})


# verbose ---------------------------------------------------------------------------
test_smoke_verbose(setup = setup.smoke)
