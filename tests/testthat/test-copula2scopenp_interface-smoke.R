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
  res_loose <- suppress_lowboots_warning(
    copula2sCOPEnp(
      formula = y ~ P + X | P,
      data = dataCopula2sCOPEnpCont,
      npcdistbw.args = list(bwmethod = "normal-reference"),
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


# verbose ---------------------------------------------------------------------------
test_smoke_verbose(setup = setup.smoke)
