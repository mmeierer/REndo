skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")

# Setup --------------------------------------------------------------------------------
setup.smoke <- list(
  method = copula2sCOPE,
  fn = fit_2scope_fast,
  data = dataCopula2sCOPECase1,
  formula = y ~ P + X | P,
  label = "copula2sCOPE"
)

# Works out of the box --------------------------------------------------------------
test_smoke_out_of_the_box(setup = setup.smoke)

test_smoke_verbose(setup = setup.smoke)


# Specifications for smoke tests ----------------------------------------------------
# Specs vary along three dimensions:
# - Intercept: w/ vs w/o
# - Exo: present vs absent
# - Endo: single vs multiple (X is not actually endo)
# Each spec is crossed with all 4 cdf options

specs <- list(
  "with intercept, with exo, single endo" = list(
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1
  ),
  "no intercept, with exo, single endo" = list(
    formula = y ~ P + X - 1 | P,
    data = dataCopula2sCOPECase1
  ),
  "with intercept, no exo, single endo" = list(
    formula = y ~ P | P,
    data = dataCopula2sCOPECase1
  ),
  "no intercept, no exo, single endo" = list(
    formula = y ~ P - 1 | P,
    data = dataCopula2sCOPECase1
  ),
  "with intercept, no exo, multiple endo" = list(
    formula = y ~ P + X | P + X,
    data = dataCopula2sCOPECase1
  ),
  "no intercept, no exo, multiple endo" = list(
    formula = y ~ P + X - 1 | P + X,
    data = dataCopula2sCOPECase1
  ),
  "with intercept, with exo, multiple endo" = list(
    formula = y ~ P + X + X2 | P + X,
    data = cbind(dataCopula2sCOPECase1, X2 = rnorm(nrow(dataCopula2sCOPECase1)))
  ),
  "no intercept, with exo, multiple endo" = list(
    formula = y ~ P + X + X2 - 1 | P + X,
    data = cbind(dataCopula2sCOPECase1, X2 = rnorm(nrow(dataCopula2sCOPECase1)))
  )
)



# specs x cdf ----------------------------------------------------------------------
test_smoke_all_cdfs(setup = setup.smoke, specs = specs)

# small sample ---------------------------------------------------------------------
test_smoke_small_sample(setup = setup.smoke)


test_smoke_na_irrelevant_col(setup = setup.smoke)

# Formula -------------------------------------------------------------------------
test_smoke_formula_edge_cases(
  setup = setup.smoke,
  cases = fixture_2scope_formula_edge_cases()
)


# Warnings ------------------------------------------------------------------------
test_that("Warns if no exogenous regressors", {
  expect_warning(
    suppress_lowboots_warning(copula2sCOPE(
      formula = y ~ P | P,
      data = dataCopula2sCOPECase1,
      num.boots = 2,
      verbose = FALSE
    )),
    regexp = "No exogenous regressors found"
  )
})
