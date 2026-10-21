skip_on_cran()
set.seed(42)

data("dataCopBMW")
data("dataCopBMWMultiEndo")



# Setup --------------------------------------------------------------------------------
setup.smoke <- list(
  method = copulaBMW,
  fn = fit_bmw_fast,
  data = dataCopBMW,
  formula = y ~ X + P | P,
  label = "copulaBMW"
)

# Out of the box --------------------------------------------------------------------
test_smoke_out_of_the_box(setup = setup.smoke)

test_smoke_verbose(setup = setup.smoke)


# specs x cdf ----------------------------------------------------------------------
# Specs vary along:
# - intercept: w/ and w/o
# - endo: 1 vs multiple
#
# All combinations: cross each spec with all 4 cdf options
specs <- list(
  "with intercept, single endo" = list(
    formula = y ~ X + P | P,
    data = dataCopBMW
  ),
  "no intercept, single endo" = list(
    formula = y ~ X + P - 1 | P,
    data = dataCopBMW
  ),
  "with intercept, multiple endo" = list(
    formula = y ~ X + P1 + P2 | P1 + P2,
    data = dataCopBMWMultiEndo
  ),
  "no intercept, multiple endo" = list(
    formula = y ~ X + P1 + P2 - 1 | P1 + P2,
    data = dataCopBMWMultiEndo
  )
)

test_smoke_all_cdfs(setup = setup.smoke, specs = specs)


# Formula -------------------------------------------------------------------------
test_smoke_formula_edge_cases(
  setup = setup.smoke,
  cases = fixture_bmw_formula_edge_cases()
)


# Tolerate irrelevant data ----------------------------------------------------------
test_smoke_na_irrelevant_col(setup = setup.smoke)


# Small sample ----------------------------------------------------------------------
test_smoke_small_sample(setup = setup.smoke)
