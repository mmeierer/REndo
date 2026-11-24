skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")
data("dataCopIMAMultiEndo")
data("dataCopIMABinExo")

# Setup --------------------------------------------------------------------------------
setup.smoke <- list(
  method = copulaIMA,
  fn = fit_ima_fast,
  data = dataCopIMAContExo,
  formula = y ~ X + P - 1 | P,
  label = "copulaIMA"
)

# Defaults -----------------------------------------------------------------------------
test_smoke_out_of_the_box(setup = setup.smoke)

test_smoke_verbose(setup = setup.smoke)

# Specifications for smoke tests ----------------------------------------------------
# Specs vary along three dimensions:
# - Intercept: with vs without
# - Exogenous regressors: continuous, or binary
# - Endogenous regressors: single vs multiple
# Each spec is crossed with all 4 cdf options

specs <- list(
  "no intercept, continuous exo, single endo" = list(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMAContExo
  ),
  "with intercept, continuous exo, single endo" = list(
    formula = y ~ X + P | P,
    data = dataCopIMAContExo
  ),
  "no intercept, binary exo, single endo" = list(
    formula = y ~ X + P - 1 | P,
    data = dataCopIMABinExo
  ),
  "with intercept, continuous exo, multiple endo" = list(
    formula = y ~ X + P1 + P2 | P1 + P2,
    data = dataCopIMAMultiEndo
  ),
  "no intercept, continuous exo, multiple endo" = list(
    formula = y ~ X + P1 + P2 - 1 | P1 + P2,
    data = dataCopIMAMultiEndo
  )
)

test_smoke_all_cdfs(setup = setup.smoke, specs = specs)

# Formula ------------------------------------------------------------------------------
test_smoke_formula_edge_cases(
  setup = setup.smoke,
  cases = fixture_ima_formula_edge_cases()
)

# Data ---------------------------------------------------------------------------------
test_smoke_na_irrelevant_col(setup = setup.smoke)

test_smoke_small_sample(setup = setup.smoke)
