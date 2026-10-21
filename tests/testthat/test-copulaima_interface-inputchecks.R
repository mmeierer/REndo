skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")


# Wired in --------------------------------------------------------------------------
# just check if parameter tests are wired in
test_that("interface has methods wired in", {
  df.na <- dataCopIMAContExo
  df.na$P[1] <- NA

  cases <- list(
    "data_basics" = list(data = NULL),
    "formula_data" = list(formula = y ~ P + X),
    "modelframe" = list(data = df.na),
    "cdf" = list(cdf = "cdf"),
    "num.boots" = list(num.boots = -1),
    "verbose" = list(verbose = NULL)
  )

  base.args <- list(
    formula = y ~ X + P | P,
    data = dataCopIMAContExo,
    cdf = "adj.ecdf",
    num.boots = 1000,
    verbose = FALSE
  )

  run_cases_error(
    fn = copulaIMA,
    base.args = base.args,
    cases = cases,
    regexp = "The above errors"
  )
})
