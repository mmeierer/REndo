skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")


# Wired in --------------------------------------------------------------------------
# just check if parameter tests are wired in
test_that("interface has methods wired in", {
  df.na <- dataCopula2sCOPECase1
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
    formula = y ~ P + X | P,
    data = dataCopula2sCOPECase1,
    cdf = "adj.ecdf",
    num.boots = 1000,
    verbose = FALSE
  )

  run_cases_error(
    fn = copula2sCOPE,
    base.args = base.args,
    cases = cases,
    regexp = "The above errors"
  )
})
