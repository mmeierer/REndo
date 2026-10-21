# Method-level parameter verification at the public interface - just check if
# parameter tests are wired in

skip_on_cran()
set.seed(42)
data("dataCopBMW")


test_that("interface has methods wired in", {
  df.na <- dataCopBMW
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
    data = dataCopBMW,
    num.boots = 1000,
    verbose = FALSE
  )

  run_cases_error(
    fn = copulaBMW,
    base.args = base.args,
    cases = cases,
    regexp = "The above errors"
  )
})
