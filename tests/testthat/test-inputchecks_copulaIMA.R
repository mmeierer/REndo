skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopIMAContExo")


# Formula specification -------------------------------------------------------------

invalid_formulas <- list(
  "missing RHS 2" = y ~ 1,
  "no endogenous spec (no |)" = y ~ X + P,
  "var in continuous() not in formula" = y ~ X + P | continuous(Z),
  "dependent var in continuous()" = y ~ X + P | continuous(y),
  "no response variable" = ~ X + P | continuous(P),
  "exo variable not in data" = y ~ X + Q + P| continuous(P),
  "endo variable not in data" = y ~ X + Q | continuous(Q),
  "illegal special" = y ~ X + P | discrete(P),
  "misspelled special" = y ~ X + P | continous(P),
  "using dot" = y ~ . | continous(P),
  "rhs1 and rhs2 swapped" = y ~ continuous(P) | X + P
)

test_that("copulaIMA rejects invalid formula specifications", {
  for (nm in names(invalid_formulas)) {
    expect_error(
      copulaIMA(
        formula = invalid_formulas[[nm]],
        data = dataCopIMAContExo
      ),
      info = nm,
      regexp = "The above errors were encountered"
    )
  }
})


# Other params ----------------------------------------------------------------------
invalid_params <- list(
  "data is NULL" = list(data = NULL),
  "data is not a df" = list(data = list(1, 2)),
  "cdf invalid string" = list(cdf = "invalid"),
  "cdf is NULL" = list(cdf = NULL),
  "num.boots is 0" = list(num.boots = 0),
  "num.boots negative" = list(num.boots = -1),
  "num.boots non-numeric" = list(num.boots = "abc"),
  "verbose is non-logical" = list(verbose = 1)
)

test_that("copulaIMA rejects invalid parameters", {
  base_args <- list(
    formula = y ~ X + P - 1 | continuous(P),
    data = dataCopIMAContExo,
    cdf = "adj.ecdf",
    num.boots = 1000,
    verbose = FALSE
  )

  for (nm in names(invalid_params)) {
    # Put together: invalid args + missing ones (modifyList does not work)
    args <- c(
      invalid_params[[nm]],
      base_args[!names(base_args) %in% names(invalid_params[[nm]])]
    )
    expect_error(
      do.call(what = copulaIMA, args = args),
      info = nm,
      regexp = "The above errors were encountered"
    )
  }
})
