skip_on_cran()

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")


# Formula param --------------------------------------------------------------------

invalid_formulas <- list(
  "missing RHS 2" = y ~ X + P,
  "no endogenous spec" = y ~ X + P | P,
  "var in continuous() not in data" = y ~ X + P | continuous(Z),
  "dependent var in continuous()" = y ~ X + P | continuous(y),
  "no response variable" = ~ X + P | continuous(P),
  "exo variable not in data" = y ~ X + Z + P | continuous(P),
  "endo variable not in data" = y ~ X + Z | continuous(Z),
  "illegal special" = y ~ X + P | discrete(P),
  "misspelled special" = y ~ X + P | continous(P),
  "using dot" = y ~ . | continuous(P),
  "rhs1 and rhs2 swapped" = y ~ continuous(P) | X + P,
  "unwrapped rhs2" = y ~ X + P | P,
  "unwrapped rhs2 v2" = y ~ X + P | continuous(P) + P,
  "unwrapped rhs2 v3" = y ~ X + P | continuous(P) + X,
  "empty continuous()" = y ~ X + P | continuous(),
  "endo in spec but not in model" = y ~ X | continuous(P),
  "response also a regressor" = X ~ P + X | continuous(P),
  "more than 2 parts" = y ~ X + P | continuous(P) | X,
  "transformed endo not wrapped" = y ~ log(P) + X | continuous(P),
  # non-formula types
  "formula as char" = "y ~ P + X | continuous(P)",
  "formula = NULL" = NULL,
  "formula is number" = 1
)

test_that("copula2sCOPE rejects invalid formula specifications", {
  for (nm in names(invalid_formulas)) {
    expect_error(
      copula2sCOPE(
        formula = invalid_formulas[[nm]],
        data = dataCopula2sCOPECase1
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
  "data is empty" = list(data = dataCopula2sCOPECase1[0, ]),

  "cdf invalid string" = list(cdf = "invalid"),
  "cdf is NULL" = list(cdf = NULL),
  "cdf case mismatch" = list(cdf = "ADJ.ECDF"),
  "cdf non-default vector" = list(cdf = c("ecdf", "kde")),

  "num.boots is 0" = list(num.boots = 0),
  "num.boots negative" = list(num.boots = -1),
  "num.boots fractional" = list(num.boots = 10.5),
  "num.boots is NULL" = list(num.boots = NULL),
  "num.boots non-numeric" = list(num.boots = "abc"),

  "verbose is non-logical" = list(verbose = 1),
  "verbose is NULL" = list(verbose = NULL)
)

test_that("copula2sCOPE rejects invalid parameters", {
  base_args <- list(
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase1,
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
      do.call(what = copula2sCOPE, args = args),
      info = nm,
      regexp = "The above errors were encountered"
    )
  }
})


# Data: NA in regressor columns ---------------------------------------------------------

test_that("copula2sCOPE rejects NA in any relevant column", {
  formula_cols <- c("y", "P", "X")
  for (col in formula_cols) {
    bad_data <- dataCopula2sCOPECase1
    bad_data[[col]][1] <- NA
    expect_error(
      copula2sCOPE(
        formula = y ~ P + X | continuous(P),
        data = bad_data
      ),
      info = col,
      regexp = "The above errors were encountered"
    )
  }
})
