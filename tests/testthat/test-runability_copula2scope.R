skip_on_cran()
set.seed(42)

# Required data ---------------------------------------------------------------------
data("dataCopula2sCOPECase1")

# Works out of the box --------------------------------------------------------------
# also checks verbose=T
test_that("Works out of the box", {
  expect_no_failure(capture_output(
    copula2sCOPE(
      formula = y ~ P + X | continuous(P),
      data = dataCopula2sCOPECase1
    )
  ))
})


# Specifications for smoke tests ----------------------------------------------------
# Specs vary along three dimensions:
# - Intercept: w/ vs w/o
# - Exo: present vs absent
# - Endo: single vs multiple (X is not actually endo)
# Each spec is crossed with all 4 cdf options
#
# warn:
# - warn = NULL: no warning allowed
# - warn = "regexp": warning must appear

specs <- list(
  "with intercept, with exo, single endo" = list(
    formula = y ~ P + X | continuous(P),
    data = dataCopula2sCOPECase1,
    warn = NULL
  ),
  "no intercept, with exo, single endo" = list(
    formula = y ~ P + X - 1 | continuous(P),
    data = dataCopula2sCOPECase1,
    warn = NULL
  ),
  "with intercept, no exo, single endo" = list(
    formula = y ~ P | continuous(P),
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "no intercept, no exo, single endo" = list(
    formula = y ~ P - 1 | continuous(P),
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "with intercept, no exo, multiple endo" = list(
    formula = y ~ P + X | continuous(P, X),
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "no intercept, no exo, multiple endo" = list(
    formula = y ~ P + X - 1 | continuous(P, X),
    data = dataCopula2sCOPECase1,
    warn = "No exogenous regressors"
  ),
  "with intercept, with exo, multiple endo" = list(
    formula = y ~ P + X + X2 | continuous(P, X),
    data = cbind(dataCopula2sCOPECase1, X2 = rnorm(nrow(dataCopula2sCOPECase1))),
    warn = NULL
  ),
  "no intercept, with exo, multiple endo" = list(
    formula = y ~ P + X + X2 - 1 | continuous(P, X),
    data = cbind(dataCopula2sCOPECase1, X2 = rnorm(nrow(dataCopula2sCOPECase1))),
    warn = NULL
  )
)

cdfs <- c("adj.ecdf", "resc.ecdf", "ecdf", "kde")


# Muffles only the bootstrap warning, other warnings still propagate
muffle_boot_warn <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("recommended to run 1000", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# specs x cdf ----------------------------------------------------------------------
test_that("copula2sCOPE runs across specs and cdf options", {
  for (spec_name in names(specs)) {
    for (cdf in cdfs) {
      spec <- specs[[spec_name]]
      call_expr <- quote(
        copula2sCOPE(
          formula = spec$formula,
          data = spec$data,
          cdf = cdf,
          verbose = FALSE,
          num.boots = 10
        )
      )

      if (is.null(spec$warn)) {
        res <- expect_no_warning(muffle_boot_warn(eval(call_expr)))
      } else {
        res <- expect_warning(muffle_boot_warn(eval(call_expr)), regexp = spec$warn)
      }

      expect_false(anyNA(fitted(res)))
      expect_false(anyNA(residuals(res)))
      expect_false(anyNA(coef(res)))
      expect_false(anyNA(vcov(res)))
      expect_false(anyNA(res$boots.params))
    }
  }
})

# small sample ---------------------------------------------------------------------
test_that("copula2sCOPE runs on small sample", {
  small_data <- dataCopula2sCOPECase1[1:50, ]

  expect_no_error(muffle_boot_warn(
    copula2sCOPE(
      formula = y ~ P + X | continuous(P),
      data = small_data,
      cdf = "adj.ecdf",
      verbose = FALSE,
      num.boots = 10
    )
  ))
})


test_that("copula2sCOPE allows NA in non-relevant column", {
  bad_data <- dataCopula2sCOPECase1
  bad_data$Q <- rnorm(nrow(dataCopula2sCOPECase1))
  bad_data$Q[1] <- NA
  expect_warning(
    copula2sCOPE(
      formula = y ~ P + X | continuous(P),
      data = bad_data,
      verbose = FALSE,
      num.boots = 10
    ),
    regexp = "to run 1000"
  )
})

# S3 methods ----------------------------------------------------------
test_that("S3 run without error", {
  res <- expect_warning(
    copula2sCOPE(
      formula = y ~ P + X | continuous(P),
      data = dataCopula2sCOPECase1,
      verbose = FALSE,
      num.boots = 10
    ),
    regexp = "recommended to run 1000"
  )

  expect_no_error(capture_output(print(res)))
  expect_no_error(capture_output(print(summary(res))))
})
