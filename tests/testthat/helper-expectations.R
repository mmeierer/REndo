# check_errmsg() + check_errmsg_empty -------------------------------------------------
# Mostly for testing the internal methods that return error messages (char vecs) and
# do not stop()

# Methods to apply expect_<X> to multiple test cases
# Expect error messages returned
check_errmsg <- function(fn, base.args = list(), cases, regexp) {
  if (length(regexp) == 1) {
    regexp <- rep(regexp, length(cases))
  }
  # for printing labels on failure
  names(regexp) <- names(cases)
  case.i <- 0

  run_cases(
    fn = fn,
    base.args = base.args,
    cases = cases,
    check = function(res) {
      # `<<-` to manipulate up the call stack (counter in enclosing method)
      case.i <<- case.i + 1
      expect_true(length(res) > 0)
      expect_match(paste(res, collapse = " "), regexp[[case.i]])
    }
  )
}

# Expect no error message returned
check_errmsg_empty <- function(fn, base.args = list(), cases) {
  run_cases(
    fn = fn,
    base.args = base.args,
    cases = cases,
    check = expect_null
  )
}

# check_clean_fit -------------------------------------------------------------------
# No NAs anywhere. Basic sanity checks. Mostly used for smoke tests.
check_clean_fit <- function(res) {
  expect_false(anyNA(fitted(res)))
  expect_false(anyNA(residuals(res)))
  expect_false(anyNA(coef(res)))
  expect_false(anyNA(vcov(res)))
  expect_false(anyNA(res$boots.params))
}

# check_print_summary ---------------------------------------------------------------
# print() and print(summary()) work and print rendo.copula.2stage specific parts
check_print_summary <- function(res) {
  expect_no_error(capture_output(print(res)))
  out <- capture_output(print(summary(res)))
  expect_match(out, "Endogenous regressors:", fixed = TRUE)
  # Used cdf only printed for methods with cdf
  if (is.null(res$cdf)) {
    expect_false(grepl("Used cdf:", out, fixed = TRUE))
  } else {
    expect_match(out, paste0("Used cdf: ", res$cdf), fixed = TRUE)
  }
}

# check_labelled_consistently --------------------------------------------------------

check_labelled_consistently <- function(res) {
  # aux terms in coefs
  expect_true(all(res$labels.pcop %in% names(coef(res))))
  # aux terms in main coefs (= shown in the coef table)
  expect_true(all(res$labels.pcop %in% names(coef(res, complete = FALSE))))
  # one aux term per endo
  expect_equal(length(res$labels.pcop), length(res$labels.endo))

  # all names of main coefs preserved (numeric, factors, ordered)
  labels.structural <- c(res$labels.exo, res$labels.endo)
  cf.names <- names(coef(res))
  # check each label separately
  for (l in labels.structural) {
    expect_true(
      # fmt: skip
      any(
        # continuous: plain name
        cf.names == l |
          # ordered: <name>.<L/Q/C>
          startsWith(cf.names, paste0(l, ".")) |
          # factor: <name><level>
          (startsWith(cf.names, l) & cf.names != l)
      ),
      info = l
    )
  }

  # coefs in summary named same as coefs
  expect_setequal(rownames(coef(summary(res))), names(coef(res)))
}


# run_cases ------------------------------------------------------------------
# run `fn` method with `cases` + `base.args` and then apply `check` as a test
# return the fitted object in list named after cases
#' @importFrom utils modifyList
run_cases <- function(fn, base.args = list(), cases, check) {
  results <- lapply(names(cases), function(nm) {
    # upsert base.args with `cases`
    args <- c(cases[[nm]], base.args[!names(base.args) %in% names(cases[[nm]])])
    res <- do.call(what = fn, args = args)
    withCallingHandlers(
      check(res),
      # catch thrown testthat expectation failures
      expectation_failure = function(e) {
        e$message <- paste0("[case: ", nm, "] ", e$message)
        stop(e)
      }
    )
    return(res)
  })
  names(results) <- names(cases)
  return(invisible(results))
}

# check_param_recovery -------------------------------------------------------------
# parameter recovery test
check_param_recovery <- function(res, true_vals) {
  coefs <- coef(res)
  ses <- sqrt(diag(vcov(res)))
  for (nm in names(true_vals)) {
    diff <- abs(coefs[nm] - true_vals[nm])
    expect_true(
      object = diff < 2 * ses[nm],
      info = sprintf(
        "%s: est=%.3f, true=%.3f, 2*SE=%.3f",
        nm,
        coefs[nm],
        true_vals[nm],
        2 * ses[nm]
      )
    )
  }
}

# check_struct_residuals ----------------------------------------------------------
#' @importFrom stats residuals fitted coef
check_struct_residuals <- function(res, aux.names) {
  # Alternative route: Remove cop contribution from augmented fit
  res.lm <- res$res.lm.augmented

  cop.coefs <- coef(res.lm)[aux.names]
  cop.matrix <- model.matrix(res.lm)[, aux.names, drop = FALSE]

  residuals.alt <- drop(residuals(res.lm) + cop.matrix %*% cop.coefs)
  fitted.alt <- drop(fitted(res.lm) - cop.matrix %*% cop.coefs)

  expect_equal(residuals(res), residuals.alt)
  expect_equal(fitted.values(res), fitted.alt)
}
