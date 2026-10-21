# Correctness tests for the copula2stage methods
# `setup`: list(fn, data, formula, label)

# Invariances --------------------------------------------------------------------------
# test_that() wrappers that fit the same specification twice and check for equal coefs

# Coefs of both fits are equal, matched by name (not position)
.expect_equal_coefs <- function(res, res.ref) {
  expect_setequal(names(coef(res)), names(coef(res.ref)))
  expect_equal(coef(res)[names(coef(res.ref))], coef(res.ref))
}

test_correctness_invariance_row_order <- function(setup) {
  test_that(paste0(setup$label, ": Row order does not change coefs"), {
    res <- setup$fn(formula = setup$formula, data = setup$data)
    res.rev <- setup$fn(
      formula = setup$formula,
      data = setup$data[rev(seq_len(nrow(setup$data))), ]
    )
    .expect_equal_coefs(res = res.rev, res.ref = res)
  })
}

test_correctness_invariance_regressor_order <- function(setup, formula.reordered = y ~ P + X | P) {
  test_that(paste0(setup$label, ": Regressor order does not change coefs"), {
    res <- setup$fn(formula = setup$formula, data = setup$data)
    res.reordered <- setup$fn(formula = formula.reordered, data = setup$data)
    .expect_equal_coefs(res = res.reordered, res.ref = res)
  })
}

test_correctness_invariance_duplicate_regressor <- function(
  setup,
  formula.duplicate = y ~ X + X + P | P
) {
  test_that(paste0(setup$label, ": Duplicate regressor does not change coefs"), {
    res <- setup$fn(formula = setup$formula, data = setup$data)
    res.dup <- setup$fn(formula = formula.duplicate, data = setup$data)
    .expect_equal_coefs(res = res.dup, res.ref = res)
  })
}

# Dot expands to all other columns: data must have exactly the variables of `formula`
test_correctness_invariance_dot <- function(setup, formula.dot = y ~ . | P) {
  stopifnot(setequal(colnames(setup$data), all.vars(setup$formula)))

  test_that(paste0(setup$label, ": Dot does not change coefs"), {
    res <- setup$fn(formula = setup$formula, data = setup$data)
    res.dot <- setup$fn(formula = formula.dot, data = setup$data)
    .expect_equal_coefs(res = res.dot, res.ref = res)
  })
}


# Formula ------------------------------------------------------------------------------
# Intercept is included / excluded as specified in the formula
test_correctness_formula_intercept <- function(
  setup,
  formula.intercept = y ~ X + P | P,
  formula.no.intercept = y ~ X + P - 1 | P
) {
  test_that(paste0(setup$label, ": Intercept in formula is respected"), {
    res.intercept <- setup$fn(formula = formula.intercept, data = setup$data)
    res.no.intercept <- setup$fn(formula = formula.no.intercept, data = setup$data)

    expect_true("(Intercept)" %in% names(coef(res.intercept)))
    expect_false("(Intercept)" %in% names(coef(res.no.intercept)))
    expect_equal(length(coef(res.intercept)), length(coef(res.no.intercept)) + 1)
  })
}

# Each formula edge case (eg from fixture_<method>_formula_edge_cases()) is fitted with
# setup$fn and the fitted object is checked with `check`
test_correctness_formula_edge_cases <- function(
  setup,
  cases,
  check = function(res) {
    check_labelled_consistently(res)
    check_struct_residuals(res = res, aux.names = res$labels.pcop)
  }
) {
  test_that(paste0(setup$label, ": Formula edge cases are fitted consistently"), {
    run_cases(fn = setup$fn, cases = cases, check = check)
  })
}
