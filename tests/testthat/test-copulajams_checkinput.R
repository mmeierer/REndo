# checkinput_copulajams_formula_single_part ------------------------------------------
test_that("_formula_single_part rejects invalid parts", {
  run_cases_errmsg(
    fn = checkinput_copulajams_formula_single_part,
    # Part 2 only
    base.args = list(rhs.part = 2, allow.zero = FALSE),
    cases = list(
      "0 but not allowed" = list(F.formula = as.Formula(y ~ P | 0 | W)),
      "dot" = list(F.formula = as.Formula(y ~ P | . | W)),
      "cannot be read" = list(F.formula = as.Formula(y ~ P | P + 2 | W)),
      "no terms, 0 allowed" = list(
        F.formula = as.Formula(y ~ P | P | 1),
        rhs.part = 3,
        allow.zero = TRUE
      ),
      "no terms, 0 not allowed" = list(F.formula = as.Formula(y ~ P | 1 | W)),
      "interaction" = list(F.formula = as.Formula(y ~ P1 * P2 | P1 + P2 + P1:P2 | W)),
      "2 variables in a term" = list(
        F.formula = as.Formula(y ~ P1 + P2 | I(P1 / P2) | W)
      ),
      "variable in 2 terms" = list(F.formula = as.Formula(y ~ P | P + I(P^2) | W))
    ),
    regexp = c(
      "part 2 .* must have terms and may not be `0`",
      "may not have a dot",
      "could not be interpreted",
      "part 3 .* must either have terms or be `0`",
      "must have terms\\.",
      "may not have interaction terms",
      "exactly 1 variable in every term but these do not: I\\(P1/P2\\)",
      "every variable in only 1 term but these are used in more: P"
    )
  )
})


# checkinput_copulajams_formula_parts ------------------------------------------------
test_that("_formula_parts rejects invalid parts", {
  run_cases_errmsg(
    fn = checkinput_copulajams_formula_parts,
    cases = list(
      "0 in part 2" = list(F.formula = as.Formula(y ~ P + W | 0 | W)),
      "invalid part 4" = list(F.formula = as.Formula(y ~ P + W + Z | P | W | .)),
      "variable in parts 2 and 3" = list(F.formula = as.Formula(y ~ P | P | P))
    ),
    regexp = c(
      "part 2 .* must have terms and may not be `0`",
      "part 4 .* may not have a dot",
      "in 1 of the right-hand parts 2-4 .* but these are used in several: P"
    )
  )
})


# checkinput_copulajams_all_vars_in_rhs1 ---------------------------------------------
test_that("_all_vars_in_rhs1 rejects variables that are not in part 1", {
  run_cases_errmsg(
    fn = checkinput_copulajams_all_vars_in_rhs1,
    base.args = list(data = fixture_copula_df()),
    cases = list(
      "only in part 3" = list(F.formula = as.Formula(y ~ x_num | x_num | x_lowcard)),
      "only in part 4" = list(
        F.formula = as.Formula(y ~ x_num + x_lowcard | x_num | x_lowcard | x_fac)
      ),
      # the dot does not expand to the response
      "response in part 2" = list(F.formula = as.Formula(y ~ . | y | 0))
    ),
    regexp = c(
      "part 3 .* but these are not: x_lowcard",
      "part 4 .* but these are not: x_fac",
      "part 2 .* but these are not: y"
    )
  )
})


# checkinput_copulajams_modelframe_single_part ---------------------------------------
test_that("_modelframe_single_part rejects invalid data of a part", {
  run_cases_errmsg(
    fn = checkinput_copulajams_modelframe_single_part,
    base.args = list(
      data = fixture_copula_df(),
      allowed.classes = "numeric",
      warn.low.card = FALSE
    ),
    cases = list(
      "wrong class in part 2" = list(
        F.formula = as.Formula(y ~ x_fac + x_num | x_fac | x_num),
        rhs.part = 2
      ),
      "wrong class in part 3" = list(
        F.formula = as.Formula(y ~ x_num + x_string | x_num | x_string),
        rhs.part = 3
      ),
      "wrong class in part 4" = list(
        F.formula = as.Formula(y ~ x_num + x_lowcard | x_num | 0 | x_lowcard),
        rhs.part = 4,
        allowed.classes = c("factor", "ordered")
      ),
      "cannot be built" = list(
        F.formula = as.Formula(y ~ x_num + x_na | x_num | x_na),
        rhs.part = 3
      ),
      "non-finite from transformation" = list(
        F.formula = as.Formula(y ~ x_num + x_lowcard | x_num | log(x_lowcard - 1)),
        rhs.part = 3
      )
    ),
    regexp = c(
      "'x_fac' has class 'factor' but must be one of: numeric",
      "'x_string' has class 'character' but must be one of: numeric",
      "'x_lowcard' has class 'numeric' but must be one of: factor, ordered",
      "model frame of right-hand part 3 .* could not be built: .*missing values",
      "finite values: log\\(x_lowcard - 1\\)"
    )
  )
})
