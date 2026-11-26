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


# checkinput_copulajams_min_obs_per_stratum ------------------------------------------
test_that("_min_obs_per_stratum rejects strata with too few observations", {
  df <- fixture_jams_formula_edge_cases()[["multiple strata"]]$data
  # Level with as many observations as parts 2 and 3 have terms: 1 too few
  df$Z_rare <- factor(c("rare", "rare", rep("common", nrow(df) - 2)))

  run_cases_errmsg_empty(
    fn = checkinput_copulajams_min_obs_per_stratum,
    base.args = list(data = df),
    cases = list(
      "no part 4" = list(F.formula = as.Formula(y ~ P1 + W | P1 | W)),
      "all strata large enough" = list(
        F.formula = as.Formula(y ~ P1 + W + Z | P1 | W | Z)
      )
    )
  )

  run_cases_errmsg(
    fn = checkinput_copulajams_min_obs_per_stratum,
    base.args = list(data = df),
    cases = list(
      "stratum at the limit" = list(
        F.formula = as.Formula(y ~ P1 + W + Z_rare | P1 | W | Z_rare)
      )
    ),
    regexp = "more than 2 observations .* but these do not: 'Z_rare=rare'\\."
  )
})


# checkinput_copulajams_formula_data -------------------------------------------------
test_that("_formula_data works with all kinds of formula edge cases", {
  run_cases_errmsg_empty(
    fn = checkinput_copulajams_formula_data,
    cases = fixture_jams_formula_edge_cases()
  )
})

# Each case fails in another method: Shows that it is called
test_that("_formula_data has every check method wired in", {
  # Stratum with a single observation
  df.rare <- fixture_copula_df()
  df.rare$x_rare <- factor(c("rare", rep("common", nrow(df.rare) - 1)))

  run_cases_errmsg(
    fn = checkinput_copulajams_formula_data,
    base.args = list(data = fixture_copula_df()),
    cases = list(
      # checkinput_copula2stage_formula_basics
      "2 parts" = list(formula = y ~ x_num | x_num),
      # checkinput_copulajams_formula_parts
      "dot in part 2" = list(formula = y ~ x_num | . | 0),
      # checkinput_copula2stage_vars_in_data
      "variable not in data" = list(
        formula = y ~ x_num + x_missing | x_num | x_missing
      ),
      # checkinput_copula2stage_response_not_in_rhs
      "response in part 1" = list(formula = y ~ y + x_num | x_num | 0),
      # checkinput_copulajams_all_vars_in_rhs1
      "variable only in part 3" = list(formula = y ~ x_num | x_num | `x space`),
      # checkinput_copula2stage_modelframe: Part 1
      "constant in part 1" = list(formula = y ~ x_num + x_const | x_num | 0),
      # checkinput_copulajams_modelframe_single_part: Parts 2 and 3
      "factor in part 2" = list(formula = y ~ x_fac + x_num | x_fac | x_num),
      # checkinput_copulajams_modelframe_single_part: Part 4
      "numeric in part 4" = list(
        formula = y ~ x_num + x_lowcard | x_num | 0 | x_lowcard
      ),
      # checkinput_copulajams_min_obs_per_stratum
      "stratum too small" = list(
        formula = y ~ x_num + x_rare | x_num | 0 | x_rare,
        data = df.rare
      )
    ),
    regexp = c(
      "3 or 4 right-hand parts",
      "part 2 .* may not have a dot",
      "not found in parameter 'data': x_missing",
      "also appear on the right-hand side: y",
      "part 3 .* but these are not: x space",
      "must not be constant: x_const",
      "'x_fac' has class 'factor' but must be one of: numeric",
      "'x_lowcard' has class 'numeric' but must be one of: factor, ordered",
      "more than 1 observations .* but these do not: 'x_rare=rare'"
    )
  )
})
