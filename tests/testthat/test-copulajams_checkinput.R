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
