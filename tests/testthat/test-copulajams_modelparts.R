# copulajams_build_matrix_from_part --------------------------------------------------
test_that("copulajams_build_matrix_from_part builds the matrix of a part", {
  df <- fixture_copula_df()
  F.formula <- as.Formula(y ~ x_num + `x space` | exp(x_num) + `x space` | 0)

  # 1 col per term, named as the term (transformed, without backticks)
  P <- copulajams_build_matrix_from_part(F.formula = F.formula, data = df, rhs.part = 2)
  expect_true(is.matrix(P))
  expect_true(is.numeric(P))
  expect_identical(colnames(P), c("exp(x_num)", "x space"))

  # part is 0: no cols but all rows
  W <- copulajams_build_matrix_from_part(F.formula = F.formula, data = df, rhs.part = 3)
  expect_true(is.matrix(W))
  expect_equal(dim(W), c(nrow(df), 0))
})


# copulajams_combine_factors_to_strata -----------------------------------------------
test_that("copulajams_combine_factors_to_strata combines factors to strata", {
  # Pasted level names are the same for 2 of the 4 combinations: a_b_c
  df.collision <- data.frame(
    Z = factor(c("a_b", "a_b", "a", "a")),
    Z2 = factor(c("c", "b_c", "c", "b_c"))
  )
  z <- copulajams_combine_factors_to_strata(df = df.collision)
  expect_equal(nlevels(z), 4)

  # 2 cols: only observed combinations are levels (not Z=1, Z2=b), labelled per col
  # and ordered by the first col, then the second
  df.two <- data.frame(
    Z = factor(c(1, 0, 0, 0)),
    Z2 = factor(c("a", "b", "a", "a"))
  )
  expect_identical(
    copulajams_combine_factors_to_strata(df = df.two),
    factor(
      c("Z=1, Z2=a", "Z=0, Z2=b", "Z=0, Z2=a", "Z=0, Z2=a"),
      levels = c("Z=0, Z2=a", "Z=0, Z2=b", "Z=1, Z2=a")
    )
  )

  # 1 col
  expect_identical(
    copulajams_combine_factors_to_strata(df = df.two[, "Z", drop = FALSE]),
    factor(c("Z=1", "Z=0", "Z=0", "Z=0"), levels = c("Z=0", "Z=1"))
  )

  # Different combinations with the same label: Z=0, Z2=a, Z2=b
  df.same.label <- data.frame(
    Z = factor(c("0, Z2=a", "0")),
    Z2 = factor(c("b", "a, Z2=b"))
  )
  expect_error(
    copulajams_combine_factors_to_strata(df = df.same.label),
    regexp = "same stratum level \\('name'\\): 'Z=0, Z2=a, Z2=b'"
  )
})
