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
