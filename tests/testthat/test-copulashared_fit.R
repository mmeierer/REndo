# copula_stop_if_constant ------------------------------------------------------------

test_that("copula_stop_if_constant requires a matrix", {
  expect_error(copula_stop_if_constant(1:5), regexp = "is.matrix")
})

test_that("copula_stop_if_constant passes for non-constant columns", {
  expect_silent(copula_stop_if_constant(cbind(a = 1:5, b = c(0, 1, 0, 1, 1))))
})

test_that("copula_stop_if_constant stops on constant cols", {
  # single column
  expect_error(
    copula_stop_if_constant(cbind(a = rep(1, 5))),
    regexp = "Constant regressor"
  )
  # single
  expect_error(
    copula_stop_if_constant(cbind(a = 1:5, b = rep(2, 5))),
    regexp = "Constant regressor"
  )
  # multiple
  expect_error(
    copula_stop_if_constant(cbind(a = rep(1, 5), b = 1:5, c = rep(0, 5))),
    regexp = "Constant regressor"
  )
})
