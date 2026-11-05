# copula2stage_stop_if_constant ------------------------------------------------------

test_that("copula2stage_stop_if_constant requires a matrix or data.frame", {
  expect_error(copula2stage_stop_if_constant(1:5), regexp = "is.matrix")
})

test_that("copula2stage_stop_if_constant passes for non-constant columns", {
  expect_silent(copula2stage_stop_if_constant(cbind(a = 1:5, b = c(0, 1, 0, 1, 1))))
})

test_that("copula2stage_stop_if_constant stops on constant cols", {
  run_cases_error(
    fn = copula2stage_stop_if_constant,
    cases = list(
      "single column" = list(X = cbind(a = rep(1, 5))),
      "single constant" = list(X = cbind(a = 1:5, b = rep(2, 5))),
      "multiple constant" = list(X = cbind(a = rep(1, 5), b = 1:5, c = rep(0, 5)))
    ),
    regexp = "must not be constant"
  )
})

test_that("copula2stage_stop_if_constant works with data.frames incl factors", {
  expect_silent(copula2stage_stop_if_constant(
    data.frame(a = 1:5, f = factor(c("x", "y", "x", "y", "y")))
  ))
  expect_error(
    copula2stage_stop_if_constant(data.frame(a = 1:5, f = factor(rep("x", 5)))),
    regexp = "must not be constant"
  )
})


# copula2stage_get_copterm_names -----------------------------------------------------

test_that("copula2stage_get_copterm_names appends _cop to the endo labels", {
  # plain
  expect_identical(copula2stage_get_copterm_names("P"), "P_cop")
  # multiple, order is kept
  expect_identical(copula2stage_get_copterm_names(c("P1", "P2")), c("P1_cop", "P2_cop"))
  # backticks are stripped (non-syntactic)
  expect_identical(copula2stage_get_copterm_names("`P 1`"), "P 1_cop")
  # transformations are kept as they are
  expect_identical(copula2stage_get_copterm_names("log(P)"), "log(P)_cop")
  expect_identical(copula2stage_get_copterm_names("I(P^2)"), "I(P^2)_cop")
})
