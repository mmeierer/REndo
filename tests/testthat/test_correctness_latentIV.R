# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - latentIV - Formula transformations")

test_that("Transformations are correct", {
  expect_silent(correct.res <- latentIV(formula = y ~ P, data = dataLatentIV, verbose = FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataLatentIV
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs <- latentIV(formula = log(y) ~ P, data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.lhs), coef(correct.res))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)))

  # Can handle transformations in RHS1
  data.altered    <- dataLatentIV
  data.altered$P <- exp(data.altered$P)
  expect_silent(res.trans.rhs1  <- latentIV(formula = y ~ log(P), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.rhs1), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.rhs1)), coef(summary(correct.res)), check.attributes=FALSE)
})

