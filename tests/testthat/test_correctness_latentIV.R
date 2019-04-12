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


  # ensure that theta5 (group membership probability) is in [0,1]
  expect_true(coef(res.trans.rhs1, complete = TRUE)["theta5"] >= 0 &&
              coef(res.trans.rhs1, complete = TRUE)["theta5"] <= 1)
})

# Data sorting ------------------------------------------------------------------------------------------------
context("Correctness - dataLatentIV - Data sorting")

test_that("Differently sorted data produces same results", {
  data.altered    <- dataLatentIV
  data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]

  expect_silent(res.orig        <- latentIV(formula = y ~ P, data = dataLatentIV, verbose = FALSE))
  expect_silent(res.diff.sorted <- latentIV(formula = y ~ P, data = data.altered, verbose = FALSE))

  expect_equal(coef(res.orig), coef(res.diff.sorted))
  expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
  # ensure that theta5 (group membership probability) is in [0,1]
  expect_true(coef(res.orig, complete = TRUE)["theta5"] >= 0 &&
              coef(res.orig, complete = TRUE)["theta5"] <= 1)
})


context("Correctness - latentIV - Example data")

test_that("Retrieve correct parameters", {
  expect_silent(res.liv <- latentIV(formula = y ~ P, data = dataLatentIV, verbose = FALSE))
  expect_equal(coef(res.liv, complete = FALSE), c("(Intercept)" = 3, P=-1), tolerance = 0.01)
  # ensure that theta5 (group membership probability) is in [0,1]
  expect_true(coef(res.liv, complete = TRUE)["theta5"] >= 0 &&
              coef(res.liv, complete = TRUE)["theta5"] <= 1)
})


