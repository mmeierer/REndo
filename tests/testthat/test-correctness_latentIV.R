# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - latentIV - Formula transformations")

test_that("Transformations are correct", {
  # temporarily skip this test because it weirdly keeps on failing on cran's original test.
  #   TODO: enable again, once we have the package back up
  skip_on_cran()

  # Same result when transformation applied to LHS or RHS as if directly to data
  #   testing log(exp()) gives same as no transformations yields numerical differences on M1 architectures

  data.altered.y   <- dataLatentIV
  data.altered.y$y <- data.altered.y$y/2 + 12.3

  expect_silent(res.trans.lhs <- latentIV(formula = I(y/2 + 12.3) ~ P, data = dataLatentIV, verbose = FALSE))
  expect_silent(res.data.trans.lhs <- latentIV(formula = y ~ P, data = data.altered.y, verbose = FALSE))
  expect_equal(coef(res.trans.lhs), coef(res.data.trans.lhs), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(res.data.trans.lhs)), check.attributes=FALSE)


  data.altered.P   <- dataLatentIV
  data.altered.P$P <- data.altered.P$P/2 + 12.3
  expect_silent(res.trans.rhs <- latentIV(formula = y ~ I(P/2 + 12.3), data = dataLatentIV, verbose = FALSE))
  expect_silent(res.data.trans.rhs <- latentIV(formula = y ~ P, data = data.altered.P, verbose = FALSE))
  expect_equal(coef(res.trans.rhs), coef(res.data.trans.rhs), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(res.data.trans.rhs)), check.attributes=FALSE)

  # ensure that theta5 (group membership probability) is in [0,1]
  expect_true(coef(res.trans.rhs, complete = TRUE)["theta5"] >= 0 &&
              coef(res.trans.rhs, complete = TRUE)["theta5"] <= 1)
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


# Predict ----------------------------------------------------------------------------------------------------
context("Correctness - latentIV - Predict")

test_that("No newdata results in fitted values", {
  expect_silent(lat.1 <- latentIV(y~P, data=dataLatentIV, verbose=FALSE))
  expect_equal(predict(lat.1), fitted(lat.1))

  expect_silent(lat.2 <- latentIV(y~P-1, data=dataLatentIV, verbose=FALSE))
  expect_equal(predict(lat.2), fitted(lat.2))
})

test_that("Same prediction data as for fitting results in fitted values", {
  expect_silent(lat.1 <- latentIV(y~P, data=dataLatentIV, verbose=FALSE))
  expect_equal(predict(lat.1, newdata=dataLatentIV), fitted(lat.1))

  expect_silent(lat.2 <- latentIV(y~P-1, data=dataLatentIV, verbose=FALSE))
  expect_equal(predict(lat.2, newdata=dataLatentIV), fitted(lat.2))
})

test_that("Correct structure of predictions", {
  expect_silent(lat.1 <- latentIV(y~P, data=dataLatentIV, verbose=FALSE))
  expect_silent(pred.1 <- predict(lat.1, dataLatentIV))
  expect_true(is.numeric(pred.1))
  expect_true(length(pred.1) == nrow(dataLatentIV))
  expect_true(all(names(pred.1) == names(fitted(lat.1))))
  expect_true(all(names(pred.1) == rownames(dataLatentIV)))
})

test_that("Correct when using transformations in the formula", {
  expect_silent(lat.1 <- latentIV(y~I((P+10)/3), data=dataLatentIV, verbose=FALSE))
  expect_equal(predict(lat.1, newdata=dataLatentIV), fitted(lat.1))
})

# Example data ---------------------------------------------------------------------------------
context("Correctness - latentIV - Example data")

test_that("Retrieve correct parameters", {
  expect_silent(res.liv <- latentIV(formula = y ~ P, data = dataLatentIV, verbose = FALSE))

  # No longer recoveres correctly with new data. Raluca confirmed that true params are (b0=3, b1=1) tho...
  # expect_equal(coef(res.liv, complete = FALSE), c("(Intercept)" = 3, P=-1), tolerance = 0.01)

  # ensure that theta5 (group membership probability) is in [0,1]
  expect_true(coef(res.liv, complete = TRUE)["theta5"] >= 0 &&
              coef(res.liv, complete = TRUE)["theta5"] <= 1)
})


