# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHetIV")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - hetErrorsIV - Formula transformations")

test_that("Transformations are correct", {
  expect_silent(correct.res <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1), data=dataHetIV, verbose=FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataHetIV
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs <- hetErrorsIV(log(y)~X1+X2+P  |P|IIV(X1), data=data.altered, verbose=FALSE))
  expect_equal(coef(res.trans.lhs), coef(correct.res))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)))

  # Can handle transformations in endo
  data.altered    <- dataHetIV
  data.altered$P <- exp(data.altered$P)
  expect_silent(res.trans.endo  <- hetErrorsIV(formula = y~X1+X2+log(P)|log(P)|IIV(X1), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.endo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), check.attributes=FALSE)

  # Can handle transformations in nonIIV exo
  data.altered    <- dataHetIV
  data.altered$X2 <- exp(data.altered$X2)
  expect_silent(res.trans.exo  <- hetErrorsIV(formula = y~X1+log(X2)+P|P|IIV(X1), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes=FALSE)

  # Can handle transformations in IIV exo
  data.altered    <- dataHetIV
  data.altered$X1 <- exp(data.altered$X1)
  expect_silent(res.trans.iiv.exo  <- hetErrorsIV(formula = y~log(X1)+X2+P|P|IIV(log(X1)), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.iiv.exo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.iiv.exo)), coef(summary(correct.res)), check.attributes=FALSE)
})

# Data sorting ------------------------------------------------------------------------------------------------
context("Correctness - hetErrorsIV - Data sorting")

test_that("Differently sorted data produces same results", {
  data.altered    <- dataHetIV
  data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]

  expect_silent(res.orig        <- hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=dataHetIV, verbose=FALSE))
  expect_silent(res.diff.sorted <- hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=data.altered, verbose=FALSE))

  expect_equal(coef(res.orig), coef(res.diff.sorted))
  expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
})

