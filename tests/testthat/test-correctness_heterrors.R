# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHetIV")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - hetErrorsIV - Formula transformations")

test_that("Transformations are correct", {
  expect_silent(correct.res <- hetErrorsIV(y~X1+X2+P  |P|IIV(X2), data=dataHetIV, verbose=FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataHetIV
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs <- hetErrorsIV(log(y)~X1+X2+P  |P|IIV(X2), data=data.altered, verbose=FALSE))
  expect_equal(coef(res.trans.lhs), coef(correct.res))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)))

  # Can handle transformations in endo
  data.altered    <- dataHetIV
  data.altered$P <- exp(data.altered$P)
  expect_silent(res.trans.endo  <- hetErrorsIV(formula = y~X1+X2+log(P)|log(P)|IIV(X2), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.endo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), check.attributes=FALSE)

  # Can handle transformations in nonIIV exo
  data.altered    <- dataHetIV
  data.altered$X1 <- exp(data.altered$X1)
  expect_silent(res.trans.exo  <- hetErrorsIV(formula = y~log(X1)+X2+P|P|IIV(X2), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes=FALSE)

  # Can handle transformations in IIV exo
  data.altered    <- dataHetIV
  data.altered$X2 <- exp(data.altered$X2)
  expect_silent(res.trans.iiv.exo  <- hetErrorsIV(formula = y~X1+log(X2)+P|P|IIV(log(X2)), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.iiv.exo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.iiv.exo)), coef(summary(correct.res)), check.attributes=FALSE)
})

# Data sorting ------------------------------------------------------------------------------------------------
context("Correctness - hetErrorsIV - Data sorting")

test_that("Differently sorted data produces same results", {
  data.altered    <- dataHetIV
  data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]

  expect_silent(res.orig        <- hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=dataHetIV, verbose=FALSE))
  expect_silent(res.diff.sorted <- hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=data.altered, verbose=FALSE))

  expect_equal(coef(res.orig), coef(res.diff.sorted))
  expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
})


# Predict ------------------------------------------------------------------------------------------------
context("Correctness - hetErrorsIV - Predict")

test_that("No newdata results in fitted values", {
  expect_silent(het.1 <- hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=dataHetIV, verbose=FALSE))
  expect_equal(predict(het.1), fitted(het.1))

  expect_warning(het.2 <- hetErrorsIV(y~X1+X2+P|P|IIV(X1, X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_equal(predict(het.2), fitted(het.2))
})

test_that("Same prediction data as for fitting results in fitted values", {
  expect_silent(het.1 <- hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=dataHetIV, verbose=FALSE))
  expect_equal(predict(het.1, newdata=dataHetIV), fitted(het.1))

  expect_warning(het.2 <- hetErrorsIV(y~X1+X2+P|P|IIV(X1, X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_equal(predict(het.2, newdata=dataHetIV), fitted(het.2))
})

test_that("Same results as ivreg with useless iiv dummies", {
  expect_silent(het.1 <- hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=dataHetIV, verbose=FALSE))
  expect_equal(predict(het.1, newdata=dataHetIV),
               AER:::predict.ivreg(het.1, newdata = cbind(dataHetIV,IIV.X2=-11)))

  expect_warning(het.2 <- hetErrorsIV(y~X1+X2+P|P|IIV(X1, X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_equal(predict(het.2, newdata=dataHetIV),
               AER:::predict.ivreg(het.2, newdata = cbind(dataHetIV,IIV.X1=1.23, IIV.X2=2.34)))
})

test_that("Correct structure of predictions", {
  expect_silent(het.1 <- hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=dataHetIV, verbose=FALSE))
  expect_silent(pred.1 <- predict(het.1, dataHetIV))
  expect_true(is.numeric(pred.1))
  expect_true(length(pred.1) == nrow(dataHetIV))
  expect_true(all(names(pred.1) == names(fitted(het.1))))
  expect_true(all(names(pred.1) == rownames(dataHetIV)))
})

test_that("Correct when using transformations in the formula", {
  # transformation in regressor
  expect_warning(het.1 <- hetErrorsIV(y~I((X1+14)/3)+X2+P|P|IIV(I((X1+14)/3)), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_equal(predict(het.1, newdata=dataHetIV), fitted(het.1))
  # transformation in endogenous
  expect_silent(het.1 <- hetErrorsIV(y~X1+X2+I((P+14)/3)|I((P+14)/3)|IIV(X2), data=dataHetIV, verbose=FALSE))
  expect_equal(predict(het.1, newdata=dataHetIV), fitted(het.1))
})


# Example data ---------------------------------------------------------------------------------
# context("Correctness - hetErrorsIV - Example data")
# **TODO: data docu wrong?
# test_that("Retrieve correct parameters", {
#   expect_silent(res.het <- hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=dataHetIV, verbose=FALSE))
#   expect_equal(coef(res.het), c("(Intercept)" = 2, X1=-1.5, X2=-3, P=-1), tolerance = 0.01)
# })

