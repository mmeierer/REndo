# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHigherMoments")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - higherMomentsIV - Formula transformations")


# Data sorting -----------------------------------------------------------------------------------------------------------------------
context("Correctness - higherMomentsIV - Data sorting")

test_that("Differently sorted data produces same results", {
  data.altered    <- dataHigherMoments
  data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]

  expect_silent(res.orig        <- higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=dataHigherMoments, verbose=FALSE))
  expect_silent(res.diff.sorted <- higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=data.altered, verbose=FALSE))

  expect_equal(coef(res.orig), coef(res.diff.sorted))
  expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
})

# Predict ----------------------------------------------------------------------------------------------------------------------------
context("Correctness - higherMomentsIV - Predict")
test_that("No newdata results in fitted values", {
  expect_silent(higher.1 <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1), data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.1), fitted(higher.1))

  expect_silent(higher.2 <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2) + IIV(iiv=yp) + IIV(iiv=g,g=x3,X1),
                                            data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.2), fitted(higher.2))
})

test_that("Same prediction data as for fitting results in fitted values", {
  expect_silent(higher.1 <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1), data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.1, newdata=dataHigherMoments), fitted(higher.1))

  expect_silent(higher.2 <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2) + IIV(iiv=yp) + IIV(iiv=g,g=x3,X1),
                                            data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.2, newdata=dataHigherMoments), fitted(higher.2))
})

test_that("Same results as ivreg with useless iiv dummies", {
  expect_silent(higher.1 <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1), data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.1, newdata=dataHigherMoments),
               AER:::predict.ivreg(higher.1, newdata = cbind(dataHigherMoments,IIV.isgp.gisx2.regisX1=-11)))

  expect_silent(higher.2 <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2) + IIV(iiv=yp) + IIV(iiv=g,g=x3,X1),
                                            data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.2, newdata=dataHigherMoments),
               AER:::predict.ivreg(higher.2, newdata = cbind(dataHigherMoments,IIV.isy2=1.23, IIV.isyp=2.34, IIV.isg.gisx3.regisX1=3.45)))
})

test_that("Correct structure of predictions", {
  expect_silent(higher.1 <- higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1), data=dataHigherMoments, verbose=FALSE))
  expect_silent(pred.1 <- predict(higher.1, dataHigherMoments))
  expect_true(is.numeric(pred.1))
  expect_true(length(pred.1) == nrow(dataHigherMoments))
  expect_true(all(names(pred.1) == names(fitted(higher.1))))
  expect_true(all(names(pred.1) == rownames(dataHigherMoments)))
})

test_that("Correct when using transformations in the formula", {
  # transformation in regressor
  expect_silent(higher.1 <- higherMomentsIV(y~I((X1+1)/2)+X2+P|P|IIV(iiv=gp, g=x2, I((X1+1)/2)), data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.1, newdata=dataHigherMoments), fitted(higher.1))
  # transformation in endogenous
  expect_silent(higher.1 <- higherMomentsIV(y~X1+X2+I((P+14)/3)|I((P+14)/3)|IIV(iiv=gp, g=x2, X1), data=dataHigherMoments, verbose=FALSE))
  expect_equal(predict(higher.1, newdata=dataHigherMoments), fitted(higher.1))
})


# ** iiv built correctly
