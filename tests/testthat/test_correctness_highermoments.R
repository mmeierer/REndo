# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHigherMoments")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - higherMomentsIV - Formula transformations")


# Data sorting ------------------------------------------------------------------------------------------------
context("Correctness - higherMomentsIV - Data sorting")

test_that("Differently sorted data produces same results", {
  data.altered    <- dataHigherMoments
  data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]

  expect_silent(res.orig        <- higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=dataHigherMoments, verbose=FALSE))
  expect_silent(res.diff.sorted <- higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=data.altered, verbose=FALSE))

  expect_equal(coef(res.orig), coef(res.diff.sorted))
  expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
})


# ** iiv built correctly
