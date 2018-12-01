# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - multilevelIV - Formula transformations")

test_that("Transformations are correct for L2", {
  expect_silent(correct.res <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                            data = dataMultilevelIV, verbose = FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs <- multilevelIV(formula = log(y) ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                              data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.lhs), coef(correct.res))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)))

  # Can handle transformations in exo
  data.altered   <- dataMultilevelIV
  data.altered$X23 <- exp(data.altered$X23)
  expect_silent(res.trans.exo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + log(X23) + X24 +
                                                X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                              data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes = FALSE)


  # Can handle transformations in endo
  data.altered   <- dataMultilevelIV
  data.altered$X15 <- exp(data.altered$X15)
  expect_silent(res.trans.endo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + log(X15) + X21 + X22 + X23 + X24 +
                                                 X31 + X32 + X33 + (1+X11 | SID) | endo(log(X15), X21),
                                               data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.endo), coef(correct.res), check.attributes = FALSE, tolerance = 1e-6)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), tolerance = 1e-6, check.attributes = FALSE)

  # Can handle transformations in slope
  data.altered   <- dataMultilevelIV
  data.altered$X11 <- exp(data.altered$X11)
  expect_silent(res.trans.slope <- multilevelIV(formula = y ~ log(X11) + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                  X31 + X32 + X33 + (1+log(X11) | SID) | endo(X15, X21),
                                                data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.slope), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.slope)), coef(summary(correct.res)), tolerance = 1e-6, check.attributes = FALSE)
})



test_that("Transformations are correct for L3", {
  expect_message(correct.res <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                               X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(X15, X21),
                                             data = dataMultilevelIV, verbose = FALSE), regexp = "singular fit")
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered$y <- exp(data.altered$y)
  expect_message(res.trans.lhs <- multilevelIV(formula = log(y) ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                 X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(X15, X21),
                                               data = data.altered, verbose = FALSE), regexp = "singular fit")
  expect_equal(coef(res.trans.lhs), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)), check.attributes = FALSE)

  # Can handle transformations in exo
  data.altered   <- dataMultilevelIV
  data.altered$X23 <- exp(data.altered$X23)
  expect_message(res.trans.exo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + log(X23) + X24 +
                                                 X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(X15, X21),
                                               data = data.altered, verbose = FALSE), regexp = "singular fit")
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes = FALSE)


  # Can handle transformations in endo
  data.altered   <- dataMultilevelIV
  data.altered$X15 <- exp(data.altered$X15)
  expect_message(res.trans.endo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + log(X15) + X21 + X22 + X23 + X24 +
                                                  X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(log(X15), X21),
                                                data = data.altered, verbose = FALSE), regexp = "singular fit")
  expect_equal(coef(res.trans.endo), coef(correct.res), tolerance = 1e-6, check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), tolerance = 1e-6, check.attributes = FALSE)

  # Can handle transformations in slope
  data.altered   <- dataMultilevelIV
  data.altered$X11 <- exp(data.altered$X11)
  expect_message(res.trans.slope <- multilevelIV(formula = y ~ log(X11) + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                   X31 + X32 + X33 + (1+log(X11) | CID) + (1 | SID) | endo(X15, X21),
                                                 data = data.altered, verbose = FALSE), regexp = "singular fit")
  expect_equal(coef(res.trans.slope), coef(correct.res), tolerance = 1e-6, check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.slope)), coef(summary(correct.res)), tolerance = 1e-6, check.attributes = FALSE)

})


# Unsorted data ------------------------------------------------------------------------------------------------
context("Correctness - multilevelIV - Data sorting")

# **correct if levels are given separately: (A|SID) + (B|SID) == (A+B|SID)
# ** Also check for W and V?


test_that("Unsorted data is correct L2", {
  # Correct = coefs + sorting of residuals / fitted

  # Distinguishable non-standard rownames
  rownames(dataMultilevelIV) <- as.character(seq(from=nrow(data.altered)+100000, to=1+100000))

  expect_silent(res.sorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                            data = dataMultilevelIV, verbose = FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered   <- data.altered[sample(x=nrow(dataMultilevelIV), size = nrow(dataMultilevelIV), replace = FALSE), ]
  expect_silent(res.unsorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                              data = data.altered, verbose = FALSE))

  # Coefs the same
  # Have to use tolerance because lmer() provides slightly different results for sorted/unsorted data
  expect_equal(coef(res.unsorted), coef(res.sorted), tolerance = 10e-5)
  expect_equal(coef(summary(res.unsorted)), coef(summary(res.sorted)), tolerance = 10e-5)

  # Sorting of fitted / residuals same as input
  expect_equal(names(fitted(res.sorted)),   rownames(dataMultilevelIV))
  expect_equal(names(resid(res.sorted)),    rownames(dataMultilevelIV))
  expect_equal(names(fitted(res.unsorted)), rownames(data.altered))
  expect_equal(names(resid(res.unsorted)),  rownames(data.altered))
})

test_that("Unsorted data is correct L3", {
  # Correct = coefs + sorting of residuals / fitted

  # Distinguishable non-standard rownames
  rownames(dataMultilevelIV) <- as.character(seq(from=nrow(data.altered)+100000, to=1+100000))
  expect_message(res.sorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1+X12|CID)+(1+X11 | SID) | endo(X15, X21),
                                            data = dataMultilevelIV, verbose = FALSE), regex = "singular fit")
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered   <- data.altered[sample(x=nrow(dataMultilevelIV), size = nrow(dataMultilevelIV), replace = FALSE), ]
  expect_message(res.unsorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                X31 + X32 + X33 + (1+X12|CID)+(1+X11 | SID) | endo(X15, X21),
                                              data = data.altered, verbose = FALSE), regex = "singular fit")

  # Coefs the same
  # Have to use tolerance because lmer() provides slightly different results for sorted/unsorted data
  expect_equal(coef(res.unsorted), coef(res.sorted), tolerance = 10e-5)
  expect_equal(coef(summary(res.unsorted)), coef(summary(res.sorted)), tolerance = 10e-5)

  # Sorting of fitted / residuals same as input
  expect_equal(names(fitted(res.sorted)),   rownames(dataMultilevelIV))
  expect_equal(names(resid(res.sorted)),    rownames(dataMultilevelIV))
  expect_equal(names(fitted(res.unsorted)), rownames(data.altered))
  expect_equal(names(resid(res.unsorted)),  rownames(data.altered))
})
