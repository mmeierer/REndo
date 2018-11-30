# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

# Formula transformations ------------------------------------------------------------------------------------------------------------

context("Correctness - multilevelIV - Formula transformations L2")

# **correct if levels are given separately: (A|SID) + (B|SID) == (A+B|SID)

context("Correctness - multilevelIV - Sorting")

# ** Also check for W and V?

test_that("Rownames are kept for fitted, residuals", {
  data.altered <- dataMultilevelIV
  rownames(data.altered) <- as.character(seq(from=nrow(data.altered)+100000, to=1+100000))
  expect_silent(res.ml2 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1| SID) | endo(X15, X21),
                                            data = data.altered, verbose = FALSE))
  expect_silent(res.ml3 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1|CID) +(1| SID) | endo(X15, X21),
                                        data = data.altered, verbose = FALSE))

  expect_equal(names(fitted(res.ml2)), rownames(data.altered))
  expect_equal(names(resid(res.ml2)),  rownames(data.altered))
  expect_equal(names(fitted(res.ml3)), rownames(data.altered))
  expect_equal(names(resid(res.ml3)),  rownames(data.altered))
})

context("Correctness - multilevelIV - Formula transformations L2")
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


context("Correctness - multilevelIV - Formula transformations L3")

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
