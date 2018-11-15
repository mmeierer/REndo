# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

context("Runability - multilevelIV - Runability")

test_that("Works with 2 levels", {
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE))
})

test_that("Works with 3 levels", {
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE))
})


test_that("Works without intercept", {
  expect_message(res.ml <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 -1 + (1 + X11 | CID) + (1 | SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE))
  expect_false("(Intercept)" %in% rownames(coef(res.ml)))

  expect_message(res.ml <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 - 1 + (1 | SID) | endo(X15),
                                        data = dataMultilevelIV, verbose = FALSE))
  expect_false("(Intercept)" %in% rownames(coef(res.ml)))
})


# Runs with single / multiple endos in different spellings
test_that("Works with multiple endos", {
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV, verbose = FALSE))
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV, verbose = FALSE))
})


# Formula transformations ------------------------------------------------------------------------------------------------------------


context("Runability - multilevelIV - Formula transformations L2")
test_that("Transformations are correct for L2", {
  expect_message(correct.res <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1 | SID) | endo(X15, X21),
                                            data = dataMultilevelIV, verbose = FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered$y <- exp(data.altered$y)
  expect_message(res.trans.lhs <- multilevelIV(formula = log(y) ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1 | SID) | endo(X15, X21),
                                            data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.lhs), coef(correct.res))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)))

  # Can handle transformations in exo
  data.altered   <- dataMultilevelIV
  data.altered$X23 <- exp(data.altered$X23)
  expect_message(res.trans.exo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + log(X23) + X24 +
                                                  X31 + X32 + X33 + (1 | SID) | endo(X15, X21),
                                                data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes = FALSE)


  # Can handle transformations in endo
  data.altered   <- dataMultilevelIV
  data.altered$X15 <- exp(data.altered$X15)
  expect_message(res.trans.endo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + log(X15) + X21 + X22 + X23 + X24 +
                                                 X31 + X32 + X33 + (1 | SID) | endo(log(X15), X21),
                                               data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.endo), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), tolerance = 1e-6, check.attributes = FALSE)
})


context("Runability - multilevelIV - Formula transformations L3")
test_that("Transformations are correct for L3", {
  expect_message(correct.res <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                               X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15, X21),
                                             data = dataMultilevelIV, verbose = FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered$y <- exp(data.altered$y)
  expect_message(res.trans.lhs <- multilevelIV(formula = log(y) ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                 X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15, X21),
                                               data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.lhs), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)), check.attributes = FALSE)

  # Can handle transformations in exo
  data.altered   <- dataMultilevelIV
  data.altered$X23 <- exp(data.altered$X23)
  expect_message(res.trans.exo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + log(X23) + X24 +
                                                 X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15, X21),
                                               data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes = FALSE)


  # Can handle transformations in endo
  data.altered   <- dataMultilevelIV
  data.altered$X15 <- exp(data.altered$X15)
  expect_message(res.trans.endo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + log(X15) + X21 + X22 + X23 + X24 +
                                                  X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(log(X15), X21),
                                                data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.endo), coef(correct.res), tolerance = 1e-6, check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), tolerance = 1e-6, check.attributes = FALSE)
})
