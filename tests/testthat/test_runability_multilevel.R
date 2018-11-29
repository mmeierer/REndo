# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

context("Runability - multilevelIV - Runability")

test_that("Works with 2 levels", {
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE))
})

test_that("Works with 3 levels", {
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE))
})


test_that("Works without intercept", {
  expect_silent(res.ml <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 -1 + (X11-1| CID) + (X14-1| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE))
  expect_false("(Intercept)" %in% rownames(coef(res.ml)))

  expect_silent(res.ml <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 - 1 + (X11-1 | SID) | endo(X15),
                                        data = dataMultilevelIV, verbose = FALSE))
  expect_false("(Intercept)" %in% rownames(coef(res.ml)))
})


# Runs with single / multiple endos in different spellings
test_that("Works with multiple endos", {
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV, verbose = FALSE))
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV, verbose = FALSE))
})

test_that("Works with all endo", {
  expect_silent(multilevelIV(formula = y ~ X15 + X31 + X32 + (1 | SID) | endo(X15,X31,X32),
                             data = dataMultilevelIV, verbose = FALSE))
  expect_silent(multilevelIV(formula = y ~ X15 + X31 + X32 + (1 | CID) + (1 | SID) | endo(X15,X31,X32),
                             data = dataMultilevelIV, verbose = FALSE))
})


test_that("Works with group ids as character", {
  dataMultilevelIV$charSID <- as.character(dataMultilevelIV$SID)
  dataMultilevelIV$charCID <- as.character(dataMultilevelIV$CID)
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                               X31 + X32 + X33 + (1 | charSID) | endo(X15),
                             data = dataMultilevelIV, verbose = FALSE))
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                               X31 + X32 + X33 + (1 | charCID) + (1 | charSID) | endo(X15),
                             data = dataMultilevelIV, verbose = FALSE))
})

test_that("Works with group ids as factor", {
  dataMultilevelIV$factorSID <- as.factor(as.character(dataMultilevelIV$SID))
  dataMultilevelIV$factorCID <- as.factor(as.character(dataMultilevelIV$CID))
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                               X31 + X32 + X33 + (1 | factorSID) | endo(X15),
                             data = dataMultilevelIV, verbose = FALSE))
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                               X31 + X32 + X33 + (1 | factorCID) + (1 | factorSID) | endo(X15),
                             data = dataMultilevelIV, verbose = FALSE))
})

test_that("Works with slopes as factors and chars", {
  dataMultilevelIV$charSLP   <- rep(LETTERS[1:5], nrow(dataMultilevelIV)/5)
  dataMultilevelIV$factorSLP <- as.factor(dataMultilevelIV$charSLP)
  # intercept + category slope
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + charSLP+(1+charSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular fit")
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + factorSLP+(1+factorSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular fit")
  # category slope only
  expect_message(multilevelIV(formula = y ~ 0+X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + charSLP+(0+charSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular fit")
  expect_message(multilevelIV(formula = y ~ 0+X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + factorSLP+(0+factorSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular fit")
})



# Formula transformations ------------------------------------------------------------------------------------------------------------

context("Runability - multilevelIV - Formula transformations L2")

# **correct if levels are given separately: (A|SID) + (B|SID) == (A+B|SID)

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


context("Runability - multilevelIV - Formula transformations L3")
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
