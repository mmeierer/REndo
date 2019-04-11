# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Correctness - multilevelIV - Formula transformations")

test_that("Transformations are correct for L2", {
  skip_on_cran()

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
  skip_on_cran()
  expect_message(correct.res <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                               X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(X15, X21),
                                             data = dataMultilevelIV, verbose = FALSE), regexp = "singular")
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered$y <- exp(data.altered$y)
  expect_message(res.trans.lhs <- multilevelIV(formula = log(y) ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                 X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(X15, X21),
                                               data = data.altered, verbose = FALSE), regexp = "singular")
  expect_equal(coef(res.trans.lhs), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)), check.attributes = FALSE)

  # Can handle transformations in exo
  data.altered   <- dataMultilevelIV
  data.altered$X23 <- exp(data.altered$X23)
  expect_message(res.trans.exo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + log(X23) + X24 +
                                                 X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(X15, X21),
                                               data = data.altered, verbose = FALSE), regexp = "singular")
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes = FALSE)


  # Can handle transformations in endo
  data.altered   <- dataMultilevelIV
  data.altered$X15 <- exp(data.altered$X15)
  expect_message(res.trans.endo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + log(X15) + X21 + X22 + X23 + X24 +
                                                  X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(log(X15), X21),
                                                data = data.altered, verbose = FALSE), regexp = "singular")
  expect_equal(coef(res.trans.endo), coef(correct.res), tolerance = 1e-5, check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), tolerance = 1e-5, check.attributes = FALSE)

  # Can handle transformations in slope
  data.altered   <- dataMultilevelIV
  data.altered$X11 <- exp(data.altered$X11)
  expect_message(res.trans.slope <- multilevelIV(formula = y ~ log(X11) + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                   X31 + X32 + X33 + (1+log(X11) | CID) + (1 | SID) | endo(X15, X21),
                                                 data = data.altered, verbose = FALSE), regexp = "singular")
  expect_equal(coef(res.trans.slope), coef(correct.res), tolerance = 1e-5, check.attributes = FALSE)
  expect_equal(coef(summary(res.trans.slope)), coef(summary(correct.res)), tolerance = 1e-5, check.attributes = FALSE)

})


# Data sorting ------------------------------------------------------------------------------------------------
context("Correctness - multilevelIV - Data sorting")

# **correct if levels are given separately: (A|SID) + (B|SID) == (A+B|SID)
# ** Also check for W and V?


test_that("Unsorted data is correct L2", {
  skip_on_cran()
  # Correct = coefs + sorting of residuals / fitted

  # Distinguishable non-standard rownames
  rownames(dataMultilevelIV) <- as.character(seq(from=nrow(dataMultilevelIV)+100000, to=1+100000))

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

# test_that("Unsorted data is correct L3", {
#   skip_on_cran()
#   # Correct = coefs + sorting of residuals / fitted
#
#   # Distinguishable non-standard rownames
#   rownames(dataMultilevelIV) <- as.character(seq(from=nrow(dataMultilevelIV)+100000, to=1+100000))
#   expect_silent(res.sorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
#                                               X31 + X32 + X33 + (1+X12|CID)+(1+X11 | SID) | endo(X15, X21),
#                                             data = dataMultilevelIV, verbose = FALSE)) #, regex = "singular")
#   # Can handle transformations in LHS
#   data.altered   <- dataMultilevelIV
#   data.altered   <- data.altered[sample.int(n=nrow(dataMultilevelIV), replace = FALSE), ]
#   expect_silent(res.unsorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
#                                                 X31 + X32 + X33 + (1+X12|CID)+(1+X11 | SID) | endo(X15, X21),
#                                               data = data.altered, verbose = FALSE)) #, regex = "singular")
#
#   # Coefs the same
#   # Have to use tolerance because lmer() provides slightly different results for sorted/unsorted data
#   expect_equal(coef(res.unsorted), coef(res.sorted), tolerance = 10e-5)
#   expect_equal(coef(summary(res.unsorted)), coef(summary(res.sorted)), tolerance = 10e-5)
#
#   # Sorting of fitted / residuals same as input
#   expect_equal(names(fitted(res.sorted)),   rownames(dataMultilevelIV))
#   expect_equal(names(resid(res.sorted)),    rownames(dataMultilevelIV))
#   expect_equal(names(fitted(res.unsorted)), rownames(data.altered))
#   expect_equal(names(resid(res.unsorted)),  rownames(data.altered))
# })


# Reproduce results ------------------------------------------------------------------------------
context("Correctness - multilevelIV - Reproduce results")

test_that("REF is same as lmer()", {
  skip_on_cran()
  # L2
  expect_silent(res.ml2 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                            data = dataMultilevelIV, verbose = FALSE))

  expect_silent(res.lmer2 <- lmer(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                   X31 + X32 + X33 + (1+X11 | SID),
                                 data = dataMultilevelIV))

  expect_equal(coef(res.ml2)[, "REF"], coef(summary(res.lmer2))[, "Estimate"])

  expect_silent(res.ml3 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1|CID)+(1+X11 | SID) | endo(X15, X21),
                                        data = dataMultilevelIV, verbose = FALSE))

  expect_silent(res.lmer3 <- lmer(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                    X31 + X32 + X33 +(1|CID)+ (1+X11 | SID),
                                  data = dataMultilevelIV))
  expect_equal(coef(res.ml3)[, "REF"], coef(summary(res.lmer3))[, "Estimate"])

})

# test_that("Retrieve generated data params", {
#   expect_silent(res.ml3 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
#                                           X31 + X32 + X33 + (1|CID)+(1| SID) | endo(X15),
#                                         data = dataMultilevelIV, verbose = FALSE))
#
#   correct.coefs <- c("(Intercept)"=64,
#                      X11 = 3, X12=9, X13=-2, X14 = 2,
#                      X15 = -1, #endo
#                      X21 = -1.5, X22 = -4, X23 = -3, X24 = 6,
#                      X31 = 0.5, X32 = 0.1, X33 = -0.5)
#
#   expect_equal(coef(res.ml3), correct.coefs, tolerance = 0.1)
# })

# test_that("Reproduce results by Kim and Frees 2007", {
#   kf.formula <- TLI ~ GRADE_3 +  RETAINED  + SWITCHSC + S_FREELU +
#                   FEMALE + BLACK + HISPANIC + OTHER+ C_COHORT+
#                   T_EXPERI + CLASS_SI+ P_MINORI + (1 + GRADE_3|NEWCHILD) | endo(CLASS_SI)
#
#   # Is in tests/testthat folder
#   df.data.kf <- read.csv("dallas2485.csv", header=TRUE)
#
#   expect_silent(res.kf <- multilevelIV(formula = kf.formula, data = df.data.kf, verbose = FALSE))
#
#   correct.coefs <- cbind(REF = c(3.375, 9.205, -0.365, -0.227, -1.234, -4.745, -3.608, 6.526, 1.497, -0.116, 0.157, 0.069))
#
#   # Compare coefs
#   expect_equal(coef(res.kf) )
#
# })
