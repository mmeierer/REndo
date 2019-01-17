# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

# General runability -----------------------------------------------------------------------------------------------------------------
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
  skip_on_cran()
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
  skip_on_cran()
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV, verbose = FALSE))
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV, verbose = FALSE))
})

test_that("Works with all endo", {
  skip_on_cran()
  expect_silent(multilevelIV(formula = y ~ X15 + X31 + X32 + (1 | SID) | endo(X15,X31,X32),
                             data = dataMultilevelIV, verbose = FALSE))
  expect_silent(multilevelIV(formula = y ~ X15 + X31 + X32 + (1 | CID) + (1 | SID) | endo(X15,X31,X32),
                             data = dataMultilevelIV, verbose = FALSE))
})


test_that("Works with group ids as character", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
