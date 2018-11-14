# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

context("Runability - multilevelIV - Runability")

test_that("Works with 2 levels", {
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | SID) | endo(X15),
                              data = dataMultilevelIV))
})

test_that("Works with 3 levels", {
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15),
                              data = dataMultilevelIV))
})


test_that("Works without intercept", {
  expect_message(res.ml <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 -1 + (1 + X11 | CID) + (1 | SID) | endo(X15),
                              data = dataMultilevelIV))
  expect_false("(Intercept)" %in% rownames(coef(res.ml)))

  expect_message(res.ml <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 - 1 + (1 | SID) | endo(X15),
                                        data = dataMultilevelIV))
  expect_false("(Intercept)" %in% rownames(coef(res.ml)))
})


# Runs with single / multiple endos in different spellings
test_that("Works with multiple endos", {
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV))
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15,X31,X32),
                              data = dataMultilevelIV))
})
