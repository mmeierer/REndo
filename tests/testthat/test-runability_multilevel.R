# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

# General runability -----------------------------------------------------------------------------------------------------------------
context("Runability - multilevelIV - Runability")

test_that("Works with 2 levels", {
  res.ml2 <- expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1 | SID) | endo(X15),
                                        data = dataMultilevelIV, verbose = FALSE))
})

test_that("Works with 3 levels", {
  res.ml3 <- expect_silent(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15),
                                        data = dataMultilevelIV, verbose = FALSE))
  assign("res.ml3", res.ml3, envir = parent.frame())
})


test_that("Works with user given lmer.control and provides different results", {
  skip_on_cran()
  # 2 levels
  expect_silent(res.cobyla.l2 <-
                  multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                 X31 + X32 + X33 + (1 | SID) | endo(X15),
                               data = dataMultilevelIV, verbose = FALSE,
                               lmer.control = lmerControl(optimizer="nloptwrap",
                                                          optCtrl=list(algorithm="NLOPT_LN_COBYLA",
                                                                       xtol_rel=1),
                                                          check.conv.grad = "ignore",
                                                          check.conv.singular = "ignore",
                                                          check.conv.hess = "ignore")))
  # different estimates
  expect_silent(res.ml2 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1 | SID) | endo(X15),
                                        data = dataMultilevelIV, verbose = FALSE))
  expect_false(isTRUE(all.equal(coef(res.ml2), coef(res.cobyla.l2))))

  # 3 levels
  expect_silent(res.cobyla.l3 <-
                  multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                 X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15),
                               data = dataMultilevelIV, verbose = FALSE,
                               lmer.control = lmerControl(optimizer="nloptwrap",
                                                          optCtrl=list(algorithm="NLOPT_LN_COBYLA",
                                                                       xtol_rel=1),
                                                          check.conv.grad = "ignore",
                                                          check.conv.singular = "ignore",
                                                          check.conv.hess = "ignore")))
  # different estimates
  expect_silent(res.ml3 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15),
                                        data = dataMultilevelIV, verbose = FALSE))
  expect_false(isTRUE(all.equal(coef(res.ml3), coef(res.cobyla.l3))))
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


test_that("Works with NA in not needed columns", {
  skip_on_cran()
  dataMultilevelIV.na <- dataMultilevelIV
  dataMultilevelIV.na[5, "X13"] <- NA_real_
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X14 + X15 + X21 + X22 + X23 + X24 +
                               X31 + X32 + X33 + (1| CID) + (1| SID) | endo(X15),
                             data = dataMultilevelIV.na, verbose = FALSE))

  dataMultilevelIV.inf <- dataMultilevelIV
  dataMultilevelIV.inf[5, "X13"] <- Inf
  expect_silent(multilevelIV(formula = y ~ X11 + X12 + X14 + X15 + X21 + X22 + X23 + X24 +
                               X31 + X32 + X33 + (1| CID) + (1| SID) | endo(X15),
                             data = dataMultilevelIV.inf, verbose = FALSE))
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
  dataMultilevelIV$charSLP   <- rep_len(LETTERS[1:2], nrow(dataMultilevelIV))
  dataMultilevelIV$factorSLP <- as.factor(dataMultilevelIV$charSLP)
  # intercept + category slope
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + charSLP+(1+charSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular")
  expect_message(multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + factorSLP+(1+factorSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular")
  # category slope only
  expect_message(multilevelIV(formula = y ~ 0+X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + charSLP+(0+charSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular")
  expect_message(multilevelIV(formula = y ~ 0+X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                X31 + X32 + X33 + factorSLP+(0+factorSLP| SID) | endo(X15),
                              data = dataMultilevelIV, verbose = FALSE), regexp = "singular")
})

test_that("Works with rank-deficient and all same-size groups, see issue #63",{
  #Model specification and example data from pinson06
  skip_on_cran()

  # Is in tests/testthat folder
  df.data.pinson06 <- read.csv("pinson06_rankdeficient_samesizegroup.csv", header=TRUE)
  expect_message(multilevelIV(formula = y ~ e + a + f + w + l + w2 + w_l + f_l + e_l + e_w + a_w + a_l + a_e +
                                             e_w_l + f_w_l + f_a_l + f_e_l + a_w_l + f_a_e_l + v + pr + t +
                                             (1 | album_id) | endo(e,a,f),
                                           data = df.data.pinson06, verbose=FALSE),
                 regexp = "matrix is rank deficient")

})
