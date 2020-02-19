# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")

all.L3.models <- c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3")
all.L2.models <- c("REF", "FE_L2", "GMM_L2")


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
  for(m in all.L2.models)
    expect_equal(coef(summary(res.trans.lhs, model = m)), coef(summary(correct.res, model = m)))

  # Can handle transformations in exo
  data.altered   <- dataMultilevelIV
  data.altered$X23 <- exp(data.altered$X23)
  expect_silent(res.trans.exo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + log(X23) + X24 +
                                                X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                              data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes = FALSE)
  for(m in all.L2.models)
    expect_equal(coef(summary(res.trans.exo, model = m)), coef(summary(correct.res, model = m)), check.attributes = FALSE)


  # Can handle transformations in endo
  data.altered   <- dataMultilevelIV
  data.altered$X15 <- exp(data.altered$X15)
  expect_silent(res.trans.endo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + log(X15) + X21 + X22 + X23 + X24 +
                                                 X31 + X32 + X33 + (1+X11 | SID) | endo(log(X15), X21),
                                               data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.endo), coef(correct.res), check.attributes = FALSE)
  for(m in all.L2.models)
    expect_equal(coef(summary(res.trans.endo, model = m)), coef(summary(correct.res, model = m)), check.attributes = FALSE)

  # Can handle transformations in slope
  data.altered   <- dataMultilevelIV
  data.altered$X11 <- exp(data.altered$X11)
  expect_silent(res.trans.slope <- multilevelIV(formula = y ~ log(X11) + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                  X31 + X32 + X33 + (1+log(X11) | SID) | endo(X15, X21),
                                                data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.slope), coef(correct.res), check.attributes = FALSE)
  for(m in all.L2.models)
    expect_equal(coef(summary(res.trans.slope, model = m)), coef(summary(correct.res, model = m)), check.attributes = FALSE)
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
  for(m in all.L3.models)
    expect_equal(coef(summary(res.trans.lhs, model = m)), coef(summary(correct.res, model = m)), check.attributes = FALSE)

  # Can handle transformations in exo
  data.altered   <- dataMultilevelIV
  data.altered$X23 <- exp(data.altered$X23)
  expect_message(res.trans.exo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + log(X23) + X24 +
                                                 X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(X15, X21),
                                               data = data.altered, verbose = FALSE), regexp = "singular")
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes = FALSE)
  for(m in all.L3.models)
    expect_equal(coef(summary(res.trans.exo, model = m)), coef(summary(correct.res, model = m)), check.attributes = FALSE)


  # Can handle transformations in endo
  data.altered   <- dataMultilevelIV
  data.altered$X15 <- exp(data.altered$X15)
  expect_message(res.trans.endo <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + log(X15) + X21 + X22 + X23 + X24 +
                                                  X31 + X32 + X33 + (1+X11 | CID) + (1 | SID) | endo(log(X15), X21),
                                                data = data.altered, verbose = FALSE), regexp = "singular")
  expect_equal(coef(res.trans.endo), coef(correct.res),  check.attributes = FALSE)
  for(m in all.L3.models)
    expect_equal(coef(summary(res.trans.endo, model = m)), coef(summary(correct.res, model = m)), check.attributes = FALSE)

  # Can handle transformations in slope
  data.altered   <- dataMultilevelIV
  data.altered$X11 <- exp(data.altered$X11)
  expect_message(res.trans.slope <- multilevelIV(formula = y ~ log(X11) + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                   X31 + X32 + X33 + (1+log(X11) | CID) + (1 | SID) | endo(X15, X21),
                                                 data = data.altered, verbose = FALSE), regexp = "singular")
  expect_equal(coef(res.trans.slope), coef(correct.res), check.attributes = FALSE)
  for(m in all.L3.models)
    expect_equal(coef(summary(res.trans.slope, model = m)), coef(summary(correct.res, model = m)), check.attributes = FALSE)

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

  # Coefs + summary statistics the same
  expect_equal(coef(res.unsorted), coef(res.sorted))

  for(m in all.L2.models){
    expect_equal(coef(summary(res.unsorted, model = m)), coef(summary(res.sorted, model = m)))
    # Sorting of fitted / residuals same as input
    expect_equal(names(fitted(res.sorted, model = m)),   rownames(dataMultilevelIV))
    expect_equal(names(resid(res.sorted, model = m)),    rownames(dataMultilevelIV))
    expect_equal(names(fitted(res.unsorted, model = m)), rownames(data.altered))
    expect_equal(names(resid(res.unsorted, model = m)),  rownames(data.altered))
  }
})

test_that("Unsorted data is correct L3", {

  skip_on_cran()
  # Correct = coefs + sorting of residuals / fitted

  # Distinguishable non-standard rownames
  rownames(dataMultilevelIV) <- as.character(seq(from=nrow(dataMultilevelIV)+100000, to=1+100000))
  expect_silent(res.sorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1+X12|CID)+(1+X11 | SID) | endo(X15, X21),
                                            data = dataMultilevelIV, verbose = FALSE))
  # Can handle transformations in LHS
  data.altered   <- dataMultilevelIV
  data.altered   <- data.altered[sample.int(n=nrow(dataMultilevelIV), replace = FALSE), ]
  expect_silent(res.unsorted <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                                X31 + X32 + X33 + (1+X12|CID)+(1+X11 | SID) | endo(X15, X21),
                                              data = data.altered, verbose = FALSE))

  # Coefs + summary statistics the same
  expect_equal(coef(res.unsorted), coef(res.sorted))
  for(m in all.L3.models){
    expect_equal(coef(summary(res.unsorted, model = m)), coef(summary(res.sorted, model = m)))

    # Sorting of fitted / residuals same as input
    expect_equal(names(fitted(res.sorted, model = m)),   rownames(dataMultilevelIV))
    expect_equal(names(resid(res.sorted, model = m)),    rownames(dataMultilevelIV))
    expect_equal(names(fitted(res.unsorted, model = m)), rownames(data.altered))
    expect_equal(names(resid(res.unsorted, model = m)),  rownames(data.altered))
  }
})


# Reproduce results ------------------------------------------------------------------------------
context("Correctness - multilevelIV - Reproduce results")

test_that("REF is same as lmer()", {
  # Have to use the same optimizer controls for lmer as used in multilevelIV
  skip_on_cran()
  # L2
  expect_silent(res.ml2 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                              X31 + X32 + X33 + (1+X11 | SID) | endo(X15, X21),
                                            data = dataMultilevelIV, verbose = FALSE))

  expect_silent(res.lmer2 <- lmer(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                   X31 + X32 + X33 + (1+X11 | SID),
                                 data = dataMultilevelIV,
                                 control = lmerControl(optimizer = "Nelder_Mead",
                                                       optCtrl = list(maxfun=100000))))

  expect_equal(coef(res.ml2)[, "REF"], coef(summary(res.lmer2))[, "Estimate"])

  expect_silent(res.ml3 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1|CID)+(1+X11 | SID) | endo(X15, X21),
                                        data = dataMultilevelIV, verbose = FALSE))

  expect_silent(res.lmer3 <- lmer(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                    X31 + X32 + X33 +(1|CID)+ (1+X11 | SID),
                                  data = dataMultilevelIV,
                                  control = lmerControl(optimizer = "Nelder_Mead",
                                                        optCtrl = list(maxfun=100000))))
  expect_equal(coef(res.ml3)[, "REF"], coef(summary(res.lmer3))[, "Estimate"])

})

test_that("Retrieve generated data params", {
  skip_on_cran()
  expect_silent(res.ml3 <- multilevelIV(formula = y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
                                          X31 + X32 + X33 + (1|CID)+(1| SID) | endo(X15),
                                        data = dataMultilevelIV, verbose = FALSE))

  correct.coefs <- c("(Intercept)"=64,
                     X11 = 3, X12=9, X13=-2, X14 = 2,
                     X15 = -1, #endo
                     X21 = -1.5, X22 = -4, X23 = -3, X24 = 6,
                     X31 = 0.5, X32 = 0.1, X33 = -0.5)

  expect_equal(coef(res.ml3)[, "REF"], correct.coefs, tolerance = 0.1)
})

test_that("Reproduce results by Kim and Frees 2007", {
  skip_on_cran()
  kf.formula <- TLI ~ GRADE_3 +  RETAINED  + SWITCHSC + S_FREELU +
                  FEMALE + BLACK + HISPANIC + OTHER+ C_COHORT+
                  T_EXPERI + CLASS_SI+ P_MINORI + (1 + GRADE_3|NEWCHILD) | endo(CLASS_SI)

  # Is in tests/testthat folder
  df.data.kf <- read.csv("dallas2485.csv", header=TRUE)

  expect_silent(res.kf <- multilevelIV(formula = kf.formula, data = df.data.kf, verbose = FALSE))

  correct.coefs <- cbind(REF = c("(Intercept)"=69.78, GRADE_3=3.375, RETAINED=9.205, SWITCHSC=-0.365, S_FREELU=-0.227, FEMALE=-1.234, BLACK=-4.745,
                                 HISPANIC=-3.608, OTHER=6.526, C_COHORT=1.497, T_EXPERI=-0.116, CLASS_SI=0.157, P_MINORI=0.069))

  # Compare coefs
  expect_equal(coef(res.kf)[, "REF"], correct.coefs[, "REF"], tolerance=0.1)

})




# Predict ----------------------------------------------------------------------------------------
context("Correctness - multilevelIV - Predict")
expect_silent(res.m2 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
                                       X32 + X33 + (1|SID) | endo(X15),
                                     data = dataMultilevelIV, verbose = FALSE))
expect_silent(res.m3 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
                                     X32 + X33 + (1| CID) + (1|SID) | endo(X15),
                                   data = dataMultilevelIV, verbose = FALSE))
test_that("No newdata results in fitted values", {
  for(m in all.L2.models)
    expect_equal(predict(res.m2, model=m), fitted(res.m2, model=m))

  for(m in all.L3.models)
    expect_equal(predict(res.m3, model=m), fitted(res.m3, model=m))
})


test_that("Same prediction data as for fitting results in fitted values", {
  for(m in all.L2.models)
    expect_equal(predict(res.m2, newdata=dataMultilevelIV,model=m), fitted(res.m2, model=m))

  for(m in all.L3.models)
    expect_equal(predict(res.m3, newdata=dataMultilevelIV, model=m), fitted(res.m3, model=m))
})

test_that("Correct structure of predictions", {
  for(m in all.L2.models){
    expect_silent(pred.2 <- predict(res.m2, newdata=dataMultilevelIV, model=m))
    expect_true(is.numeric(pred.2))
    expect_true(length(pred.2) == nrow(dataMultilevelIV))
    expect_true(all(names(pred.2) == names(fitted(res.m2, model=m))))
    expect_true(all(names(pred.2) == rownames(dataMultilevelIV)))
  }

  for(m in all.L3.models){
    expect_silent(pred.3 <- predict(res.m3, newdata=dataMultilevelIV, model=m))
    expect_true(is.numeric(pred.3))
    expect_true(length(pred.3) == nrow(dataMultilevelIV))
    expect_true(all(names(pred.3) == names(fitted(res.m3, model=m))))
    expect_true(all(names(pred.3) == rownames(dataMultilevelIV)))
  }
})

test_that("Correct when using transformations in the formula", {
  skip_on_cran()
  # do endogenous and non-endogenous at the same time
  expect_silent(res.m2 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + I((X15+14)/4) + X21 + X22 + X23 +
                                         X24 + log(X31) + X32 + X33 + (1|SID) | endo(I((X15+14)/4)),
                                       data = dataMultilevelIV, verbose = FALSE))
  for(m in all.L2.models)
    expect_equal(predict(res.m2, newdata=dataMultilevelIV,model=m), fitted(res.m2, model=m))

  expect_silent(res.m3 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + I((X15+14)/4) + X21 + X22 + X23 +
                                         X24 + log(X31) + X32 + X33 + (1| CID) + (1|SID) | endo(I((X15+14)/4)),
                                       data = dataMultilevelIV, verbose = FALSE))
  for(m in all.L3.models)
    expect_equal(predict(res.m3, newdata=dataMultilevelIV, model=m), fitted(res.m3, model=m))
})

