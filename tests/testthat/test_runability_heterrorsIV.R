# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHetIV")

# Runability ----------------------------------------------------------------------------------------------------------------------
context("Runability - hetErrorsIV - Runability")
test_that("Works with intercept", {
  expect_silent(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=dataHetIV, verbose=F))
})

test_that("Works without intercept", {
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|IIV(X1), data=dataHetIV, verbose=F))
})


test_that("Produces output", {
  expect_message(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=dataHetIV, verbose=TRUE),
                 regexp = "The following internal instruments were built:")
})


test_that("Works with single exo", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|IIV(X1), data=dataHetIV, verbose=F))
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|IIV(X1), data=dataHetIV, verbose=F))
})

test_that("Works with two exo", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|IIV(X1,X2), data=dataHetIV, verbose=F))
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|IIV(X1,X2), data=dataHetIV, verbose=F))
})

test_that("Same result for swapped regs in IIVs", {
  expect_silent(res.1 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1,X2), data=dataHetIV, verbose=F))
  expect_silent(res.2 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X2,X1), data=dataHetIV, verbose=F))
  expect_equal(coef(summary(res.1)), coef(summary(res.2)))
})

test_that("Same result for separated IIVs",{
  expect_silent(res.1 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1,X2), data=dataHetIV, verbose=F))
  expect_silent(res.2 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1)+IIV(X2), data=dataHetIV, verbose=F))
  expect_equal(coef(summary(res.1)), coef(summary(res.2)))
})

test_that("Works with external IV", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|IIV(X1) | X2, data=dataHetIV, verbose=F))
  expect_silent(hetErrorsIV(y~X1   +P  |P|IIV(X1) | X2, data=dataHetIV, verbose=F))
})

test_that("Non-numerics can be used in exogenous data", {
  expect_silent(hetErrorsIV(y~X1+X2+P+color |P|IIV(X1), verbose=F,
                            data=cbind(dataHetIV, color=factor(x = c("red", "green", "blue", "white", "yellow")))))
  expect_silent(hetErrorsIV(y~X1+X2+P+color-1 |P|IIV(X1), verbose=F,
                            data=cbind(dataHetIV, color=factor(x = c("red", "green", "blue", "white", "yellow")))))
})

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Runability - hetErrorsIV - Formula transformations")
test_that("Works with transformation in exogenous", {
  # Non IIV exo
  expect_silent(hetErrorsIV(y~log(X1)+X2+P  |P|IIV(X2), data=dataHetIV, verbose=F))
  # IIV exo
  expect_silent(hetErrorsIV(y~log(X1)+X2+P  |P|IIV(log(X1), X2), data=dataHetIV, verbose=F))
})

test_that("Works with endo transformation", {
  expect_silent(hetErrorsIV(y~X1+X2+I(P+1)|I(P+1)|IIV(X1,X2), data=dataHetIV, verbose=F))
})

test_that("Weak instruments trigger warning", {
  expect_warning(hetErrorsIV(y~X1+X2+exp(P)|exp(P)|IIV(X1,X2), data=dataHetIV, verbose=F))
})

test_that("Transformations are correct", {
  expect_silent(correct.res <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1), data=dataHetIV, verbose=F))
  # Can handle transformations in LHS
  data.altered   <- dataHetIV
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs <- hetErrorsIV(log(y)~X1+X2+P  |P|IIV(X1), data=data.altered, verbose=FALSE))
  expect_equal(coef(res.trans.lhs), coef(correct.res))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res)))

  # Can handle transformations in endo
  data.altered    <- dataHetIV
  data.altered$P <- exp(data.altered$P)
  expect_silent(res.trans.endo  <- hetErrorsIV(formula = y~X1+X2+log(P)|log(P)|IIV(X1), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.endo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.endo)), coef(summary(correct.res)), check.attributes=FALSE)

  # Can handle transformations in nonIIV exo
  data.altered    <- dataHetIV
  data.altered$X2 <- exp(data.altered$X2)
  expect_silent(res.trans.exo  <- hetErrorsIV(formula = y~X1+log(X2)+P|P|IIV(X1), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.exo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.exo)), coef(summary(correct.res)), check.attributes=FALSE)

  # Can handle transformations in IIV exo
  data.altered    <- dataHetIV
  data.altered$X1 <- exp(data.altered$X1)
  expect_silent(res.trans.iiv.exo  <- hetErrorsIV(formula = y~log(X1)+X2+P|P|IIV(log(X1)), data = data.altered, verbose = FALSE))
  expect_equal(coef(res.trans.iiv.exo), coef(correct.res), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.iiv.exo)), coef(summary(correct.res)), check.attributes=FALSE)
})
