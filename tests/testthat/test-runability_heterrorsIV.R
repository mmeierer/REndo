# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHetIV")

# Runability ----------------------------------------------------------------------------------------------------------------------
context("Runability - hetErrorsIV - Runability")
test_that("Works with intercept", {
  expect_silent(hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=dataHetIV, verbose=FALSE))
})

test_that("Works without intercept", {
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|IIV(X2), data=dataHetIV, verbose=FALSE))
})


test_that("Produces output", {
  expect_message(hetErrorsIV(y~X1+X2+P|P|IIV(X2), data=dataHetIV, verbose=TRUE),
                 regexp = "The following internal instruments were built:")
})


test_that("Works with single exo", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|IIV(X2), data=dataHetIV, verbose=FALSE))
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|IIV(X2), data=dataHetIV, verbose=FALSE))
})

test_that("Works with two exo", {
  expect_warning(hetErrorsIV(y~X1+X2+P  |P|IIV(X1,X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_warning(hetErrorsIV(y~X1+X2+P-1|P|IIV(X1,X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
})

test_that("Same result for swapped regs in IIVs", {
  expect_warning(res.1 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1,X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_warning(res.2 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X2,X1), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_equal(coef(summary(res.1)), coef(summary(res.2)))
})

test_that("Same result for separated IIVs",{
  expect_warning(res.1 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1,X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_warning(res.2 <- hetErrorsIV(y~X1+X2+P  |P|IIV(X1)+IIV(X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
  expect_equal(coef(summary(res.1)), coef(summary(res.2)))
})

test_that("Works with external IV", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|IIV(X2) | X2, data=dataHetIV, verbose=FALSE))
  expect_warning(hetErrorsIV(y~X1  +P  |P|IIV(X1) | X2, data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
})

test_that("Non-numerics can be used in exogenous data", {
  expect_silent(hetErrorsIV(y~X1+X2+P+color |P|IIV(X2), verbose=FALSE,
                            data=cbind(dataHetIV, color=factor(x = c("red", "green", "blue", "white", "yellow")))))
  expect_silent(hetErrorsIV(y~X1+X2+P+color-1 |P|IIV(X2), verbose=FALSE,
                            data=cbind(dataHetIV, color=factor(x = c("red", "green", "blue", "white", "yellow")))))
})

test_that("Works with NA in not needed columns", {
  dataHetIV.na <- dataHetIV
  dataHetIV.na[5, "X1"] <- NA_real_
  expect_silent(hetErrorsIV(y~X2+P|P|IIV(X2), data=dataHetIV.na, verbose=FALSE))

  dataHetIV.inf <- dataHetIV
  dataHetIV.inf[5, "X2"] <- Inf
  expect_warning(hetErrorsIV(y~X1+P|P|IIV(X1), data=dataHetIV.inf, verbose=FALSE), regexp = "Breusch-Pagan")
})


# Formula transformations ------------------------------------------------------------------------------------------------------------
context("Runability - hetErrorsIV - Formula transformations")
test_that("Works with transformation in exogenous", {
  # Non IIV exo
  expect_silent(hetErrorsIV(y~log(X1+100)+X2+P  |P|IIV(X2), data=dataHetIV, verbose=FALSE))
  # IIV exo
  expect_silent(hetErrorsIV(y~log(X1+100)+X2+P  |P|IIV(log(X1+100)), data=dataHetIV, verbose=FALSE))
  expect_silent(hetErrorsIV(y~log(X1+100)+X2+P  |P|IIV(log(X1+100), X2), data=dataHetIV, verbose=FALSE))
})

test_that("Works with endo transformation", {
  expect_warning(hetErrorsIV(y~X1+X2+I(P+1)|I(P+1)|IIV(X1,X2), data=dataHetIV, verbose=FALSE), regexp = "Breusch-Pagan")
})

test_that("Weak instruments trigger warning", {
  expect_warning(hetErrorsIV(y~X1+X2+exp(P)|exp(P)|IIV(X1,X2), data=dataHetIV, verbose=FALSE))
})
