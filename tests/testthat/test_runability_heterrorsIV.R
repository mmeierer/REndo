# TEST RUNABILITY ====================================================================================================================

context("hetErrorsIV - Runability")

# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHetIV")


test_that("Works with intercept", {
  expect_silent(hetErrorsIV(y~X1+X2+P|P|X1, data=dataHetIV, verbose=F))
})

test_that("Works without intercept", {
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|X1, data=dataHetIV, verbose=F))
})


test_that("Works with single exo", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|X1, data=dataHetIV, verbose=F))
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|X1, data=dataHetIV, verbose=F))
})

test_that("Works with two exo", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|X1+X2, data=dataHetIV, verbose=F))
  expect_silent(hetErrorsIV(y~X1+X2+P-1|P|X1+X2, data=dataHetIV, verbose=F))
})

test_that("Works with external IV", {
  expect_silent(hetErrorsIV(y~X1+X2+P  |P|X1 | X2, data=dataHetIV, verbose=F))
  expect_silent(hetErrorsIV(y~X1   +P  |P|X1 | X2, data=dataHetIV, verbose=F))
})

test_that("Non-numerics can be used in exogenous data", {
  expect_silent(hetErrorsIV(y~X1+X2+P+color |P|X1, verbose=F,
                            data=cbind(dataHetIV, color=factor(x = c("red", "green", "blue", "white", "yellow")))))
  expect_silent(hetErrorsIV(y~X1+X2+P+color-1 |P|X1, verbose=F,
                            data=cbind(dataHetIV, color=factor(x = c("red", "green", "blue", "white", "yellow")))))
})

# Formula transformations ------------------------------------------------------------------------------------------------------------
context("hetErrors - Formula transformations")
test_that("Works with transformation in exogenous", {})

test_that("Works with single endo transformation", {})

test_that("Works with transformed and untransformed endo", {})

test_that("Works with all endo transformed", {})

test_that("Transformations are correct", {})
