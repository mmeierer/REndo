# TEST INPUT CHECKS ================================================================================================================================================================
# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopC1")

# formula ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous1 - Parameter formula")
test.formula.single.endo(function.to.test = copulaCorrectionContinuous1, function.std.data = dataCopC1,
                         additional.args =list(verbose=F, start.params = c("(Intercept)"=1.983, X1=1.507, X2=-3.014, P=-0.891)))

test_that("Fails if more than 1 endogenous is given", {
  expect_error(copulaCorrectionContinuous1(formula =y ~ X1 + X2 + P | P + X1 , data = dataCopC1))
})

# data -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous1 - Parameter data")
test.data.single.endo(function.to.test = copulaCorrectionContinuous1, function.std.data = dataCopC1, forbidden.col.name = "PStar",
                      additional.args =list(verbose=F, start.params = c("(Intercept)"=1.983, X1=1.507, X2=-3.014, P=-0.891)))


# num.boots --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous1 - Parameter num.boots")
# Start params required for silence
test.positive.numeric.whole.number(function.to.test = copulaCorrectionContinuous1, parameter.name="num.boots", formula=y~X1+X2+P|P, function.std.data=dataCopC1,
                                   additional.args =list(verbose=F, start.params = c("(Intercept)"=1.983, X1=1.507, X2=-3.014, P=-0.891)))

test_that("Warning if too few bootstraps (<10)", {
  expect_warning(copulaCorrectionContinuous1(num.boots = 9, formula = y ~ X1 + X2 + P|P, data = dataCopC1))
})

# verbose ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous1 - Parameter verbose")
test.single.logical(function.to.test = copulaCorrectionContinuous1, parameter.name="verbose", formula=y~X1+X2+P|P, function.std.data=dataCopC1,
                    additional.args =list(start.params = c("(Intercept)"=1.983, X1=1.507, X2=-3.014, P=-0.891)))


# # start.params ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous1 - Parameter start.params")

test_that("start.params is vector and all numeric", {
  # Any parameter is character, logical, factor, matrix
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = as.character(1), X2 = -2, P = 0),
                                           formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = as.factor(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = as.logical(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = as.matrix(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = complex(1,4,2), X2 = -2, P = 0),
                                           formula = y ~ X1 + X2 + P|P, data = dataCopC1))
})

test_that("start.params is not NA",{
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = NA_integer_, X2 = -2, P = 0),    formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = NA_real_, X2 = -2, P = 0),       formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = NA_integer_, formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = NA_real_, formula = y ~ X1 + X2 + P|P, data = dataCopC1))
})

test_that("start.params is NULL or missing but runs with warning", {
  expect_warning(copulaCorrectionContinuous1(start.params =     , formula = y ~ X1 + X2 + P |P, data = dataCopC1, num.boots = 2, verbose=F))
  expect_warning(copulaCorrectionContinuous1(start.params = NULL, formula = y ~ X1 + X2 + P |P, data = dataCopC1, num.boots = 2, verbose=F))
})

test_that("start.params is named correctly", {
  # Unnamed vec given
  expect_error(copulaCorrectionContinuous1(start.params = c(2, 1, -2,0),                                formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  # Partially named vec given
  expect_error(copulaCorrectionContinuous1(start.params = c(2, X1 = 1, X2=-2,P=0),                      formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  # Wrong case
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, x1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  # Same param name twice
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X2 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  # Unrelated name
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X10 = 1, X2 = -2, P = 0),  formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  # Param missing (main, endo, intercept)
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, P = 0),            formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2),          formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = c(X1 = 1, X2 = -2, P = 0),                    formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  # Intercept spelling
  expect_error(copulaCorrectionContinuous1(start.params = c("Intercept"=2, X1 = 1, X2 = -2, P = 0),     formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  expect_error(copulaCorrectionContinuous1(start.params = c("(intercept)"=2, X1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|P, data = dataCopC1))
  # Intercept given but none required
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0),
                                           formula = y ~ X1 + X2 + P -1 |P, data = dataCopC1))
  # Additional, not required param given
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, X3 = 99, P = 0), formula = y ~ X1 + X2 + P |P, data = dataCopC1))
})


test_that("start.params contains no parameter rho or sigma", {
  # Given additionally
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0, rho=1), formula = y ~ X1 + X2 + P |P, data = dataCopC1, verbose=F))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0, sigma=1), formula = y ~ X1 + X2 + P |P, data = dataCopC1, verbose=F))
  # Given instead of other parameters
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, rho=1), formula = y ~ X1 + X2 + P |P, data = dataCopC1, verbose=F))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, sigma=1), formula = y ~ X1 + X2 + P |P, data = dataCopC1, verbose=F))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, sigma = -2, P=1), formula = y ~ X1 + X2 + P |P, data = dataCopC1, verbose=F))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, rho = -2, P=1), formula = y ~ X1 + X2 + P |P, data = dataCopC1, verbose=F))
  expect_error(copulaCorrectionContinuous1(start.params = c("(Intercept)"=2, X1 = 1, sigma = -2, rho=1), formula = y ~ X1 + X2 + P |P, data = dataCopC1, verbose=F))
})
