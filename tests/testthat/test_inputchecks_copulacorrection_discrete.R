# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopDis")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionDiscrete - Parameter formula")
test.formula.multiple.endo(copulaCorrectionDiscrete, function.std.data=dataCopDis, additional.args = list(num.simulations=100))


# data ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionDiscrete - Parameter data")
test.data.multiple.endo(function.to.test = copulaCorrectionDiscrete, function.std.data = dataCopDis, forbidden.col.name="PStar", additional.args = list(num.simulations=100))

# num.simulations -------------------------------------------------------------------------------------------------------------------------------------------------------------------
test.positive.numeric.whole.number(function.to.test = copulaCorrectionDiscrete, parameter.name="num.simulations", formula=y~X1+X2+P|P, function.std.data=dataCopDis)

test_that("Warn if to few num.simulations", {
  expect_warning(copulaCorrectionDiscrete(num.simulations = 99,  formula = y ~ X1 + X2 + P1+P2|P1+P2, data = dataCopDis))
})

test_that("Fail if num.simulations not > 1", {
  # everything < 1 is checked already in test.positive.numeric.whole.number
  expect_error(copulaCorrectionDiscrete(num.simulations = 1,     formula = y ~ X1 + X2 + P1+P2|P1+P2, data = dataCopDis))
})
