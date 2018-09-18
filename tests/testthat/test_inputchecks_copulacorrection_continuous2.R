# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopC2")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous2 - Parameter formula")
test.formula.multiple.endo(copulaCorrectionContinuous2, function.std.data=dataCopC2)

# data --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous2 - Parameter data")
test.data.multiple.endo(function.to.test = copulaCorrectionContinuous2, function.std.data = dataCopC2, forbidden.col.name="PStar", additional.args = list())
