context("General runability")

# test_that("Can run Copula Correction Discrete", {
#   expect_silent(data(dataCopDis))
#   expect_silent(copulaCorrectionDiscrete(formula = y ~ X1 + X2 + P|P, data = dataCopDis))
# })
#
# test_that("Can run Copula Correction Continuous 1", {
#   expect_silent(data(dataCopC1))
#   expect_silent(copulaCorrectionContinuous1(formula = y ~ X1 + X2 + P+1 | P, data=dataCopC1, num.boots = 10))
#   # expect_(summary(c1))
# })
#
# test_that("Can run Copula Correction Continuous 2", {
#   expect_success(data("dataCopC2"))
#   expect_success(copulaCorrectionContinuous2(formula = y ~ X1 + X2 + P1 + P2|P1+P2,data=dataCopC2))
# })
#
# test_that("Can run hetErrorsIV", {
#   expect_success(data("dataHetIV"))
#   expect_success(hetErrorsIV(y ~ P| X1 + X2 | X1 + X2, data = dataHetIV))
# })
#
# test_that("Can run latentIV", {
#   expect_success(data("dataLatentIV"))
#   expect_success(latentIV(y ~ P-1, data = dataLatentIV))
# })
