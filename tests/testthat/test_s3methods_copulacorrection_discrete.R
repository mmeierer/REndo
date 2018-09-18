# TEST S3 METHODS  ================================================================================================================================================================

# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopDis")

# Run once to get result ----------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionDiscrete - S3 Methods")
dis.input.form <- y ~ X1 + X2 + P1+P2|P1+P2 #needed as var to compare against
expect_silent(res.dis <- copulaCorrectionDiscrete(formula = dis.input.form, data = dataCopDis))

test.s3methods.multi.endo.lm.models(res.dis, input.form = dis.input.form, function.std.data=dataCopDis, req.df=8)

# Summary and summary s3 methods --------------------------------------------------------------------------------------------------------------------------------------------------
# test_that("summary", {
#   expect_silent(res.sum <- summary(res.dis))
#   expect_s3_class(res.sum, "summary.rendo.copulacorrection.discrete")
# })
#
# test_that("summary formula", {})
#
# test_that("summary coef", {
#   # coef
#   expect_silent(res.sum.coef <- coef(summary(res.dis)))
#   expect_type(res.sum.coef, "double")
#
#   res.rown <- rownames(res.sum.coef)
#   expect_equal(nrow(res.sum.coef), length(coef(res.dis)))
#   expect_equal(res.rown, c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))
#   expect_equal(res.rown, names(coef(res.dis)))
#
#   res.coln <- colnames(res.sum.coef)
#   expect_equal(ncol(res.sum.coef), 4)
#   expect_equal(res.rown, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
#   # Pvalues between 0 and 1
#   expect_true(res.sum.coef[,4]>=0 & res.sum.coef[,4]<1)
# })
#
# test_that("summary vcov", {
#   expect_silent(res.sum.vcov <- vcov(summary(res.dis)))
#   res.attr <- attributes(res.sum.vcov)
#   expect_named(res.attr, c("dim", "dimnames"))
#   expect_length(res.attr$dimnames, 2)
#   expect_equal(res.attr$dim, c(7, 7) )
#   expect_equal(res.attr$dimnames[[1]], names(coef(res.dis)))
#   expect_equal(res.attr$dimnames[[2]], names(coef(res.dis)))
#
#   expect_equal(res.sum.vcov, vcov(res.dis))
# })
#
#
#
#
