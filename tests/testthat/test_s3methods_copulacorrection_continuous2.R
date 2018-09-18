# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopC2")

# Run once to get result ----------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous2 - S3 Methods")
c2.input.form <- y ~ X1 + X2 + P1+P2|P1+P2 # needed as var to compare against
expect_silent(res.c2 <- copulaCorrectionContinuous2(formula = c2.input.form, data = dataCopC2))

test.s3methods.multi.endo.lm.models(res.c2, input.form = c2.input.form, function.std.data=dataCopC2, req.df=8)

