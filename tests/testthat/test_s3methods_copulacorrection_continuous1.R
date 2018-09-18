# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopC1")

# Run once to get result ----------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrectionContinuous1 - S3 Methods")
c1.input.form <- y ~ X1 + X2 + P|P # needed as var to compare against
expect_silent(res.c1 <- copulaCorrectionContinuous1(formula = c1.input.form, data = dataCopC1, verbose=F))

test.s3methods.continuous1(res.c1, input.form = c1.input.form, function.std.data=dataCopC1, req.df=7)

