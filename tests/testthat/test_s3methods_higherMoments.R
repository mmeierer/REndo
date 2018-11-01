# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataHigherMoments")

# Run once to get result ----------------------------------------------------------------------------------------------------------------------------------------------------------
context("S3methods - higherMomentsIV - S3methods")
hm.input.form <- y~X1+X2+P|P|IIV(iiv=gp, g=x2, X1, X2)
expect_silent(res.hm <- higherMomentsIV(hm.input.form,data = dataHigherMoments, verbose = FALSE))

test.s3methods.ivreg.models(res.ivreg.model=res.hm, input.form=hm.input.form, function.std.data=dataHigherMoments,
                            full.coefs=c("(Intercept)", "X1", "X2", "P"))
