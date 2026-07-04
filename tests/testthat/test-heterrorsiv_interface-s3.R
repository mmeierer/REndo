# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataHetIV")

# Run once to get result ----------------------------------------------------------------------------------------------------------------------------------------------------------
context("S3methods - hetErrorsIV - S3methods")
hetiv.input.form <- y~ X1+X2+P|P|IIV(X1)+IIV(X2)
expect_warning(res.hetiv <- hetErrorsIV(formula = hetiv.input.form, data = dataHetIV, verbose = FALSE), regexp = "Breusch-Pagan")

test.s3methods.ivreg.models(res.ivreg.model=res.hetiv, input.form=hetiv.input.form, function.std.data=dataHetIV,
                            full.coefs=c("(Intercept)", "X1", "X2", "P"))
