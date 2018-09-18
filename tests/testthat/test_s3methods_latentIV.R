# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

# Run once to get result ----------------------------------------------------------------------------------------------------------------------------------------------------------
context("latentIV - S3 Methods")
latentiv.input.form <- y ~ P # needed as var to compare against
expect_warning(res.latent <- latentIV(formula = latentiv.input.form, data = dataLatentIV))

test.s3methods.latent(res.lm.model=res.latent, input.form = latentiv.input.form, function.std.data=dataLatentIV, req.df=9)

