# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

# Run once to get result ----------------------------------------------------------------------------------------------------------------------------------------------------------
context("latentIV - S3 Methods")
latentiv.input.form <- y ~ P # needed as var to compare against
expect_message(res.latent <- latentIV(formula = latentiv.input.form, data = dataLatentIV), regexp = "No start parameters were given")

test.s3methods.rendooptimLL(res.model = res.latent, input.form = latentiv.input.form, function.std.data=dataLatentIV, req.df=9,
                            full.coefs=c("(Intercept)", "P", "pi1", "pi2","theta5","theta6","theta7","theta8"))
