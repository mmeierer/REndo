# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

context("S3methods - latentIV - S3methods")

# Run once to get result
latentiv.input.form <- y ~ P # needed as var to compare against
expect_message(res.latent <- latentIV(formula = latentiv.input.form, data = dataLatentIV), regexp = "No start parameters were given")
# Test like all renodoptimLL functions
test.s3methods.rendooptimLL(res.model = res.latent, input.form = latentiv.input.form, function.std.data=dataLatentIV,
                            req.df=8,
                            full.coefs=c("(Intercept)", "P", "pi1", "pi2","theta5","theta6","theta7","theta8"))
