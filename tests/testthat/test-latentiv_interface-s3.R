# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

context("S3methods - latentIV - S3methods")

# Run once to get result
latentiv.input.form <- y ~ P # needed as var to compare against
expect_message(res.latent <- latentIV(formula = latentiv.input.form, data = dataLatentIV), regexp = "No start parameters were given")


# Test base structure ------------------------------------------------------------------------------------------------
# should inherit from rendo.base
full.coefs <- c("(Intercept)", "P", "pi1", "pi2","theta5","theta6","theta7","theta8")
req.df <- 8
.test.s3methods.rendobase(res.model=res.latent, req.df = req.df, input.form=latentiv.input.form,function.std.data=dataLatentIV,
                          full.coefs=full.coefs)


# Test latent IV specific --------------------------------------------------------------------------------------------
# No longer a rendo.optim object, therefore no shared function. all testing code here
#   largely taken from test.s3methods.rendooptimL

test_that("logLik", {
  expect_silent(res.loglik <- logLik(res.latent))
  expect_s3_class(res.loglik, "logLik")
  res.attr <- attributes(res.loglik)
  expect_named(res.attr, expected = c("nall", "nobs", "df", "class"))
  expect_equal(res.attr$nall, nrow(dataLatentIV))
  expect_equal(res.attr$nobs, nrow(dataLatentIV))
  expect_equal(res.attr$df,   req.df)
})

test_that("AIC/BIC", {
  expect_silent(res.AIC <- AIC(res.latent))
  expect_silent(res.BIC <- BIC(res.latent))

  # Check value correct
  nparam <- length(full.coefs)
  # AIC: 2     *npar-2LL
  # BIC: log(n)*npar-2LL
  expect_equal(as.numeric(res.AIC), 2 *                  nparam - 2*as.numeric(logLik(res.latent)))
  expect_equal(as.numeric(res.BIC), log(nobs(res.latent))*nparam - 2*as.numeric(logLik(res.latent)))
})



test_that("latentIV summary() object structure", {
  expect_silent(res.sum <- summary(res.latent))
  expect_is(res.sum, "summary.rendo.latent.IV")
  expect_true(is.list(res.sum))
  expect_named(res.sum, c("call", "start.params", "KKT1", "KKT2", "AIC", "BIC","conv.code",
                          "log.likelihood", "coefficients", "vcov",
                          "names.main.coefs"),
               ignore.order = TRUE)
  expect_is(res.sum$call, "call")
  expect_is(res.sum$start.params, "numeric")
  expect_is(res.sum$KKT1, "logical")
  expect_is(res.sum$KKT2, "logical")
  expect_is(res.sum$AIC, "numeric")
  expect_is(res.sum$BIC, "numeric")
  expect_is(res.sum$log.likelihood, "numeric")
  expect_is(res.sum$conv.code, "numeric")
  expect_true(length(res.sum$start.params) == nrow(res.sum$coefficients))
})


test_that("Printing methods", {
  # Just that they work and return their input

  expect_output(res <- show(res.latent))
  expect_null(res)

  expect_output(res <- print(res.latent))
  expect_identical(res, res.latent)

  # Summary
  expect_silent(res.sum <- summary(res.latent))

  expect_output(res <- show(res.sum))
  expect_null(res)

  expect_output(res <- print(res.sum))
  expect_equal(res, res.sum)
})
