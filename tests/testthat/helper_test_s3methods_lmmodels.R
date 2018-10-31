test.s3methods.continuous1<- function(res.c1.model, input.form, function.std.data, req.df){
  .test.s3methods.lm.models(res.lm.model = res.c1.model, input.form = input.form,
                              function.std.data=function.std.data, req.df = req.df,
                              full.coefs = c("(Intercept)", "X1", "X2", "P", "rho","sigma"))
}

test.s3methods.latent <- function(res.lm.model, input.form, function.std.data, req.df){
  .test.s3methods.lm.models(res.lm.model = res.lm.model, input.form = input.form, function.std.data=function.std.data,
                            req.df = req.df, full.coefs = c("(Intercept)", "P"))
}

test.s3methods.multi.endo.lm.models <- function(res.lm.model, input.form, function.std.data, req.df){
  .test.s3methods.lm.models(res.lm.model = res.lm.model, input.form = input.form, function.std.data=function.std.data,
                           req.df = req.df, full.coefs = c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))
}

test.s3methods.single.endo.lm.models <- function(res.lm.model, input.form, function.std.data, req.df){
  .test.s3methods.lm.models(res.lm.model = res.lm.model, input.form = input.form, function.std.data=function.std.data,
                           req.df = req.df, full.coefs = c("(Intercept)", "X1", "X2", "P", "PStar.P"))
}


.test.s3methods.lm.models <- function(res.lm.model, input.form, function.std.data, req.df, full.coefs){

  .test.s3methods.basic.structure(res.model=res.lm.model, input.form=input.form,
                                  function.std.data=function.std.data, full.coefs=full.coefs)

  test_that("logLik", {
    expect_silent(res.loglik <- logLik(res.lm.model))
    expect_s3_class(res.loglik, "logLik")
    res.attr <- attributes(res.loglik)
    expect_named(res.attr, expected = c("nall", "nobs", "df", "class"))
    expect_equal(res.attr$nall, nrow(function.std.data))
    expect_equal(res.attr$nobs, nrow(function.std.data))
    expect_equal(res.attr$df,   req.df)
  })

  test_that("AIC/BIC", {
    expect_silent(AIC(res.lm.model))
    expect_silent(BIC(res.lm.model))
  })
}
