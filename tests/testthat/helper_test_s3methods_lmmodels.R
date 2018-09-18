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


  # test_that("confint", {
  #   expect_silent(res.confi <- confint(res.lm.model))
  #
  #   # Level works and provides different values
  #   res.confi.90 <- confint(res.lm.model, level = 0.9)
  #   res.confi.99 <- confint(res.lm.model, level = 0.99)
  #   expect_false(all.equal(res.confi,res.confi.90,check.attributes=F))
  #   expect_false(all.equal(res.confi,res.confi.99,check.attributes=F))
  #
  #   # Rightly named, all same
  #   expect_equal(rownames(res.confi), names(coef(res.lm.model)))
  #   expect_equal(rownames(res.confi), rownames(res.confi.90))
  #   expect_equal(rownames(res.confi), rownames(res.confi.99))
  #
  #   # param works
  #   # With names
  #   expect_silent(res.confi.X1   <- confint(res.lm.model, parm = "X1")) # Single
  #   expect_silent(res.confi.X1X2 <- confint(res.lm.model, parm = c("X1", "X2"))) # vector
  #   expect_silent(res.confi.P1   <- confint(res.lm.model, parm = "P1")) # for endo
  #   expect_silent(res.confi.P1.P2.X1   <- confint(res.lm.model, parm = c("X1","P1", "P2"))) # multi endo
  #   expect_silent(res.confi.PStar1.PStar2   <- confint(res.lm.model, parm = c("PStar.P1","PStar.P2"))) # pstar
  #   expect_equal(rownames(res.confi.X1),       names(coef(res.lm.model)["X1"]))
  #   expect_equal(rownames(res.confi.X1X2),     names(coef(res.lm.model)[c("X1", "X2")]))
  #   expect_equal(rownames(res.confi.P1),       names(coef(res.lm.model)["P1"]))
  #   expect_equal(rownames(res.confi.P1.P2.X1), names(coef(res.lm.model)[c("X1","P1", "P2")]))
  #   expect_equal(rownames(res.confi.PStar1.PStar2), names(coef(res.lm.model)[c("PStar.P1","PStar.P2")]))
  #
  #   # With indices
  #   # y ~ X1 + X2 + P1+P2|P1+P2 -> I + X1 + X2 + P1+P2 + PStar.P1 + PStar.P2
  #   expect_silent(res.confi.X1   <- confint(res.lm.model, parm = 2)) # Single
  #   expect_silent(res.confi.X1X2 <- confint(res.lm.model, parm = c(2,3))) # vector
  #   expect_silent(res.confi.P1   <- confint(res.lm.model, parm = 4)) # for endo
  #   expect_silent(res.confi.P1.P2.X1   <- confint(res.lm.model, parm = c("X1","P1", "P2"))) # multi endo
  #   expect_silent(res.confi.PStar1.PStar2   <- confint(res.lm.model, parm = 6:7)) # pstar
  #   expect_equal(rownames(res.confi.X1),       names(coef(res.lm.model)["X1"]))
  #   expect_equal(rownames(res.confi.X1X2),     names(coef(res.lm.model)[c("X1", "X2")]))
  #   expect_equal(rownames(res.confi.P1),       names(coef(res.lm.model)["P1"]))
  #   expect_equal(rownames(res.confi.P1.P2.X1), names(coef(res.lm.model)[c("X1","P1", "P2")]))
  #   expect_equal(rownames(res.confi.PStar1.PStar2), names(coef(res.lm.model)[c("PStar.P1","PStar.P2")]))
  # })


}
