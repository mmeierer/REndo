test.s3methods.lm.models <- function(res.lm.model, input.form, function.std.data, full.coefs){

  .test.s3methods.basic.structure(res.model=res.lm.model, input.form=input.form,
                                  function.std.data=function.std.data, full.coefs=full.coefs)

  test_that("class is lm", {
    expect_s3_class(res.lm.model, "lm")
    expect_s3_class(res.lm.model, "rendo.pstar.lm")
    expect_true(length(class(res.lm.model)) == 2)
  })

  test_that("summary works", {
    expect_silent(res.sum <- summary(res.lm.model))
    expect_s3_class(res.sum, "summary.lm")
    expect_silent(coef(res.sum))
    expect_silent(vcov(res.sum))
  })

  test_that("Confint works as normal", {
    expect_silent(confint(res.lm.model))
  })

  test_that("logLik", {
    expect_silent(res.loglik <- logLik(res.lm.model))
    expect_s3_class(res.loglik, "logLik")
    res.attr <- attributes(res.loglik)
    expect_named(res.attr, expected = c("nall", "nobs", "df", "class"))
    expect_equal(res.attr$nall, nrow(function.std.data))
    expect_equal(res.attr$nobs, nrow(function.std.data))
    expect_equal(res.attr$df,   1+length(full.coefs))
  })

  test_that("AIC/BIC", {
    expect_silent(AIC(res.lm.model))
    expect_silent(BIC(res.lm.model))
  })
}
