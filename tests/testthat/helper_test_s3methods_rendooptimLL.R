test.s3methods.rendooptimLL <- function(res.model, input.form, function.std.data, req.df,
                                         full.coefs){
  input.form <- Formula::as.Formula(input.form)
  .test.s3methods.basic.structure(res.model=res.model, input.form=input.form,
                                  function.std.data=function.std.data, full.coefs=full.coefs)

  test_that("logLik", {
    expect_silent(res.loglik <- logLik(res.model))
    expect_s3_class(res.loglik, "logLik")
    res.attr <- attributes(res.loglik)
    expect_named(res.attr, expected = c("nall", "nobs", "df", "class"))
    expect_equal(res.attr$nall, nrow(function.std.data))
    expect_equal(res.attr$nobs, nrow(function.std.data))
    expect_equal(res.attr$df,   req.df)
  })

  test_that("AIC/BIC", {
    expect_silent(res.AIC <- AIC(res.model))
    expect_silent(res.BIC <- BIC(res.model))

    # Check value correct
    nparam <- length(full.coefs)
    # AIC: 2     *npar-2LL
    # BIC: log(n)*npar-2LL
    expect_equal(as.numeric(res.AIC), 2 *                  nparam - 2*as.numeric(logLik(res.model)))
    expect_equal(as.numeric(res.BIC), log(nobs(res.model))*nparam - 2*as.numeric(logLik(res.model)))
  })

  test_that("terms", {
    expect_silent(terms(res.model))
  })

  test_that("case.names", {
    expect_silent(res.cases <- case.names(res.model))
    expect_type(res.cases, "character")
    expect_equal(res.cases, rownames(function.std.data))
    expect_length(res.cases, nrow(function.std.data))
  })

  test_that("labels", {
    expect_silent(res.labels <- labels(res.model))
    expect_type(res.labels, "character")
    message(res.labels)
    expect_setequal(res.labels, intersect(labels(terms(input.form)),
                                          names(coef(res.model))))
  })

  test_that("summary() object structure", {
    expect_silent(res.sum <- summary(res.model))
    expect_is(res.sum, "summary.rendo.optim.LL")
    expect_true(is.list(res.sum))
    expect_named(res.sum, c("call", "start.params", "KKT1", "KKT2", "AIC", "BIC","conv.code",
                            "log.likelihood", "coefficients", "vcov", "estim.params.se",
                            "names.main.coefs"),
                 ignore.order = T)
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

  test_that("summary() - vcov", {
    expect_silent(res.vcov <- vcov(summary(res.model)))
    # Copy from standard vcov
    res.attr <- attributes(res.vcov)
    expect_named(res.attr, c("dim", "dimnames"))
    expect_length(res.attr$dimnames, 2)
    expect_equal(res.attr$dim, c(length(coef(res.model)), length(coef(res.model))) )
    expect_equal(res.attr$dimnames[[1]], names(coef(res.model)))
    expect_equal(res.attr$dimnames[[2]], names(coef(res.model)))
  })

  test_that("summary() - coef", {
    expect_silent(sum.coef <- coef(summary(res.model)))
    # right cols
    expect_true(ncol(sum.coef) == 4)
    expect_true(all(names(sum.coef) != ""))
    # right rows
    expect_true(nrow(sum.coef) == length(coef(res.model)))
    expect_true(all(rownames(sum.coef) == names(coef(res.model))))
  })


  test_that("Printing methods", {
    # Just that they work and return their input

    expect_output(res <- show(res.model))
    expect_null(res)

    expect_output(res <- print(res.model))
    expect_identical(res, res.model)

    # Summary
    expect_silent(res.sum <- summary(res.model))

    expect_output(res <- show(res.sum))
    expect_null(res)

    expect_output(res <- print(res.sum))
    expect_equal(res, res.sum)
  })

}
