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


  # Confint ---------------------------------------------------------------------------------------

  test_that("Confint works with different alphas", {
    expect_silent(ci.99 <- confint(res.model, level = 0.99))
    expect_silent(ci.95 <- confint(res.model, level = 0.95))
    expect_silent(ci.90 <- confint(res.model, level = 0.90))
    expect_silent(ci.70 <- confint(res.model, level = 0.70))

    # Level works and provides different values
    expect_false(isTRUE(all.equal(ci.99,ci.95,check.attributes=F)))
    expect_false(isTRUE(all.equal(ci.95,ci.90,check.attributes=F)))
    expect_false(isTRUE(all.equal(ci.90,ci.70,check.attributes=F)))

    # Rightly named, all same
    expect_equal(rownames(ci.99), names(coef(res.model)))
    expect_equal(rownames(ci.99), rownames(ci.95))
    expect_equal(rownames(ci.95), rownames(ci.90))
    expect_equal(rownames(ci.90), rownames(ci.70))

    # Also title label correct
    expect_equal(colnames(ci.95), c("2.5 %", "97.5 %"))
    expect_equal(colnames(ci.90), c("5 %", "95 %"))
    expect_equal(colnames(ci.99), c("0.5 %", "99.5 %"))

    # CI have to be larger for higher level (with very high probability)
    # expect_gt(ci.99[, 2] - ci.99[, 2], ) ***maybe do as well if time
  })

  test_that("Confint works with character param", {
    # Single
    for(p in names(coef(res.model)))
      expect_equal(rownames(confint(res.model,num.simulations=10, parm = p)), expected = p)
    # Multiple
    p <- names(coef(res.model))[1:3]
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = p)), expected = p)
    p <- names(coef(res.model))[1:2]
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = p)), expected = p)
    # All - excplicitely
    p <- names(coef(res.model))
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = p)), expected = p)
    # All - implicitely (ie none given)
    expect_equal(rownames(confint(res.model)), expected = p)
  })

  test_that("Confint works with integer param", {
    p <- names(coef(res.model))
    # Single
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = 2)), expected = p[2])
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = 4)), expected = p[4])
    # Sequence
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = 1:3)), expected = p[1:3])
    expect_equal(rownames(confint(res.model,num.simulations=10, parm =c(1,2,4))), expected = p[c(1,2,4)])
    # All - excplicitely
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = seq(length(p)))), expected = p)
    # All - implicitely (ie none given)
    expect_equal(rownames(confint(res.model)), expected = p)

    # Minus removes
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = -2)), expected = p[-2])
    expect_equal(rownames(confint(res.model,num.simulations=10, parm = -c(2,4))), expected = p[-c(2,4)])
    # Remove all
    expect_null(rownames(confint(res.model,num.simulations=10, parm = -seq(length(p)))))
  })
  # same behavior as lm
  test_that("confint NA if unknown parm", {
    # Unknown character
    expect_true(all(is.na( confint(res.model,num.simulations=10, parm = "abc") )))
    expect_true(all(is.na( confint(res.model,num.simulations=10, parm = c("abc", "zcgd")) )))
    # Wrong indices
    expect_true(all(is.na( confint(res.model,num.simulations=10, parm = 50:100))))
    expect_true(all(is.na( confint(res.model,num.simulations=10, parm = 99) )))
    # Part of it are known
    expect_true(all(names(coef(res.model)) %in%
                      rownames(confint(res.model,num.simulations=10, parm = 1:100))))
    expect_true(!all(is.na( confint(res.model,num.simulations=10, parm = 1:100))))
  })


}
