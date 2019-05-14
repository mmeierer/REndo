test.s3methods.rendoboots <- function(res.model, input.form, function.std.data, req.df,
                                        full.coefs){

  input.form <- Formula::as.Formula(input.form)

  # Rendo base part (because inherit from it)
  .test.s3methods.rendobase(res.model=res.model, input.form=input.form, function.std.data=function.std.data,
                            req.df=req.df,full.coefs=full.coefs)


  # rendo.boots specific parts

  test_that("confint stops if too many NAs in one parameter", {
    boots.params.bck <- res.model$boots.params

    # All NAs fails
    num.boots.used <- ncol(res.model$boots.params)
    res.model$boots.params[1, ] <- rep(x = NA_real_, times=num.boots.used)
    expect_error(confint(res.model), regexp = "Not enough bootstraps")
    res.model$boots.params <- boots.params.bck # write back needed again

    # 85% NAs fails for 0.95 level
    res.model$boots.params[1,seq(num.boots.used*0.85)] <- rep(NA_real_, num.boots.used*0.85)
    expect_error(confint(res.model, level = 0.95), regexp = "Not enough bootstraps")
    res.model$boots.params <- boots.params.bck # write back if needed later on

    # 10% NAs works with warning
    res.model$boots.params[1,seq(num.boots.used*0.1)] <- rep(NA_real_, num.boots.used*0.1)
    expect_warning(confint(res.model), regexp = "Confidence intervals might be unreliable")
    #   summary also works but with warning
    expect_warning(summary(res.model), regexp = "Confidence intervals might be unreliable")
    res.model$boots.params <- boots.params.bck # write back if needed later on
  })


  test_that("rendo.boots object structure", {
    expect_is(res.model, "rendo.boots")
    expect_true(is.list(res.model))
    expect_true("boots.params" %in% names(res.model))
  })


  test_that("summary() object structure, boots part", {
    expect_silent(res.sum <- summary(res.model))
    expect_is(res.sum, "summary.rendo.boots")
    expect_true(is.list(res.sum))
    expect_true(all(c("call", "names.main.coefs", "coefficients", "num.boots", "vcov") %in%
                      names(res.sum)))
    expect_is(res.sum$call, "call")
    expect_is(res.sum$names.main.coefs, "character")
    expect_is(res.sum$num.boots, "integer")
    expect_is(res.sum$coefficients, "matrix")
    expect_is(res.sum$vcov, "matrix")
  })


  test_that("boots summary() coef structure", {
    expect_silent(res.sum <- summary(res.model))
    expect_silent(res.cf  <- coef(res.sum))

    expect_true(all( c("Point Estimate", "Boots SE", "Lower Boots CI (95%)", "Upper Boots CI (95%)")
                     %in% colnames(res.cf)))
    expect_true(all(rownames(res.cf) == names(coef(res.model))))

    if(res.sum$num.boots >=100)
      expect_false(anyNA(coef(res.sum)))
    else
      expect_true(anyNA(coef(res.sum)))
  })


  test_that("boots summary() prints", {
    expect_silent(res.sum <- summary(res.model))

    expect_output(show(res.sum))
    expect_output(res.print <- print(res.sum))
    expect_equal(res.print, res.sum)
  })


}
