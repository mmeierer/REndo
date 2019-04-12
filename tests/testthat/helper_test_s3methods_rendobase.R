.test.s3methods.rendobase <- function(res.model, input.form, function.std.data, req.df,
                                      full.coefs){
  input.form <- Formula::as.Formula(input.form)
  .test.s3methods.basic.structure(res.model=res.model, input.form=input.form,
                                  function.std.data=function.std.data, full.coefs=full.coefs)

  test_that("object structure", {
    expect_true(all(c("call", "formula", "terms", "coefficients",
                      "names.main.coefs", "fitted.values", "residuals") %in%
                      names(res.model)))
    expect_s3_class(res.model, "rendo.base")
    expect_type(res.model$call, "language")
    expect_s3_class(res.model$formula, "Formula")
    expect_s3_class(res.model$model, "data.frame")
    expect_s3_class(res.model$terms, "terms")
    expect_type(res.model$names.main.coefs, "character")
    expect_type(res.model$fitted.values, "double")
    expect_type(res.model$residuals, "double")
  })

  test_that("coef has default = TRUE", {
    expect_equal(eval(formals(REndo:::coef.rendo.base)[["complete"]]), TRUE)
  })

  test_that("coef with complete=TRUE", {
    expect_silent(res.cf <- coef(res.model, complete = TRUE))
    # equal to all parameters
    expect_equal(res.cf, expected = res.model$coefficients)
  })

  test_that("coef with complete=FALSE", {
    expect_silent(res.cf <- coef(res.model, complete = FALSE))
    # equal to main parameters only
    expect_equal(res.cf, expected = res.model$coefficients[res.model$names.main.coefs])
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
    expect_true(all(colnames(sum.coef) != ""))
    # right rows
    expect_true(nrow(sum.coef) == length(coef(res.model)))
    expect_true(all(rownames(sum.coef) == names(coef(res.model))))
  })


  test_that("Printing methods work", {
    # Just that they work and return their input

    expect_output(res <- show(res.model), regexp = "Coefficients")
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
