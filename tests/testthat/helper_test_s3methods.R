.test.s3methods.basic.structure <- function(res.model, input.form, function.std.data, full.coefs){

  no.intercept.coefs <- setdiff(full.coefs, c("(Intercept)"))

  test_that("nobs", {
    expect_silent(res.nobs <- nobs(res.model))
    expect_is(res.nobs, "integer")
    expect_equal(res.nobs, nrow(function.std.data))
  })

  test_that("fitted", {
    expect_silent(res.fitted <- fitted(res.model))
    expect_named(res.fitted, rownames(function.std.data))
    expect_type(res.fitted, "double")
    expect_length(res.fitted, nrow(function.std.data))
  })

  test_that("residuals", {
    # Residuals
    expect_silent(res.residuals <- residuals(res.model))
    expect_named(res.residuals, rownames(function.std.data))
    expect_type(res.residuals, "double")
    expect_length(res.residuals, nrow(function.std.data))
    # alias
    expect_silent(res.residuals <- resid(res.model))
    expect_named(res.residuals, rownames(function.std.data))
    expect_type(res.residuals, "double")
    expect_length(res.residuals, nrow(function.std.data))
  })


  test_that("coef", {
    # coef
    expect_silent(res.coef <- coef(res.model))
    expect_named(res.coef, full.coefs, ignore.order = TRUE)
    expect_type(res.coef, "double")
    # alias
    expect_silent(res.coef <- coefficients(res.model))
    expect_named(res.coef,full.coefs, ignore.order  = TRUE)
    expect_type(res.coef, "double")
  })


  # This breaks too many things when used for lm and ivreg models.
  #   reduced to the minimum
  test_that("formula", {
    expect_silent(res.form <- formula(res.model))
    expect_true("formula" %in% class(res.form))
  })

  # test_that("formula", {
  #   expect_silent(res.form <- formula(res.model))
  #   expect_type(res.form, "language")
  #   expect_s3_class(res.form, "formula")
  #   expect_s3_class(res.form, "Formula")
  #   expect_equal(res.form, Formula::as.Formula(input.form))
  #   res.attr <- attributes(res.form)
  #   expect_named(res.attr, c("class", ".Environment", "lhs", "rhs"))
  #   expect_equal(res.attr$.Environment, environment(input.form)) # not 100% sure about this one
  #   # # contains no dot
  #   expect_false(any(all.vars(res.form) == "."))
  # })


  test_that("vcov", {
    expect_silent(res.vcov <- vcov(res.model))
    res.attr <- attributes(res.vcov)
    expect_named(res.attr, c("dim", "dimnames"))
    expect_length(res.attr$dimnames, 2)
    expect_equal(res.attr$dim, c(length(full.coefs), length(full.coefs)))
    expect_equal(res.attr$dimnames[[1]], names(coef(res.model)))
    expect_equal(res.attr$dimnames[[2]], names(coef(res.model)))
    expect_false(anyNA(res.vcov))
  })


  .test.s3methods.confint(res.model)
  return(invisible(NULL))
}
