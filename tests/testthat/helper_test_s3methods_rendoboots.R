test.s3methods.rendoboots <- function(res.model, input.form, function.std.data, req.df,
                                        full.coefs){

  input.form <- Formula::as.Formula(input.form)

  # Rendo base part (because inherit from it)
  .test.s3methods.rendobase(res.model=res.model, input.form=input.form, function.std.data=function.std.data,
                            req.df=req.df,full.coefs=full.coefs)


  # rendo.boots specific parts

  test_that("rendo.boots object structure", {
    expect_is(res.model, "rendo.boots")
    expect_true(is.list(res.model))
    expect_true("boots.params" %in% names(res.model))
  })


  test_that("summary() object structure, boots part", {
    expect_silent(res.sum <- summary(res.model))
    expect_is(res.sum, "summary.rendo.boots")
    expect_true(is.list(res.sum))
    expect_true(all(c("call", "coefficients", "vcov", "names.main.coefs") %in% names(res.sum)))
    expect_is(res.sum$call, "call")
  })




}
