test.s3methods.ivreg.models <- function(res.ivreg.model, input.form, function.std.data, full.coefs){


  # Check that the object has actually added rendo.ivreg and check that it is at first position
  test_that("Class of return object is correct",{
    expect_s3_class(res.ivreg.model, "rendo.ivreg")
    expect_s3_class(res.ivreg.model, "ivreg")
    expect_true(length(class(res.ivreg.model)) == 2)
    # correct order
    expect_true(all(class(res.ivreg.model) == c("rendo.ivreg", "ivreg")))
  })

  # Summary result. Class and content
  test_that("Summary works", {
    expect_silent(res.sum <- summary(res.ivreg.model))
    expect_s3_class(res.sum, "summary.ivreg")
    expect_silent(coef(res.sum))
  })
}
