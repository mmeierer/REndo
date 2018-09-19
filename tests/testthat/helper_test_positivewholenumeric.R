test.positive.numeric.whole.number <- function(function.to.test, parameter.name, formula, function.std.data, additional.args=list()){

  std.args <- modifyList(alist(data=function.std.data, formula=formula), additional.args)

  # test_that(paste0(parameter.name, " has default value"),{
  #   expect_is(eval(formals(function.to.test)[[parameter.name]]), c("numeric","integer"))
  # })

  test_that(paste0(parameter.name, " is single number"),{
    # Fail for non-numbers
    expect_error(do.call(function.to.test, c(setNames(alist(p=complex(1,4,2)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p="0"),            parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=as.factor(250)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=as.logical(1)),  parameter.name),std.args)), regexp = "The above errors were encountered!")
    # Fail for > 1 parameter
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(150, 250)),    parameter.name),std.args)), regexp = "The above errors were encountered!")
  })


  test_that(paste0(parameter.name, " is whole number"),{
    # Whole number
    expect_error(do.call(function.to.test, c(setNames(alist(p=500.5),           parameter.name),std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " is not NULL or missing"),{
    expect_error(do.call(function.to.test, c(setNames(alist(p=numeric()), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=integer()), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=NULL),      parameter.name),std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " is not NA"),{
    expect_error(do.call(function.to.test, c(setNames(alist(p=NA_integer_),    parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=NA_real_),       parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(10,NA_real_)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(NA_real_,10)), parameter.name),std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " is > 0"),{
    # Invalid number of bootstraps
    expect_error(do.call(function.to.test, c(setNames(alist(p=-1),   parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=-10),  parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=0),    parameter.name),std.args)), regexp = "The above errors were encountered!")
  })
}
