test.optimx.args <- function(function.to.test, parameter.name, formula, function.std.data, additional.args=list()){

  std.args <- modifyList(alist(data=function.std.data, formula=formula), additional.args)

  # Cannot check this in copualCorrection as not part of top-level interface
  # test_that("Has default value empty list()",{
  #   default.arg <- eval(formals(function.to.test)[[parameter.name]])
  #   expect_equal(class(default.arg), "list") # S3 class does not work
  # })

  test_that("Fails if optimx.args is not a list", {
    expect_error(do.call(function.to.test, c(setNames(alist(p=NULL), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=NA), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(itnmax = 5000)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=data.frame(itnmax = 5000)), parameter.name),std.args)), regexp = "The above errors were encountered!")
  })

  test_that("Fails if optimx.args is not named", {
    expect_error(do.call(function.to.test, c(setNames(alist(p=list(5000)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=list(itnmax = 5000, TRUE)), parameter.name),std.args)), regexp = "The above errors were encountered!")
  })


  test_that("Fails if optimx.args contains arguments not in optimx", {
    # Misspelled
    expect_error(do.call(function.to.test, c(setNames(alist(p=list(itmax = 5000)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=list(kktol = 0.01)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=list(itnmax = 5000, kktol = 0.01)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=list(itmax = 5000, kkttol = 0.01)), parameter.name),std.args)), regexp = "The above errors were encountered!")

    # Inexistent
    expect_error(do.call(function.to.test, c(setNames(alist(p=list(a = 12)), parameter.name),std.args)), regexp = "The above errors were encountered!")
  })
}
