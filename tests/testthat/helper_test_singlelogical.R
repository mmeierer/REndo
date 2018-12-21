test.single.logical <- function(function.to.test, parameter.name, formula, function.std.data, additional.args=list()){

  std.args <- modifyList(alist(data=function.std.data, formula=formula), additional.args)

  test_that(paste0(parameter.name, "has default value"), {
    expect_is(eval(formals(function.to.test)[[parameter.name]]), "logical")
  })

  test_that(paste0(parameter.name, "has to be logical"), {
    expect_error(do.call(function.to.test, c(setNames(alist(p="TRUE"), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=0),   parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=1), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=as.factor(TRUE)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=1L), parameter.name),std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " is single value"), {
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(TRUE, TRUE)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(FALSE, TRUE)), parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(FALSE, FALSE)), parameter.name),std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " cannot be NA"), {
    expect_error(do.call(function.to.test, c(setNames(alist(p=NA),       parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=c(TRUE, NA)), parameter.name),std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " cannot be NULL or missing"), {
    expect_error(do.call(function.to.test, c(setNames(alist(p=logical()),  parameter.name),std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p=NULL),       parameter.name),std.args)), regexp = "The above errors were encountered!")
  })
}
