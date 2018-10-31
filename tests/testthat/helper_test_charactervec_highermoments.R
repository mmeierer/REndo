test.character.vector.basicstructure <- function(function.to.test, parameter.name, formula, function.std.data, additional.args=list()){

  std.args <- modifyList(alist(data=function.std.data, formula=formula), additional.args)

  test_that(paste0(parameter.name, " has no default value"),{
    expect_null(eval(formals(function.to.test)[[parameter.name]]))
  })

  test_that(paste0(parameter.name, " fails if is missing or NULL"),{
    expect_error(do.call(function.to.test, c(setNames(alist(p= ), parameter.name), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p= NULL), parameter.name), std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " fails if is not character vector"),{
    # Not vector
    expect_error(do.call(function.to.test, c(setNames(alist(p= list("1","2","3")), parameter.name), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p= data.frame("1","2","3", stringsAsFactors = FALSE)), parameter.name), std.args)), regexp = "The above errors were encountered!")

    # Not character
    expect_error(do.call(function.to.test, c(setNames(alist(p= c(1,2,3)), parameter.name), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p= as.logical(c(1,2,3))), parameter.name), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p= as.factor(c(1,2,3))), parameter.name), std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " fails if contains NA"),{
    expect_error(do.call(function.to.test, c(setNames(alist(p= NA_character_), parameter.name), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(setNames(alist(p= c("123", NA_character_)), parameter.name), std.args)), regexp = "The above errors were encountered!")
  })

  test_that(paste0(parameter.name, " fails no elements"),{
    expect_error(do.call(function.to.test, c(setNames(alist(p= character()), parameter.name), std.args)), regexp = "The above errors were encountered!")
  })

}
