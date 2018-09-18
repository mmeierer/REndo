# Tests the data parameter of a function that takes formulas of structure: y ~ X1 + X2 + P | P
test.data.single.endo <- function(function.to.test, function.std.data, forbidden.col.name, additional.args=list()){

  std.args <- modifyList(alist(formula=y ~ X1 + X2 + P|P), additional.args)

  test_that("Fail if is NA, NULL or missing", {
    expect_error(do.call(function.to.test, c(alist(data=), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=NULL), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=NA_real_), std.args)), regexp = "The above errors were encountered!")
  })

  test_that("Fail if not data.frame", {
    expect_error(do.call(function.to.test, c(alist(data=   c(y=1:10, X1=1:10, X2=1:10, P=1:10)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=list(y=1:10, X1=1:10, X2=1:10, P=1:10)), std.args)), regexp = "The above errors were encountered!")
  })

  test_that("Fail if no rows or cols",{
    expect_error(do.call(function.to.test, c(alist(data= data.frame()), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data= data.frame(y=integer(), X1=numeric(), X2=numeric(), P=integer())), std.args)), regexp = "The above errors were encountered!")
  })

  test_that("Fail if wrong data type in any of the formula parts", {
    # Only allow numericals in all relevant columns
    # Factor
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=factor(1:10), X1=1:10, X2=1:10, P=1:10)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=1:10, X1=factor(1:10), X2=1:10, P=1:10)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=1:10, X1=1:10, X2=1:10, P=factor(1:10))), std.args)), regexp = "The above errors were encountered!")
    # Characters
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=as.character(1:10), X1=1:10, X2=1:10, P=1:10, stringsAsFactors=F)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=1:10, X1=as.character(1:10), X2=1:10, P=1:10, stringsAsFactors=F)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=1:10, X1=1:10, X2=1:10, P=as.character(1:10), stringsAsFactors=F)), std.args)), regexp = "The above errors were encountered!")
    # Logicals (as indicate dichotomous variable (=factor))
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=as.logical(0:9), X1=1:10, X2=1:10, P=1:10)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=1:10, X1=as.logical(0:9), X2=1:10, P=1:10)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(data=data.frame(y=1:10, X1=1:10, X2=1:10, P=as.logical(0:9))), std.args)), regexp = "The above errors were encountered!")
  })

  test_that("Allow wrong data type in irrelevant columns", {
    # Allow wrong data types in unused columns
    expect_silent(do.call(function.to.test, c(list(data = cbind(function.std.data,
                                                                unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = F)), std.args)))
  })

  test_that(paste0("No column is named ",forbidden.col.name,".ENDO"), {
    expect_error(do.call(function.to.test, c(alist(data=cbind(data.frame(y=1:10, X1=1:10, X2=1:10, P=1:10), setNames(data.frame(p=11:20), paste0(forbidden.col.name,".P")))), std.args)), regexp = "The above errors were encountered!")
  })

}
