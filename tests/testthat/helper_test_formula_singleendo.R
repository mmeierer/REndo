# Tests the formula parameter of a function that takes formulas of structure: y ~ X1 + X2 + P | P
test.formula.single.endo <- function(function.to.test, function.std.data, additional.args){

  std.args <- modifyList(list(data=function.std.data), additional.args)

  test_that("Fail if no formula object is passed",  {
    expect_error(do.call(function.to.test, c(alist(formula=data.frame(1:3)), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=NULL),            std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=NA),              std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula= ),               std.args)), regexp = "The above errors were encountered!")
  })

  test_that("Fail if bad 2nd RHS", {
    # Fail for missing 2nd RHS
    expect_error(do.call(function.to.test, c(alist(formula=y ~ X1 + X2 + P), std.args)), regexp = "The above errors were encountered!")
    # Fail for 2nd RHS not in 1st RHS
    expect_error(do.call(function.to.test, c(alist(formula=y ~ X1 + X2 |P), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y ~ X2 + P|X1),  std.args)), regexp = "The above errors were encountered!")

    # Fail if all regressors are endogenous
    expect_error(do.call(function.to.test, c(alist(formula=y ~ P|P), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y ~ X1+P|X1+P),  std.args)), regexp = "The above errors were encountered!")
    # expect_error(function.to.test(formula = y ~ X1 + X2 + P|., data = function.std.data), regexp = "The above errors were encountered!") # dot version
  })

  test_that("Fail if > 2 RHS", {
    expect_error(do.call(function.to.test, c(alist(formula=y ~ X1 + X2 | P |P), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y ~ X1 + X2 | P |X1P), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y ~ X1 + X2 | P | X1| X2), std.args)), regexp = "The above errors were encountered!")
  })

  test_that("Fail if bad LHS", {
    # Fail for missing LHS
    expect_error(do.call(function.to.test, c(alist(formula=~ X1 + X2 + P|P), std.args)), regexp = "The above errors were encountered!")
    # Fail for > 1 LHS
    expect_error(do.call(function.to.test, c(alist(formula=y1 + y2      ~ X1 + X2 + P|P), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y1 + y2 + y3 ~ X1 + X2 + P|P), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y1 | y2      ~ X1 + X2 + P|P), std.args)), regexp = "The above errors were encountered!") # multipart LHS
    # Fail for LHS in RHS and vice-versa
    expect_error(do.call(function.to.test, c(alist(formula=X1 ~ X1 + X2 + P|P), std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y ~ y + X2 + P|P),   std.args)), regexp = "The above errors were encountered!")
    # Fail for dot in LHS as not more than one LHS allowed
    # expect_error(function.to.test(formula = . ~ X1 + X2 + P|P, data = function.std.data), regexp = "The above errors were encountered!")
  })

  test_that("Fail if formula variables are not in data", {
    # Fail if any regressors not in data (RHS1, RHS2, LHS1)
    expect_error(do.call(function.to.test, modifyList(std.args, alist(formula=y ~ X1 + X2 + P|P, data=data.frame(y=1:10, X1=1:10, P=1:10)))), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, modifyList(std.args, alist(formula=y ~ X1 + X2 + P|P, data=data.frame(y=1:10, X1=1:10, X2=1:10)))), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, modifyList(std.args, alist(formula=y ~ X1 + X2 + P|P, data=data.frame(X1=1:10,X2=1:10, P=1:10)))), regexp = "The above errors were encountered!")
  })

  test_that("Fail if formula contains dot (.)", {
    # Fail if dot (.) is in formula in any part
    expect_error(do.call(function.to.test, c(alist(formula=. ~ X1 + X2 + P|P),     std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y ~ . |P),              std.args)), regexp = "The above errors were encountered!")
    expect_error(do.call(function.to.test, c(alist(formula=y ~ y ~ X1 + X2 + P|.), std.args)), regexp = "The above errors were encountered!")
  })

  # ***outcommented for covr
  # test_that("Transformations can be handled and are correct", {
  #   correct.res <- do.call(function.to.test, c(alist(formula=y ~ X1 + X2 + P|P), std.args))
  #   # Can handle transformations in LHS
  #   data.altered   <- function.std.data
  #   data.altered$y <- data.altered$y*2.5
  #   res.trans.lhs <- do.call(function.to.test, modifyList(std.args, alist(formula=I(y/2.5) ~ X1 + X2 + P|P, data=data.altered)))
  #   expect_equal(coef(res.trans.lhs), coef(correct.res), check.attributes=F)
  #   # expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res))) # will vary greatly because of limited bootstrappings
  #
  #   # Can handle transformations in RHS1
  #   data.altered    <- function.std.data
  #   data.altered$X1 <- data.altered$X1+111
  #   res.trans.rhs1 <- do.call(function.to.test, modifyList(std.args, alist(formula=y ~ I(X1-111) + X2 + P|P, data=data.altered)))
  #   expect_equal(coef(res.trans.rhs1), coef(correct.res), check.attributes=F)
  #   # expect_equal(coef(summary(res.trans.rhs1)), coef(summary(correct.res))) # will vary greatly because of limited bootstrappings
  # })

  # test_that("Fail if transformations in RHS2", {
  #   expect_error(function.to.test(formula = y ~ X1 + X2 + log(P)|P,      data = function.std.data), regexp = "The above errors were encountered!")
  #   expect_error(function.to.test(formula = y ~ X1 + X2 + P|log(P),      data = function.std.data), regexp = "The above errors were encountered!")
  #   expect_error(function.to.test(formula = y ~ X1 + X2 + log(P)|log(P), data = function.std.data), regexp = "The above errors were encountered!")
  # })

  test_that("Works without intercept", {
    # Remove Intercept start.param if present
    if(any(names(std.args) == "start.params")){
      sp <- std.args[["start.params"]]
      std.args[["start.params"]] <- sp[setdiff(names(sp),"(Intercept)")]
    }
    # Works
    expect_silent(res.no.intercept <- do.call(function.to.test, c(alist(formula=y ~ X1 + X2 + P-1|P), std.args)))
    # Does not fit intercept
    expect_false(any(names(coef(res.no.intercept)) == "(Intercept)"))
  })
}

