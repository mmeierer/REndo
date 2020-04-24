# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataHigherMoments")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - higherMomentsIV - Parameter formula")

test_that("Fail if bad 2nd RHS", {
  # Fail for missing 2nd RHS
  expect_error(higherMomentsIV(y~X1+X2+P||IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for 2nd RHS not in 1st RHS
  expect_error(higherMomentsIV(y~X1+X2|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+P|X2|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P2|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail if all regressors are endogenous
  expect_error(higherMomentsIV(y~X1|X1|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail if not exactly the same in model
  expect_error(higherMomentsIV(y~X1+X2+log(P)|P|IIV(iiv=gp, g=x2, X1, X2), data = dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|log(P)|IIV(iiv=gp, g=x2, X1, X2), data = dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if more than a single endo regressor is given", {
  expect_error(higherMomentsIV(y~X1+X2+P|P+X1|IIV(iiv=gp, g=x2, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P+X2|IIV(iiv=gp, g=x2, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|X1+X2|IIV(iiv=gp,g=x2, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if bad 3rd RHS", {
  # Fail for missing 3nd RHS
  expect_error(higherMomentsIV(y~X1+X2+P|P), regexp = "The above errors were encountered!")
  # Fail for 3nd RHS not in 1st RHS
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X3), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+P|P|IIV(g=x2, iiv=g, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for 3rd RHS in 2nd RHS
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P+X1|IIV(g=x2, iiv=g, X1, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P+X2|IIV(g=x2, iiv=g, X1, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if > 4RHS",{
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1)|IIV(g=x2, iiv=g, X2)|IIV(g=x3, iiv=g, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1)| X2|X1), regexp = "The above errors were encountered!")
})

test_that("Fail if bad LHS", {
  # Fail for missing LHS
  expect_error(higherMomentsIV(~X1+X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for > 1 LHS
  expect_error(higherMomentsIV(y|X1~X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for LHS in RHS and vice-versa
  expect_error(higherMomentsIV(y~y+X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(X1~X1+X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Special in LHS
  expect_error(higherMomentsIV(y+IIV(g=x2, iiv=g, X1)~X1+X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if formula contains dot (.)", {
  # Fail if dot (.) is in formula in any part
  expect_error(higherMomentsIV(y~X1+X2+.|P|IIV(g=x2, iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(.~X1+X2+X2|P|IIV(g=x2, iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+X2|.|IIV(g=x2, iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+X2|P|IIV(g=x2, iiv=gp, .), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if no special function", {
  # Not at all
  expect_error(higherMomentsIV(y~X1+X2+P|P|x2+gp+X1, data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|X1+X2), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Only for some
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV+IIV(g=x2, iiv=gp, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if missing regressors in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, ), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if missing iiv in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = NULL, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = NA_character_, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = character(0), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = "", X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if missing g in IIV", {
  # iiv gy requires it
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NA_character_, iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=character(0), iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})



test_that("Fail if invalid iiv in IIV", {
  # Allowed: c("g","gp","gy","yp","p2","y2")
  # Imaginary iiv
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gpp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=y3, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # some type
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=c(g, gy), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=c("g", "g"), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=list(g), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if multiple iiv in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,(iiv=g), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if multiple g in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, g=x3, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, g=x3, iiv=g, X1)+IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=c(x2,x3), iiv=g, X1)+IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=c("x2","x3"), iiv=g, X1)+IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=list("x2","x3"), iiv=g, X1)+IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if invalid colname in IIV", {
  # missing
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # wrong
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X3), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X2, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, X3), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if non existent special function", {
  # Misspelled
  expect_error(higherMomentsIV(y~X1+X2+P|P|iiv(g=x2, iiv=g, X1,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1,X2)+iiv(g=x2, iiv=gy, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Imaginary
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1,X2)+brunz(), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if transformation inside special", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, log(X1)), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, log(X1+X2)), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, I(X1^2)), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x^2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=log(x), iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g*p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=g2*p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=x2*p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


# Transformation in IIV but not var tested before
test_that("Fail if transformation only for one var but not in special", {
  expect_error(higherMomentsIV(y~X1+log(X2)+P|P|IIV(g=x2, iiv=g,     X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+log(X2)+P|P|IIV(g=x2, iiv=g, log(X1)), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~log(X1)+log(X2)+P|P|IIV(g=x2, iiv=g, log(X1+X2)), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if special outside RHS3", {
  expect_error(higherMomentsIV(y~IIV(g=x2, iiv=g,X1)+X2+P|P|IIV(g=x2, iiv=g,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV(g=x2, iiv=yp,y)~X1+X2+P|P|IIV(g=x2, iiv=g,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|IIV(g=x2, iiv=yp,X1)|IIV(g=x2, iiv=g,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # In EIV
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=yp,P)|IIV(g=x2, iiv=g,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

# .g vs iiv ---------------------------------------------------------------------------------------------------------

test_that("Fail if missing g but IIV does require it", {
  # "g","gp","gy",
  # missing
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")

  # open
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")

  # NULL
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")

})

test_that("Warning if g but IIV does not need it", {
  # warning for the others: "yp","p2","y2"
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=yp, X1, X2), data=dataHigherMoments), regexp = "ignored", all = TRUE)
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=p2, X1, X2), data=dataHigherMoments), regexp = "ignored", all = TRUE)
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=y2, X1, X2), data=dataHigherMoments), regexp = "ignored", all = TRUE)
})

test_that("Warning if exo regr but IIV does not need it", {
  # warning for the others: "yp","p2","y2"
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=yp, X1, X2), data=dataHigherMoments), regexp = "ignored because they are not needed to built", all = TRUE)
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=p2, X1, X2), data=dataHigherMoments), regexp = "ignored because they are not needed to built", all = TRUE)
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2, X1, X2), data=dataHigherMoments), regexp = "ignored because they are not needed to built", all = TRUE)
})

test_that("Silent if no g and IIV does not need it", {
  # Need to fit all in one because not enough IV for num regressors
  # Missing
  expect_silent(higherMomentsIV(y~X1+P|P|IIV(iiv=yp)+IIV(iiv=yp)+IIV(iiv=y2),
                                data=dataHigherMoments, verbose = FALSE))
  # expect_silent(higherMomentsIV(y~X1P|P|IIV(iiv=p2), data=dataHigherMoments))
  # expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2), data=dataHigherMoments))

  # Open
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=yp)+IIV(g=,iiv=p2)+IIV(g=,iiv=y2),
                                data=dataHigherMoments, verbose=FALSE))

  # NULL
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=yp)+IIV(g=,iiv=p2)+IIV(g=,iiv=y2),
                                data=dataHigherMoments, verbose = FALSE))
})





# data -----------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - higherMomentsIV - Parameter data")

test_that("Fail if is NA, NULL or missing", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=    ), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=NULL), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=NA_real_), regexp = "The above errors were encountered!")
})

test_that("Fail if not data.frame", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data=    c(y=1:10, X1=1:10, X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data= list(y=1:10, X1=1:10, X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
})

test_that("Fail if no rows or cols",{
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data= data.frame()), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1), data= data.frame(y=integer(), X1=numeric(), X2=numeric(), P=integer())), regexp = "The above errors were encountered!")
})

test_that("Fail if contains any non-finite", {
  call.args <- list(formula=y~X1+X2+P|P|IIV(g=x2,iiv=g, X1))
  test.nonfinite.in.data(data = dataHigherMoments, name.col = "y",  fct = higherMomentsIV, call.args = call.args)
  test.nonfinite.in.data(data = dataHigherMoments, name.col = "X1", fct = higherMomentsIV, call.args = call.args)
  test.nonfinite.in.data(data = dataHigherMoments, name.col = "X2", fct = higherMomentsIV, call.args = call.args)
  test.nonfinite.in.data(data = dataHigherMoments, name.col = "P",  fct = higherMomentsIV, call.args = call.args)
})

test_that("Fail if EIV not in data", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1)|EIV,data= dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1)|X1+eiv,data= dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if wrong data type in endo", {
  # Factor/Chars/ Logicals (as indicate dichotomous variable (=factor))
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data= data.frame(y=1:10, X1=1:10, X2=1:10, P=factor(1:10))), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data= data.frame(y=1:10, X1=1:10, X2=1:10, P=as.character(1:10), stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data= data.frame(y=1:10, X1=1:10, X2=1:10, P=as.logical(0:9))), regexp = "The above errors were encountered!")
})

test_that("Fail if wrong data type in exo used in IIV", {
  # Factor/Chars/ Logicals (as indicate dichotomous variable (=factor))
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data= data.frame(y=1:10, X1=factor(1:10), X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data=data.frame(y=1:10, X1=as.character(1:10), X2=1:10, P=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data= data.frame(y=1:10, X1=as.logical(0:9), X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
})

# test_that("Fail if wrong data type in exo used in y", {
# expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data=data.frame(y=as.character(1:10), X1=1:10, X2=1:10, P=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
# expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X1),data= data.frame(y=as.logical(0:9), X1=1:10, X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
# })


test_that("Allow wrong data type in irrelevant columns", {
  # Allow wrong data types in unused columns
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g,X2)+IIV(iiv=p2)+IIV(iiv=y2)+IIV(iiv=gp,g=x2,X1), verbose=FALSE,
                         data = cbind(dataHigherMoments,
                                      unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = FALSE)))
})




# verbose ----------------------------------------------------------------------
context("Inputchecks - higherMomentsIV - Parameter verbose")
test.single.logical(function.to.test = higherMomentsIV, parameter.name="verbose",
                    formula=y~X1+X2+P|P|IIV(g=x2,iiv=g, X1, X2), function.std.data=dataHigherMoments)

