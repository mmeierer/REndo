# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataHetIV")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - hetErrorsIV - Parameter formula")

test_that("Fail if bad 2nd RHS", {
  # Fail for missing 2nd RHS
  expect_error(hetErrorsIV(y~X1+X2+P||IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  # Fail for 2nd RHS not in 1st RHS
  expect_error(hetErrorsIV(y~X1+X2|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+P|X2|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P2|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  # Fail if all regressors are endogenous
  expect_error(hetErrorsIV(y~X1|X1|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~P|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  # Fail if not exactly the same in model
  expect_error(hetErrorsIV(y~X1+X2+log(P)|P|IIV(X1)+IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|log(P)|IIV(X1)+IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
})

test_that("Fail if more than a single endo regressor is given", {
  expect_error(hetErrorsIV(y~X1+X2+P|P+X1|IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P+X2|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|X1+X2|IIV(P), data=dataHetIV), regexp = "The above errors were encountered!")
})

# test_that("Fail if bad 3rd RHS", {
#   # Fail for missing 3nd RHS
#   expect_error(hetErrorsIV(y~X1+X2+P|P, data=dataHetIV), regexp = "The above errors were encountered!")
#   # Fail for 3nd RHS not in 1st RHS
#   expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X3), data=dataHetIV), regexp = "The above errors were encountered!")
#   expect_error(hetErrorsIV(y~X1+P|P|IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
#   expect_error(hetErrorsIV(y~X2+P|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
#   # Fail for 3rd RHS in 2nd RHS
#   expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(P), data=dataHetIV), regexp = "The above errors were encountered!")
#   expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1, P), data=dataHetIV), regexp = "The above errors were encountered!")
#   expect_error(hetErrorsIV(y~X1+X2+P|P+X1|IIV(X1, X2), data=dataHetIV), regexp = "The above errors were encountered!")
#   expect_error(hetErrorsIV(y~X1+X2+P|P+X2|IIV(X1, X2), data=dataHetIV), regexp = "The above errors were encountered!")
# })

test_that("Fail if > 4RHS",{
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1)|IIV(X2)|IIV(g=x3, iiv=g, X2), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1)|X2|X1), regexp = "The above errors were encountered!")
})

test_that("Fail if bad LHS", {
  # Fail for missing LHS
  expect_error(hetErrorsIV(~X1+X2+P|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  # Fail for > 1 LHS
  expect_error(hetErrorsIV(y|X1~X2+P|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  # Fail for LHS in RHS and vice-versa
  expect_error(hetErrorsIV(y~y+X2+P|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(X1~X1+X2+P|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
  # Special in LHS
  expect_error(hetErrorsIV(y+IIV(X1)~X1+X2+P|P|IIV(X1), data=dataHetIV), regexp = "The above errors were encountered!")
})

test_that("Fail if formula contains dot (.)", {
  # Fail if dot (.) is in formula in any part
  expect_error(hetErrorsIV(y~X1+X2+.|P|IIV(g=x2, iiv=gp, X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(.~X1+X2+X2|P|IIV(g=x2, iiv=gp, X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+X2|.|IIV(g=x2, iiv=gp, X1), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+X2|P|IIV(g=x2, iiv=gp, .), data=dataHetIV), regexp = "The above errors were encountered!")
})

test_that("Fail if no special function", {
  # Not at all
  expect_error(hetErrorsIV(y~X1+X2+P|P|x2+gp+X1, data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|X1+X2), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(), data=dataHetIV), regexp = "The above errors were encountered!")
  # Only for some
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV+IIV(g=x2, iiv=gp, X2), data=dataHetIV), regexp = "The above errors were encountered!")
})


test_that("Fail if missing regressors in IIV", {
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(,), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1)+IIV(), data=dataHetIV), regexp = "The above errors were encountered!")
})

test_that("Fail if invalid colname in IIV", {
  # missing
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g), data=dataHetIV), regexp = "The above errors were encountered!")
  # wrong
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X3), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(P), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1, P), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X2, P), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1, X3), data=dataHetIV), regexp = "The above errors were encountered!")
})


test_that("Fail if non existent special function", {
  # Misspelled
  expect_error(hetErrorsIV(y~X1+X2+P|P|iiv(X1,X2), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1,X2)+iiv(X2), data=dataHetIV), regexp = "The above errors were encountered!")
  # Imaginary
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1,X2)+brunz(), data=dataHetIV), regexp = "The above errors were encountered!")
})



test_that("Fail if special outside RHS3", {
  expect_error(hetErrorsIV(y~IIV(X1)+X2+P|P|IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(IIV(y)~X1+X2+P|P|IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|IIV(P)|IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
  # In EIV
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1)|IIV(X2), data=dataHetIV), regexp = "The above errors were encountered!")
})


# data -----------------------------------------------------------------------------------------
context("Inputchecks - hetErrorsIV - Parameter data")


test_that("Fail if is NA, NULL or missing", {
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=    ), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=NULL), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=NA_real_), regexp = "The above errors were encountered!")
})


test_that("Fail if contains any non-finite", {
  call.args <- list(formula=y~X1+X2+P|P|IIV(X1))
  test.nonfinite.in.data(data = dataHetIV, name.col = "y",  fct = hetErrorsIV, call.args = call.args)
  test.nonfinite.in.data(data = dataHetIV, name.col = "X1", fct = hetErrorsIV, call.args = call.args)
  test.nonfinite.in.data(data = dataHetIV, name.col = "X2", fct = hetErrorsIV, call.args = call.args)
  test.nonfinite.in.data(data = dataHetIV, name.col = "P",  fct = hetErrorsIV, call.args = call.args)
})


test_that("Fail if not data.frame", {
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data=    c(y=1:10, X1=1:10, X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data= list(y=1:10, X1=1:10, X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
})

test_that("Fail if no rows or cols",{
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data= data.frame()), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1), data= data.frame(y=integer(), X1=numeric(), X2=numeric(), P=integer())), regexp = "The above errors were encountered!")
})

test_that("Fail if EIV not in data", {
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1)|EIV,data= dataHetIV), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1)|X1+eiv,data= dataHetIV), regexp = "The above errors were encountered!")
})

test_that("Fail if wrong data type in endo", {
  # Factor/Chars/ Logicals (as indicate dichotomous variable (=factor))
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data= data.frame(y=1:10, X1=1:10, X2=1:10, P=factor(1:10))), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data= data.frame(y=1:10, X1=1:10, X2=1:10, P=as.character(1:10), stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data= data.frame(y=1:10, X1=1:10, X2=1:10, P=as.logical(0:9))), regexp = "The above errors were encountered!")
})

test_that("Fail if wrong data type in exo used in IIV", {
  # Factor/Chars/ Logicals (as indicate dichotomous variable (=factor))
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data= data.frame(y=1:10, X1=factor(1:10), X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data=data.frame(y=1:10, X1=as.character(1:10), X2=1:10, P=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data= data.frame(y=1:10, X1=as.logical(0:9), X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
})

# test_that("Fail if wrong data type in exo used in y", {
# expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data=data.frame(y=as.character(1:10), X1=1:10, X2=1:10, P=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
# expect_error(hetErrorsIV(y~X1+X2+P|P|IIV(X1),data= data.frame(y=as.logical(0:9), X1=1:10, X2=1:10, P=1:10)), regexp = "The above errors were encountered!")
# })


test_that("Allow wrong data type in irrelevant columns", {
  # Allow wrong data types in unused columns
  expect_silent(hetErrorsIV(y~X1+X2+P|P|IIV(X2), verbose=FALSE,
                                data = cbind(dataHetIV,
                                             unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = FALSE)))
})


# verbose ----------------------------------------------------------------------
context("Inputchecks - hetErrorsIV - Parameter verbose")
test.single.logical(function.to.test = hetErrorsIV, parameter.name="verbose",
                    formula=y~X1+X2+P|P|IIV(X1), function.std.data=dataHetIV)

