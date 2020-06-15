# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - latentIV - Parameter formula")

test_that("Fail if no formula object is passed",  {
  expect_error(latentIV(formula = data.frame(1:3), data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = NULL,            data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = NA,              data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula =   ,              data = dataLatentIV), regexp = "The above errors were encountered!")
})

test_that("Fail if bad 1st RHS", {
  # Fail if > 1 regressor in RHS
  expect_error(latentIV(formula = y ~ I + P ,       data = cbind(I=1, dataLatentIV)),              regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ X1 + X2 + P , data = cbind(X1=1.23, X2=2.34, dataLatentIV)), regexp = "The above errors were encountered!")
})

test_that("Fail if bad 2nd RHS", {
  # Fail if ANY 2nd RHS
  expect_error(latentIV(formula = y ~ P | P, data = dataLatentIV), regexp = "The above errors were encountered!")
})

test_that("Fail if bad LHS", {
  # Fail for missing LHS
  expect_error(latentIV(formula = ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Fail for > 1 LHS
  expect_error(latentIV(formula = y1 + y2      ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y1 + y2 + y3 ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y1 | y2      ~ P, data = dataLatentIV), regexp = "The above errors were encountered!") # multipart LHS
  # Fail for LHS in RHS and vice-versa
  expect_error(latentIV(formula = P ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ y, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Fail for dot in LHS as not more than one LHS allowed
  # expect_error(latentIV(formula = . ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})

test_that("Fail if formula variables are not in data", {
  # Fail if any regressors not in data (RHS1, RHS2, LHS1)
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, X1=1:10, X2=1:10)), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = data.frame(X1=1:10,X2=1:10, P=1:10)),  regexp = "The above errors were encountered!")
})

test_that("Fail if formula contains dot (.)", {
  # Fail if dot (.) is in formula in any part
  expect_error(latentIV(formula = . ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ ., data = dataLatentIV), regexp = "The above errors were encountered!")
})


test_that("Fail if formula variables are not in data", {
  # Fail if any regressors not in data (RHS1, RHS2, LHS1)
  expect_error(latentIV(formula= y ~ P ,data=data.frame(y=1:10)), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula= y ~ P ,data=data.frame(P=1:10)), regexp = "The above errors were encountered!")
})


# data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - latentIV - Parameter data")
test_that("Fail if not data.frame", {
  expect_error(latentIV(formula = y ~ P, data =     ),                regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = NULL),                regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = NA_integer_),         regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = c(y=1:10,   P=1:10)), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = list(y=1:10,P=1:10)), regexp = "The above errors were encountered!")
})

test_that("Fail if no rows or cols",{
  expect_error(latentIV(formula = y ~ P, data = data.frame()), regexp = "The above errors were encountered!") # no cols
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=integer(), P=integer())), regexp = "The above errors were encountered!")
})


test_that("Fail if contains any non-finite", {
  call.args <- list(formula=y ~ P)
  test.nonfinite.in.data(data = dataLatentIV, name.col = "y",  fct = latentIV, call.args = call.args)
  test.nonfinite.in.data(data = dataLatentIV, name.col = "P",  fct = latentIV, call.args = call.args)
})


test_that("Fail if wrong data type in any of the formula parts", {
  # Only allow numericals in all relevant columns
  # Factor
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=factor(1:10), P=1:10)), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, P=factor(1:10)), regexp = "The above errors were encountered!"))
  # Characters
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=as.character(1:10), P=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, P=as.character(1:10), stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  # Logicals (as indicate dichotomous variable (=factor))
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=as.logical(0:9), P=1:10)), regexp = "The above errors were encountered!")
  expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, P=as.logical(0:9)), regexp = "The above errors were encountered!"))
})

test_that("Allow wrong data type in irrelevant columns", {
  # Allow wrong data types in unused columns
  expect_silent(latentIV(formula = y ~ P, verbose = FALSE,
                         data = cbind(dataLatentIV,
                                      unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = FALSE)))
})


# start.params ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - latentIV - Parameter start.params")

test_that("start.params is vector and all numeric", {
  # Any parameter is character, logical, factor, matrix
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = as.character(1)), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = as.factor(c("(Intercept)"=2, P = 1)),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = as.logical(c("(Intercept)"=2, P = 1)),   formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = as.matrix(c("(Intercept)"=2, P = 0)),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = complex(1,4,2)),  formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})

test_that("start.params is not NA",{
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = NA_integer_),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = NA_real_),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=NA_integer_, P = 2),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=NA_real_, P = 2),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = NA_integer_,                            formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = NA_real_,                               formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})

test_that("start.params is NULL or missing but runs with message", {
  expect_message(latentIV(start.params =     , formula = y ~ P, data = dataLatentIV), regexp = "No start parameters were given")
  expect_message(latentIV(start.params = NULL, formula = y ~ P, data = dataLatentIV), regexp = "No start parameters were given")
})

test_that("start.params is named correctly", {
  # Unnamed vec given
  expect_error(latentIV(start.params = c(2, 1),                          formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Partially named vec given
  expect_error(latentIV(start.params = c(2, P=0),                        formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Wrong case
  expect_error(latentIV(start.params = c("(Intercept)"=2, p = 1),        formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Same param name twice
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 1, P = 2), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Unrelated name
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 1, P2 =3), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Param missing (main, endo, intercept)
  expect_error(latentIV(start.params = c("(Intercept)"=2),               formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c(P = 2),                         formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Intercept spelling
  expect_error(latentIV(start.params = c("Intercept"=2,    P = 0),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(intercept)"=2,  P = 0),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Intercept given but none required
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0),
                                           formula = y ~ X1 + X2 + P -1 |P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Additional, not required param given
  expect_error(latentIV(start.params = c("(Intercept)"=2, X1 = 1, P = 0), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})


test_that("start.params contains no parameter named pi1, pi2, theta5, theta6, theta7, theta8", {
  # Given additionally
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta5=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta6=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta7=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta8=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1, theta5=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta7=1, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1, pi2=1, theta5=1, theta6=1, theta7=1, theta8=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Given instead of other parameters
  expect_error(latentIV(start.params = c("(Intercept)"=2, pi1=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta5=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta6=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta7=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta8=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})

# optimx.args  -----------------------------------------------------------------------------------------------------------
context("Inputchecks - latentIV - Parameter optimx.args")
test.optimx.args(function.to.test = latentIV, parameter.name = "optimx.args", formula=y~P,
                 function.std.data = dataLatentIV)

test_that("Has default value empty list()",{
  default.arg <- eval(formals(REndo:::latentIV)[["optimx.args"]])
  expect_equal(class(default.arg), "list") # S3 class does not work
})


# verbose ----------------------------------------------------------------------------------------------------------------
context("Inputchecks - latentIV - Parameter verbose")
test.single.logical(function.to.test = latentIV, parameter.name="verbose",
                    formula=y~P, function.std.data=dataLatentIV)


