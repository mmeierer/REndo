# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopC1")
data("dataCopC2")
data("dataCopDis")
data("dataCopDisCont")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter formula")

# *** TODO: Test if NA/NULL/MISSING/not formula....

test_that("Fail if bad 2nd RHS", {
  # Fail for missing 2nd RHS
  expect_error(copulaCorrection(formula= y ~ X1 + X2 + P1+P2, data=dataCopC2), regexp = "The above errors were encountered!")
  # Fail for 2nd RHS not in 1st RHS
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P1|continuous(P2),                    data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P2|continuous(P1),                    data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ y ~ X1 + X2 + P1|continuous(P1)+continuous(P2), data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P2|continuous(P1)+continuous(P2),     data=dataCopC2), regexp = "The above errors were encountered!")

  # Fail if all regressors are endogenous
  expect_error(copulaCorrection(formula=  y ~ P1|continuous(P1),                                          data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ P1+P2|continuous(P1)+continuous(P2),                        data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P2|continuous(X1)+continuous(X2)+continuous(P2),  data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P2+P1|continuous(P1)+continuous(P2)+continuous(X1)+continuous(X2),  data=dataCopC2), regexp = "The above errors were encountered!")

  # expect_error(function.to.test(formula = y ~ X1 + X2 + P|., data = function.std.data), regexp = "The above errors were encountered!") # dot version
})

test_that("Fail if > 2 RHS", {
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|continuous(P1)|continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|continuous(P1)|discrete(P2),  data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|discrete(P1)|discrete(P2),    data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|continuous(P1)|discrete(P2)|continuous(X1),    data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if bad LHS", {
  # Fail for missing LHS
  expect_error(copulaCorrection(formula=   ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  # Fail for > 1 LHS
  expect_error(copulaCorrection(formula= y1 + y2  ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y1 + y2 + y3 ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y1 | y2 ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")

  # Fail for LHS in RHS and vice-versa
  expect_error(copulaCorrection(formula= X1 ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y  ~  y+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y  ~  y+X2+P1+P2|continuous(y)+continuous(P2),data=dataCopC2),  regexp = "The above errors were encountered!")

  # Fail for dot in LHS as not more than one LHS allowed
  # expect_error(function.to.test(formula = . ~ X1 + X2 + P|P, data = function.std.data), regexp = "The above errors were encountered!")
})



test_that("Fail if formula contains dot (.)", {
  # Fail if dot (.) is in formula in any part
  expect_error(copulaCorrection(formula= . ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ . |continuous(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(.)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(.),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(.),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if no special function", {
  # Non at all
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P1,data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P1+P2,data=dataCopC2), regexp = "The above errors were encountered!")
  # Only for some
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+P2,data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+P2,data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P2 + discrete(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P1 + continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
})


test_that("Fail if regressor in different special functions", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P1),   data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+continuous(P1),   data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1,P2)+discrete(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1,P2)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P1, P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+continuous(P1, P2),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if non existent special function", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|brunz(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+brunz(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|brunz(P1)+continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
})

# **TODO remove
test_that("Fail if function inside special", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1+P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1*P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1:P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(log(P1)),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(I(P1^2)),data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1+P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1*P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1:P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(log(P1)),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(I(P1^2)),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if function across special", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)*continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)*discrete(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)*discrete(P2),data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|I(continuous(P1)/continuous(P2)),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|I(discrete(P1)/discrete(P2)),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|I(continuous(P1)/discrete(P2)),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if empty special", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+discrete(),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+continuous(),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if special in RHS1", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|discrete(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P1),data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|continuous(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P1),data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P2),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if special in LHS", {
  expect_error(copulaCorrection(formula= continuous(y) ~ X1+X2+P1+P2|continuous(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= continuous(y) ~ X1+X2+P1+P2|P1,data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if var is in both specials", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2)+discrete(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2)+continuous(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2)+continuous(X1, P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2)+discrete(X1, P1),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if formula variables are not in data", {
  # Fail if any regressors not in data (RHS1, RHS2, LHS1)
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, P1=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, X2=1:10,  P1=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, X2=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(X1=1:10,X2=1:10, P1=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
})

# data --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter data")

test_that("Fail if is NA, NULL or missing", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=NULL), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=NA_real_), regexp = "The above errors were encountered!")
})

test_that("Fail if not data.frame", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=   c(y=1:10, X1=1:10, X2=1:10, P1=1:10,P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=list(y=1:10, X1=1:10, X2=1:10, P1=1:10,P2=1:10)), regexp = "The above errors were encountered!")
})

test_that("Fail if no rows or cols",{
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=data.frame()), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=data.frame(y=integer(), X1=numeric(), X2=numeric(), P1=integer(), P2=integer())), regexp = "The above errors were encountered!")
})




test_that("Fail if wrong data type in endogenous formula part", {
  # Only allow numericals in endogenous columns
  # Factor
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=factor(1:10), P2=1:10)  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=factor(1:10))  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=factor(1:10), P2=1:10)  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=factor(1:10))  ), regexp = "The above errors were encountered!")
  # Characters
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.character(1:10), P2=1:10, stringsAsFactors=F)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.character(1:10), stringsAsFactors=F)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.character(1:10), P2=1:10, stringsAsFactors=F)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.character(1:10), stringsAsFactors=F)), regexp = "The above errors were encountered!")

  # Logicals (as indicate dichotomous variable (=factor))
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.logical(0:9), P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.logical(0:9))), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.logical(0:9), P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.logical(0:9))), regexp = "The above errors were encountered!")
})

# test_that("Allow wrong data type in irrelevant columns", {
#   expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),verbose=FALSE, data=
#                                    cbind(dataCopC2, unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = F)))
# })

test_that(paste0("No column is named PStar.ENDO for discrete, >1 continuous, and mixed models PStar.ENDO"), {
  # Discrete case
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P2=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10, PStar.P2=1:10)), regexp = "The above errors were encountered!")
  # Copula continuous 2 case
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P2=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10, PStar.P2=1:10)), regexp = "The above errors were encountered!")
  # Mixed case
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P2=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10, PStar.P2=1:10)), regexp = "The above errors were encountered!")
})


test_that("Warn if binomial/dummy data passed in endo regressor", {
  expect_warning(copulaCorrection(formula=y ~ X1+X2+D|discrete(D),
                                  data = cbind(dataCopDis, D = c(0,1))), regexp = "may not be binomial")
})

# verbose --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter verbose")
test.single.logical(function.to.test = copulaCorrection, parameter.name="verbose",
                    formula=y~X1+X2+P1+P2|continuous(P1)+discrete(P2), function.std.data=dataCopC2)


# num.boots --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter num.boots")
# Single continuous
# Failure tests
test.positive.numeric.whole.number(function.to.test = copulaCorrection, parameter.name="num.boots",
                                   formula=y~X1+X2+P|continuous(P), function.std.data=dataCopC1)
# Start params required for silence
# additional.args =list(verbose=F, start.params = c("(Intercept)"=1.983, X1=1.507, X2=-3.014, P1=-0.891)

test_that("Warning if num.boots < 10", {
  # only for continuous 1
  expect_warning(copulaCorrection(num.boots = 2, verbose=FALSE,formula= y ~ X1+X2+P|continuous(P),data=dataCopC1),all=FALSE, regexp = "It is recommended to run more than")
})

# Warning if num.boots given for any other case
test_that("Warning if unneeded num.boots given", {
  # >1 continuous
  expect_warning(copulaCorrection(num.boots = 10, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=dataCopC2),all=TRUE, regexp = "Additional parameters given in the ... argument are ignored because they are not needed.")
  # Mixed
  expect_warning(copulaCorrection(num.boots = 10, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data=dataCopDisCont), all=TRUE, regexp = "Additional parameters given in the ... argument are ignored because they are not needed.")
  # Discrete
  expect_warning(copulaCorrection(num.boots = 10, verbose=FALSE,formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=dataCopDis),all=TRUE,regexp = "Additional parameters given in the ... argument are ignored because they are not needed.")
})

# start.params ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter start.params")

test_that("start.params is vector and all numeric", {
  # Any parameter is character, logical, factor, matrix
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = as.character(1), X2 = -2, P = 0),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = as.factor(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = as.logical(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = as.matrix(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = complex(1,4,2), X2 = -2, P = 0),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
})

test_that("start.params is not NA",{
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = NA_integer_, X2 = -2, P = 0),    formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = NA_real_, X2 = -2, P = 0),       formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = NA_integer_, formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = NA_real_, formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
})

test_that("start.params is NULL or missing but runs with message", {
  # Does not work with missing start.params in ...
  # expect_warning(copulaCorrection(start.params =     , formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1, num.boots = 2, verbose=TRUE))
  expect_message(copulaCorrection(start.params = NULL, formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1, verbose=TRUE),regexp = "The linear model", all = FALSE)
})

test_that("start.params is named correctly", {
  # Unnamed vec given
  expect_error(copulaCorrection(start.params = c(2, 1, -2,0),                                formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Partially named vec given
  expect_error(copulaCorrection(start.params = c(2, X1 = 1, X2=-2,P=0),                      formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Wrong case
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, x1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Same param name twice
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X2 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Unrelated name
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X10 = 1, X2 = -2, P = 0),  formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Param missing (main, endo, intercept)
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, P = 0),            formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2),          formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c(X1 = 1, X2 = -2, P = 0),                    formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Intercept spelling
  expect_error(copulaCorrection(start.params = c("Intercept"=2, X1 = 1, X2 = -2, P = 0),     formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(intercept)"=2, X1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Intercept given but none required
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P -1 |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Additional, not required param given
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, X3 = 99, P = 0), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
})


test_that("start.params contains no parameter rho or sigma", {
  # Given additionally
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0, rho=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0, sigma=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  # Given instead of other parameters
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, rho=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, sigma=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, sigma = -2, P=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, rho = -2, P=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, sigma = -2, rho=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopC1), regexp = "The above errors were encountered!")
})



# ...  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter 3-dots")
test_that("Warning if further unneded params are given", {
  # 1 continuous
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P|continuous(P),data=dataCopC1),all=TRUE, regexp = "are ignored")
  # >1 continuous
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=dataCopC2),all=TRUE, regexp = "are ignored")
  # Mixed
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data=dataCopDisCont), all=TRUE, regexp = "are ignored")
  # Discrete
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=dataCopDis),all=TRUE,regexp = "are ignored")
})
