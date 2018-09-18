# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopC2")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter formula")

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
  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|P1,data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|P1,data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|continuous(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P1),data=dataCopC2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|continuous(P2),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P2),data=dataCopC2), regexp = "The above errors were encountered!")
})

test_that("Fail if special in LHS", {
  expect_error(copulaCorrection(formula= continuous(y) ~ X1+X2+P1+P2|continuous(P1),data=dataCopC2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= continuous(y) ~ X1+X2+P1+P2|P1,data=dataCopC2), regexp = "The above errors were encountered!")
})


#
# ****TODO
# test_that("Fail if formula variables are not in data", {
#   # Fail if any regressors not in data (RHS1, RHS2, LHS1)
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, P1=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, X2=1:10,  P1=1:10)), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, X2=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(X1=1:10,X2=1:10, P1=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
# })

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

test_that("Fail if wrong data type in any of the formula parts", {
  # Only allow numericals in all relevant columns
  # Factor
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=factor(1:10), X1=1:10, X2=1:10, P1=1:10, P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=factor(1:10), X2=1:10, P1=1:10, P2=1:10)  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=factor(1:10), P2=1:10)  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=factor(1:10))  ), regexp = "The above errors were encountered!")
  # Characters
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=as.character(1:10), X1=1:10, X2=1:10, P1=1:10, P2=1:10, stringsAsFactors=F)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=1:10, X1=as.character(1:10), X2=1:10, P1=1:10, P2=1:10, stringsAsFactors=F)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.character(1:10), P2=1:10, stringsAsFactors=F)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.character(1:10), stringsAsFactors=F)), regexp = "The above errors were encountered!")

  # Logicals (as indicate dichotomous variable (=factor))
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=as.logical(0:9), X1=1:10, X2=1:10, P1=1:10, P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=as.logical(0:9), X2=1:10, P1=1:10, P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.logical(0:9), P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.logical(0:9))), regexp = "The above errors were encountered!")
})

test_that("Allow wrong data type in irrelevant columns", {
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),verbose=FALSE, data=
                                   cbind(dataCopC2, unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = F)))
})

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

# verbose --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter verbose")


# num.boots --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - Parameter num.boots")
