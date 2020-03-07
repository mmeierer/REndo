
# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")
f.multilevel <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15)

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - multilevelIV - Parameter formula")

test_that("Fail if no formula object is passed",  {
  expect_error(multilevelIV(formula = data.frame(1:3), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = NULL,            data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = NA,              data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula =   ,              data = dataMultilevelIV), regexp = "The above errors were encountered!")
})


test_that("Fail if formula contains dot (.)", {
  expect_error(multilevelIV(formula = y ~ . + (1| CID) + (1 | SID),data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if formula contains no random effect parts", {
  expect_error(multilevelIV(formula = y ~ X11 + X12 | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if formula variables are not in data", {
  # Fail if any regressors not in data (RHS1, RHS2, LHS1)
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, X11=1:10, X12=1:10)), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(X11=1:10,X12=1:10, ABC=1:10)), regexp = "The above errors were encountered!")
})


test_that("Fail if more than 2 parts given", {
  expect_error(multilevelIV(formula = y ~ X11 + X12 + (1 | SID) | endo(X11) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + (1 | SID) | endo(X11) | endo(X12)| endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if more than 3 levels given", {
  expect_error(multilevelIV(formula = y ~ X11 + X12 + (1|SID)+(1|CID)+(1|A)|endo(X11), data = cbind(dataMultilevelIV, A=1:4)), regexp = "The above errors were encountered!")
})


test_that("Fail if less than 2 levels given", {
  expect_error(multilevelIV(formula = y ~ X11 + X12|endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if endo() not in model", {
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(X33), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(X11, X33), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X12+(1|CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if endo in brackets part", {
  # L2
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X33 +(1+X33|SID) | endo(X33), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X33 +(1+X33+X12|SID) | endo(X33), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X33 +(1+X12+X33|SID) | endo(X33), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X33 +(1+X12+X33|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X33 +(1+X33+X12|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  # L3
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1+X33|CID)+(1+X12|SID) | endo(X33), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1+X12|CID)+(1+X33|SID) | endo(X33), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1+X33|CID)+(1+X12|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1+X12|CID)+(1+X33|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1+X33+X12|CID)+(1|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1+X12+X33|CID)+(1|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1|CID)+(1+X33+X12|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+ X31+ X33 +(1|CID)+(1+X12+X33|SID) | endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if transformations different in model and endo()", {
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(log(X11)), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ log(X11) + X12+(1|CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if empty special (endo()) given", {
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(  ), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if empty special (endo()) given", {
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(  ), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | endo(X11)+endo(), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if no / misspelled special (endo()) given", {
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | edno(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | edno(X11)+endo(X12), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12+(1|CID) | X11, data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail special not in RHS2", {
  expect_error(multilevelIV(formula = y+endo(y) ~ X11 + X12+(1|CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = endo(y) ~ X11 + X12+(1|CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ endo(X11) + X12 + X15+(1|CID) | endo(X15), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if level grouping id in model", {
  # L2
  expect_error(multilevelIV(formula = y ~ X11 + X12 + X15 + CID (1|CID) | endo(X15), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + X15 + (1|X11) | endo(X15), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  # L3
  expect_error(multilevelIV(formula = y ~ X11 + X12 + X15 + SID + (1|CID) + (1|SID) | endo(X15), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + X15 + CID + (1|CID) + (1|SID) | endo(X15), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + X15 + (1 | X11) + (1|SID) | endo(X15), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + X15 + (1 | CID) + (1|X11) | endo(X15), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})


test_that("Fail if no slope provided",{
  expect_error(multilevelIV(formula = y ~ X11 + X12  + X15+ (0|CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12  + X15+ (0|CID)+(1|SID) | endo(X11), data = dataMultilevelIV), regexp = "Please revise your data and formula.")
  expect_error(multilevelIV(formula = y ~ X11 + X12  + X15+ (1|CID)+(0|SID) | endo(X11), data = dataMultilevelIV), regexp = "Please revise your data and formula.")
})

test_that("Fail if slopes are not in model",{
  # L2
  expect_error(multilevelIV(formula = y ~ X11 + X12  + (1+X15   |CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12  + (X15   |CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12  + (X33   |CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  # L3
  expect_error(multilevelIV(formula = y ~ X11 + X12  + (X15|CID)+(1|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12  + (1|CID)+(X15|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12  + (X12|CID)+(X15|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12  + (X15|CID)+(X12|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if not same transformations in slope and model",{
  # L2
  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (1+X15   |CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (1+I(X15)|CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + exp(X15) + (1+X15|CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")

  # L3
  # In either part
  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (1+X15   |CID)+(1|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (I(X15)+1|CID)+(1|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + exp(X15) + (1+X15|CID)+(1|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")

  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (1|CID)      +(1+X15|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (1|CID)+(I(X15)+1|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + exp(X15) + (1|CID)+(1+X15|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")

  # In one but not other
  expect_error(multilevelIV(formula = y ~ X11 + X12 + exp(X15) + (exp(X15)|CID)+(X15|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + exp(X15) + (X15|CID)+(exp(X15)|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (I(X15+1)|CID)+(X15|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ X11 + X12 + I(X15+1) + (X15|CID)+(I(X15+1)|SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})

test_that("Fail if slope intercept not in model",{
  # L2
  expect_error(multilevelIV(formula = y ~ 0+ X11 + X12 + X15 + (X15 |CID)   | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ 0+ X11 + X12 + X15 + (1+X15 |CID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")

  # L3
  expect_error(multilevelIV(formula = y ~ 0+ X11 + X12 + X15 + (1 |CID)+ (X15-1 |SID)   | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ 0+ X11 + X12 + X15 + (X15-1|CID)+(1 |SID) | endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  # Split
  expect_error(multilevelIV(formula = y ~ 0+ X11 + X12 + X15 + (X15-1 |CID)+(1|CID)| endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = y ~ 0+ X11 + X12 + X15 + (X15-1 |CID)+(X11|CID)| endo(X11), data = dataMultilevelIV), regexp = "The above errors were encountered!")
})




# data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - multilevelIV - Parameter data")
test_that("Fail if not data.frame", {
  expect_error(multilevelIV(formula = f.multilevel, data =     ),                regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = NULL),                regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = NA_integer_),         regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = c(y=1:10,   P=1:10)), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = list(y=1:10,P=1:10)), regexp = "The above errors were encountered!")
})

test_that("Fail if no rows or cols",{
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame()), regexp = "The above errors were encountered!") # no cols
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=integer(), P=integer())), regexp = "The above errors were encountered!")
})


test_that("Fail if contains any non-finite", {
  call.args <- list(formula=f.multilevel)
  # y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15)
  test.nonfinite.in.data(data = dataMultilevelIV, name.col = "y",  fct = multilevelIV, call.args = call.args, regexp=NULL)
  test.nonfinite.in.data(data = dataMultilevelIV, name.col = "X11",  fct = multilevelIV, call.args = call.args, regexp=NULL)
  test.nonfinite.in.data(data = dataMultilevelIV, name.col = "X12",  fct = multilevelIV, call.args = call.args, regexp=NULL)
  test.nonfinite.in.data(data = dataMultilevelIV, name.col = "X13",  fct = multilevelIV, call.args = call.args, regexp=NULL)
  test.nonfinite.in.data(data = dataMultilevelIV, name.col = "X14",  fct = multilevelIV, call.args = call.args, regexp=NULL)
  test.nonfinite.in.data(data = dataMultilevelIV, name.col = "X15",  fct = multilevelIV, call.args = call.args, regexp=NULL)
  test.nonfinite.in.data(data = dataMultilevelIV, name.col = "X32",  fct = multilevelIV, call.args = call.args, regexp=NULL)
})



test_that("Fail if wrong data type in any of the formula parts", {
  # Only allow numericals in all relevant columns
  # Factor
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=factor(1:10), P=1:10)), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, P=factor(1:10)), regexp = "The above errors were encountered!"))
  # Characters
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=as.character(1:10), P=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, P=as.character(1:10), stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  # Logicals (as indicate dichotomous variable (=factor))
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=as.logical(0:9), P=1:10)), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, P=as.logical(0:9))), regexp = "The above errors were encountered!")
})



# lmer.control -----------------------------------------------------------------
context("Inputchecks - multilevelIV - Parameter lmer.control")
test_that("lmer.control fails for NULL and NA", {
  expect_error(multilevelIV(formula = f.multilevel, data = dataMultilevelIV, lmer.control = NULL), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = dataMultilevelIV, lmer.control = NA), regexp = "The above errors were encountered!")
})

test_that("lmer.control fails for other input than from lmerControl", {
  expect_error(multilevelIV(formula = f.multilevel, data = dataMultilevelIV, lmer.control = list(optimizer="Nelder_Mead")), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = dataMultilevelIV, lmer.control = data.frame(optimizer="Nelder_Mead")), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = dataMultilevelIV, lmer.control = c(optimizer="Nelder_Mead")), regexp = "The above errors were encountered!")
})


# verbose ----------------------------------------------------------------------
context("Inputchecks - multilevelIV - Parameter verbose")
test.single.logical(function.to.test = multilevelIV, parameter.name="verbose",
                    formula=f.multilevel, function.std.data=dataMultilevelIV)


# summary model parameter ------------------------------------------------------
context("Inputchecks - multilevelIV summary - Parameter model")
# Inputchecks for parameter model are all the same for all S3 methods

expect_silent(res.ml <- multilevelIV(formula = f.multilevel, data = dataMultilevelIV, verbose = FALSE))
test_that("Fails for forms of no input",{
  expect_error(summary(res.ml, model = NULL), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = NA), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = NA_character_), regexp = "The above errors were encountered!")
})


test_that("Fails for non character input",{
  expect_error(summary(res.ml, model = 123), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = list("REF")), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = data.frame(model="REF")), regexp = "The above errors were encountered!")
})

test_that("Fails for more than one model",{
  expect_error(summary(res.ml, model = c("REF", "REF")), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = c("REF", "FE_L2")), regexp = "The above errors were encountered!")
})

test_that("Fails for misspelled/unknown input",{
  expect_error(summary(res.ml, model = "ref"), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = "REf"), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = "ReF"), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = "FE_l2"), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = "FE_l3"), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = "fe_l3"), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml, model = "fe_l3"), regexp = "The above errors were encountered!")
})

