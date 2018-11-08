
# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")
f.multilevel <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID)

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - multilevelIV - Parameter formula")

test_that("Fail if no formula object is passed",  {
  expect_error(multilevelIV(formula = data.frame(1:3), data = dataMultilevelIV,name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = NULL,            data = dataMultilevelIV,name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = NA,              data = dataMultilevelIV,name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula =   ,              data = dataMultilevelIV,name.endo="X15"), regexp = "The above errors were encountered!")
})


# test_that("Fail if formula contains dot (.)", {
# ** TODO **
# })


test_that("Fail if formula variables are not in data", {
  # Fail if any regressors not in data (RHS1, RHS2, LHS1)
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, X1=1:10, X2=1:10),name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(X1=1:10,X2=1:10, P=1:10)),  regexp = "The above errors were encountered!")
})

# ** TODO
# test_that("Fail if no levels given", {})
# test_that("Fail if one level given", {})
# test_that("Fail if more than 3 levels given", {})


# data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - multilevelIV - Parameter data")
test_that("Fail if not data.frame", {
  expect_error(multilevelIV(formula = f.multilevel, data =     ,name.endo="X15"),                regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = NULL,name.endo="X15"),                regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = NA_integer_,name.endo="X15"),         regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = c(y=1:10,   P=1:10),name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = list(y=1:10,P=1:10),name.endo="X15"), regexp = "The above errors were encountered!")
})

test_that("Fail if no rows or cols",{
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(),name.endo="X15"), regexp = "The above errors were encountered!") # no cols
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=integer(), P=integer()),name.endo="X15"), regexp = "The above errors were encountered!")
})

test_that("Fail if wrong data type in any of the formula parts", {
  # Only allow numericals in all relevant columns
  # Factor
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=factor(1:10), P=1:10),name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, P=factor(1:10),name.endo="X15"), regexp = "The above errors were encountered!"))
  # Characters
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=as.character(1:10), P=1:10, stringsAsFactors=F),name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, P=as.character(1:10), stringsAsFactors=F),name.endo="X15"), regexp = "The above errors were encountered!")
  # Logicals (as indicate dichotomous variable (=factor))
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=as.logical(0:9), P=1:10),name.endo="X15"), regexp = "The above errors were encountered!")
  expect_error(multilevelIV(formula = f.multilevel, data = data.frame(y=1:10, P=as.logical(0:9)),name.endo="X15"), regexp = "The above errors were encountered!")
})

# ** TODO: not silent yet
# test_that("Allow wrong data type in irrelevant columns", {
#   # Allow wrong data types in unused columns
#   expect_silent(multilevelIV(formula = f.multilevel, verbose = FALSE, name.endo = "X15",
#                          data = cbind(dataMultilevelIV,
#                                       unused1=as.logical(0:4), unused2=as.character(1:5),unused3=as.factor(1:5), stringsAsFactors = F)))
# })



# verbose ----------------------------------------------------------------------
context("Inputchecks - multilevelIV - Parameter verbose")
test.single.logical(function.to.test = multilevelIV, parameter.name="verbose",
                    formula=f.multilevel, function.std.data=dataMultilevelIV)


# summary model parameter ------------------------------------------------------
context("Inputchecks - multilevelIV summary - Parameter model")
res.ml <- multilevelIV(formula = f.multilevel, name.endo = "X15", data = dataMultilevelIV, verbose = FALSE)
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

