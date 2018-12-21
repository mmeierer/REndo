# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")
f.multilevel.L3 <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 | CID) + (1 | SID) | endo(X15)
f.multilevel.L2 <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 | SID) | endo(X15)

context("S3methods - multilevelIV - S3methods")
expect_silent(res.ml.L3 <- multilevelIV(formula = f.multilevel.L3, data = dataMultilevelIV, verbose = FALSE))
expect_silent(res.ml.L2 <- multilevelIV(formula = f.multilevel.L2, data = dataMultilevelIV, verbose = FALSE))

all.L3.models <- c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3")
all.L2.models <- c("REF", "FE_L2", "GMM_L2")

# Summary ----------------------------------------------------------------------------------------------------------------------------------

test_that("Summary works for every model for L3 case",{
  expect_silent(summary(res.ml.L3)) #non given
  for(m in all.L3.models)
    expect_silent(summary(res.ml.L3, model = m))
})

test_that("Summary works for every L2 model for L2 case",{
  for(m in all.L2.models)
    expect_silent(summary(res.ml.L2, model = m))
})

test_that("Summary fails for L3 models in L2 object",{
  expect_error(summary(res.ml.L2, model = "FE_L3"), regexp = "The above errors were encountered!")
  expect_error(summary(res.ml.L2, model = "GMM_L3"), regexp = "The above errors were encountered!")
})


test_that("summary() - coef", {
  for(m in all.L3.models){
    expect_silent(sum.coef <- coef(summary(res.ml.L3, model=m)))
    # right cols
    expect_true(ncol(sum.coef) == 4)
    expect_true(all(colnames(sum.coef) != ""))
    # right rows
    expect_true(nrow(sum.coef) == nrow(coef(res.ml.L3)))
    expect_true(all(rownames(sum.coef) == rownames(coef(res.ml.L3))))
  }
})

fct.check.vcov.structure <- function(res.model, vcov){
  res.attr <- attributes(vcov)
  expect_named(res.attr, c("dim", "dimnames"))
  expect_length(res.attr$dimnames, 2)
  expect_equal(res.attr$dim, c(nrow(coef(res.model)), nrow(coef(res.model))) )
  expect_equal(res.attr$dimnames[[1]], rownames(coef(res.model)))
  expect_equal(res.attr$dimnames[[2]], rownames(coef(res.model)))
}
test_that("summary() - vcov", {
  for(m in all.L3.models)
    fct.check.vcov.structure(res.model = res.ml.L3, vcov=vcov(summary(res.ml.L3, model=m)))
})

test_that("S3 functions have all models as default argument model", {
  for(fct in c("fitted.rendo.multilevel", "residuals.rendo.multilevel",
               "vcov.rendo.multilevel", "confint.rendo.multilevel",
               "summary.rendo.multilevel")){
    default.arg <- eval(formals(get(x=fct, envir = getNamespace("REndo")))[["model"]])
    expect_equal(default.arg, c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3"))
  }
})



# vcov ----------------------------------------------------------------------------------------------------------------------------------
test_that("vcov works for every model for L3 case", {
  expect_silent(res.vcov <- vcov(res.ml.L3))
  for(m in all.L3.models)
    expect_silent(res.vcov <- vcov(res.ml.L3, m))
})

test_that("vcov works for every model for L2 case", {
  expect_silent(res.vcov <- vcov(res.ml.L2)) # no given
  for(m in all.L2.models)
    expect_silent(res.vcov <- vcov(res.ml.L2, m))
})

test_that("vcov fails for L3 models in L2 object",{
  expect_error(vcov(res.ml.L2, model = "FE_L3"), regexp = "The above errors were encountered!")
  expect_error(vcov(res.ml.L2, model = "GMM_L3"), regexp = "The above errors were encountered!")
})


test_that("vcov structure", {
  expect_silent(res.vcov <- vcov(res.ml.L3))
  fct.check.vcov.structure(res.model = res.ml.L3, vcov=res.vcov)
})




# Basic S3 functionalities -----------------------------------------------------------------------------

# Cannot use .test.s3methods.basic.structure as to many deviations
#   Copy and adapt
names.coef <- c("(Intercept)","X11","X12","X13","X14","X15","X21","X22","X23","X24","X31","X32","X33")

test_that("nobs", {
  expect_silent(res.nobs <- nobs(res.ml.L3))
  expect_is(res.nobs, "integer")
  expect_equal(res.nobs, nrow(dataMultilevelIV))
})

test_that("fitted", {
  expect_silent(fitted(res.ml.L3)) #non given
  for(m in all.L3.models){
    # Residuals
    expect_silent(res <- fitted(res.ml.L3, model=m))
    expect_length(res, nrow(dataMultilevelIV))
    expect_named(res, rownames(dataMultilevelIV), ignore.order = TRUE)
    expect_type(res, "double")
    # alias
    expect_silent(res <- fitted.values(res.ml.L3, model=m))
    expect_length(res, nrow(dataMultilevelIV))
    expect_named(res, rownames(dataMultilevelIV), ignore.order = TRUE)
    expect_type(res, "double")
  }
})

test_that("residuals", {
  expect_silent(residuals(res.ml.L3)) #non given
  for(m in all.L3.models){
    # Residuals
    expect_silent(res <- residuals(res.ml.L3, model=m))
    expect_length(res, nrow(dataMultilevelIV))
    expect_named(res, rownames(dataMultilevelIV), ignore.order = TRUE)
    expect_type(res, "double")
    # alias
    expect_silent(res <- resid(res.ml.L3, model=m))
    expect_length(res, nrow(dataMultilevelIV))
    expect_named(res, rownames(dataMultilevelIV), ignore.order = TRUE)
    expect_type(res, "double")
  }
})


test_that("coef", {
  # coef
  expect_silent(res.coef <- coef(res.ml.L3))
  expect_named(res.coef[,1], names.coef, ignore.order = TRUE)
  expect_type(res.coef, "double")
  # alias
  expect_silent(res.coef <- coefficients(res.ml.L3))
  expect_named(res.coef[,1], names.coef, ignore.order  = TRUE)
  expect_type(res.coef, "double")
})

# This breaks too many things when used for lm and ivreg models.
#   reduced to the minimum
test_that("formula", {
  expect_silent(res.form <- formula(res.ml.L3))
  expect_true("formula" %in% class(res.form))
})

# test_that("formula", {
#   expect_silent(res.form <- formula(res.ml.L3))
#   expect_type(res.form, "language")
#   expect_s3_class(res.form, "formula")
#   expect_s3_class(res.form, "Formula")
#   expect_equal(res.form, Formula::as.Formula(input.form))
#   res.attr <- attributes(res.form)
#   expect_named(res.attr, c("class", ".Environment", "lhs", "rhs"))
#   expect_equal(res.attr$.Environment, environment(input.form)) # not 100% sure about this one
#   # # contains no dot
#   expect_false(any(all.vars(res.form) == "."))
# })


test_that("Printing methods", {
  # Just that they work and return their input
  expect_output(res <- show(res.ml.L3))
  expect_null(res)

  expect_output(res <- print(res.ml.L3))
  expect_identical(res, res.ml.L3)

  # Summary
  for(m in all.L3.models){
    expect_silent(res.sum <- summary(res.ml.L3, model=m))

    expect_output(res <- show(res.sum))
    expect_null(res)

    expect_output(res <- print(res.sum))
    expect_equal(res, res.sum)
  }
})


# confint ------------------------------------------------------------------------------------------------------------------------
# confint structure is correct
.test.s3methods.confint.multilevel(res.model = res.ml.L2)
.test.s3methods.confint.multilevel(res.model = res.ml.L3)


test_that("confint works for all L2 models", {
  expect_silent(confint(object = res.ml.L2)) #non given
  for(m in all.L2.models){
    expect_silent(res.ci <- confint(object = res.ml.L2, model=m))
    expect_true(!is.null(res.ci))
    expect_true(all.equal(rownames(res.ci), rownames(coef(res.ml.L2))))
    expect_true(all.equal(colnames(res.ci), c("2.5 %", "97.5 %")))
    expect_true(nrow(res.ci) == nrow(coef(res.ml.L2)))
  }
})

test_that("confint works for all L3 models", {
  expect_silent(confint(object = res.ml.L3)) #non given
  for(m in all.L3.models){
    expect_silent(res.ci <- confint(object = res.ml.L3, model=m))
    expect_true(!is.null(res.ci))
    expect_true(all.equal(rownames(res.ci), rownames(coef(res.ml.L3))))
    expect_true(all.equal(colnames(res.ci), c("2.5 %", "97.5 %")))
    expect_true(nrow(res.ci) == nrow(coef(res.ml.L3)))
  }
})
