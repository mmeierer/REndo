# Required data -------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataMultilevelIV")
f.multilevel.L3 <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 + X11 | CID) + (1 | SID) | endo(X15)
f.multilevel.L2 <- y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 + X32 + X33 + (1 | SID) | endo(X15)
context("S3methods - multilevelIV - S3methods")
# ** TODO: add expect_silent
res.ml.L3 <- multilevelIV(formula = f.multilevel.L3, data = dataMultilevelIV, verbose = FALSE)
res.ml.L2 <- multilevelIV(formula = f.multilevel.L2, data = dataMultilevelIV, verbose = FALSE)

all.L3.models <- c("REF", "FE_L2", "FE_L3", "GMM_L2", "GMM_L3")
all.L2.models <- c("REF", "FE_L2", "GMM_L2")

test_that("Summary works for every model for L3 case",{
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

test_that("Has default argument REF",{
  default.arg <- eval(formals(REndo:::summary.rendo.multilevel)[["model"]])
  expect_equal(default.arg, "REF")
})



# Cannot use .test.s3methods.basic.structure as to many deviations ---------------------------
#   Copy and adapt
names.coef <- c("(Intercept)","X11","X12","X13","X14","X15","X21","X22","X23","X24","X31","X32","X33")

test_that("nobs", {
  expect_silent(res.nobs <- nobs(res.ml.L3))
  expect_is(res.nobs, "integer")
  expect_equal(res.nobs, nrow(dataMultilevelIV))
})

test_that("fitted", {
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

fct.check.vcov.structure <- function(vcov){
  expect_type(vcov, "list")
  expect_true(length(vcov) == 2)
  expect_named(vcov, c("V", "W"), ignore.order = TRUE)
  V <- vcov$V
  W <- vcov$W
  # Checks that named sparse matrices
  expect_s4_class(V, "dgCMatrix")
  expect_true(all(rownames(V) %in% rownames(dataMultilevelIV)))
  expect_true(all(colnames(V) %in% rownames(dataMultilevelIV)))
  expect_s4_class(W, "dgCMatrix")
  expect_true(all(rownames(W) %in% rownames(dataMultilevelIV)))
  expect_true(all(colnames(W) %in% rownames(dataMultilevelIV)))
}

test_that("vcov", {
  expect_silent(res.vcov <- vcov(res.ml.L3))
  fct.check.vcov.structure(res.vcov)
})

test_that("summary() - vcov", {
  for(m in all.L3.models)
    fct.check.vcov.structure(vcov(summary(res.ml.L3)))
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

# ** TODO **
# test_that("summary() object structure",
