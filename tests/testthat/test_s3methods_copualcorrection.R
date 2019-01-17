# TEST S3 METHODS ==================================================================================================================================================================

# The tests are only required for the optim LL case and the discrete only case

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load from test/testthat folder
data("dataCopDis2") # only use 2 case version
data("dataCopCont")
data("dataCopCont2")
data("dataCopDisCont")

# Discrete case --------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Standard S3 methods checks
context("S3methods - copulaCorrection - 1 discrete")

d.input.form <- y ~ X1 + X2 + P1+P2|discrete(P1, P2)
expect_silent(res.d <- copulaCorrection(formula = d.input.form, data = dataCopDis2, verbose=FALSE))

test_that("Discrete confint placeholder", {
  # Put in separate test_that to be able to skip it on cran
  skip_on_cran()

  #include placeholder test as otherwise the whole block will be skipped because it is regarded as empty
  expect_true(TRUE)

  test.s3methods.lm.models(res.lm.model=res.d, input.form=d.input.form, function.std.data=dataCopDis2,
                         full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))
})



# Discrete case confint --------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - discrete confint")
# Do not use .S3.helper.confint because also num.sumulations can be supplied

# Get results to work with
expect_silent(res.dis.only <- copulaCorrection(formula=y~X1+X2+P1+P2|discrete(P1, P2),
                                               data=dataCopDis2, verbose = FALSE))

test_that("Confint works with different alphas", {
  expect_silent(ci.99 <- confint(res.dis.only, level = 0.99))
  expect_silent(ci.95 <- confint(res.dis.only, level = 0.95))
  expect_silent(ci.90 <- confint(res.dis.only, level = 0.90))
  expect_silent(ci.70 <- confint(res.dis.only, level = 0.70))

  # Level works and provides different values
  expect_false(isTRUE(all.equal(ci.99,ci.95,check.attributes=FALSE)))
  expect_false(isTRUE(all.equal(ci.95,ci.90,check.attributes=FALSE)))
  expect_false(isTRUE(all.equal(ci.90,ci.70,check.attributes=FALSE)))

  # Rightly named, all same
  expect_equal(rownames(ci.99), names(coef(res.dis.only)))
  expect_equal(rownames(ci.99), rownames(ci.95))
  expect_equal(rownames(ci.95), rownames(ci.90))
  expect_equal(rownames(ci.90), rownames(ci.70))
})

test_that("Confint works with character param", {
  # Single
  for(p in names(coef(res.dis.only)))
    expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = p)), expected = p)
  # Multiple
  p <- c("X1","X2","P1")
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = p)), expected = p)
  p <- c("X2","P1","P2")
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = p)), expected = p)
  # All - excplicitely
  p <- c("(Intercept)","X1","X2","P1","P2", "PStar.P1", "PStar.P2")
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = p)), expected = p)
  # All - implicitely (ie none given)
  expect_equal(rownames(confint(res.dis.only)), expected = p)
})

test_that("Confint works with integer param", {
  p <- names(coef(res.dis.only))
  # Single
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = 3)), expected = p[3])
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = 5)), expected = p[5])
  # Sequence
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = 1:3)), expected = p[1:3])
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm =c(2,4,6))), expected = p[c(2,4,6)])
  # All - excplicitely
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = seq(length(p)))), expected = p)
  # All - implicitely (ie none given)
  expect_equal(rownames(confint(res.dis.only)), expected = p)

  # Minus removes
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = -2)), expected = p[-2])
  expect_equal(rownames(confint(res.dis.only,num.simulations=10, parm = -c(2,4))), expected = p[-c(2,4)])
  # Remove all
  expect_null(rownames(confint(res.dis.only,num.simulations=10, parm = -seq(length(p)))))
})

# same behavior as lm
test_that("confint NA if unknown parm", {
  # Unknown character
  expect_true(all(is.na( confint(res.dis.only,num.simulations=10, parm = "abc") )))
  expect_true(all(is.na( confint(res.dis.only,num.simulations=10, parm = c("abc", "zcgd")) )))
  # Wrong indices
  expect_true(all(is.na( confint(res.dis.only,num.simulations=10, parm = 50:100))))
  expect_true(all(is.na( confint(res.dis.only,num.simulations=10, parm = 99) )))
  # Part of it are known
  expect_true(all(names(coef(res.dis.only)) %in%
                rownames(confint(res.dis.only,num.simulations=10, parm = 1:100))))
  expect_true(!all(is.na( confint(res.dis.only,num.simulations=10, parm = 1:100))))
})

test_that("num.simulations has integer default value", {
  expect_is(eval(formals(REndo:::confint.rendo.pstar.lm)[["num.simulations"]]), "integer")
})


test_that("Not discrete only returns normal lm confint", {
  expect_silent(res.dis.cont <- copulaCorrection(formula=y~X1+X2+P1+P2|discrete(P1)+continuous(P2),
                                                 data=dataCopDisCont, verbose = FALSE))
  expect_identical(confint(res.dis.cont), confint.lm(res.dis.cont))
})


# C1 optim LL case ----------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 1 continuous")

# Test all S3 methods
c1.input.form <- y ~ X1 + X2 + P|continuous(P) # needed as var to compare against

expect_warning(res.c1 <- copulaCorrection(formula = c1.input.form, data = dataCopCont, verbose=FALSE, num.boots = 2),
               regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)

test.s3methods.rendooptimLL(res.model=res.c1, input.form=c1.input.form, function.std.data=dataCopCont,
                            req.df=6,full.coefs=c("(Intercept)", "X1", "X2", "P", "rho","sigma"))

# C2 case -------------------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 2 continuous")

c2.input.form <- y ~ X1 + X2 + P1+P2|continuous(P1, P2)
expect_silent(res.c2 <- copulaCorrection(formula = c2.input.form, data = dataCopCont2, verbose=FALSE))
test.s3methods.lm.models(res.lm.model=res.c2, input.form=c2.input.form, function.std.data=dataCopCont2,
                         full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))


# Mixed case -------------------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 1 continuous, 1 discrete")

cd.input.form <- y ~ X1 + X2 + P1+P2|discrete(P1)+continuous(P2)
expect_silent(res.cd <- copulaCorrection(formula = cd.input.form, data = dataCopDisCont, verbose=FALSE))
test.s3methods.lm.models(res.lm.model=res.cd, input.form=cd.input.form, function.std.data=dataCopDisCont,
                         full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))


# confint for all other than discrete ---------------------------------------------------------------------------------
test_that("Throws warning if num.simulations not needed", {
  expect_warning(confint(res.c2, num.simulations = 100), regexp = "is ignored because this model")
  expect_warning(confint(res.cd, num.simulations = 100), regexp = "is ignored because this model")
})


