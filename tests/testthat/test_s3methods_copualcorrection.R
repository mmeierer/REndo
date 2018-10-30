# TEST S3 METHODS ==================================================================================================================================================================

# The tests are only required for the optim LL case and the discrete only case

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load from test/testthat folder
load(file = "./old_dataCopDis.rda")
load(file = "./old_dataCopC1.rda")
load(file = "./old_dataCopC2.rda")
data("dataCopDisCont")

# Discrete case --------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("copulaCorrection - S3 Methods / confint")
# Get results to work with
expect_silent(res.dis.only <- copulaCorrection(formula=y~X1+X2+P1+P2|discrete(P1, P2),
                                               data=dataCopDis, verbose = FALSE))

test_that("Confint works with different alphas", {
  expect_silent(ci.99 <- confint(res.dis.only, level = 0.99))
  expect_silent(ci.95 <- confint(res.dis.only, level = 0.95))
  expect_silent(ci.90 <- confint(res.dis.only, level = 0.90))
  expect_silent(ci.70 <- confint(res.dis.only, level = 0.70))

  # Level works and provides different values
  expect_false(isTRUE(all.equal(ci.99,ci.95,check.attributes=F)))
  expect_false(isTRUE(all.equal(ci.95,ci.90,check.attributes=F)))
  expect_false(isTRUE(all.equal(ci.90,ci.70,check.attributes=F)))

  # Rightly named, all same
  expect_equal(rownames(ci.99), names(coef(res.dis.only)))
  expect_equal(rownames(ci.99), rownames(ci.95))
  expect_equal(rownames(ci.95), rownames(ci.90))
  expect_equal(rownames(ci.90), rownames(ci.70))

  # CI have to be larger for higher level (with very high probability)
  # expect_gt(ci.99[, 2] - ci.99[, 2], ) ***maybe do as well if time

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
test_that("NA if unknown parm", {
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
  expect_is(eval(formals(REndo2:::confint.rendo.pstar.lm)[["num.simulations"]]), "integer")
})


test_that("Not discrete only returns normal lm confint", {
  expect_silent(res.dis.cont <- copulaCorrection(formula=y~X1+X2+P1+P2|discrete(P1)+continuous(P2),
                                                 data=dataCopDisCont, verbose = FALSE))
  expect_identical(confint(res.dis.cont), confint.lm(res.dis.cont))
})


# C1 optim LL case --------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Test all S3 methods
c1.input.form <- y ~ X1 + X2 + P|continuous(P) # needed as var to compare against
expect_silent(res.c1 <- copulaCorrection(formula = c1.input.form, data = dataCopC1, verbose=FALSE))

test.s3methods.rendooptimLL(res.model=res.c1, input.form=c1.input.form, function.std.data=dataCopC1,
                            req.df=6,full.coefs=c("(Intercept)", "X1", "X2", "P", "rho","sigma"))
