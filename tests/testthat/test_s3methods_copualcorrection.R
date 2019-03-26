# TEST S3 METHODS ==================================================================================================================================================================

# The tests are only required for the optim LL case and the discrete only case

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load from test/testthat folder
data("dataCopDis2") # only use 2 case version
data("dataCopCont")
data("dataCopCont2")
data("dataCopDisCont")

# C1 optim LL case ----------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 1 continuous")

c1.input.form <- y ~ X1 + X2 + P|continuous(P) # needed as var to compare against
expect_warning(res.c1 <- copulaCorrection(formula = c1.input.form, data = dataCopCont, verbose=FALSE, num.boots = 2),
               regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)

test_that("Test S3 methods only off cran because needs 100bootstraps for confint", {
# Put in separate test_that to be able to skip it on cran
  # need to fit 100 boots for confint
  skip_on_cran()

  expect_warning(res.c1.many <- copulaCorrection(formula = c1.input.form, data = dataCopCont, verbose=FALSE, num.boots = 100),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)

  # Basic rendo.boots part
  test.s3methods.rendoboots(res.model=res.c1.many, input.form=c1.input.form, function.std.data=dataCopCont,
                              req.df=6,full.coefs=c("(Intercept)", "X1", "X2", "P", "rho","sigma"))
})

# Case 1 specific methods
test_that("case 1 specific S3 methods work", {
  expect_silent(logLik(res.c1))
  expect_silent(AIC(res.c1))
  expect_silent(BIC(res.c1))
  expect_s3_class(res.c1$res.optimx, "optimx")
})

# Case 1 summary
test_that("Case 1 summary has additional specific structure", {
  expect_silent(res.sum <- summary(res.c1))
  expect_s3_class(res.sum, "summary.rendo.copula.c1")
  expect_true(all(c("start.params","KKT1", "KKT2", "AIC", "BIC", "log.likelihood", "conv.code") %in%
                    names(res.sum)))
  expect_is(res.sum$start.params, "numeric")
  expect_named(object = res.sum$start.params, expected = names(coef(res.c1, complete = TRUE)),
               ignore.order = TRUE)
  expect_is(res.sum$KKT1, "logical")
  expect_is(res.sum$KKT2, "logical")
  expect_is(res.sum$AIC, "numeric")
  expect_is(res.sum$BIC, "numeric")
  expect_is(res.sum$log.likelihood, "numeric")
  expect_is(res.sum$conv.code, "numeric")
})



# C2 2 continuous case -------------------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 2 continuous")

c2.input.form <- y ~ X1 + X2 + P1+P2|continuous(P1, P2)
expect_warning(res.c2 <- copulaCorrection(formula = c2.input.form, data = dataCopCont2, verbose=FALSE, num.boots=100),
               regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)
test.s3methods.rendoboots(res.model=res.c2, input.form=c2.input.form, function.std.data=dataCopCont2,
                          full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))

test_that("Case 2 summary has additional specific structure", {
  expect_silent(res.sum <- summary(res.c2))
  expect_s3_class(res.sum, "summary.rendo.copula.c2")
  expect_true(is.list(res.sum))
  expect_true(all(c("names.vars.continuous", "names.vars.discrete") %in%
                    names(res.sum)))
  expect_is(res.sum$names.vars.continuous, "character")
  expect_is(res.sum$names.vars.discrete, "character")

})

# C2 Discrete case --------------------------------------------------------------------------------------------------------------------------------------------------------------------

context("S3methods - copulaCorrection - 2 discrete")

d.input.form <- y ~ X1 + X2 + P1+P2|discrete(P1, P2)
expect_warning(res.d <- copulaCorrection(formula = d.input.form, data = dataCopDis2, verbose=FALSE, num.boots=100),
               regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

test.s3methods.rendoboots(res.model =res.d, input.form=d.input.form, function.std.data=dataCopDis2,
                            full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))

test_that("Case 2 summary has additional specific structure", {
  expect_silent(res.sum <- summary(res.d))
  expect_s3_class(res.sum, "summary.rendo.copula.c2")
  expect_true(is.list(res.sum))
  expect_true(all(c("names.vars.continuous", "names.vars.discrete") %in%
                    names(res.sum)))
  expect_is(res.sum$names.vars.continuous, "character")
  expect_is(res.sum$names.vars.discrete, "character")

})


# C2 Mixed case -------------------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 1 continuous, 1 discrete")

cd.input.form <- y ~ X1 + X2 + P1+P2|discrete(P1)+continuous(P2)
expect_warning(res.cd <- copulaCorrection(formula = cd.input.form, data = dataCopDisCont, verbose=FALSE, num.boots = 100),
               regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)
test.s3methods.rendoboots(res.model =res.cd, input.form=cd.input.form, function.std.data=dataCopDisCont,
                          full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))

test_that("Case 2 summary has additional specific structure", {
  expect_silent(res.sum <- summary(res.cd))
  expect_s3_class(res.sum, "summary.rendo.copula.c2")
  expect_true(is.list(res.sum))
  expect_true(all(c("names.vars.continuous", "names.vars.discrete") %in%
                    names(res.sum)))
  expect_is(res.sum$names.vars.continuous, "character")
  expect_is(res.sum$names.vars.discrete, "character")

})
