# TEST S3 METHODS ==================================================================================================================================================================

# The tests are only required for the optim LL case and the discrete only case

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load from test/testthat folder
data("dataCopDis2") # only use 2 case version
data("dataCopCont")
data("dataCopCont2")
data("dataCopDisCont")

fct.helper.check.copula.correction.summary <- function(res.c){
  test_that("summary has basic structure", {
    expect_silent(res.sum <- summary(res.c))
    expect_s3_class(res.sum, "summary.rendo.copula.correction")
    expect_true(is.list(res.sum))
    expect_true(all(c("names.vars.continuous", "names.vars.discrete") %in%
                      names(res.sum)))
    expect_is(res.sum$names.vars.continuous, "character")
    expect_is(res.sum$names.vars.discrete, "character")
  })
  # mainly for covr coverage
  test_that("copula correction summary prints", {
    expect_silent(res.sum <- summary(res.c))
    expect_output(res <- show(res.sum))
    expect_null(res)

    expect_output(res <- print(res.sum))
    expect_identical(res, res.sum)
  })
}

# C1 optim LL case ----------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 1 continuous")

c1.input.form <- y ~ X1 + X2 + P|continuous(P) # needed as var to compare against
expect_warning(res.c1 <- copulaCorrection(formula = c1.input.form, data = dataCopCont, verbose=FALSE, num.boots = 2),
               regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)

# Basic copulaCorrection specific correctness of object
fct.helper.check.copula.correction.summary(res.c1)

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

test_that("object structure is c1 specific",{
  # has c1 specific
  expect_true(c("res.optimx") %in% names(res.c1))
  expect_true(c("start.params") %in% names(res.c1))
  expect_s3_class(res.c1$res.optimx, "optimx")
  expect_is(res.c1$start.params, "numeric")
  expect_named(res.c1$start.params, expected = names(coef(res.c1)),
               ignore.order = FALSE, ignore.case = FALSE)

  # does not have c2 specific!
  expect_false(c("res.lm.real.data") %in% names(res.c1))
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


test_that("Case 1 summary prints", {
  expect_silent(res.sum <- summary(res.c1))
  expect_output(res <- show(res.sum))
  expect_null(res)

  expect_output(res <- print(res.sum))
  expect_identical(res, res.sum)
})



# C2 2 continuous case -------------------------------------------------------------------------------------------------------------
fct.helper.check.copula.correction.lm.case <- function(res.c2){
  test_that("object structure is c2 specific",{
    # has c2 specific
    expect_true(c("res.lm.real.data") %in% names(res.c2))
    expect_s3_class(res.c2$res.lm.real.data, "lm")
    # does not have c1 specific!
    expect_false(c("res.optimx") %in% names(res.c2))
    expect_false(c("start.params") %in% names(res.c2))
  })
  test_that("c1 specific s3 methods fail", {
    expect_error(logLik(res.c2),regexp = "No logLik available")
    expect_error(AIC(res.c2),regexp = "No logLik available")
    expect_error(BIC(res.c2),regexp = "No logLik available")
  })
}


context("S3methods - copulaCorrection - 2 continuous")

c2.input.form <- y ~ X1 + X2 + P1+P2|continuous(P1, P2)
expect_warning(res.c2 <- copulaCorrection(formula = c2.input.form, data = dataCopCont2, verbose=FALSE, num.boots=100),
               regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)

test.s3methods.rendoboots(res.model=res.c2, input.form=c2.input.form, function.std.data=dataCopCont2,
                          full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))

# Basic copulaCorrection specific correctness of object
fct.helper.check.copula.correction.lm.case(res.c2)
fct.helper.check.copula.correction.summary(res.c2)


# C2 Discrete case --------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 2 discrete")

d.input.form <- y ~ X1 + X2 + P1+P2|discrete(P1, P2)
expect_warning(res.d <- copulaCorrection(formula = d.input.form, data = dataCopDis2, verbose=FALSE, num.boots=100),
               regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

test.s3methods.rendoboots(res.model =res.d, input.form=d.input.form, function.std.data=dataCopDis2,
                            full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))

# Basic copulaCorrection specific correctness of object
fct.helper.check.copula.correction.lm.case(res.d)
fct.helper.check.copula.correction.summary(res.d)



# C2 Mixed case -------------------------------------------------------------------------------------------------------------
context("S3methods - copulaCorrection - 1 continuous, 1 discrete")

cd.input.form <- y ~ X1 + X2 + P1+P2|discrete(P1)+continuous(P2)
expect_warning(res.cd <- copulaCorrection(formula = cd.input.form, data = dataCopDisCont, verbose=FALSE, num.boots = 100),
               regexp = "It is recommended to run 1000 or more bootstraps.", all=TRUE)
test.s3methods.rendoboots(res.model =res.cd, input.form=cd.input.form, function.std.data=dataCopDisCont,
                          full.coefs=c("(Intercept)", "X1", "X2", "P1", "P2", "PStar.P1", "PStar.P2"))

# Basic copulaCorrection specific correctness of object
fct.helper.check.copula.correction.lm.case(res.cd)
fct.helper.check.copula.correction.summary(res.cd)
