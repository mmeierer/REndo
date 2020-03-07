# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

# Runability ----------------------------------------------------------------------------------------------------------------------
context("Runability - latentIV - Runability")
test_that("Works with intercept", {
  expect_silent(latentIV(formula = y~P, data = dataLatentIV, verbose=FALSE))
})

test_that("Verbose produces output", {
  expect_message(latentIV(formula = y~P, data = dataLatentIV, verbose=TRUE),
                regexp = "No start parameters were given. The linear model")
})

test_that("Works without intercept", {
  # Works
  expect_silent(res.no.i <- latentIV(formula = y~P-1, data = dataLatentIV, verbose=FALSE))
  # Has no intercept in coefs
  expect_false("(Intercept)" %in% coef(res.no.i))
  # Also not in summary
  expect_false("(Intercept)" %in% rownames(coef(suppressWarnings(summary(res.no.i)))))

  # Different result from with intercept
  expect_silent(res.w.i <- latentIV(formula = y~P, data = dataLatentIV, verbose=FALSE))
  expect_false(isTRUE(all.equal(coef(res.no.i)["P"], coef(res.w.i)["P"])))
  expect_false(isTRUE(all.equal(coef(res.no.i), coef(res.w.i))))
})

test_that("Works with start.params given", {
  expect_silent(latentIV(formula = y~P, start.params = c("(Intercept)"=2.5, P=-0.5), data = dataLatentIV, verbose=FALSE))
})


test_that("Works with start.params and transformations", {
  expect_silent(latentIV(formula = y~I(P+1), start.params = c("(Intercept)"=2.5, "I(P + 1)"=-0.5), data = dataLatentIV, verbose=FALSE))
})


test_that("Same results with start.params swapped", {
  expect_silent(res.lat.1 <- latentIV(formula = y~P, start.params = c("(Intercept)"=2.5, P=-0.5), data = dataLatentIV, verbose=FALSE))
  expect_silent(res.lat.2 <- latentIV(formula = y~P, start.params = c(P=-0.5, "(Intercept)"=2.5), data = dataLatentIV, verbose=FALSE))
  expect_identical(coef(res.lat.1), coef(res.lat.2))
})

test_that("Fails graciously for bad start.params", {
  expect_error(latentIV(formula = y~P, start.params = c("(Intercept)"=10e99, P=10e99), data = dataLatentIV, verbose=FALSE),
               regexp = "Failed to optimize the log-likelihood function with error")
})

test_that("Works with function in lhs", {
  expect_silent(latentIV(formula = I(y+1)~P, data = dataLatentIV, verbose = FALSE))
})

test_that("Works with all endo transformed", {
  expect_silent(latentIV(formula = y~I(P/2), data = dataLatentIV, verbose = FALSE))
})

test_that("Works with proper optimx.args", {
  expect_silent(latentIV(optimx.args = list(itnmax = 1000), formula = y~P, data = dataLatentIV, verbose = FALSE))
  expect_silent(latentIV(optimx.args = list(itnmax = 1000, control=list(kkttol=0.01)), formula = y~P, data = dataLatentIV, verbose = FALSE))
})


# Well, this is not possible: All columns are always needed (y and P)
# test_that("Works with NA in not needed columns", {
# })


test_that("Summary prints about SE unavailable", {
  # model fitting possible without warnings
  expect_silent(res.latent <- latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=9999), verbose = FALSE,data = dataLatentIV))

  expect_warning(res.sum <- summary(res.latent), regexp = "For some parameters the standard error could not be calculated.")
  expect_output(print(res.sum), all = FALSE,
                regexp = "because the Std. Errors are unavailable")
  expect_true(anyNA(coef(res.sum)))
})


test_that("Stops if lm fails for start",{
  # linearly dependend on intercept
  expect_error(latentIV(y~K, data=data.frame(y=1:100, K=rep(2, 100))),
               regexp = "The start parameters could not be derived by fitting a linear model")
})


# Too stable now :(
# test_that("Hessian cannot be solved - produce warning + cannot do vcov", {
#   # As of print the hessian contains no NAs but simply cannot be solved
#   set.seed(0xcaffee)
#   expect_warning(res.lat.warn <- latentIV(formula = y ~ K-1, data = cbind(dataLatentIV, K=rnorm(nrow(dataLatentIV))),
#                                           start.params = c(K=1.23), verbose = FALSE),
#                  regexp = "Eigenvalue failure after method Nelder-Mea")
#   # Hessian is fine but cannot derive the SEs
#   # Manually set non-finite hessian
#   # res.lat.warn$hessian[3,4] <- NA_real_
#   # can run summary just fine but SE are not available -> no zscore/pvals
#   expect_silent(sum.coef <- coef(summary(res.lat.warn)))
#   expect_true(all(is.na(sum.coef[, c(2,3,4)]))) # SE/zscore/pval all NA for all coefs
#
#   # Summary vcov works but all NA
#   expect_true(all(is.na(vcov(summary(res.lat.warn)))))
#
#   # Cannot do vcov
#   expect_error(vcov(res.lat.warn), reges="The vcov matrix cannot be calculated because the")
#
# })
