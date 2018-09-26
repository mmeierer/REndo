# TEST RUNABILITY ==================================================================================================================================================================

context("latentIV - Runability")
data("dataLatentIV")

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
  expect_false("(Intercept)" %in% rownames(coef(summary(res.no.i))))

  # Different result from with intercept
  expect_silent(res.w.i <- latentIV(formula = y~P, data = dataLatentIV, verbose=FALSE))
  expect_false(isTRUE(all.equal(coef(res.no.i), coef(res.w.i))))
})

test_that("Works with start.params given", {
  expect_silent(latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=-2), data = dataLatentIV, verbose=FALSE))
})

test_that("Same results with start.params swapped", {
  expect_silent(res.lat.1 <- latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=-2), data = dataLatentIV, verbose=FALSE))
  expect_silent(res.lat.2 <- latentIV(formula = y~P, start.params = c(P=-2, "(Intercept)"=1), data = dataLatentIV, verbose=FALSE))
  expect_identical(coef(res.lat.1), coef(res.lat.2))
})

test_that("Fails graciously for bad start.params", {
  expect_error(latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=9999), data = dataLatentIV, verbose=FALSE),
               regexp = "Failed to optimize the log-likelihood function with error")
})

# test_that("Summary prints about SE unavailable", {
#   expect_warning(res.latent <- latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=2), data = dataLatentIV),
# regexp=)
#   expect_
# })


test_that("Hessian cannot be solved - produce warning + cannot do vcov", {
  # As of print the hessian contains no NAs but simply cannot be solved
  set.seed(0xcaffee)
  expect_warning(res.lat.warn <- latentIV(formula = y ~ K-1, data = cbind(dataLatentIV, K=rnorm(nrow(dataLatentIV))), start.params = c(K=1.23)),
                 regexp = "cannot be solved for the standard errors. All SEs set to NA.")
  # can run summary just fine but SE are not available -> no zscore/pvals
  expect_silent(sum.coef <- coef(summary(res.lat.warn)))
  expect_true(all(is.na(sum.coef[, c(2,3,4)]))) # SE/zscore/pval all NA for all coefs
})
