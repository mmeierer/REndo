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

# test_that("Summary prints about SE unavailable", {
#   expect_warning(res.latent <- latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=2), data = dataLatentIV),
# regexp=)
#   expect_
# })

test_that("Works with start.params given", {
  expect_silent(latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=-2), data = dataLatentIV, verbose=FALSE))
})


test_that("Same results with start.params swapped", {
  expect_silent(res.lat.1 <- latentIV(formula = y~P, start.params = c("(Intercept)"=1, P=-2), data = dataLatentIV, verbose=FALSE))
  expect_silent(res.lat.2 <- latentIV(formula = y~P, start.params = c(P=-2, "(Intercept)"=1), data = dataLatentIV, verbose=FALSE))
  expect_identical(res.lat.1, res.lat.2)
})
