# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataLatentIV")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("latentIV - Parameter formula")
test.formula.latentIV(dataLatentIV=dataLatentIV)

context("latentIV - Parameter data")
test.data.latentIV(dataLatentIV=dataLatentIV)

context("latentIV - Run tests")
test_that("Hessian cannot be solved - produce warning + cannot do vcov", {
  # As of print the hessian contains no NAs but simply cannot be solved
  set.seed(0xcaffee)
  expect_warning(res.lat.warn <- latentIV(formula = y ~ K-1, data = cbind(dataLatentIV, K=rnorm(nrow(dataLatentIV))), start.params = c(K=1.23)),
                 regexp = "cannot be solved for the standard errors. All SEs set to NA.")
  # can run summary just fine but SE are not available -> no zscore/pvals
  expect_silent(sum.coef <- coef(summary(res.lat.warn)))
  expect_true(all(is.na(sum.coef[, c(2,3,4)]))) # SE/zscore/pval all NA for all coefs
})

# start.params ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("latentIV - Parameter start.params")

test_that("start.params is vector and all numeric", {
  # Any parameter is character, logical, factor, matrix
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = as.character(1)), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = as.factor(c("(Intercept)"=2, P = 1)),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = as.logical(c("(Intercept)"=2, P = 1)),   formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = as.matrix(c("(Intercept)"=2, P = 0)),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = complex(1,4,2)),  formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})

test_that("start.params is not NA",{
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = NA_integer_),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = NA_real_),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=NA_integer_, P = 2),    formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=NA_real_, P = 2),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = NA_integer_,                            formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = NA_real_,                               formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})

test_that("start.params is NULL or missing but runs with warning", {
  expect_warning(latentIV(start.params =     , formula = y ~ P, data = dataLatentIV), regexp = "No start parameters were given")
  expect_warning(latentIV(start.params = NULL, formula = y ~ P, data = dataLatentIV), regexp = "No start parameters were given")
})

test_that("start.params is named correctly", {
  # Unnamed vec given
  expect_error(latentIV(start.params = c(2, 1),                          formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Partially named vec given
  expect_error(latentIV(start.params = c(2, P=0),                        formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Wrong case
  expect_error(latentIV(start.params = c("(Intercept)"=2, p = 1),        formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Same param name twice
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 1, P = 2), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Unrelated name
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 1, P2 =3), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Param missing (main, endo, intercept)
  expect_error(latentIV(start.params = c("(Intercept)"=2),               formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c(P = 2),                         formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Intercept spelling
  expect_error(latentIV(start.params = c("Intercept"=2,    P = 0),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(intercept)"=2,  P = 0),       formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Intercept given but none required
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0),
                                           formula = y ~ X1 + X2 + P -1 |P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Additional, not required param given
  expect_error(latentIV(start.params = c("(Intercept)"=2, X1 = 1, P = 0), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})


test_that("start.params contains no parameter named pi1, pi2, theta5, theta6, theta7, theta8", {
  # Given additionally
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta5=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta6=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta7=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta8=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1, theta5=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, theta7=1, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, P = 0, pi1=1, pi2=1, theta5=1, theta6=1, theta7=1, theta8=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  # Given instead of other parameters
  expect_error(latentIV(start.params = c("(Intercept)"=2, pi1=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, pi2=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta5=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta6=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta7=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  expect_error(latentIV(start.params = c("(Intercept)"=2, theta8=1), formula = y ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
})


