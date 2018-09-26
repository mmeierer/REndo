test.formula.latentIV <- function(dataLatentIV){

  test_that("Fail if no formula object is passed",  {
    expect_error(latentIV(formula = data.frame(1:3), data = dataLatentIV), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = NULL,            data = dataLatentIV), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = NA,              data = dataLatentIV), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula =   ,              data = dataLatentIV), regexp = "The above errors were encountered!")
  })

  test_that("Fail if bad 1st RHS", {
    # Fail if > 1 regressor in RHS
    expect_error(latentIV(formula = y ~ I + P ,       data = cbind(I=1, dataLatentIV)),              regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ X1 + X2 + P , data = cbind(X1=1.23, X2=2.34, dataLatentIV)), regexp = "The above errors were encountered!")
  })

  test_that("Fail if bad 2nd RHS", {
    # Fail if ANY 2nd RHS
    expect_error(latentIV(formula = y ~ P | P, data = dataLatentIV), regexp = "The above errors were encountered!")
  })

  test_that("Fail if bad LHS", {
    # Fail for missing LHS
    expect_error(latentIV(formula = ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
    # Fail for > 1 LHS
    expect_error(latentIV(formula = y1 + y2      ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y1 + y2 + y3 ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y1 | y2      ~ P, data = dataLatentIV), regexp = "The above errors were encountered!") # multipart LHS
    # Fail for LHS in RHS and vice-versa
    expect_error(latentIV(formula = P ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ y, data = dataLatentIV), regexp = "The above errors were encountered!")
    # Fail for dot in LHS as not more than one LHS allowed
    # expect_error(latentIV(formula = . ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
  })

  test_that("Fail if formula variables are not in data", {
    # Fail if any regressors not in data (RHS1, RHS2, LHS1)
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, X1=1:10, X2=1:10)), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = data.frame(X1=1:10,X2=1:10, P=1:10)),  regexp = "The above errors were encountered!")
  })

  test_that("Fail if formula contains dot (.)", {
    # Fail if dot (.) is in formula in any part
    expect_error(latentIV(formula = . ~ P, data = dataLatentIV), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ ., data = dataLatentIV), regexp = "The above errors were encountered!")
  })

  # **** FIX LATENTIV
  # test_that("Transformations can be handled and are correct", {
  #   correct.res <- latentIV(formula = y ~ P, data = dataLatentIV)
  #   # Can handle transformations in LHS
  #   data.altered   <- dataLatentIV
  #   data.altered$y <- data.altered$y*2.5
  #   res.trans.lhs <- latentIV(formula = I(y/2.5) ~ P, data = data.altered)
  #   expect_equal(coef(res.trans.lhs), coef(correct.res), check.attributes=F)
  #   # expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res))) # will vary greatly because of limited bootstrappings
  #
  #   # Can handle transformations in RHS1
  #   data.altered    <- dataLatentIV
  #   data.altered$X1 <- data.altered$P+111
  #   res.trans.rhs1  <- latentIV(formula = y ~ I(P-111), data = data.altered)
  #   expect_equal(coef(res.trans.rhs1), coef(correct.res), check.attributes=F)
  #   # expect_equal(coef(summary(res.trans.rhs1)), coef(summary(correct.res))) # will vary greatly because of limited bootstrappings
  # })

  test_that("Fail if formula variables are not in data", {
    # Fail if any regressors not in data (RHS1, RHS2, LHS1)
    expect_error(latentIV(formula= y ~ P ,data=data.frame(y=1:10)), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula= y ~ P ,data=data.frame(P=1:10)), regexp = "The above errors were encountered!")
  })
}
