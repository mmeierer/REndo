test.data.latentIV <- function(dataLatentIV){

  test_that("Fail if not data.frame", {
    expect_error(latentIV(formula = y ~ P, data =     ),                regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = NULL),                regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = NA_integer_),         regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = c(y=1:10,   P=1:10)), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = list(y=1:10,P=1:10)), regexp = "The above errors were encountered!")
  })

  test_that("Fail if no rows or cols",{
    expect_error(latentIV(formula = y ~ P, data = data.frame()), regexp = "The above errors were encountered!") # no cols
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=integer(), P=integer())), regexp = "The above errors were encountered!")
  })

  test_that("Fail if wrong data type in any of the formula parts", {
    # Only allow numericals in all relevant columns
    # Factor
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=factor(1:10), P=1:10)), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, P=factor(1:10)), regexp = "The above errors were encountered!"))
    # Characters
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=as.character(1:10), P=1:10, stringsAsFactors=F)), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, P=as.character(1:10), stringsAsFactors=F)), regexp = "The above errors were encountered!")
    # Logicals (as indicate dichotomous variable (=factor))
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=as.logical(0:9), P=1:10)), regexp = "The above errors were encountered!")
    expect_error(latentIV(formula = y ~ P, data = data.frame(y=1:10, P=as.logical(0:9)), regexp = "The above errors were encountered!"))
  })

  test_that("Allow wrong data type in irrelevant columns", {
    # Allow wrong data types in unused columns
    expect_silent(latentIV(formula = y ~ P, start.params = c("(Intercept)"=2.9, P = -0.852),
                                   data = cbind(dataLatentIV,
                                                unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = F)))
  })
}
