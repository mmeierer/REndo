# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataHigherMoments")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("higherMomentsIV - Parameter formula")

test_that("Fail if bad 2nd RHS", {
  # Fail for missing 2nd RHS
  expect_error(higherMomentsIV(y~X1+X2+P||IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for 2nd RHS not in 1st RHS
  expect_error(higherMomentsIV(y~X1+X2|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+P|X2|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P2|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail if all regressors are endogenous
  expect_error(higherMomentsIV(y~X1+P|X1+P|IIV(g=x2, iiv=g, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|X1+X2+P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if bad 3rd RHS", {
  # Fail for missing 3nd RHS
  expect_error(higherMomentsIV(y~X1+X2+P|P), regexp = "The above errors were encountered!")
  # Fail for 3nd RHS not in 1st RHS
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X3), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+P|P|IIV(g=x2, iiv=g, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for 3rd RHS in 2nd RHS
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P+X1|IIV(g=x2, iiv=g, X1, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P+X2|IIV(g=x2, iiv=g, X1, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if >3 RHS",{
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1)|IIV(g=x2, iiv=g, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1)| X2), regexp = "The above errors were encountered!")
})

test_that("Fail if bad LHS", {
  # Fail for missing LHS
  expect_error(higherMomentsIV(~X1+X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for > 1 LHS
  expect_error(higherMomentsIV(y|X1~X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Fail for LHS in RHS and vice-versa
  expect_error(higherMomentsIV(y~y+X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(X1~X1+X2+P|P|IIV(g=x2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if formula contains dot (.)", {
  # Fail if dot (.) is in formula in any part
  expect_error(higherMomentsIV(y~X1+X2+.|P|IIV(g=x2, iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(.~X1+X2+X2|P|IIV(g=x2, iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+X2|.|IIV(g=x2, iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+X2|P|IIV(g=x2, iiv=gp, .), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if no special function", {
  # Not at all
  expect_error(higherMomentsIV(y~X1+X2+P|P|x2+gp+X1, data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|X1+X2), regexp = "The above errors were encountered!")
  # Only for some
  expect_error(higherMomentsIV(y~X1+X2+P|P|+IIV(g=x2, iiv=gp, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if missing regressors in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, ), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if missing iiv in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if invalid g in IIV", {
  # Only fail for the iiv's where g is required: c("g","gp","gy",
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")

  # and warning for the others: "yp","p2","y2") + g
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=yp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=p2, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=y2, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if invalid iiv in IIV", {
  # Allowed: c("g","gp","gy","yp","p2","y2")
  # Imaginary iiv
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gpp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=y3, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # quoted iiv
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv="gp", X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if multiple iiv in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,(iiv=g), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g+g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if multiple g in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, g=x3, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, g=x3, iiv=g, X1)+IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if invalid colname in IIV", {
  # missing
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # wrong
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X3), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X2, P), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, X3), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if non-matching dimensions for gp", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, X1,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P+X1|IIV(g=x2, iiv=gp, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if non existent special function", {
  # Misspelled
  expect_error(higherMomentsIV(y~X1+X2+P|P|iiv(g=x2, iiv=g, X1,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1,X2)+iiv(g=x2, iiv=gy, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # Imaginary
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1,X2)+brunz(), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if function inside special", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, log(X1)), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, log(X1+X2)), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, I(X1^2)), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x^2, iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=log(x), iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g*p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=g2*p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=x2*p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if special outside RHS3", {
  expect_error(higherMomentsIV(y~IIV(g=x2, iiv=g,X1)+X2+P|P|IIV(g=x2, iiv=g,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV(g=x2, iiv=yp,y)~X1+X2+P|P|IIV(g=x2, iiv=g,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|IIV(g=x2, iiv=yp,P)|IIV(g=x2, iiv=g,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

# **TODO Fail if underlying assumptions violated
# **TODO Silent when g missing but iiv does not need it
# **TODO Silent without intercept

# data -----------------------------------------------------------------------------------------------------------------------------------------------------------------
context("higherMomentsIV - Parameter data")

# ?? Data may not be named after iiv or g to avoid confusion when naming. could interfere with unique(list) in readout
# test.data.highermomentsiv(dataHigherMoments=dataHigherMoments)

#' # g ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' context("higherMomentsIV - Parameter g")
#' test.character.vector.basicstructure(function.to.test=higherMomentsIV, parameter.name="g", formula=,
#'                                      function.std.data=dataHigherMoments, additional.args=list(IIV="gp"))
#'
#' test_that("Fail if g not of  c(x2, x3, lnx, 1/x)", {
#'   # Other input
#'   expect_error(higherMomentsIV(g =c("x4"),     formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("1/x2"),   formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("logx"),   formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("log(x)"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("ln(x)"),  formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("x"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("1"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   # Wrongly spelled
#'   expect_error(higherMomentsIV(g =c("X2"),  formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("X3"),  formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("LNx"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("Lnx"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("1/X"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#' })
#' test_that("Fail if g is more than a single elemement", {
#'   expect_error(higherMomentsIV(g =c("1/x", "x2"),       formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("1/x", "lnx"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("1/x", NA_character_),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(g =c("1/x", "x2", "x3"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
#' })
#'
#' # IIV ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' context("higherMomentsIV - Parameter IIV")
#' test.character.vector.basicstructure(function.to.test=higherMomentsIV, parameter.name="IIV", formula=,
#'                                      function.std.data=dataHigherMoments, additional.args=list(g="x2"))
#'
#' test_that("Fail if IIV not of  c(g, gp, gy, yp, p2, y2)", {
#'   # Other input
#'   expect_error(higherMomentsIV(IIV =c("g", "yp", "a"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(IIV =c("x", "yp", "p2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(IIV =c("x", "yp", "p2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#'   # Wrongly spelled
#'   expect_error(higherMomentsIV(IIV =c("YP", "p2"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(IIV =c("GY", "yp", "p2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(IIV =c("gg", "yp", "P2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#' })
#'
#' test_that("Fail if IIV elements are given more than once", {
#'   expect_error(higherMomentsIV(IIV =c("yp", "yp", "p2"),    formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(IIV =c("g", "yp", "p2", "g"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#'   expect_error(higherMomentsIV(IIV =c("p2", "p2"),          formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
#' })
#'
#' #' @importFrom utils combn
#' test_that("Can run with every combination of g and IIV", {
#'   allowed.iiv <- c("g", "gp", "gy", "yp", "p2", "y2")
#'   l.all.combs.iiv <- lapply(seq(length(allowed.iiv)), function(m)combn(allowed.iiv, m))
#'   # l.all.combs.iiv is list of matrices with combinations in columns
#'   # Extract each possible iiv combination to single list
#'   l.all.single.combs.iiv <- lapply(l.all.combs.iiv, function(iiv.m){
#'     lapply(seq(ncol(iiv.m)), function(col.i){iiv.m[, col.i]})})
#'   # flatten to single list
#'   l.all.single.combs.iiv <- unlist(l.all.single.combs.iiv, recursive = F)
#'
#'   # combine each of these columns with all possible single gs
#'   gs <- c("x2", "x3", "lnx", "1/x")
#'   l.g.iiv.args <- lapply(gs, function(single.g){
#'     lapply(l.all.single.combs.iiv, function(single.iiv){
#'       return(list(g=single.g, IIV = single.iiv)) })})
#'   # flatten to single list
#'   l.g.iiv.args <- unlist(l.g.iiv.args, recursive = F)
#'
#'   # Try all combinations on function
#'   for(args in l.g.iiv.args){
#'     expect_silent(l <- do.call(higherMomentsIV, c(list(formula=y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments), args)))
#'   }
#' })
