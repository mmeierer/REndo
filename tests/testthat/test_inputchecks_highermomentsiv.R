# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataHigherMoments")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("higherMomentsIV - Parameter formula")
# test.formula.highermomentIV(dataHigherMoments=dataHigherMoments)

context("higherMomentsIV - Parameter data")
# test.data.highermomentsiv(dataHigherMoments=dataHigherMoments)

# g ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("higherMomentsIV - Parameter g")
test.character.vector.basicstructure(function.to.test=higherMomentsIV, parameter.name="g", formula=,
                                     function.std.data=dataHigherMoments, additional.args=list(IIV="gp"))

test_that("Fail if g not of  c(x2, x3, lnx, 1/x)", {
  # Other input
  expect_error(higherMomentsIV(g =c("x4"),     formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("1/x2"),   formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("logx"),   formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("log(x)"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("ln(x)"),  formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("x"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("1"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  # Wrongly spelled
  expect_error(higherMomentsIV(g =c("X2"),  formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("X3"),  formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("LNx"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("Lnx"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("1/X"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
})
test_that("Fail if g is more than a single elemement", {
  expect_error(higherMomentsIV(g =c("1/x", "x2"),       formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("1/x", "lnx"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("1/x", NA_character_),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(g =c("1/x", "x2", "x3"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, IIV = "gp"), regexp = "The above errors were encountered!")
})

# IIV ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("higherMomentsIV - Parameter IIV")
test.character.vector.basicstructure(function.to.test=higherMomentsIV, parameter.name="IIV", formula=,
                                     function.std.data=dataHigherMoments, additional.args=list(g="x2"))

test_that("Fail if IIV not of  c(g, gp, gy, yp, p2, y2)", {
  # Other input
  expect_error(higherMomentsIV(IIV =c("g", "yp", "a"), formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV =c("x", "yp", "p2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV =c("x", "yp", "p2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
  # Wrongly spelled
  expect_error(higherMomentsIV(IIV =c("YP", "p2"),      formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV =c("GY", "yp", "p2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV =c("gg", "yp", "P2"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
})

test_that("Fail if IIV elements are given more than once", {
  expect_error(higherMomentsIV(IIV =c("yp", "yp", "p2"),    formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV =c("g", "yp", "p2", "g"),formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(IIV =c("p2", "p2"),          formula = y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments, g = "x2"), regexp = "The above errors were encountered!")
})

#' @importFrom utils combn
test_that("Can run with every combination of g and IIV", {
  allowed.iiv <- c("g", "gp", "gy", "yp", "p2", "y2")
  l.all.combs.iiv <- lapply(seq(length(allowed.iiv)), function(m)combn(allowed.iiv, m))
  # l.all.combs.iiv is list of matrices with combinations in columns
  # Extract each possible iiv combination to single list
  l.all.single.combs.iiv <- lapply(l.all.combs.iiv, function(iiv.m){
    lapply(seq(ncol(iiv.m)), function(col.i){iiv.m[, col.i]})})
  # flatten to single list
  l.all.single.combs.iiv <- unlist(l.all.single.combs.iiv, recursive = F)

  # combine each of these columns with all possible single gs
  gs <- c("x2", "x3", "lnx", "1/x")
  l.g.iiv.args <- lapply(gs, function(single.g){
    lapply(l.all.single.combs.iiv, function(single.iiv){
      return(list(g=single.g, IIV = single.iiv)) })})
  # flatten to single list
  l.g.iiv.args <- unlist(l.g.iiv.args, recursive = F)

  # Try all combinations on function
  for(args in l.g.iiv.args){
    expect_silent(l <- do.call(higherMomentsIV, c(list(formula=y ~ X1 + X2 + P | X1 + X2 |X1, data=dataHigherMoments), args)))
  }
})
