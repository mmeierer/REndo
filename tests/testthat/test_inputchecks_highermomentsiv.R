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
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV+IIV(g=x2, iiv=gp, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
})


test_that("Fail if missing regressors in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, ), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if missing iiv in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = NULL, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = NA_character_, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = character(0), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv = "", X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if missing g in IIV", {
  # iiv gy requires it
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NA_character_, iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=character(0), iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})



test_that("Fail if invalid iiv in IIV", {
  # Allowed: c("g","gp","gy","yp","p2","y2")
  # Imaginary iiv
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gpp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=p, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=y3, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  # some type
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=c(g, gy), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=c("g", "g"), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=list(g), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
})

test_that("Fail if multiple iiv in IIV", {
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,(iiv=g), X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
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

# **TODO after check is built in
# test_that("Fail if non-matching dimensions for gp", {
#   expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, X1,X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
#   expect_error(higherMomentsIV(y~X1+X2+P|P+X1|IIV(g=x2, iiv=gp, X2), data=dataHigherMoments), regexp = "The above errors were encountered!")
# })

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

# .g vs iiv ---------------------------------------------------------------------------------------------------------

test_that("Fail if missing g but IIV does require it", {
  # "g","gp","gy",
  # missing
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")

  # open
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")

  # NULL
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=g, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=gp, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")
  expect_error(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=gy, X1), data=dataHigherMoments), regexp = "The above errors were encountered!")

})

test_that("Warning if g but IIV does not need it", {
  # warning for the others: "yp","p2","y2"
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=yp, X1), data=dataHigherMoments), regexp = "ignored", all = TRUE)
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=p2, X1), data=dataHigherMoments), regexp = "ignored", all = TRUE)
  expect_warning(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=y2, X1), data=dataHigherMoments), regexp = "ignored", all = TRUE)
})
test_that("Silent if no g and IIV does not need it", {
  # Missing
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=yp, X1), data=dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=p2, X1), data=dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2, X1), data=dataHigherMoments))

  # Open
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=yp, X1), data=dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=p2, X1), data=dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=,iiv=y2, X1), data=dataHigherMoments))

  # NULL
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=yp, X1), data=dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=p2, X1), data=dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=NULL,iiv=y2, X1), data=dataHigherMoments))
})


test_that("Every g works with every ivv", {
  allowed.iiv <- c("g", "gp", "gy", "yp", "p2", "y2")
  allowed.g  <- c("x2", "x3", "lnx", "1/x")
  all.combs  <- expand.grid(g=allowed.g, iiv=allowed.iiv)

  # Try all combinations on function
  # for(args in l.g.iiv.args){
  for(i in seq(NROW(all.combs))){
    iiv <- all.combs[i, "iiv"]
    g   <- all.combs[i, "g"]
    if(any(iiv %in%  c("yp", "p2", "y2"))){
      # no g required
      f <- as.Formula(paste0("y~X1 + X2 + P | P | IIV(iiv=",iiv,",X1)"))
      expect_silent(higherMomentsIV(f,data = dataHigherMoments))
    }
    else{
      f <- as.Formula(paste0("y~X1 + X2 + P | P | IIV(g=",g,",iiv=",iiv,",X1)"))
      expect_silent(higherMomentsIV(f,data = dataHigherMoments))
    }
  }
})

# **TODO Silent without intercept
# **TODO Fail if underlying assumptions violated

# data -----------------------------------------------------------------------------------------------------------------------------------------------------------------
context("higherMomentsIV - Parameter data")

# ?? Data may not be named after iiv or g to avoid confusion when naming. could interfere with unique(list) in readout
# test.data.highermomentsiv(dataHigherMoments=dataHigherMoments)
