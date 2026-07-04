# Required data ----------------------------------------------------------------------------------------------------------------------
data("dataHigherMoments")

# Runability ----------------------------------------------------------------------------------------------------------------------
context("Runability - higherMomentsIV - Runability")
test_that("Works with a single IIV",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=lnx,iiv=gp, X1,X2),data = dataHigherMoments, verbose = FALSE))
})

test_that("Works with a two IIVs",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, X1)+IIV(iiv=y2), data = dataHigherMoments, verbose = FALSE))
})

test_that("Works with 3 IIVs",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=lnx,iiv=gy,X1)+IIV(iiv=y2)+IIV(iiv=g, g=x3,X2),
                                data = dataHigherMoments, verbose = FALSE))
})


test_that("Works with transformation in IIV",{
  expect_silent(higherMomentsIV(y~X1+log(X2)+P|P|IIV(g=x2, iiv=gp,log(X2))+IIV(iiv=y2),
                                data = dataHigherMoments, verbose = FALSE))
  expect_silent(higherMomentsIV(y~log(X1)+X2+P|P|IIV(g=x2, iiv=gp,log(X1))+IIV(iiv=y2),
                                data = dataHigherMoments, verbose = FALSE))
})


test_that("Works with non-numeric in exogenous not in IIV", {
  # Factor/Chars/ Logicals (as indicate dichotomous variable (=factor))
  # No exo used in IIV
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2),data= data.frame(y=1:10, X1=factor(1:10), X2=1:10, P=1:10), verbose=FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2),data = data.frame(y=1:10, X1=as.character(1:10), X2=1:10, P=1:10, stringsAsFactors=FALSE), verbose=FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2),data= data.frame(y=1:10, X1=as.logical(0:9), X2=1:10, P=1:10), verbose=FALSE))
  # Other exo used in IIV
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X2),data= data.frame(y=1:10, X1=factor(1:10), X2=1:10, P=1:10), verbose=FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X2),data=data.frame(y=1:10, X1=as.character(1:10), X2=1:10, P=1:10, stringsAsFactors=FALSE), verbose=FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=g, X2),data= data.frame(y=1:10, X1=as.logical(0:9), X2=1:10, P=1:10), verbose=FALSE))
})

test_that("Works with NA in not needed columns", {
  dataHigherMoments.na <- dataHigherMoments
  dataHigherMoments.na[5, "X2"] <- NA_real_
  expect_silent(higherMomentsIV(y~X1+P|P|IIV(g=lnx,iiv=gp, X1), verbose = FALSE,
                                data = dataHigherMoments.na))

  dataHigherMoments.inf <- dataHigherMoments
  dataHigherMoments.inf[5, "X2"] <- Inf
  expect_silent(higherMomentsIV(y~X1+P|P|IIV(g=lnx,iiv=gp, X1), verbose = FALSE,
                                data = dataHigherMoments.inf))

})

test_that("Multiple exo in single IIV equal single IIV with multiple exo", {
  expect_silent(res.single  <- higherMomentsIV(y~X1+X2+X3+P|P|IIV(g=x2,iiv=g,X1)+IIV(g=x2,iiv=g,X2)+IIV(g=x2,iiv=g,X3),
                                data = cbind(dataHigherMoments, X3=1:10), verbose = FALSE))
  expect_silent(res.multi   <- higherMomentsIV(y~X1+X2+X3+P|P|IIV(g=x2,iiv=g,X1, X2, X3),
                                               data = cbind(dataHigherMoments, X3=1:10), verbose = FALSE))
  expect_silent(res.multi.2 <- higherMomentsIV(y~X1+X2+X3+P|P|IIV(g=x2,iiv=g,X1, X2) + IIV(g=x2,iiv=g,X3),
                                               data = cbind(dataHigherMoments, X3=1:10), verbose = FALSE))
  expect_equal(coef(res.single), coef(res.multi))
  expect_equal(coef(res.single), coef(res.multi.2))
})


test_that("Works with two exo in IIV",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, X2),data = dataHigherMoments, verbose = FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g,X1,X2) +
                                                IIV(g=1/x, iiv=gy,X1,X2),data = dataHigherMoments, verbose = FALSE))
})

test_that("Works iiv=gp/gy and >1 exo reg",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=gp, X1, X2),data = dataHigherMoments, verbose = FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2,iiv=gy, X1, X2),data = dataHigherMoments, verbose = FALSE))
})

test_that("Works with external IIV",{
  # 1 IIV
  expect_silent(higherMomentsIV(y~X1+P|P|IIV(g=lnx,iiv=gp, X1)|X2,data = dataHigherMoments, verbose = FALSE))
  expect_silent(higherMomentsIV(y~X1+P|P|IIV(g=lnx,iiv=gp, X1)|eiv,data = cbind(dataHigherMoments, eiv=1:10), verbose = FALSE))
  # 2 IIV
  expect_silent(higherMomentsIV(y~X2+P|P|IIV(g=lnx,iiv=gp, X2)+IIV(g=x2,iiv=gp, X2)|X1,data = dataHigherMoments, verbose = FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=lnx,iiv=gp, X2)+IIV(g=x2,iiv=gp, X2)|eiv,
                                data = cbind(dataHigherMoments, eiv=1:10), verbose = FALSE))
})


test_that("Works without intercept", {
  expect_silent(res.higher <- higherMomentsIV(y~X1+X2+P-1|P|IIV(g=x2, iiv=gp, X1)+IIV(iiv=y2),
                                              data = dataHigherMoments, verbose = FALSE))
  # Did not fit any
  expect_false("(Intercept)" %in% coef(res.higher))
})

test_that("Works without exo data for IIV that do not require them",{
  # Add X1+X2 as EIV to avoid warning from too few regressors
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2)|X1+X2,data = dataHigherMoments, verbose = FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=p2)|X1+X2,data = dataHigherMoments, verbose = FALSE))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=yp)|X1+X2,data = dataHigherMoments, verbose = FALSE))
})

test_that("Returns object of class ivreg and rendo.ivreg",{
  expect_silent(res.higher <- higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, X1,X2),
                                              data = dataHigherMoments, verbose = FALSE))
  expect_s3_class(res.higher, "rendo.ivreg")
  expect_s3_class(res.higher, "ivreg")
  # REndo class has to be first
  expect_equal(class(res.higher), c("rendo.ivreg", "ivreg"))
})


# ***TODO: CHeck that print are correct for highermoments.
# ***TODO: check that IIVs columns are named correctly regardless of the order in which they were specified


test_that("Every g works together with every ivv", {
  allowed.iiv <- c("g", "gp", "gy", "yp", "p2", "y2")
  allowed.g   <- c("x2", "x3", "lnx", "1/x")
  all.combs   <- expand.grid(g=allowed.g, iiv=allowed.iiv)

  # Try all combinations on function
  #   add X1+X2 as EIV to avoid too few IV warning
  for(i in seq(NROW(all.combs))){
    iiv <- all.combs[i, "iiv"]
    g   <- all.combs[i, "g"]
    if(any(iiv %in%  c("yp", "p2", "y2"))){
      # no g and no X required
      f <- Formula::as.Formula(paste0("y~X1 + X2 + P | P | IIV(iiv=",iiv,")|X1+X2"))
      expect_silent(higherMomentsIV(f,data = dataHigherMoments, verbose = FALSE))
    }
    else{
      f <- Formula::as.Formula(paste0("y~X1 + X2 + P | P | IIV(g=",g,",iiv=",iiv,",X1)|X1+X2"))
      expect_silent(higherMomentsIV(f,data = dataHigherMoments, verbose = FALSE))
    }
  }
})
