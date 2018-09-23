# TEST RUNABILITY ==================================================================================================================================================================

context("higherMomentsIV - Runability")
data("dataHigherMoments")

test_that("Works with a single IIV",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=lnx,iiv=gp, X1),data = dataHigherMoments))
})

test_that("Works with a two IIVs",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp, X1)+IIV(iiv=y2,X2), data = dataHigherMoments))
})

test_that("Works with 3 IIVs",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(g=lnx,iiv=gy,X1)+IIV(iiv=y2)+IIV(iiv=g, g=x3,X2),
                                data = dataHigherMoments))
})

test_that("Works with two exo in IIV",{
  expect_silent(res.higher <- higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=g, X1, X2),data = dataHigherMoments))
  expect_silent(res.higher <- higherMomentsIV(y~X1+X2+P|P|IIV(g=x2, iiv=gp,X1,X2)+
                                                IIV(g=1/x, iiv=gy,X1,X2),data = dataHigherMoments))
})

test_that("Works without intercept", {
  expect_silent(res.higher <- higherMomentsIV(y~X1+X2+P-1|P|IIV(g=x2, iiv=gp, X1)+IIV(iiv=y2,X2),data = dataHigherMoments))
  # Did not fit any
  expect_false("(Intercept)" %in% coef(res.higher))
})

test_that("Works without exo data for IIV that do not require them",{
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=y2),data = dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=p2),data = dataHigherMoments))
  expect_silent(higherMomentsIV(y~X1+X2+P|P|IIV(iiv=yp),data = dataHigherMoments))
})

test_that("Returns object of class ivreg and rendo.ivreg",{
  expect_silent(res.higher <- higherMomentsIV(y~X1+X2+P|P|IIV(x=x2, iiv=gp, X1),data = dataHigherMoments))
  expect_s3_class(res.higher, "rendo.ivreg")
  expect_s3_class(res.higher, "ivreg")
  # REndo class has to be first
  expect_equal(class(res.higher, c("rendo.ivreg", "ivreg")))
})

test_that("Every g works together with every ivv", {
  allowed.iiv <- c("g", "gp", "gy", "yp", "p2", "y2")
  allowed.g  <- c("x2", "x3", "lnx", "1/x")
  all.combs  <- expand.grid(g=allowed.g, iiv=allowed.iiv)

  # Try all combinations on function
  # for(args in l.g.iiv.args){
  for(i in seq(NROW(all.combs))){
    iiv <- all.combs[i, "iiv"]
    g   <- all.combs[i, "g"]
    if(any(iiv %in%  c("yp", "p2", "y2"))){
      # no g and no X required
      f <- as.Formula(paste0("y~X1 + X2 + P | P | IIV(iiv=",iiv,")"))
      expect_silent(higherMomentsIV(f,data = dataHigherMoments))
    }
    else{
      f <- as.Formula(paste0("y~X1 + X2 + P | P | IIV(g=",g,",iiv=",iiv,",X1)"))
      expect_silent(higherMomentsIV(f,data = dataHigherMoments))
    }
  }
})
