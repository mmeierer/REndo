# TEST INPUT CHECKS ================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopCont")
data("dataCopCont2")
data("dataCopDis")
data("dataCopDis2")
data("dataCopDisCont")

# formula --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - copulaCorrection - Parameter formula")

test_that("Fail if no formula object is passed",{
  expect_error(copulaCorrection(formula= ,                data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= NULL,            data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= NA,              data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= data.frame(1:3), data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if bad 2nd RHS", {
  # Fail for missing 2nd RHS
  expect_error(copulaCorrection(formula= y ~ X1 + X2 + P1+P2, data=dataCopCont2), regexp = "The above errors were encountered!")
  # Fail for 2nd RHS not in 1st RHS
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P1|continuous(P2),                    data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P2|continuous(P1),                    data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ y ~ X1 + X2 + P1|continuous(P1)+continuous(P2), data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P2|continuous(P1)+continuous(P2),     data=dataCopCont2), regexp = "The above errors were encountered!")

  # Fail if not exactly the same in model
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + log(P1)|continuous(P1), data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1 + X2 + P1|continuous(log(P1)), data=dataCopCont2), regexp = "The above errors were encountered!")

  # expect_error(function.to.test(formula = y ~ X1 + X2 + P|., data = function.std.data), regexp = "The above errors were encountered!") # dot version
})

test_that("Fail if > 2 RHS", {
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|continuous(P1)|continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|continuous(P1)|discrete(P2),  data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|discrete(P1)|discrete(P2),    data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula=  y ~ X1+X2+P1+P2|continuous(P1)|discrete(P2)|continuous(X1),    data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if bad LHS", {
  # Fail for missing LHS
  expect_error(copulaCorrection(formula=   ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  # Fail for > 1 LHS
  expect_error(copulaCorrection(formula= y1 + y2  ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y1 + y2 + y3 ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y1 | y2 ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")

  # Fail for LHS in RHS and vice-versa
  expect_error(copulaCorrection(formula= X1 ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y  ~  y+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y  ~  y+X2+P1+P2|continuous(y)+continuous(P2),data=dataCopCont2),  regexp = "The above errors were encountered!")

  # Fail for dot in LHS as not more than one LHS allowed
  # expect_error(function.to.test(formula = . ~ X1 + X2 + P|P, data = function.std.data), regexp = "The above errors were encountered!")
})



test_that("Fail if formula contains dot (.)", {
  # Fail if dot (.) is in formula in any part
  expect_error(copulaCorrection(formula= . ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ . |continuous(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(.)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(.),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(.),data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if no special function", {
  # Non at all
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P1,data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P1+P2,data=dataCopCont2), regexp = "The above errors were encountered!")
  # Only for some
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+P2,data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+P2,data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P2 + discrete(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|P1 + continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
})


test_that("Fail if regressor in different special functions", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P1),   data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+continuous(P1),   data=dataCopCont2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1,P2)+discrete(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1,P2)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P1, P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+continuous(P1, P2),data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if non existent special function", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|brunz(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+brunz(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|brunz(P1)+continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
})


test_that("Fail if function across special", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)*continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)*discrete(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)*discrete(P2),data=dataCopCont2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|I(continuous(P1)/continuous(P2)),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|I(discrete(P1)/discrete(P2)),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|I(continuous(P1)/discrete(P2)),data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if empty special", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+discrete(),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1)+continuous(),data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if special in RHS1", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|discrete(P1),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P1),data=dataCopCont2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|continuous(P1),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P1),data=dataCopCont2), regexp = "The above errors were encountered!")

  expect_error(copulaCorrection(formula= y ~ X1+X2+continuous(P1)+P2|continuous(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+discrete(P1)+P2|discrete(P2),data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if special in LHS", {
  expect_error(copulaCorrection(formula= continuous(y) ~ X1+X2+P1+P2|continuous(P1),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= continuous(y) ~ X1+X2+P1+P2|P1,data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if var is in both specials", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P1),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2)+discrete(P1),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2)+continuous(P1),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2)+continuous(X1, P1),data=dataCopCont2), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2)+discrete(X1, P1),data=dataCopCont2), regexp = "The above errors were encountered!")
})

test_that("Fail if formula variables are not in data", {
  # Fail if any regressors not in data (RHS1, RHS2, LHS1)
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, P1=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, X2=1:10,  P1=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(y=1:10, X1=1:10, X2=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+continuous(P2),data=data.frame(X1=1:10,X2=1:10, P1=1:10,  P2=1:10)), regexp = "The above errors were encountered!")
})


test_that("Fails if transformations are not in special", {
  # C1
  expect_error(copulaCorrection(formula= y ~ X1+X2+I(P/2)|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),
                 regexp = "The above errors were encountered!")
  # C2, Dis, DisCont
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P1, P2), verbose = FALSE, data=dataCopCont2),regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2),regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont),regexp = "The above errors were encountered!")
})

test_that("Fails if transformations are not named exactly the same", {
  # C1
  expect_error(copulaCorrection(formula= y ~ X1+X2+I(P/2)|continuous(I(P/1)), verbose = FALSE, num.boots=2, data=dataCopCont),
               regexp = "The above errors were encountered!")
  # C2, Dis, DisCont
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P1, I(P2/1)), verbose = FALSE, data=dataCopCont2),regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|discrete(P1, I(P2/1)),   verbose = FALSE, data=dataCopDis2),regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P1)+discrete(I(P2/1)), verbose = FALSE, data=dataCopDisCont),regexp = "The above errors were encountered!")
})

test_that("Fails if transformations are done for different endo", {
  # C1
  # expect_warning(copulaCorrection(formula= y ~ X1+X2+I(P/2)|continuous(I(P/1)), verbose = FALSE, num.boots=2, data=dataCopCont),
  #                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # C2, Dis, DisCont
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P2, I(P1/2)), verbose = FALSE, data=dataCopCont2),regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|discrete(P2, I(P1/2)),   verbose = FALSE, data=dataCopDis2),regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P2)+discrete(I(P1/2)), verbose = FALSE, data=dataCopDisCont),regexp = "The above errors were encountered!")
})



# data --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - copulaCorrection - Parameter data")

test_that("Fail if is NA, NULL or missing", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=NULL), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=NA_real_), regexp = "The above errors were encountered!")
})

test_that("Fail if not data.frame", {
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=   c(y=1:10, X1=1:10, X2=1:10, P1=1:10,P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=list(y=1:10, X1=1:10, X2=1:10, P1=1:10,P2=1:10)), regexp = "The above errors were encountered!")
})

test_that("Fail if no rows or cols",{
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=data.frame()), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1),data=data.frame(y=integer(), X1=numeric(), X2=numeric(), P1=integer(), P2=integer())), regexp = "The above errors were encountered!")
})


test_that("Fails if contains any non-finite", {
  # 1 Continuous
  call.args.c1 <- list(formula=y~X1+X2+P|continuous(P))
  test.nonfinite.in.data(dataCopCont,name.col="y", fct=copulaCorrection, call.args = call.args.c1)
  test.nonfinite.in.data(dataCopCont,name.col="X1",fct=copulaCorrection, call.args = call.args.c1)
  test.nonfinite.in.data(dataCopCont,name.col="P", fct=copulaCorrection, call.args = call.args.c1)

  # 2 Continuous
  call.args.c2 <- list(formula=y~X1+X2+P1+P2|continuous(P2))
  test.nonfinite.in.data(dataCopCont2,name.col="y", fct=copulaCorrection,   call.args = call.args.c2)
  test.nonfinite.in.data(dataCopCont2,name.col="X1", fct=copulaCorrection,  call.args = call.args.c2)
  test.nonfinite.in.data(dataCopCont2,name.col="P1", fct=copulaCorrection,  call.args = call.args.c2)
  test.nonfinite.in.data(dataCopCont2,name.col="P2", fct=copulaCorrection,  call.args = call.args.c2)

  # 1 Discrete
  call.args.d1 <- list(formula=y~X1+X2+P|discrete(P))
  test.nonfinite.in.data(dataCopDis,name.col="y", fct=copulaCorrection,   call.args = call.args.d1)
  test.nonfinite.in.data(dataCopDis,name.col="X1", fct=copulaCorrection,  call.args = call.args.d1)
  test.nonfinite.in.data(dataCopDis,name.col="P", fct=copulaCorrection,   call.args = call.args.d1)

  # 2 Discrete
  call.args.d2 <- list(formula=y~X1+X2+P1+P2|discrete(P1))
  test.nonfinite.in.data(dataCopDis2,name.col="y", fct=copulaCorrection,    call.args = call.args.d2)
  test.nonfinite.in.data(dataCopDis2,name.col="X1", fct=copulaCorrection,   call.args = call.args.d2)
  test.nonfinite.in.data(dataCopDis2,name.col="P1", fct=copulaCorrection,   call.args = call.args.d2)
  test.nonfinite.in.data(dataCopDis2,name.col="P2", fct=copulaCorrection,   call.args = call.args.d2)

  # 1 Discrete, 1 Continuous
  call.args.c1.d1 <- list(formula=y~X1+X2+P1+P2|discrete(P1)+continuous(P2))
  test.nonfinite.in.data(dataCopDisCont,name.col="y", fct=copulaCorrection,    call.args = call.args.c1.d1)
  test.nonfinite.in.data(dataCopDisCont,name.col="X1", fct=copulaCorrection,   call.args = call.args.c1.d1)
  test.nonfinite.in.data(dataCopDisCont,name.col="P1", fct=copulaCorrection,   call.args = call.args.c1.d1)
  test.nonfinite.in.data(dataCopDisCont,name.col="P2", fct=copulaCorrection,   call.args = call.args.c1.d1)
})
#
# test_that("Fails if contains any NA", {
#
#   fct.test.na <- function(data, name.col, call.args){
#     data[5, name.col] <- NA_real_
#     call.args <- modifyList(call.args, list(data= data))
#     expect_error(do.call(what = copulaCorrection, args=call.args), regexp = "The above errors were encountered!")
#   }
#
#   # 1 Continuous
#   call.args.c1 <- list(formula=y ~ X1+X2+P|continuous(P))
#   fct.test.na(dataCopCont,name.col="y",  call.args = call.args.c1)
#   fct.test.na(dataCopCont,name.col="X1", call.args = call.args.c1)
#   fct.test.na(dataCopCont,name.col="P",  call.args = call.args.c1)
#
#
#   # 2 Continuous
#   call.args.c2 <- list(formula=y~X1+X2+P1+P2|continuous(P2))
#   fct.test.na(dataCopCont2,name.col="y",   call.args = call.args.c2)
#   fct.test.na(dataCopCont2,name.col="X1",  call.args = call.args.c2)
#   fct.test.na(dataCopCont2,name.col="P1",  call.args = call.args.c2)
#   fct.test.na(dataCopCont2,name.col="P2",  call.args = call.args.c2)
#
#   # 1 Discrete
#   call.args.d1 <- list(formula=y~X1+X2+P|discrete(P))
#   fct.test.na(dataCopDis,name.col="y",   call.args = call.args.d1)
#   fct.test.na(dataCopDis,name.col="X1",  call.args = call.args.d1)
#   fct.test.na(dataCopDis,name.col="P",   call.args = call.args.d1)
#
#   # 2 Discrete
#   call.args.d2 <- list(formula=y~X1+X2+P1+P2|discrete(P1))
#   fct.test.na(dataCopDis,name.col="y",    call.args = call.args.d2)
#   fct.test.na(dataCopDis,name.col="X1",   call.args = call.args.d2)
#   fct.test.na(dataCopDis,name.col="P1",   call.args = call.args.d2)
#   fct.test.na(dataCopDis,name.col="P2",   call.args = call.args.d2)
#
#   # 1 Discrete, 1 Continuous
#   call.args.c1.d1 <- list(formula=y~X1+X2+P1+P2|discrete(P1)+continuous(P2))
#   fct.test.na(dataCopDisCont,name.col="y",    call.args = call.args.c1.d1)
#   fct.test.na(dataCopDisCont,name.col="X1",   call.args = call.args.c1.d1)
#   fct.test.na(dataCopDisCont,name.col="P1",   call.args = call.args.c1.d1)
#   fct.test.na(dataCopDisCont,name.col="P2",   call.args = call.args.c1.d1)
#
# })
#
#
# test_that("Fails if contains any Non-Finite", {
#
#   fct.test.nonfin <- function(data, name.col, call.args){
#     data[5, name.col] <- Inf
#     call.args <- modifyList(call.args, list(data= data))
#     expect_error(do.call(what = copulaCorrection, args=call.args), regexp = "The above errors were encountered!")
#     data[5, name.col] <- -Inf
#     call.args <- modifyList(call.args, list(data= data))
#     expect_error(do.call(what = copulaCorrection, args=call.args), regexp = "The above errors were encountered!")
#     data[5, name.col] <- NaN
#     call.args <- modifyList(call.args, list(data= data))
#     expect_error(do.call(what = copulaCorrection, args=call.args), regexp = "The above errors were encountered!")
#   }
#
#   # 1 Continuous
#   call.args.c1 <- list(formula=y ~ X1+X2+P|continuous(P))
#   fct.test.nonfin(dataCopCont,name.col="y", call.args = call.args.c1)
#   fct.test.nonfin(dataCopCont,name.col="X1", call.args = call.args.c1)
#   fct.test.nonfin(dataCopCont,name.col="P", call.args = call.args.c1)
#
#
#   # 2 Continuous
#   call.args.c2 <- list(formula=y~X1+X2+P1+P2|continuous(P2))
#   fct.test.nonfin(dataCopCont2,name.col="y",   call.args = call.args.c2)
#   fct.test.nonfin(dataCopCont2,name.col="X1",  call.args = call.args.c2)
#   fct.test.nonfin(dataCopCont2,name.col="P1",  call.args = call.args.c2)
#   fct.test.nonfin(dataCopCont2,name.col="P2",  call.args = call.args.c2)
#
#
#   # 1 Discrete
#   call.args.d1 <- list(formula=y~X1+X2+P|discrete(P))
#   fct.test.nonfin(dataCopDis,name.col="y",   call.args = call.args.d1)
#   fct.test.nonfin(dataCopDis,name.col="X1",  call.args = call.args.d1)
#   fct.test.nonfin(dataCopDis,name.col="P",   call.args = call.args.d1)
#
#   # 2 Discrete
#   call.args.d2 <- list(formula=y~X1+X2+P1+P2|discrete(P1))
#   fct.test.nonfin(dataCopDis2,name.col="y",   call.args = call.args.d2)
#   fct.test.nonfin(dataCopDis2,name.col="X1",  call.args = call.args.d2)
#   fct.test.nonfin(dataCopDis2,name.col="P1",   call.args = call.args.d2)
#   fct.test.nonfin(dataCopDis2,name.col="P2",   call.args = call.args.d2)
#
#   # 1 Discrete, 1 Continuous
#   call.args.c1.d1 <- list(formula=y~X1+X2+P1+P2|discrete(P1)+continuous(P2))
#   fct.test.nonfin(dataCopDisCont,name.col="y",    call.args = call.args.c1.d1)
#   fct.test.nonfin(dataCopDisCont,name.col="X1",   call.args = call.args.c1.d1)
#   fct.test.nonfin(dataCopDisCont,name.col="P1",   call.args = call.args.c1.d1)
#   fct.test.nonfin(dataCopDisCont,name.col="P2",   call.args = call.args.c1.d1)
#
# })



test_that("Fail if wrong data type in endogenous formula part", {
  # Only allow numericals in endogenous columns
  # Factor
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=factor(1:10), P2=1:10)  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=factor(1:10))  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=factor(1:10), P2=1:10)  ), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=factor(1:10))  ), regexp = "The above errors were encountered!")
  # Characters
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.character(1:10), P2=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.character(1:10), stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.character(1:10), P2=1:10, stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=   data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.character(1:10), stringsAsFactors=FALSE)), regexp = "The above errors were encountered!")

  # Logicals (as indicate dichotomous variable (=factor))
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.logical(0:9), P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.logical(0:9))), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=as.logical(0:9), P2=1:10)), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=as.logical(0:9))), regexp = "The above errors were encountered!")
})

# test_that("Allow wrong data type in irrelevant columns", {
#   expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),verbose=FALSE, data=
#                                    cbind(dataCopCont2, unused1=as.logical(0:9), unused2=as.character(1:10),unused3=as.factor(1:10), stringsAsFactors = FALSE)))
# })

test_that(paste0("No column is named PStar.ENDO for discrete, >1 continuous, and mixed models PStar.ENDO"), {
  # Discrete case
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P2=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10, PStar.P2=1:10)), regexp = "The above errors were encountered!")
  # Copula continuous 2 case
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P2=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10, PStar.P2=1:10)), regexp = "The above errors were encountered!")
  # Mixed case
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P2=1:10               )), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data= data.frame(y=1:10, X1=1:10, X2=1:10, P1=1:10, P2=1:10, PStar.P1=1:10, PStar.P2=1:10)), regexp = "The above errors were encountered!")
})


test_that("Warn if binomial/dummy data passed in endo regressor", {
  expect_warning(copulaCorrection(formula=y ~ X1+X2+D|discrete(D), num.boots = 2, verbose = FALSE,
                                  data = cbind(dataCopDis2, D = c(0,1))), regexp = "may not be binomial")
})

# verbose --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - copulaCorrection - Parameter verbose")
test.single.logical(function.to.test = copulaCorrection, parameter.name="verbose",
                    formula=y~X1+X2+P1+P2|continuous(P1)+discrete(P2), function.std.data=dataCopCont2)


# num.boots --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - copulaCorrection - Parameter num.boots")
# Single continuous
# Failure tests
test.positive.numeric.whole.number(function.to.test = copulaCorrection, parameter.name="num.boots",
                                   formula=y~X1+X2+P|continuous(P), function.std.data=dataCopCont)

test_that("Warning if num.boots < 1000", {
  # only for continuous 1
  expect_warning(copulaCorrection(num.boots = 2, verbose=FALSE,formula= y ~ X1+X2+P|continuous(P),data=dataCopCont),all=FALSE, regexp = "It is recommended to run 1000 or more bootstraps.")
})

# Warning if num.boots given for any other case
#   Num.boots now needed in all cases
# test_that("Warning if unneeded num.boots given", {
#   # >1 continuous
#   expect_warning(copulaCorrection(num.boots = 10, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=dataCopCont2),all=TRUE, regexp = "Additional parameters given in the ... argument are ignored because they are not needed.")
#   # Mixed
#   expect_warning(copulaCorrection(num.boots = 10, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data=dataCopDisCont), all=TRUE, regexp = "Additional parameters given in the ... argument are ignored because they are not needed.")
#   # Discrete
#   expect_warning(copulaCorrection(num.boots = 10, verbose=FALSE,formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=dataCopDis2),all=TRUE,regexp = "Additional parameters given in the ... argument are ignored because they are not needed.")
# })

# start.params ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - copulaCorrection - Parameter start.params")

test_that("start.params is vector and all numeric", {
  # Any parameter is character, logical, factor, matrix
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = as.character(1), X2 = -2, P = 0),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = as.factor(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = as.logical(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = as.matrix(c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0)),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = complex(1,4,2), X2 = -2, P = 0),
                                           formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
})

test_that("start.params is not NA",{
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = NA_integer_, X2 = -2, P = 0),    formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = NA_real_, X2 = -2, P = 0),       formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = NA_integer_, formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = NA_real_, formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
})

# Leave out because requires full run with 1000 bootstraps as otherwise also a warning is produced
# test_that("start.params is NULL or missing but runs with message", {
#   # Does not work with missing start.params in ...
#   # expect_warning(copulaCorrection(start.params =     , formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont, num.boots = 2, verbose=TRUE))
#   expect_message(copulaCorrection(start.params = NULL, formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont, verbose=TRUE, num.boots=2),regexp = "The linear model", all = FALSE)
# })

test_that("Fails if start.params cannot be derived with lm", {
  expect_error(copulaCorrection(formula = y ~ X1+X2+X3+P |continuous(P), data = cbind(dataCopCont, X3=dataCopCont$X2*2)),
               regexp = "The start parameters could not be derived by fitting a linear model")
})


test_that("start.params is named correctly", {
  # Unnamed vec given
  expect_error(copulaCorrection(start.params = c(2, 1, -2,0),                                formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Partially named vec given
  expect_error(copulaCorrection(start.params = c(2, X1 = 1, X2=-2,P=0),                      formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Wrong case
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, x1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Same param name twice
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X2 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Unrelated name
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X10 = 1, X2 = -2, P = 0),  formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Param missing (main, endo, intercept)
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, P = 0),            formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2),          formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c(X1 = 1, X2 = -2, P = 0),                    formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Intercept spelling
  expect_error(copulaCorrection(start.params = c("Intercept"=2, X1 = 1, X2 = -2, P = 0),     formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(intercept)"=2, X1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P|continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Intercept given but none required
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0),   formula = y ~ X1 + X2 + P -1 |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Additional, not required param given
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, X3 = 99, P = 0), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
})


test_that("start.params with non-numeric exo requires multiple per factor",{
  expect_error(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P),
                                data=cbind(dataCopCont,color=factor(x = c("red", "green", "blue", "white", "yellow"))),
                                start.params = c("(Intercept)"=2, X1=1.5,X2=-3, P=-1,
                                                 color=1)),
               regexp = "The above errors were encountered!")
  # 1 dummy only -> still not only color!
  expect_error(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P),
                                data=cbind(dataCopCont,color=factor(x = c("red", "green"))),
                                start.params = c("(Intercept)"=2, X1=1.5,X2=-3, P=-1,
                                                 color=1)),
               regexp = "The above errors were encountered!")
})

test_that("start.params contains no parameter rho or sigma", {
  # Given additionally
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0, rho=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, P = 0, sigma=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # Given instead of other parameters
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, rho=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, X2 = -2, sigma=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, sigma = -2, P=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, rho = -2, P=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1, sigma = -2, rho=1), formula = y ~ X1 + X2 + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
})


test_that("start.params fails if transformation missing", {
  # Not endo
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1.5, X2 = -3, P = -1), formula = y ~ X1 + exp(X2) + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1.5, X2 = -3, P = -1), formula = y ~ X1 + I(X2/2) + P |continuous(P), data = dataCopCont), regexp = "The above errors were encountered!")
  # in endo
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1.5, X2 = -3, P = -1), formula = y ~ X1 + X2 + exp(P) |continuous(exp(P)), data = dataCopCont), regexp = "The above errors were encountered!")
  expect_error(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1.5, X2 = -3, P = -1), formula = y ~ X1 + X2 + I(P/2) |continuous(I(P/2)), data = dataCopCont), regexp = "The above errors were encountered!")
})


# optimx.args --------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - copulaCorrection - Parameter optimx.args")
test.optimx.args(function.to.test = copulaCorrection, parameter.name = "optimx.args",
                 formula=y ~ X1+X2+P|continuous(P), function.std.data = dataCopCont)

test_that("Has default value empty list()",{
  default.arg <- eval(formals(REndo:::copulaCorrection_optimizeLL)[["optimx.args"]])
  expect_equal(class(default.arg), "list") # S3 class does not work
})



# ...  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
context("Inputchecks - copulaCorrection - Parameter 3-dots")
test_that("Warning if further unneded params are given", {
  # 1 continuous
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P|continuous(P),data=dataCopCont, num.boots=2),all=FALSE, regexp = "are ignored")
  # >1 continuous
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1, P2),data=dataCopCont2, num.boots = 2),all=FALSE, regexp = "are ignored")
  # Mixed
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2),data=dataCopDisCont, num.boots = 2), all=FALSE, regexp = "are ignored")
  # Discrete
  expect_warning(copulaCorrection(abc=123, verbose=FALSE,formula= y ~ X1+X2+P1+P2|discrete(P1, P2),data=dataCopDis2, num.boots = 2),all=FALSE,regexp = "are ignored")
})
