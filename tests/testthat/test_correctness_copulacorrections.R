# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopDis2")
data("dataCopCont")
data("dataCopCont2")
data("dataCopDisCont")


# Transformations in formula ---------------------------------------------------------------------------------------------------
context("Correctness - copulaCorrection - Formula transformations")


test_that("Formula transformations are correct", {

  # C2, Dis, DisCont
  set.seed(0xcaffee)
  expect_warning(correct.res.c1      <- copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_silent(correct.res.c2      <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2))
  expect_silent(correct.res.dis     <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2))
  expect_silent(correct.res.disCont <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont))

  # Can handle transformations in LHS
  set.seed(0xcaffee)
  # C1
  data.altered   <- dataCopCont
  data.altered$y <- exp(data.altered$y)
  expect_warning(res.trans.lhs  <- copulaCorrection(formula= log(y) ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=data.altered),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_equal(coef(res.trans.lhs), coef(correct.res.c1))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.c1)))
  # C2
  data.altered   <- dataCopCont2
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs      <- copulaCorrection(formula= log(y) ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.lhs), coef(correct.res.c2))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.c2)))
  # Dis
  data.altered   <- dataCopDis2
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs     <- copulaCorrection(formula= log(y) ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.lhs), coef(correct.res.dis))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.dis)))
  # DisCont
  data.altered   <- dataCopDisCont
  data.altered$y <- exp(data.altered$y)
  expect_silent(res.trans.lhs <- copulaCorrection(formula= log(y) ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.lhs), coef(correct.res.disCont))
  expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.disCont)))

  # Can handle transformations in RHS1
  set.seed(0xcaffee)
  # C1
  data.altered    <- dataCopCont
  data.altered$P <- exp(data.altered$P)
  expect_warning(res.trans.rhs  <- copulaCorrection(formula= y ~ X1+X2+log(P)|continuous(log(P)), verbose = FALSE, num.boots=2, data=data.altered),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_equal(coef(res.trans.rhs), coef(correct.res.c1), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.c1)), check.attributes=FALSE)
  # C2
  data.altered    <- dataCopCont2
  data.altered$P2 <- exp(data.altered$P2)
  expect_silent(res.trans.rhs      <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|continuous(P1, log(P2)), verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.rhs), coef(correct.res.c2), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.c2)), check.attributes=FALSE)
  # Dis
  data.altered    <- dataCopDis2
  data.altered$P2 <- exp(data.altered$P2)
  expect_silent(res.trans.rhs   <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|discrete(P1, log(P2)),   verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.rhs), coef(correct.res.dis), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.dis)), check.attributes=FALSE)
  # Discont
  data.altered    <- dataCopDisCont
  data.altered$P2 <- exp(data.altered$P2)
  expect_silent(res.trans.rhs <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|continuous(P1)+discrete(log(P2)), verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.rhs), coef(correct.res.disCont), check.attributes=FALSE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.disCont)), check.attributes=FALSE)
})

