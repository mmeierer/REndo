# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopCont")
data("dataCopCont2")
data("dataCopDis2")
data("dataCopDisCont")


# Transformations in formula ---------------------------------------------------------------------------------------------------
# context("Correctness - copulaCorrection - Formula transformations")
#
# test_that("Formula transformations are correct", {
#
#   # C2, Dis, DisCont
#   set.seed(0xcaffee)
#   expect_warning(correct.res.c1      <- copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),
#                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
#   expect_silent(correct.res.c2      <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2))
#   expect_silent(correct.res.dis     <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2))
#   expect_silent(correct.res.disCont <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont))
#
#   # Can handle transformations in LHS
#   set.seed(0xcaffee)
#   # C1
#   data.altered   <- dataCopCont
#   data.altered$y <- exp(data.altered$y)
#   expect_warning(res.trans.lhs  <- copulaCorrection(formula= log(y) ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=data.altered),
#                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
#   expect_equal(coef(res.trans.lhs), coef(correct.res.c1))
#   expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.c1)))
#   # C2
#   data.altered   <- dataCopCont2
#   data.altered$y <- exp(data.altered$y)
#   expect_silent(res.trans.lhs      <- copulaCorrection(formula= log(y) ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=data.altered))
#   expect_equal(coef(res.trans.lhs), coef(correct.res.c2))
#   expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.c2)))
#   # Dis
#   data.altered   <- dataCopDis2
#   data.altered$y <- exp(data.altered$y)
#   expect_silent(res.trans.lhs     <- copulaCorrection(formula= log(y) ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=data.altered))
#   expect_equal(coef(res.trans.lhs), coef(correct.res.dis))
#   expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.dis)))
#   # DisCont
#   data.altered   <- dataCopDisCont
#   data.altered$y <- exp(data.altered$y)
#   expect_silent(res.trans.lhs <- copulaCorrection(formula= log(y) ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=data.altered))
#   expect_equal(coef(res.trans.lhs), coef(correct.res.disCont))
#   expect_equal(coef(summary(res.trans.lhs)), coef(summary(correct.res.disCont)))
#
#   # Can handle transformations in RHS1
#   set.seed(0xcaffee)
#   # C1
#   data.altered    <- dataCopCont
#   data.altered$P <- exp(data.altered$P)
#   expect_warning(res.trans.rhs  <- copulaCorrection(formula= y ~ X1+X2+log(P)|continuous(log(P)), verbose = FALSE, num.boots=2, data=data.altered),
#                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
#   expect_equal(coef(res.trans.rhs), coef(correct.res.c1), check.attributes=FALSE)
#   expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.c1)), check.attributes=FALSE)
#   # C2
#   data.altered    <- dataCopCont2
#   data.altered$P2 <- exp(data.altered$P2)
#   expect_silent(res.trans.rhs      <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|continuous(P1, log(P2)), verbose = FALSE, data=data.altered))
#   expect_equal(coef(res.trans.rhs), coef(correct.res.c2), check.attributes=FALSE)
#   expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.c2)), check.attributes=FALSE)
#   # Dis
#   data.altered    <- dataCopDis2
#   data.altered$P2 <- exp(data.altered$P2)
#   expect_silent(res.trans.rhs   <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|discrete(P1, log(P2)),   verbose = FALSE, data=data.altered))
#   expect_equal(coef(res.trans.rhs), coef(correct.res.dis), check.attributes=FALSE)
#   expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.dis)), check.attributes=FALSE)
#   # Discont
#   data.altered    <- dataCopDisCont
#   data.altered$P2 <- exp(data.altered$P2)
#   expect_silent(res.trans.rhs <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|continuous(P1)+discrete(log(P2)), verbose = FALSE, data=data.altered))
#   expect_equal(coef(res.trans.rhs), coef(correct.res.disCont), check.attributes=FALSE)
#   expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.disCont)), check.attributes=FALSE)
# })
#
#

# Data sorting ------------------------------------------------------------------------------------------------
context("Correctness - copulaCorrection - Data sorting")

test_that("Differently sorted data produces same results C1", {
  skip_on_cran()
  data.altered <- dataCopCont
  data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]

  set.seed(0xcaffee) #bootstrap
  expect_warning(res.orig  <- copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2,
                                               data=dataCopCont), regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  set.seed(0xcaffee) #bootstrap
  expect_warning(res.diff.sorted  <- copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2,
                                                      data=data.altered), regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

  expect_equal(coef(res.orig), coef(res.diff.sorted))

  # Cannot check because random sampling for bootstrapping results in different
  #   selected sub-samples and therefore different SEs (- summary statistics)
  # expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
})

# Does not work: Sorting and random vars does result in different pstar data
# test_that("Differently sorted data produces same results C2", {
#
#   data.altered <- dataCopCont2
#   data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]
#   set.seed(0xcaffee) # pstar
#   expect_silent(res.orig  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2))
#
#   set.seed(0xcaffee) # pstar
#   expect_silent(res.diff.sorted  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=data.altered))
#
#   expect_equal(coef(res.orig), coef(res.diff.sorted))
#   expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
# })
#
#
# test_that("Differently sorted data produces same results Dis", {
#
#   data.altered <- dataCopDis2
#   data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]
#   set.seed(0xcaffee) # pstar
#   expect_silent(res.orig  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2))
#
#   set.seed(0xcaffee) # pstar
#   expect_silent(res.diff.sorted  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=data.altered))
#
#   expect_equal(coef(res.orig), coef(res.diff.sorted))
#   expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
# })
#
# test_that("Differently sorted data produces same results DisCont", {
#
#   data.altered <- dataCopDisCont
#   data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]
#   set.seed(0xcaffee) # pstar
#   expect_silent(res.orig        <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont))
#
#   set.seed(0xcaffee) # pstar
#   expect_silent(res.diff.sorted <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=data.altered))
#
#   expect_equal(coef(res.orig), coef(res.diff.sorted))
#   expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
# })

