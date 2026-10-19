# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopCont")
data("dataCopCont2")
data("dataCopDis2")
data("dataCopDisCont")


# ** Now rho / sigma range check is done in runability but should be here
# Transformations in formula ---------------------------------------------------------------------------------------------------

test_that("Formula transformations are correct", {

  skip_on_cran()

  # C2, Dis, DisCont
  set.seed(0xcaffee)
  correct.res.c1      <- suppress_lowboots_warning(copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont))
  expect_silent(correct.res.c2      <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2))
  expect_silent(correct.res.dis     <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2))
  expect_silent(correct.res.disCont <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont))

  # Can handle transformations in LHS
  set.seed(0xcaffee)
  # C1
  data.altered   <- dataCopCont
  data.altered$y <- exp(data.altered$y)
  res.trans.lhs  <- suppress_lowboots_warning(copulaCorrection(formula= log(y) ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=data.altered))
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
  res.trans.rhs  <- suppress_lowboots_warning(copulaCorrection(formula= y ~ X1+X2+log(P)|continuous(log(P)), verbose = FALSE, num.boots=2, data=data.altered))
  expect_equal(coef(res.trans.rhs), coef(correct.res.c1), ignore_attr=TRUE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.c1)), ignore_attr=TRUE)
  # C2
  data.altered    <- dataCopCont2
  data.altered$P2 <- exp(data.altered$P2)
  expect_silent(res.trans.rhs      <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|continuous(P1, log(P2)), verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.rhs), coef(correct.res.c2), ignore_attr=TRUE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.c2)), ignore_attr=TRUE)
  # Dis
  data.altered    <- dataCopDis2
  data.altered$P2 <- exp(data.altered$P2)
  expect_silent(res.trans.rhs   <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|discrete(P1, log(P2)),   verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.rhs), coef(correct.res.dis), ignore_attr=TRUE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.dis)), ignore_attr=TRUE)
  # Discont
  data.altered    <- dataCopDisCont
  data.altered$P2 <- exp(data.altered$P2)
  expect_silent(res.trans.rhs <- copulaCorrection(formula= y ~ X1+X2+P1+log(P2)|continuous(P1)+discrete(log(P2)), verbose = FALSE, data=data.altered))
  expect_equal(coef(res.trans.rhs), coef(correct.res.disCont), ignore_attr=TRUE)
  expect_equal(coef(summary(res.trans.rhs)), coef(summary(correct.res.disCont)), ignore_attr=TRUE)
})



# Data sorting ------------------------------------------------------------------------------------------------

test_that("Differently sorted data produces same results C1", {
# ** Does not work anymore because bootstrapping is used every run now.
#     sample() does return same indices with set.seed but there are other elements there now after diff.sorting.
#   More bootstraps would be needed now
  skip_on_cran()

  data.altered <- dataCopCont
  data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]

  set.seed(0xcaffee)
  res.orig  <- suppress_lowboots_warning(copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2,
                                                          data=dataCopCont))
  set.seed(0xcaffee)
  res.diff.sorted  <- suppress_lowboots_warning(copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2,
                                                                 data=data.altered))

  expect_equal(coef(res.orig), coef(res.diff.sorted))

  # Cannot check because random sampling for bootstrapping results in different
  #   selected sub-samples and therefore different SEs (- summary statistics)
  # expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
})

test_that("Differently sorted data produces same results C2", {

  data.altered <- dataCopCont2
  data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]
  set.seed(0xcaffee) # pstar
  res.orig  <- suppress_lowboots_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2, num.boots = 2))

  set.seed(0xcaffee) # pstar
  res.diff.sorted  <- suppress_lowboots_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=data.altered, num.boots = 2))

  expect_equal(coef(res.orig), coef(res.diff.sorted))
  # Does not work: Same bootstrapping happens but sorting has changed, so different obs are selected
  # expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
})


# Predict ----------------------------------------------------------------------------------------------------

# * C1 --------------------------------------------------------------------------------------------------------
test_that("No newdata results in fitted values C1", {
  c1.1 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P|continuous(P), num.boots=2, data=dataCopCont, verbose = FALSE))
  expect_equal(predict(c1.1), fitted(c1.1))

  c1.2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P-1|continuous(P), num.boots=2, data=dataCopCont, verbose = FALSE))
  expect_equal(predict(c1.2), fitted(c1.2))
})

test_that("Same prediction data as for fitting results in fitted values C1", {
  c1.1 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P|continuous(P), num.boots=2, data=dataCopCont, verbose = FALSE))
  expect_equal(predict(c1.1,dataCopCont), fitted(c1.1))

  c1.2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P-1|continuous(P), num.boots=2, data=dataCopCont, verbose = FALSE))
  expect_equal(predict(c1.2,dataCopCont), fitted(c1.2))

})

test_that("Correct when using transformations in the formula C1", {
  # transformation in regressor
  c1.1 <- suppress_lowboots_warning(copulaCorrection(y~I((X1+1)/2)+X2+P|continuous(P), num.boots=2, data=dataCopCont, verbose = FALSE))
  expect_equal(predict(c1.1,dataCopCont), fitted(c1.1))
  # transformation in endogenous
  c1.1 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+log(P+10)|continuous(log(P+10)), num.boots=2, data=dataCopCont, verbose = FALSE))
  expect_equal(predict(c1.1,dataCopCont), fitted(c1.1))
})

test_that("Correct structure of predictions C1", {
  c1.1 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P|continuous(P), num.boots=2, data=dataCopCont, verbose = FALSE))
  expect_silent(pred.1 <- predict(c1.1, dataCopCont))
  expect_true(is.numeric(pred.1))
  expect_true(length(pred.1) == nrow(dataCopCont))
  expect_true(all(names(pred.1) == names(fitted(c1.1))))
  expect_true(all(names(pred.1) == rownames(dataCopCont)))
})

# * C2 --------------------------------------------------------------------------------------------------------
test_that("No newdata results in fitted values C2", {
  # 2 cont
  c2.cont2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+P2|continuous(P1, P2), num.boots=2, data=dataCopCont2, verbose = FALSE))
  expect_equal(predict(c2.cont2), fitted(c2.cont2))

  c2.dis2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+P2|discrete(P1, P2), num.boots=2, data=dataCopDis2, verbose = FALSE))
  expect_equal(predict(c2.dis2), fitted(c2.dis2))

  c2.discont <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+continuous(P2), num.boots=2, data=dataCopDisCont, verbose = FALSE))
  expect_equal(predict(c2.discont), fitted(c2.discont))
})

test_that("Same prediction data as for fitting results in fitted values C2", {
  c2.cont2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+P2|continuous(P1, P2), num.boots=2, data=dataCopCont2, verbose = FALSE))
  expect_equal(predict(c2.cont2,dataCopCont2), fitted(c2.cont2))

  c2.dis2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+P2|discrete(P1, P2), num.boots=2, data=dataCopDis2, verbose = FALSE))
  expect_equal(predict(c2.dis2,dataCopDis2), fitted(c2.dis2))

  c2.discont <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+P2|discrete(P1)+continuous(P2), num.boots=2, data=dataCopDisCont, verbose = FALSE))
  expect_equal(predict(c2.discont,dataCopDisCont), fitted(c2.discont))
})

test_that("Correct when using transformations in the formula C2", {
  skip_on_cran()
  # Transformation in endogenous
  c2.cont2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+log(P1+50)+P2|continuous(log(P1+50), P2), num.boots=2, data=dataCopCont2, verbose = FALSE))
  expect_equal(predict(c2.cont2,dataCopCont2), fitted(c2.cont2))

  c2.dis2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+log(P2+10)|discrete(P1, log(P2+10)), num.boots=2, data=dataCopDis2, verbose = FALSE))
  expect_equal(predict(c2.dis2,dataCopDis2), fitted(c2.dis2))

  c2.discont <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+log(P2+50)|discrete(P1)+continuous(log(P2+50)), num.boots=2, data=dataCopDisCont, verbose = FALSE))
  expect_equal(predict(c2.discont,dataCopDisCont), fitted(c2.discont))

  # Transformation in regular
  c2.cont2 <- suppress_lowboots_warning(copulaCorrection(y~X1+I((X2+10)/3)+P1+P2|continuous(P1, P2), num.boots=2, data=dataCopCont2, verbose = FALSE))
  expect_equal(predict(c2.cont2,dataCopCont2), fitted(c2.cont2))

  c2.dis2 <- suppress_lowboots_warning(copulaCorrection(y~X1+I((X2+10)/3)+P1+P2|discrete(P1, P2), num.boots=2, data=dataCopDis2, verbose = FALSE))
  expect_equal(predict(c2.dis2,dataCopDis2), fitted(c2.dis2))

  c2.discont <- suppress_lowboots_warning(copulaCorrection(y~X1+I((X2+10)/3)+P1+P2|discrete(P1)+continuous(P2), num.boots=2, data=dataCopDisCont, verbose = FALSE))
  expect_equal(predict(c2.discont,dataCopDisCont), fitted(c2.discont))
})

test_that("Correct structure of predictions C2", {
  c2.cont2 <- suppress_lowboots_warning(copulaCorrection(y~X1+X2+P1+P2|continuous(P1, P2), num.boots=2, data=dataCopCont2, verbose = FALSE))
  expect_silent(pred.1 <- predict(c2.cont2, dataCopCont2))
  expect_true(is.numeric(pred.1))
  expect_true(length(pred.1) == nrow(dataCopCont2))
  expect_true(all(names(pred.1) == names(fitted(c2.cont2))))
  expect_true(all(names(pred.1) == rownames(dataCopCont2)))
})





# test_that("Differently sorted data produces same results Dis", {
#
#   data.altered <- dataCopDis2
#   data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]
#   set.seed(0xcaffee) # pstar
#   expect_warning(res.orig  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2, num.boots = 2),
#                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
#
#   set.seed(0xcaffee) # pstar
#   expect_warning(res.diff.sorted  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=data.altered, num.boots = 2),
#                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
#
#   expect_equal(coef(res.orig), coef(res.diff.sorted))
#   # Does not work: Same bootstrapping happens but sorting has changed, so different obs are selected
#   # expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
# })
#
# test_that("Differently sorted data produces same results DisCont", {
#
#   data.altered <- dataCopDisCont
#   data.altered <- data.altered[sample(nrow(data.altered), nrow(data.altered), replace = FALSE), ]
#   set.seed(0xcaffee) # pstar
#   expect_warning(res.orig <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont, num.boots = 2),
#                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
#
#   set.seed(0xcaffee) # pstar
#   expect_warning(res.diff.sorted <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=data.altered, num.boots = 2),
#                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
#
#   expect_equal(coef(res.orig), coef(res.diff.sorted))
#   # Does not work: Same bootstrapping happens but sorting has changed, so different obs are selected
#   # expect_equal(coef(summary(res.orig)), coef(summary(res.diff.sorted)))
# })

