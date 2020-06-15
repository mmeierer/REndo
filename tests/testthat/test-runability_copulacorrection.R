# TEST RUNABILITY ==================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopDis2")
data("dataCopCont")
data("dataCopCont2")
data("dataCopDisCont")

context("Runability - copulaCorrection - Runability")

test_that("Works with intercept", {
  skip_on_cran()
  # C1
  expect_warning(res.c1 <- copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

  expect_true(coef(res.c1)["rho"] > 0 & coef(res.c1)["rho"] < 1)
  expect_true(coef(res.c1)["sigma"] > 0)

  # C2, Dis, DisCont
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})

test_that("Works without intercept", {
  skip_on_cran()
  # C1
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P-1|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # C2, Dis, DisCont
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2-1|continuous(P1, P2), verbose = FALSE, data=dataCopCont2, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2-1|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2-1|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})


test_that("Produces output", {
  # C1
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P-1|continuous(P), verbose = TRUE, num.boots=2, data=dataCopCont),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # C2, Dis, DisCont
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2-1|continuous(P1, P2), verbose = TRUE, data=dataCopCont2, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2-1|discrete(P1, P2),   verbose = TRUE, data=dataCopDis2, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2-1|continuous(P1)+discrete(P2), verbose = TRUE, data=dataCopDisCont, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

})


# Does not work because of the randomness - which ever is called first
# test_that("Same results with swapped endos", {
#   # C1 only has single endo, discont are fixed by type
#   expect_warning(res.c2.1  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2))
#   expect_warning(res.dis.1 <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2))
#
#   expect_warning(res.c2.2  <- copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P2, P1), verbose = TRUE, data=dataCopCont2))
#   expect_warning(res.dis.2 <- copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P2, P1),   verbose = TRUE, data=dataCopDis2))
#
#   expect_setequal(coef(res.c2.1),  coef(res.c2.2))
#   expect_setequal(coef(res.dis.1), coef(res.dis.2))
# })

# test_that("No output when verbose = FALSE", {
# })

# test_that("Warning and message when verbose = TRUE", {
#
# })
#

# Too stable now :(
# test_that("Copula 1 fails gracefully for optimx crashes", {
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P|continuous(P), verbose=FALSE, data=dataCopCont,
#                                 start.params = c("(Intercept)"=2, X1=1.5,X2=1, P=-100)),
#                regexp = "Failed to optimize the log-likelihood function with error")
# })



test_that("Non-numerics can be used in exogenous data", {
  skip_on_cran()

  # factors + characters + logicals
  # C1
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P), verbose = FALSE, num.boots=2,
                                 data=cbind(dataCopCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))),
                                    regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P), verbose = FALSE, num.boots=2,
                                 data=cbind(dataCopCont,color=c("red", "green", "blue", "white", "yellow"))),
                                    regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P), verbose = FALSE, num.boots=2,
                                 data=cbind(dataCopCont,color=c(TRUE,FALSE))),
                                  regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # C2
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=c("red", "green", "blue", "white", "yellow"))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=c(TRUE,FALSE))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # Disc
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=c("red", "green", "blue", "white", "yellow"))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=c(TRUE,FALSE))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # DiscCont
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1)+continuous(P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1)+continuous(P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=c("red", "green", "blue", "white", "yellow"))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1)+continuous(P2), verbose = FALSE, num.boots = 2,
                                 data=cbind(dataCopDisCont,color=c(TRUE,FALSE))),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})


test_that("Start params work with non-numeric", {
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P), verbose = FALSE, num.boots=2,
                                 data=cbind(dataCopCont,color=factor(x = c("red", "green", "blue", "white", "yellow"))),
                                 start.params = c("(Intercept)"=2, X1=1.5,X2=-3, P=-1,
                                                  colorgreen=-0.1, colorred=0, colorwhite=0, coloryellow=0)),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})


test_that("Works with proper optimx.args", {
  expect_warning(copulaCorrection(optimx.args = list(itnmax = 100), formula = y~X1+X2+P|continuous(P),
                                 data = dataCopCont, verbose = FALSE, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.")
  expect_warning(copulaCorrection(optimx.args = list(itnmax = 100, control=list(kkttol=0.01)), formula = y~X1+X2+P|continuous(P),
                                  data = dataCopCont, verbose = FALSE, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.")
})


test_that("Works with NA in not needed columns", {
  # LL
  dataCopCont.na <- dataCopCont
  dataCopCont.na[5, "X2"] <- NA_real_
  expect_warning(copulaCorrection(formula = y~X1+P|continuous(P), optimx.args = list(itnmax = 10),
                                  data = dataCopCont.na, verbose = FALSE, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.")

  dataCopCont.inf <- dataCopCont
  dataCopCont.inf[5, "X2"] <- Inf
  expect_warning(copulaCorrection(formula = y~X1+P|continuous(P), optimx.args = list(itnmax = 10),
                                  data = dataCopCont.inf, verbose = FALSE, num.boots = 2),
                 regexp = "It is recommended to run 1000 or more bootstraps.")

  # Augmented OLS
  dataCopDis.na <- dataCopDis
  dataCopDis.na[5, "X2"] <- NA_real_
  expect_warning(copulaCorrection(formula = y~X1+P|discrete(P),data = dataCopDis.na, verbose = FALSE,
                 num.boots=2), regexp = "It is recommended to run 1000 or more bootstraps.")
  dataCopDis.inf          <- dataCopDis
  dataCopDis.inf[5, "X2"] <- Inf
  expect_warning(copulaCorrection(formula = y~X1+P|discrete(P),data = dataCopDis.inf, verbose = FALSE,
                                  num.boots=2), regexp = "It is recommended to run 1000 or more bootstraps.")
})

# Transformations in formula ---------------------------------------------------------------------------------------------------
context("Runability - copulaCorrection - Formula transformations")

test_that("Works with function in exogenous", {
  skip_on_cran()

  # C1
  # ****TODO optimx warning bounds. Use elsewhere.
  # expect_warning(copulaCorrection(formula= y ~ exp(X1)+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),

  #                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ I(X1/3)+X2+P|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),
                                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # C2, Dis, DisCont
  expect_warning(copulaCorrection(formula= y ~ exp(X1)+X2+P1+P2|continuous(P1, P2), verbose = FALSE, data=dataCopCont2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ exp(X1)+X2+P1+P2|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ exp(X1)+X2+P1+P2|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})


test_that("Works with single endo transformation", {
  skip_on_cran()
  # C1
  expect_warning(copulaCorrection(formula= y ~ X1+X2+I(P/2)|continuous(I(P/2)), verbose = FALSE, num.boots=2, data=dataCopCont),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # C2, Dis, DisCont
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P1, I(P2/2)), verbose = FALSE, data=dataCopCont2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|discrete(P1, I(P2/2)),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+I(P2/2)|continuous(P1)+discrete(I(P2/2)), verbose = FALSE, data=dataCopDisCont, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})


test_that("Works with transformed and untransformed endo", {
  skip_on_cran()

  # check that not the same are taken by comparing results

  # ****TODO: Ask raluca why they are not unequal?
  # C1
  # expect_warning(res.c1.1 <- copulaCorrection(formula= y ~ X1+X2+P+I(P/100)|continuous(I(P/100)), verbose = FALSE, num.boots=2, data=dataCopCont),

  #                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # expect_warning(res.c1.2 <- copulaCorrection(formula= y ~ X1+X2+P+exp(P/100)|continuous(P), verbose = FALSE, num.boots=2, data=dataCopCont),
  #                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # expect_false(isTRUE(all.equal(coef(res.c1.1), coef(res.c1.2))))

  # C2
  expect_warning(res.c2.1 <- copulaCorrection(formula= y ~ X1+X2+P1+P2+exp(P2/2)|continuous(P1, exp(P2/2)), verbose = FALSE, data=dataCopCont2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(res.c2.2 <- copulaCorrection(formula= y ~ X1+X2+P1+P2+exp(P2/2)|continuous(P1, P2), verbose = FALSE, data=dataCopCont2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_false(isTRUE(all.equal(coef(res.c2.1), coef(res.c2.2))))

  #  Dis
  expect_warning(res.d1 <- copulaCorrection(formula= y ~ X1+X2+P1+P2+I(P2/2)|discrete(P1, I(P2/2)),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(res.d2 <- copulaCorrection(formula= y ~ X1+X2+P1+P2+I(P2/2)|discrete(P1, P2),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_false(isTRUE(all.equal(coef(res.d1), coef(res.d2))))

  # DisCont
  expect_warning(res.cd1 <- copulaCorrection(formula= y ~ X1+X2+P1+P2+I(P2/2)|continuous(P1)+discrete(I(P2/2)), verbose = FALSE, data=dataCopDisCont, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(res.cd2 <- copulaCorrection(formula= y ~ X1+X2+P1+P2+I(P2/2)|continuous(P1)+discrete(P2), verbose = FALSE, data=dataCopDisCont, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_false(isTRUE(all.equal(coef(res.cd1), coef(res.cd2))))
})

test_that("Works with all endo transformed", {
  # C1 done already in single because only has 1 endo
  # different endos
  expect_warning(copulaCorrection(formula= y ~ X1+X2+exp(P1)+I(P2/2)|continuous(exp(P1), I(P2/2)), verbose = FALSE, data=dataCopCont2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+exp(P1)+I(P2/2)|continuous(exp(P1), I(P2/2)),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+exp(P1)+I(P2/2)|discrete(exp(P1), I(P2/2)),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+exp(P1)+I(P2/2)|continuous(exp(P1))+discrete(I(P2/2)), verbose = FALSE, data=dataCopDisCont, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  # Same endos transformed twice
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+exp(P2)+I(P2/2)|continuous(exp(P2), I(P2/2)), verbose = FALSE, data=dataCopCont2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+exp(P2)+I(P2/2)|continuous(exp(P2), I(P2/2)),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+exp(P2)+I(P2/2)|discrete(exp(P2), I(P2/2)),   verbose = FALSE, data=dataCopDis2, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+exp(P2)+I(P2/2)|continuous(exp(P2))+discrete(I(P2/2)), verbose = FALSE, data=dataCopDisCont, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})

test_that("start.params works with transformation", {
  expect_warning(copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1.5, X2 = -3, "I(P + 1)" = -1),
                                 formula = y ~ X1 + X2 + I(P+1) |continuous(I(P+1)), data = dataCopCont, verbose=FALSE, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
})

test_that("Correct for start.params swapped", {
  set.seed(0xcaffee)
  expect_warning(res.c1.1 <- copulaCorrection(start.params = c("(Intercept)"=2, X1 = 1.5, X2 = -3, "I(P + 1)" = -1),
                                 formula = y ~ X1 + X2 + I(P+1) |continuous(I(P+1)), data = dataCopCont, verbose=FALSE, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)
  set.seed(0xcaffee)
  expect_warning(res.c1.2 <- copulaCorrection(start.params = c(X2 = -3,"I(P + 1)" = -1, "(Intercept)"=2, X1 = 1.5),
                                 formula = y ~ X1 + X2 + I(P+1) |continuous(I(P+1)), data = dataCopCont, verbose=FALSE, num.boots=2),
                regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

  # Equal, including param orderin
  expect_equal(coef(res.c1.1), coef(res.c1.2))
})

test_that("Fails if lm cannot be used to derive start.params", {
  expect_error(res.c1.1 <- copulaCorrection(formula= y ~ X1+X2+P+I(P/1.1)|continuous(I(P/1.1)),data=dataCopCont, verbose=FALSE),
                 regexp = "The start parameters could not be derived by fitting a linear model.")
})


test_that("Works for L-BFGS-B", {
  skip_on_cran()

  expect_warning(res.bfgs <- copulaCorrection(y~X1+X2+P|continuous(P), data = dataCopCont, num.boots = 20, verbose=FALSE,
                                              optimx.args = list(control = list(trace = 0), method="L-BFGS-B")),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

  expect_false(anyNA(res.bfgs$boots.params))
  expect_silent(confint(res.bfgs))
  expect_silent(vcov(res.bfgs))
})


test_that("Works for Nelder-Mead", {
  skip_on_cran()

  expect_warning(res.nm <- copulaCorrection(y~X1+X2+P|continuous(P), data = dataCopCont, num.boots = 20, verbose=FALSE,
                                              optimx.args = list(control = list(trace = 0), method="Nelder-Mead")),
                 regexp = "It is recommended to run 1000 or more bootstraps.", all = TRUE)

  expect_false(anyNA(res.nm$boots.params))
  expect_silent(confint(res.nm))
  expect_silent(vcov(res.nm))
})




# test_that("Works for multivar transformations",{
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1+P2),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1*P2),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(P1:P2),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(log(P1)),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|continuous(I(P1^2)),data=dataCopCont2), regexp = "The above errors were encountered!")
#
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1+P2),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1*P2),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(P1:P2),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(log(P1)),data=dataCopCont2), regexp = "The above errors were encountered!")
#   expect_error(copulaCorrection(formula= y ~ X1+X2+P1+P2|discrete(I(P1^2)),data=dataCopCont2), regexp = "The above errors were encountered!")
# })

# test_that("Factors can be used in endogenous data as discrete", {
#   # C1
#   expect_warning(copulaCorrection(formula= y ~ X1+X2+color|continuous(color), verbose = FALSE,
#                                  data=cbind(dataCopCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
#   # C2
#   # expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, color), verbose = FALSE,
#                                  # data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
#   # Disc
#   expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, color), verbose = FALSE,
#                                  data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
#   # DiscCont
#   expect_warning(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(color)+continuous(P2), verbose = FALSE,
#                                  data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
# })
