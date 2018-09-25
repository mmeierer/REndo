# TEST RUNABILITY ==================================================================================================================================================================

# Required data --------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("dataCopC1")
data("dataCopC2")
data("dataCopDis")
data("dataCopDisCont")

context("copulaCorrection - single continuous")
test_that("Non-numerics can be used in exogenous data", {
  # factors + characters + logicals
  # C1
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P), verbose = FALSE,
                                 data=cbind(dataCopC1,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P), verbose = FALSE,
                                 data=cbind(dataCopC1,color=c("red", "green", "blue", "white", "yellow"))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P+color|continuous(P), verbose = FALSE,
                                 data=cbind(dataCopC1,color=c(TRUE,FALSE))))
  # C2
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=c("red", "green", "blue", "white", "yellow"))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=c(TRUE,FALSE))))
  # Disc
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=c("red", "green", "blue", "white", "yellow"))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=c(TRUE,FALSE))))
  # DiscCont
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1)+continuous(P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1)+continuous(P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=c("red", "green", "blue", "white", "yellow"))))
  expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1)+continuous(P2), verbose = FALSE,
                                 data=cbind(dataCopDisCont,color=c(TRUE,FALSE))))
})


# test_that("Factors can be used in endogenous data as discrete", {
#   # C1
#   # expect_silent(copulaCorrection(formula= y ~ X1+X2+color|continuous(color), verbose = FALSE,
#                                  # data=cbind(dataCopC1,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
#   # C2
#   # expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|continuous(P1, color), verbose = FALSE,
#                                  # data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
#   # Disc
#   expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(P1, color), verbose = FALSE,
#                                  data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
#   # DiscCont
#   expect_silent(copulaCorrection(formula= y ~ X1+X2+P1+P2+color|discrete(color)+continuous(P2), verbose = FALSE,
#                                  data=cbind(dataCopDisCont,color=factor(x = c("red", "green", "blue", "white", "yellow")))))
# })

#
# test_that("No output when verbose = F", {
# })
#
# test_that("Warning and message when verbose = T", {
#
# })
#
# test_that("Runs without intercept", {
#
# })


