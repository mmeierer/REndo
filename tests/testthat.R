library(testthat)
library(REndo2)

# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_formula_singleendo.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_formula_multiendo.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_formula_latentIV.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_formula_latentIV.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_data_singleendo.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_data_multiendo.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_positivewholenumeric.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_charactervec_highermoments.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_singlelogical.R")
# source("/Users/patrik/StudentAssi/REndo/REndo2/tests/testthat/helper_test_s3methods_lm_multiendo.R")


# Cannot write one general functions to test the "formula" parameter. Reason is that the formula
# structure is different for (nearly) all methods and the testing variations of the formula needs to be adapted accordingly.
# Doing this by code is to time-consuming for only 7 methods.
# The "data" parameter which depends on the formula can however be reduced to only 3 cases:
# y~X1+X2+P1+P2 (Cop2->multiendo), y~P (latentIV->latentIV), y~X1+X2+P (all others->singleendo)

test_check("REndo2")
