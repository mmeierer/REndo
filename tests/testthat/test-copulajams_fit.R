# copulajams_get_stratum_copterm_names -----------------------------------------------
test_that("copulajams_get_stratum_copterm_names appends the stratum to the names", {
  # 1 name, stratum of 1 factor
  expect_identical(
    copulajams_get_stratum_copterm_names(
      copterm.names = "P1_cop",
      stratum.level = "Z=0"
    ),
    "P1_cop[Z=0]"
  )

  # multiple names (with space, transformed), stratum of 2 factors: order is kept
  expect_identical(
    copulajams_get_stratum_copterm_names(
      copterm.names = c("P 1_cop", "log(P2)_cop"),
      stratum.level = "Z=0, Z2=a"
    ),
    c("P 1_cop[Z=0, Z2=a]", "log(P2)_cop[Z=0, Z2=a]")
  )
})
