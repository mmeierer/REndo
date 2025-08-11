context("Inputchecks - tscope - Parameter num.boots")

# num.boots must be a positive whole number ------------------------------------------------------------------------

test_that("num.boots validation works", {
  expect_true(length(checkinput_tscope_numboots(num.boots = 0))  > 0)
  expect_true(length(checkinput_tscope_numboots(num.boots = -5)) > 0)
  expect_true(length(checkinput_tscope_numboots(num.boots = 1.5)) > 0)
  expect_equal(length(checkinput_tscope_numboots(num.boots = 10)), 0)
})
