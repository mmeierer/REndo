test.nonfinite.in.data <- function(data, name.col, fct, call.args, regexp="The above errors were encountered!"){
  data[5, name.col] <- NA_real_
  call.args <- modifyList(call.args, list(data= data))
  expect_error(do.call(what = fct, args=call.args), regexp = regexp)

  data[5, name.col] <- Inf
  call.args <- modifyList(call.args, list(data= data))
  expect_error(do.call(what = fct, args=call.args), regexp = regexp)

  data[5, name.col] <- -Inf
  call.args <- modifyList(call.args, list(data= data))
  expect_error(do.call(what = fct, args=call.args), regexp = regexp)

  data[5, name.col] <- NaN
  call.args <- modifyList(call.args, list(data= data))
  expect_error(do.call(what = fct, args=call.args), regexp = regexp)
}

