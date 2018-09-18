test.s3methods.heterrorsiv <- function(res.het, input.form, function.std.data, req.df){
  .test.s3methods.ivreg.models(res.ivreg.model = res.het, input.form = input.form, function.std.data=function.std.data,
                               req.df = req.df, full.coefs = c("(Intercept)", "X1", "X2", "P"))
}

.test.s3methods.ivreg.models <- function(res.ivreg.model, input.form, function.std.data, req.df, full.coefs){
  .test.s3methods.basic.structure(res.model=res.ivreg.model, input.form=input.form,
                                  function.std.data=function.std.data, full.coefs=full.coefs)
}
