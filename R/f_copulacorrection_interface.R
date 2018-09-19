#' @importFrom Formula as.Formula
#' @importFrom methods formalArgs
#' @export
copulaCorrection <- function(formula, data, verbose=TRUE, ...){
  # Catch stuff ------------------------------------------------------------------------------------------------
  cl <- match.call()
  l.ellipsis <- list(...)

  # Input checks -----------------------------------------------------------------------------------------------
  check_err_msg(checkinput_copulacorrection_formula(formula=formula))
  check_err_msg(checkinput_copulacorrection_data(data=data))
  check_err_msg(checkinput_copulacorrection_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_copulacorrection_verbose(verbose=verbose))


  # Read out specials ------------------------------------------------------------------------------------------
  F.formula <- as.Formula(formula)
  names.vars.continuous <- fct.readout.special(F.formula = F.formula, name.special = "continuous", from.rhs=2)
  names.vars.discrete   <- fct.readout.special(F.formula = F.formula, name.special = "discrete",   from.rhs=2)

  # Determine case
  if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0)
    optimizeLL <- TRUE
  else
    optimizeLL <- FALSE


  # Dispatch to either LL optimization or PStar+lm -------------------------------------------------------------
  if(optimizeLL){
    # Single continuous - copula method 1 (optimize LL)

    # Call the optimizeLL function with the exact same arguments as the interface was called
    # optLL <- cl
    # optLL[[1]] <- quote(REndo2:::copulaCorrection_optimizeLL)
    # res <- eval(expr = optLL, envir = parent.frame())
    # subset to allowed args - forward everything**
    # l.ellipsis <- l.ellipsis[which(names(l.ellipsis) %in% formalArgs(copulaCorrection_optimizeLL))]
    # Call the optimizeLL function with the additional args in the ellipsis
    res <- do.call(what = copulaCorrection_optimizeLL, args = c(list(data=data, formula=formula, verbose=verbose),
                                                                l.ellipsis))

    # res <- copulaCorrection_optimizeLL(data = data, formula=formula, name.var.continuous = names.vars.continuous,
    #                                    start.params = if("start.params" %in% names(l.ellipsis)) l.ellipsis[["start.params"]] else NULL,
    #                                    names.main.model = )
  }else{

    # All other cases use pstar data + lm
    if(verbose){
      # Inform what is done
      message("For XXXXXX ** TO DO *** ")
      # Warn if unnecessary parameters (anything in ...) are given because user seemingly doesnt understand what he is doing
      if(length(l.ellipsis)>0)
        warning("Additional parameters given in the ... argument are ignored because they are not needed.",
                call. = FALSE, immediate. = TRUE)
    }

    res <- copulaCorrection_linearmodel(F.formula = F.formula, data = data,
                                           names.vars.continuous = names.vars.continuous,
                                           names.vars.discrete   = names.vars.discrete)
  }

  # discrete only gets an additional class for its own confidence interval simulation function
  if(length(names.vars.continuous) == 0 & length(names.vars.discrete) > 0)
    class(res) <- c("rendo.copulacorrection.lm.discrete",class(res))

  # Adapt return -----------------------------------------------------------------------------------------------
  res$formula <- F.formula
  res$call    <- cl
  res$names.vars.continuous <- names.vars.continuous
  res$names.vars.discrete   <- names.vars.discrete
  return(res)
  # if(length(names.vars.discrete)>0){
  #   # Single/multiple discrete and "mixed" (continuous+discrete) all use pstar
  # }else{
  #   # Only continuous
  #   if(length(names.vars.continuous) == 1 & length(names.vars.discrete) == 0){
  #     # Single continuous - copula method 1 (optimize LL)
  #
  #   }else{
  #     # Multiple continuous - pstar continuous
  #   }
  # }
}

