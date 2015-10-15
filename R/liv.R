#'@title  Fitting Linear Models with 1 Endogenous Regressor using Latent Instrumental Variables
#
# Description
#'@description  Fits linear models with one endogenous regressor and no additional explanatory variables using the latent instrumental variable approach
#'presented in Ebbes,P., Wedel,M., Boeckenholt, U., and Steerneman, A. G. M. (2005). This is a statistical technique to address the endogeneity problem where no external instrumental
#'variables are needed. The important assumption of the model is that the latent variables are discrete with at least two groups with different means and
#'the structural error is normally distributed.
#
# Arguments
#'@param formula var1 ~ var2     var1: vector or matrix containing the dependent variable. var2: a vector with the endogenous variable.
#'@param  param - a vector of initial values for the parameters of the model to be supplied to the optimization algorithm.
#'The first parameter is the intercept, then the coefficient of the endogenous variable followed by the means of the two groups of the latent IV,
#'then the next three parameters are for the variance-covariance matrix. The last parameter is the probability for group 1.
#
#'@details The method has been programmed such that the latent variable has two groups. Ebbes et al.(2005) show in a Monte Carlo experiement that
#'even if the true number of the categories of the instrument is larger than two the LIV estimates are approximately consistent. Besides, overfitting in terms
#'of the number of groups/categories reduces the degrees of freedom and leads to efficiency loss. For a model with additonal explanatory variables a Bayesian approach is needed, since
#'in a frequentist approach identification issues appear. The optimization algorithm used is BFGS.
#'
#Return Value
#'@return It returns the optimal values of the parameters as computed by maximum likelihood using BFGS algorithm. To obtain the
#'standard errors bootsptrapping is needed, using the boots() function from the same package.
#'\item{coefficients}{returns the value of the parameters for the intercept and the endogenous regressor as computed with maximum likelihood.}
#'\item{means}{returns the value of the parameters for the means of the two categories/groups of the latent instrumental variable.}
#'\item{sigma}{returns the variance-covariance matrix sigma, where on the main diagonal are the variances of the structural error and that of
#'the endogenous regressor and the off-diagonal terms are equal to the covariance between the errors.}
#'\item{probabilityG1}{returns the probability of group 1. Since the model assumes that the latent instrumental variable has two groups,
#'1-probabilityG1 gives the probability of group 2.}
#'\item{value}{the value of the log-likelihood function corresponding to param.}
#'\item{convergence}{An integer code, the same as the output returned by optim. 0 indicates successful completion. A possible error code is 1 which dicates that the iteration
#'limit maxit had been reached.}
#'\item{hessian}{A symmetric matrix giving an estimate of the Hessian at the solution found.}
#'@keywords endogenousdata:
#'@keywords latent
#'@keywords instruments
#'@author The implementation of the model formula by Raluca Gui based on the paper of Ebbes et al. (2005).
#'@references   Ebbes, P., Wedel,M., Boeckenholt, U., and Steerneman, A. G. M. (2005). 'Solving and testing for regressor-error
#'(in)dependence when no instrumental variables are available: With new evidence for the effect of education on income'. Quantitative Marketing and Economics,
#' 3:365--392.
#make availble to the package users
#'@export
liv <- function(formula, param){

  if( ncol(get_all_vars(formula)) != 2 )
    stop("A wrong number of parameters were passed in the formula")

  y <- get_all_vars(formula)[1]
  P <- as.matrix(get_all_vars(formula)[2])

  b <- optimx::optimx( par=param,fn=logL, y=y,P=P,
               method="BFGS",hessian=T,
               control=list(trace=0))

  obj <- new("liv")
  obj@formula <- formula

  slot(obj, "coefficients") <- c(b$p1, b$p2)      # coefficients

  obj@group_means <-  c(b$p3, b$p4)     # means of the 2 groups of the latent IV

  # variance-covariance matrix of errors
  obj@sigma <- matrix(c(b$p5^2,b$p5*b$p6,b$p5*b$p6,b$p6^2+b$p7^2),2,2)

  obj@prob_G1 <- exp(b$p8)     # probability of group 1

  obj@value <- b$value          # the value of the likelihood function corresponding to param
  obj@convergence <- as.integer(b$convcode)    # message whether if converged

  hess <- attr(b,"details")[,"nhatend"]         # hessian matrix
  std_par <- sqrt(diag(solve(do.call(rbind,hess))))
  obj@hessian <- hess[[1]]


  obj@se_coefficients <- std_par[1:2]
  obj@se_means <- std_par[3:4]
  obj@se_probG1 <- std_par[8]

  return(obj)
}
