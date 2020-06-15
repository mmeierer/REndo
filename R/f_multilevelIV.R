#'
#' @title Fitting Multilevel GMM Estimation with Endogenous Regressors
#'
#' @template template_param_formuladataverbose
#' @param lmer.control An output from \code{lmerControl} that will be used to fit the \code{lmer} model from which the variance and
#' correlation are obtained.
#'
#' @description
#' Estimates multilevel models (max. 3 levels) employing the GMM approach presented in Kim and Frees (2007).
#' One of the important features is that, using the hierarchical structure of the data, no external instrumental
#' variables are needed, unlike traditional instrumental variable techniques. Specifically, the approach controls for
#' endogeneity at higher levels in the data hierarchy. For example, for a three-level model, endogeneity can be handled
#' either if present at level two, at level three or at both levels. Level one endogeneity, where the regressors are correlated
#' with the structural errors (errors at level one), is not addressed. Moreover, if considered, random slopes cannot be endogenous.
#' Also, the dependent variable has to have a continuous distribution.
#' The function returns the coefficient estimates obtained with fixed effects, random effects and the GMM estimator proposed
#' by Kim and Frees (2007), such that a comparison across models can be done.
#' Asymptotically, the multilevel GMM estimators share the same properties of corresponding fixed effects estimators, but they
#' allow the estimation of all the variables in the model, unlike the fixed effects counterpart.
#'
#' To facilitate the choice of the estimator to be used for the given data, the function also conducts
#' omitted variable test based on the Hausman-test for panel data (Hausman, 1978). It allows to compare
#' a robust estimator and an estimator that is efficient under the null hypothesis of no omitted variables,
#' and to compare two robust estimators at different levels. The results of these tests are returned when
#' calling \code{\link[REndo:summary.rendo.multilevel]{summary()}} on a fitted model.
#'
#' @details
#' \subsection{Method}{
#' Multilevel modeling is a generalization of regression methods that recognize the existence of such data hierarchies
#' by allowing for residual components at each level in the hierarchy. For example, a three-level multilevel model which
#' allows for grouping of students within classrooms, over time, would include time, student and classroom residuals
#' (see equation below). Thus, the residual variance is partitioned into four components:
#' between-classroom (the variance of the classroom-level residuals), within-classroom (the variance of the student-level residuals),
#' between student (the variance of the student-level residuals) and within-student (the variance of the time-level residuals).
#' The classroom residuals represent the unobserved classroom characteristics that affect student's outcomes.
#' These unobserved variables lead to correlation between outcomes for students from the same classroom.
#' Similarly, the unobserved time residuals lead to correlation between a student's outcomes over time.
#' A three-level model can be described as follows:
#'
#' \ifelse{html}{
#' \out{ <center>y<sub>cst</sub> = Z<sup>1</sup><sub>cst</sub> &beta;<sup>1</sup><sub>cs</sub> + X<sup>1</sup><sub>cst</sub> &beta;<sub>1</sub> &epsilon;<sup>1</sup><sub>cst</sub> </center>
#' <br><center> &beta;<sup>1</sup><sub>cs</sub> =  Z<sup>2</sup><sub>cs</sub> &beta;<sup>2</sup><sub>c</sub> + X <sup>2</sup><sub>cs</sub> &beta;<sub>2</sub> &epsilon;<sup>2</sup><sub>cs</sub> </center>
#' <br><center> &beta;<sup>2</sup><sub>c</sub> = X<sup>3</sup><sub>c</sub>&beta;<sub>3</sub> &epsilon;<sup>3</sup><sub>c</sub> </center>}}{
#'  \deqn{
#'  y_{cst} = Z^{1}_{cst} \beta^{1}_{cs} + X^{1}_{cst} \beta_{1} +\epsilon^{1}_{cst}}
#'  \deqn{
#'  \beta^{1}_{cs} = Z^{2}_{cs} \beta^{2}_{c} + X^{2}_{cs} \beta_{2} +\epsilon^{2}_{cs}}
#'  \deqn{
#'  \beta^{2}_{c} = X^{3}_{c} \beta_{3} +\epsilon^{3}_{c}}.}
#'
#' Like in single-level regression, in multilevel models endogeneity is also a concern. The additional problem is that in multilevel models
#' there are multiple independent assumptions involving various random components at different levels. Any moderate correlation between some
#' predictors and a random component or error term, can result in a significant bias of the coefficients and of the variance components.
#' The multilevel GMM approach for addressing endogeneity uses both the between and within variations of the exogenous variables, but only the within
#' variation of the variables assumed endogenous. The assumptions in the multilevel generalized moment of moments model is that the errors at each level
#' are normally distributed and independent of each other. Moreover, the slope variables are assumed exogenous. Since the model does not handle
#' "level 1 dependencies", an additional assumption is that the level 1 structural error is uncorrelated with any of the regressors.
#' If this assumption is not met, additional, external instruments are necessary.
#' The coefficients of the explanatory variables appear in the vectors \ifelse{html}{\out{&beta;<sub>1</sub>}}{\eqn{\beta_{1}}},
#' \ifelse{html}{\out{&beta;<sub>2</sub>}}{\eqn{\beta_{2}}} and \ifelse{html}{\out{&beta;<sub>3</sub>}}{\eqn{\beta_{3}}}.
#' The term \ifelse{html}{\out{&beta;<sup>1</sup><sub>cs</sub>}}{\eqn{\beta^{1}_{cs}}} captures latent, unobserved characteristics that are classroom and student specific
#' while \ifelse{html}{\out{&beta;<sup>2</sup><sub>c</sub>}}{\eqn{\beta^{2}_{c}}} captures latent,
#' unobserved characteristics that are classroom specific. For identification, the disturbance term
#' \ifelse{html}{\out{&epsilon;<sub>cst</sub>}}{\eqn{\epsilon_{cst}}} is assumed independent of
#' the other variables, \ifelse{html}{\out{Z<sup>1</sup><sub>cst</sub>}}{\eqn{Z^{1}_{cst}}} and \ifelse{html}{\out{X<sup>1</sup><sub>cst</sub>}}{\eqn{X^{1}_{cst}}}.
#' When all model variables are assumed exogenous, the GMM estimator is the usual GLS estimator, denoted as REF. When all variables (except the variables used as slope)
#' are assumed endogenous, the fixed-effects estimator is used, FE. While REF assumes all explanatory variables are uncorrelated with the
#' random intercepts and slopes in the model, FE allows for endogeneity of all effects but sweeps out the random components as well as the
#' explanatory variables at the same levels. The more general estimator GMM proposed by Kim and Frees (2007) allows for some of the explanatory
#' variables to be endogenous and uses this information to build instrumental variables. The multilevel GMM estimator uses both the between and
#' within variations of the exogenous variables, but only the within variation of the variables assumed endogenous. When all variables are assumed
#' exogenous, GMM estimator equals REF. When all covariates are assume endogenous, GMM equals FE.
#' }
#'
#'\subsection{Formula parameter}{
#'
#' The \code{formula} argument follows a two part notation:
#'
#' In the first part, the model is specified while in the second part, the endogenous regressors are indicated.
#' These two parts are separated by a single vertical bar (\code{|}).
#'
#' The first RHS follows the exact same model specification as required by the \code{\link[lme4]{lmer}}
#' function of package \code{lme4} and internally will be used to fit a \code{lmer} model. In the second part,
#' one or multiple endogenous regressors are indicated by passing them to the special function \code{endo}
#' (e.g. \code{endo(X1, X2)}). Note that no argument to \code{endo()} is to be supplied as character
#' but as symbols without quotation marks.
#'
#' See the example section for illustrations on how to specify the \code{formula} parameter.
#' }
#'
#' @seealso \code{\link[lme4]{lmer}} for more details on how to specify the \code{formula} parameter
#' @seealso \code{\link[lme4]{lmerControl}} for more details on how to provide the \code{lmer.control} parameter
#' @seealso \code{\link[REndo:summary.rendo.multilevel]{summary}} for how fitted models are summarized
#'
#' @return
#' \code{multilevelIV} returns an object of class "\code{rendo.multilevel}".
#'
#' The generic accessor functions \code{coef}, \code{fitted}, \code{residuals}, \code{vcov}, \code{confint}, and \code{nobs}, are available.
#' Note that an additional argument \code{model} with possible values \code{"REF", "FE_L2", "FE_L3", "GMM_L2"}, or \code{"GMM_L3"} is
#' available for \code{\link[REndo:summary.rendo.multilevel]{summary}}, \code{fitted}, \code{residuals}, \code{confint}, and \code{vcov}
#' to extract the features for the specified model.
#'
#' Note that the obtained coefficients are rounded with \code{round(x, digits=getOption("digits"))}.
#'
#'
#' An object of class \code{rendo.multilevel} is returned that is a list and contains the following components:
#'
#' \item{formula}{the formula given to specify the model to be fitted.}
#' \item{num.levels}{the number of levels detected from the model.}
#' \item{dt.model.data}{a data.table of model data including data for slopes and level group ids}
#' \item{coefficients}{a matrix of rounded coefficients, one column per model.}
#' \item{coefficients.se}{a matrix of coefficients' SE, one column per model.}
#' \item{l.fitted}{a named list which contains the fitted values per model sorted as the input data}
#' \item{l.residuals}{a named list which contains the residuals per model sorted as the input data}
#' \item{l.vcov}{a list of variance-covariance matrix, named per model.}
#' \item{V}{the variance–covariance matrix V of the disturbance term.}
#' \item{W}{the weight matrix W, such that W=V^(-1/2) per highest level group.}
#' \item{l.ovt}{a list of results of the Hausman OVT, named per model.}
#'
#' @examples
#' \donttest{
#' data("dataMultilevelIV")
#'
#' # Two levels
#' res.ml.L2 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
#'                               X32 + X33 + (1|SID) | endo(X15),
#'                           data = dataMultilevelIV, verbose = FALSE)
#'
#' # Three levels
#' res.ml.L3 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
#'                               X32 + X33 + (1| CID) + (1|SID) | endo(X15),
#'                           data = dataMultilevelIV, verbose = FALSE)
#'
#'
#' # L2 with multiple endogenous regressors
#' res.ml.L2 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
#'                               X32 + X33 + (1|SID) | endo(X15, X21, X22),
#'                           data = dataMultilevelIV, verbose = FALSE)
#'
#' # same as above
#' res.ml.L2 <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 + X31 +
#'                               X32 + X33 + (1|SID) | endo(X15, X21) + endo(X22),
#'                           data = dataMultilevelIV, verbose = FALSE)
#'
#' # Fit above model with different settings for lmer()
#' lmer.control <- lme4::lmerControl(optimizer="nloptwrap",
#'                                   optCtrl=list(algorithm="NLOPT_LN_COBYLA",
#'                                                xtol_rel=1e-6))
#' res.ml.L2.cob <- multilevelIV(y ~ X11 + X12 + X13 + X14 + X15 + X21 + X22 + X23 + X24 +
#'                                   X31 + X32 + X33 + (1|SID) | endo(X15, X21) + endo(X22),
#'                               data = dataMultilevelIV, verbose = FALSE,
#'                               lmer.control = lmer.control) # use different controls for lmer
#'
#'
#' # specify argument "model" in the S3 methods to obtain results for the respective model
#' # default is "REF" for all methods
#'
#' summary(res.ml.L3)
#' # same as above
#' summary(res.ml.L3, model = "REF")
#'
#' # complete pval table for L3 fixed effects
#' L3.FE.p <- coef(summary(res.ml.L3, model = "FE_L3"))
#'
#' # variance covariance matrix
#' L2.FE.var  <- vcov(res.ml.L2, model = "FE_L2")
#' L2.GMM.var <- vcov(res.ml.L2, model = "GMM_L2")
#' # residuals
#' L3.REF.resid <- resid(res.ml.L3, model = "REF")
#' }
#'
#' @references
#' Hausman J (1978). “Specification Tests in Econometrics.” Econometrica, 46(6), 1251–1271.
#'
#' Kim, Jee-Seon and Frees, Edward W. (2007). "Multilevel Modeling with Correlated Effects". Psychometrika, 72(4), 505-533.
#'
#' @importFrom lme4 lmer VarCorr lFormula nobars lmerControl
#' @importFrom Formula as.Formula
#' @importFrom data.table as.data.table
#' @export
multilevelIV <- function(formula, data, lmer.control=lmerControl(optimizer = "Nelder_Mead", optCtrl=list(maxfun=100000)), verbose=TRUE){
  # As default optimizer for consistent estimates use NelderMead (100k evals to be sure)

  .SD <- NULL # cran silence

  cl <- match.call()

  # Model Definitions -----------------------------------------------------------
  # (1) = level-1 model
  #   (1)_sc = variability at child level
  #
  # (2) = level-2 model
  #   Z(2)_sc, X(2)_sc=child or school and do not vary over time
  #
  # (3) = level-3 model = variability at school level
  #   X(3)_s = depend on school
  #
  # Define:
  #   Z_2sct = Z(1)_sct
  #   Z_3sct = Z(1)_sctZ(2)_sc
  #   Z_3s = stacked Z_3sct
  #   X_sct = (X(1)_sct:Z_2sct)
  #
  #   X(1)=stacked X(1)_sc

  # Check input -----------------------------------------------------------------
  check_err_msg(checkinput_multilevel_formula(formula=formula))
  check_err_msg(checkinput_multilevel_data(data=data))
  check_err_msg(checkinput_multilevel_dataVSformula(formula=formula, data=data))
  check_err_msg(checkinput_multilevel_lmercontrol(lmer.control=lmer.control))
  check_err_msg(checkinput_multilevel_verbose(verbose = verbose))


  # Extract information ---------------------------------------------------------
  F.formula <- Formula::as.Formula(formula)
  f.lmer    <- formula(F.formula, lhs = 1, rhs = 1)
  names.endo <- formula_readout_special(F.formula = F.formula, name.special = "endo",
                                       from.rhs = 2, params.as.chars.only = TRUE)

  # Let lme4 do the formula processing
  l4.form    <- lme4::lFormula(formula = f.lmer, data=data)
  num.levels <- lme4formula_get_numberoflevels(l4.form)

  # Build model data ------------------------------------------------------------
  #
  # data.table to split into groupwise matrices
  #
  # Consists of:
  #   Response
  #   Intercept, if needed
  #   Model variables
  #   slope variables
  #   L2 / L3 Group Ids
  #   rownames, always
  #
  #   model.matrix: intercept + model vars + slope vars
  #   model.frame:  response + group ids
  #
  #   Rownames: There always are at least the standard rownames in a data.frame and it cannot
  #               be distinguished between real and standard rownames. Therefore, and to mimic
  #               stats::lm, rownames are always saved here and then added back to the results

  #
  # dependent variable
  dt.response <- as.data.table(l4.form$fr[, 1, drop = FALSE], keep.rownames = "rownames")
  name.y      <- colnames(l4.form$fr)[1]  # always at first position, same as model.response reads out

  #
  # FE / X
  #   X1 is everything but endogenous
  dt.FE    <- as.data.table(l4.form$X)
  names.X  <- colnames(dt.FE)
  names.X1 <- setdiff(names.X, names.endo)

  #
  # Slopes / Z
  #   Has to be taken from X (model matrix) in case of factors in slope
  #   Z names cannot be read as depends on number of levels
  dt.slp <- as.data.table(l4.form$X[, unique(unlist(l4.form$reTrms$cnms)), drop=FALSE])


  #
  # Group ids
  #   Do not copy from input data but from model.frame because the user could base it on
  #     transformation (ie y~X+(1|I(id == "abc"))) and because row sorting could be different
  dt.groudids <- as.data.table(l4.form$fr[, unique(unlist(names(l4.form$reTrms$cnms))), drop=FALSE])

  #
  # Make single data.table with minimum required cols only
  names.min.req.cols <- unique(c(colnames(dt.response), colnames(dt.FE),
                                 colnames(dt.slp), colnames(dt.groudids)))
  dt.model.data <- cbind(dt.response, dt.FE, dt.slp, dt.groudids)[, .SD, .SDcols = names.min.req.cols]
  rm(dt.response, dt.FE, dt.slp, dt.groudids)


  # Fit REML -----------------------------------------------------------------------------------
  # p515: "With the transformed data, we may apply any of the usual procedures for
  #         estimating variance components, including maximum likelihood, restricted maximum likelihood (REML)"
  #
  # Same for L2 and L3 cases and doing it here avoids passing and processing the formula and original data
  #   in the level functions
  # Has to use the original data because dt.model.data already contains the data
  #   with applied transformations

  if(verbose)
    message("Fitting linear mixed-effects model ",format(f.lmer),".")

  res.lmer <- tryCatch(lme4::lmer(formula = f.lmer, data=data, REML = TRUE,
                                  control = lmer.control),
                       error = function(e)return(e))
  if(is(res.lmer, "error"))
    stop("lme4::lmer() could not be fitted with error: ",
        sQuote(res.lmer$message), "\nPlease revise your data and formula.", call. = FALSE)

  res.VC <- tryCatch(lme4::VarCorr(res.lmer),
                     error = function(e)return(e))
  if(is(res.VC, "error"))
    stop("lme4::VarCorr() could not be fitted with error: ",
         sQuote(res.VC$message), "\nPlease revise your data and formula.", call. = FALSE)


  # Fit multilevel model -----------------------------------------------------------------------

  if(num.levels == 2){
    # Read Z names here as depends on num.levels
    name.groupid.L2  <- names(l4.form$reTrms$cnms)[[1]]
    names.Z2         <- l4.form$reTrms$cnms[[name.groupid.L2]]

    res <- multilevel_2levels(cl = cl, f.orig = formula, dt.model.data = dt.model.data, res.VC = res.VC,
                              name.group.L2 = name.groupid.L2, name.y = name.y, names.X = names.X,
                              names.X1 = names.X1, names.Z2 = names.Z2,
                              verbose = verbose)
  } else{

    # Read out the level names
    #   cnms is list sorted by cluster size with smalles cluster (=L2) first
    name.groupid.L2  <- names(l4.form$reTrms$cnms)[[1]]
    name.groupid.L3  <- names(l4.form$reTrms$cnms)[[2]]
    names.Z2         <- l4.form$reTrms$cnms[[name.groupid.L2]]
    names.Z3         <- l4.form$reTrms$cnms[[name.groupid.L3]]

    res <- multilevel_3levels(cl = cl, f.orig = formula, dt.model.data = dt.model.data, res.VC = res.VC,
                                name.group.L2 = name.groupid.L2, name.group.L3 = name.groupid.L3,
                                name.y = name.y, names.X = names.X, names.X1 = names.X1,
                                names.Z2 = names.Z2, names.Z3 = names.Z3,
                                verbose = verbose)
  }

  # Sort fitted and residuals by original input data (rownames)
  res$l.fitted    <- lapply(res$l.fitted,    function(fit){fit[rownames(data)]})
  res$l.residuals <- lapply(res$l.residuals, function(resid){resid[rownames(data)]})

  return(res)
}
