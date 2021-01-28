#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

// To increase readability for R users, do not put Rcpp:: before every command
using namespace Rcpp;

// [[Rcpp::export]]
double copulaCorrection_LL_rcpp(const NumericVector& params,
                                const NumericVector& vec_y,
                                const NumericMatrix& m_data_exo_endo,
                                const NumericVector& vec_data_endo_pstar,
                                const NumericVector& param_pos_data,
                                const double& param_pos_sigma,
                                const double& param_pos_rho){

  // # Extract params from optimx inputs --------------------------------------------------------
  NumericVector params_endo_exo       = params[param_pos_data - 1];
  double sigma                        = params[param_pos_sigma - 1];
  double rho                          = params[param_pos_rho - 1];

  // # Constrain rho to [0,1] and sigma to [0, +Inf]
  // #   (incl bound because can be very large which flips to 0 and 1)
  // #   The vcov is not derived from the hessian anymore, but BFGS still does not accept
  // #     NA/Inf
  // rho <- exp(rho)
  // rho <- rho / (1+rho)
  rho = std::exp(rho);
  rho = rho / (1.0+rho);

  // sigma <- exp(sigma)
  sigma = exp(sigma);

  // # epsilon, incl. endo regressor ------------------------------------------------------------
  // # Reorder params to fit matrix col order
  CharacterVector m_colnames = colnames(m_data_exo_endo);
  params_endo_exo = params_endo_exo[m_colnames];


  // Short excursion to RcppEigen: Matrix multiplication
  const Eigen::Map<Eigen::MatrixXd> A = Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(m_data_exo_endo);
  const Eigen::Map<Eigen::VectorXd> B = Rcpp::as<Eigen::Map<Eigen::VectorXd> >(params_endo_exo);

  // Math! and immediately back to regular Rcpp again
  NumericVector matMultRes = Rcpp::wrap(A*B);

  NumericVector eps_1 = vec_y - matMultRes;

  // # PPnorm -----------------------------------------------------------------------------------
  // # residulas - epsilon should be normally distributed
  NumericVector ppnorm         = pnorm(eps_1, 0.0, sigma);
  ppnorm[ppnorm >= 0.999998]   = 0.999998;
  ppnorm[ppnorm <= 0.0001]     = 0.0001;

  // # epsilon star -----------------------------------------------------------------------------
  // eps.star <- stats::qnorm(ppnorm)
  NumericVector eps_star = qnorm(ppnorm);

  // # l.eps ------------------------------------------------------------------------------------
  // l.eps <- sum(dnorm(eps.1,mean=0,sd=sigma, log = TRUE))
  double l_eps = sum(dnorm(eps_1, 0.0, sigma, true));

  // # s
  // s <- sum((vec.data.endo.pstar^2 + eps.star^2)/(2*(1-rho^2)) -
  //   (rho*vec.data.endo.pstar*eps.star)/(1-rho^2))
  double s = sum((pow(vec_data_endo_pstar,2.0) + pow(eps_star,2.0)) /
                       (2*(1-std::pow(rho,2.0))) -
             (rho*vec_data_endo_pstar*eps_star)/(1-std::pow(rho,2.0)));
  // # mm
  // mm <- (length(vec.y)/2)* log(1-rho^2)
  double mm = (vec_y.size()/2.0)*std::log(1.0-std::pow(rho,2.0));

  // # LL
  // log.LL <- -mm - s + l.eps
  double log_LL = -mm - s + l_eps;
  return(-1 * log_LL);
}
