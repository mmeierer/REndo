#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
double copulaCorrection_LL_rcpp(const Rcpp::NumericVector& params,
                                const Rcpp::NumericVector& vec_y,
                                const Rcpp::NumericMatrix& m_data_exo_endo,
                                const Rcpp::NumericVector& vec_data_endo_pstar,
                                const Rcpp::NumericVector& param_pos_data,
                                const double param_pos_sigma,
                                const double param_pos_rho){

  // # Extract params from optimx inputs --------------------------------------------------------
  Rcpp::NumericVector params_endo_exo = params[param_pos_data - 1];
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
  Rcpp::CharacterVector m_colnames = Rcpp::colnames(m_data_exo_endo);
  params_endo_exo = params_endo_exo[m_colnames];

  // R function matrix multiplication
  // Rcpp::Environment base("package:base");
  // Rcpp::Function R_matmult = base["%*%"];
  // Rcpp::NumericVector matMultRes = R_matmult(m_data_exo_endo, params_endo_exo);

  // arma:: Matrix multiplication
  // const arma::mat arma_data_exo_endo = Rcpp::as<arma::mat>(m_data_exo_endo);
  // const arma::vec arma_param_endo_exo = Rcpp::as<arma::vec>(params_endo_exo);
  // Rcpp::NumericVector matMultRes = Rcpp::wrap(arma_data_exo_endo * arma_param_endo_exo);

  // Short excursion to RcppEigen: Matrix multiplication
  const Eigen::MatrixXd A = Rcpp::as<Eigen::MatrixXd>(m_data_exo_endo);
  const Eigen::VectorXd B = Rcpp::as<Eigen::VectorXd>(params_endo_exo);
  Eigen::MatrixXd C = A * B;
  // Back to regular Rcpp again
  Rcpp::NumericVector matMultRes = Rcpp::wrap(A*B);

  Rcpp::NumericVector eps_1 = vec_y - matMultRes;

  // # PPnorm -----------------------------------------------------------------------------------
  // # residulas - epsilon should be normally distributed
  Rcpp::NumericVector ppnorm   = Rcpp::pnorm(eps_1, 0.0, sigma);
  ppnorm[ppnorm >= 0.999998]   = 0.999998;
  ppnorm[ppnorm <= 0.0001]     = 0.0001;

  // # epsilon star -----------------------------------------------------------------------------
  // eps.star <- stats::qnorm(ppnorm)
  Rcpp::NumericVector eps_star = Rcpp::qnorm(ppnorm);

  // # l.eps ------------------------------------------------------------------------------------
  // l.eps <- sum(dnorm(eps.1,mean=0,sd=sigma, log = TRUE))
  double l_eps = Rcpp::sum(Rcpp::dnorm(eps_1, 0.0, sigma, true));

  // # s
  // s <- sum((vec.data.endo.pstar^2 + eps.star^2)/(2*(1-rho^2)) -
  //   (rho*vec.data.endo.pstar*eps.star)/(1-rho^2))
  double s = Rcpp::sum((Rcpp::pow(vec_data_endo_pstar,2.0) + Rcpp::pow(eps_star,2.0)) /
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
