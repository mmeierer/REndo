#'@title Inverse-Normal Distribution of the Empirical Distribution Function
#'
# Description
#'@description  Computes the empirical distribution function of a variable and the inverse-normal distribution of ECDF.
#
# Arguments
#'@param      P  the variable for which the inverse-normal distribution of its empirical distribution function is needed.
#
# Return Value
#'@return Returns the inverse-normal distribution of the empirical distribution function of variable P.
#'@keywords endogeneity
#'@keywords copula
#'@seealso \code{\link{copulaEndo}}
copulaPStar <- function(P){
	
	H.p <- stats::ecdf(P)

	H.p <- H.p(P)

	H.p <- ifelse(H.p==0,0.0000001,H.p)
	H.p <- ifelse(H.p==1,0.9999999,H.p)

	U.p <- H.p
	p.star <- stats::qnorm(U.p)
	p.star <- ifelse(p.star==0,0.0000001,p.star)
	return(p.star)
	
}