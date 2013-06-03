#ifndef SIMULATEEKOP_CC
#define SIMULATEEKOP_CC

#include <RcppArmadillo.h>

#include <R.h>
#include <R_ext/Utils.h>
#include <Rdefines.h>
#include <Rinternals.h>

void simulateEKOP(const unsigned int nobs, const double alpha,
			const double epsilon, const double delta, 
			const double mu, const double T, arma::umat &sample)
{
	GetRNGstate();
	for(unsigned int i = 0; i < nobs; ++i) {
		if(R::runif(0, 1) < alpha) {
			if(R::runif(0, 1) > delta) {
				sample(i, 1) = R::rpois((epsilon + mu) * T);
				sample(i, 2) = R::rpois(epsilon * T);
				sample(i, 3) = sample(i, 1) + sample(i, 2); 
			}
			else {
				sample(i, 1) = R::rpois(epsilon * T);
				sample(i, 2) = R::rpois((epsilon + mu) * T);
				sample(i, 3) = sample(i, 1) + sample(i, 2);
			}
		}
		else {
			sample(i, 1) = R::rpois(epsilon * T);
			sample(i, 2) = R::rpois(epsilon * T);
			sample(i, 3) = sample(i, 1) + sample(i, 2);
		}
	}
	PutRNGstate();
}
RcppExport SEXP simulateEKOP_cc(SEXP size_R, SEXP alpha_R, SEXP epsilon_R,
				SEXP delta_R, SEXP mu_R, SEXP T_R) 
{
	const unsigned int nobs = Rcpp::as<unsigned int>(size_R); 
	const double alpha = Rcpp::as<double>(alpha_R);
	const double epsilon = Rcpp::as<double>(epsilon_R);
	const double delta = Rcpp::as<double>(delta_R);
	const double mu = Rcpp::as<double>(mu_R);
	const double T = Rcpp::as<double>(T_R);
	arma::umat sample = arma::zeros<arma::umat>(nobs, 4);
	simulateEKOP(nobs, alpha, epsilon, delta, mu, T, sample);
	return Rcpp::wrap(sample);
}
#endif
