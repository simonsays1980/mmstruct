#ifndef SIMULATEEKOPMIS_CC
#define SIMULATEEKOPMIS_CC

#include "include/scythestat/matrix.h"
#include "include/scythestat/rng/mersenne.h"
#include "include/scythestat/distributions.h"
#include "MCfcds.h"

#include<R.h>
#include<R_ext/Utils.h>
#include<Rmath.h>
#undef rpois 
#include<Rdefines.h>
#include<Rinternals.h>
#include<omp.h>

void simulateEKOPMis(const unsigned int nobs, const double alpha, const double epsilon, const double delta, const double mu, 
		const double T, const double mis, SEXP& sample_SEXP){
		
		scythe::Matrix<> sample(nobs, 4);
		mersenne mt_rng;
		mt_rng.initialize(1); 
		double x = 0;
		double d_buy;
		double d_sell;
		double d_mis;

		for(unsigned int i = 0; i < nobs; ++i) {
			d_buy = 0;
			d_sell = 0;
			d_mis = 0;
			/* positive news event */
			if(mt_rng() < alpha) {
				if(mt_rng() > delta) {
					/* generate uninformed order flow */
					x = mt_rng.rpois(2 * epsilon * T);
					for(unsigned int j = 0; j < x; ++j) {
						if(mt_rng() < 0.5) {
							if(mt_rng() > mis) {
								d_buy += 1;
							}
							else {
								d_sell += 1;
								d_mis += 1;
							}
						} 
						else {	
							if(mt_rng() > mis) {
								d_sell += 1;
							}
							else {
								d_buy += 1;
								d_mis += 1;
							}
						}
					}
					/* generate informed order flow */
					x = mt_rng.rpois(mu * T);
					for(unsigned int j = 0; j < x; ++j) {
						if(mt_rng() > 15) {
							d_buy += 1;
						}
						else {
							d_sell += 1;
							d_mis += 1;
						}
					}
					sample(i, 0) = d_mis;
					sample(i, 1) = d_buy;
					sample(i, 2) = d_sell;
					sample(i, 3) = d_sell + d_buy;
				}
				/* negative news event */
				else {
					/* generate uninformed order flow */
				       	x = mt_rng.rpois(2 * epsilon * T);
					for(unsigned int j = 0; j < x; ++j) {
						if(mt_rng() < 0.5) {
							if(mt_rng() > mis) {
								d_buy += 1;
							}
							else {
								d_sell += 1;
								d_mis += 1;
							}
						} 
						else {	
							if(mt_rng() > mis) {
								d_sell += 1;
							}
							else {
								d_buy += 1;
								d_mis += 1;
							}
						}
					}
					/* generate informed order flow */
					x = mt_rng.rpois(mu * T);
					for(unsigned int j = 0; j < x; ++j) {
						if(mt_rng() > mis) {
							d_sell += 1;
						}
						else {
							d_buy += 1;
							d_mis += 1;
						}
					}
					sample(i, 0) = d_mis;
					sample(i, 1) = d_buy;
					sample(i, 2) = d_sell;
					sample(i, 3) = d_sell + d_buy;
				}
			}
			/* no news event */
			else {
				x = mt_rng.rpois(2 * epsilon * T);
				for(unsigned int j = 0; j < x; ++j) {
					if(mt_rng() < 0.5) {
						if(mt_rng() > mis) {
							d_buy += 1;
						}
						else {
							d_sell += 1;
							d_mis += 1;
						}
					} 
					else {	
						if(mt_rng() > mis) {
							d_sell += 1;
						}
						else {
							d_buy += 1;
							d_mis += 1;
						}
					}
				}	
				sample(i, 0) = d_mis;
				sample(i, 1) = d_buy;
				sample(i, 2) = d_sell;
				sample(i, 3) = d_sell + d_buy;
			}	
		}

		for(unsigned int i = 0; i < nobs; ++i) {
			for(unsigned int j = 0; j < 4; ++j) {
				REAL(sample_SEXP)[i + j * nobs] = sample(i, j);
			}
		}
}

extern "C" {
	SEXP simulateEKOPMis_cc(SEXP size_R, SEXP alpha_R, SEXP epsilon_R, SEXP delta_R, SEXP mu_R, SEXP T_R, SEXP mis_R) {
		const unsigned int nobs = INTEGER(size_R)[0];
		SEXP sample_SEXP;
		PROTECT(sample_SEXP = allocMatrix(REALSXP, nobs, 4));

		simulateEKOPMis(nobs, REAL(alpha_R)[0], REAL(epsilon_R)[0], REAL(delta_R)[0], REAL(mu_R)[0],
				REAL(T_R)[0], REAL(mis_R)[0], sample_SEXP);
		UNPROTECT(1);
		return sample_SEXP;
	}
}
#endif
