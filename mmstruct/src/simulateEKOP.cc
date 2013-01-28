#ifndef SIMULATEEKOP_CC
#define SIMULATEEKOP_CC

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

void simulateEKOP(const unsigned int nobs, double alpha, double epsilon, double delta, double mu, 
		double T, SEXP& sample_SEXP){
		
		scythe::Matrix<> sample(nobs, 4);
		mersenne mt_rng;
		mt_rng.initialize(1); 
	  	Rprintf("alpha: %f\n", 1.222);
		Rprintf("epsilon: %f\n", epsilon);	
		Rprintf("delta: %f\n", delta);
		Rprintf("mu: %f\n", mu);
		for(unsigned int i = 0; i < nobs; ++i) {
			if(mt_rng() < alpha) {
				if(mt_rng() > delta) {
					sample(i, 1) = mt_rng.rpois((epsilon + mu) * T);
					sample(i, 2) = mt_rng.rpois(epsilon * T);
					sample(i, 3) = sample(i, 1) + sample(i, 2);
				}
				else {
					sample(i, 1) = mt_rng.rpois(epsilon * T);
					sample(i, 2) = mt_rng.rpois((epsilon + mu) * T);
					sample(i, 3) = sample(i, 1) + sample(i, 2);
				}
			}
			else {
				sample(i, 1) = mt_rng.rpois(epsilon * T);
				sample(i, 2) = mt_rng.rpois(epsilon * T);
				sample(i, 3) = sample(i, 1) + sample(i, 2);
			}	
		}

		for(unsigned int i = 0; i < nobs; ++i) {
			for(unsigned int j = 0; j < 4; ++j) {
				REAL(sample_SEXP)[i + j * nobs] = sample(i, j);
			}
		}
}

extern "C" {
	SEXP simulateEKOP_cc(SEXP size_R, SEXP alpha_R, SEXP epsilon_R, SEXP delta_R, SEXP mu_R, SEXP T_R) {
		const unsigned int nobs = INTEGER(size_R)[0];
		SEXP sample_SEXP;
		PROTECT(sample_SEXP = allocMatrix(REALSXP, nobs, 4));

		simulateEKOP(nobs, REAL(alpha_R)[0], REAL(epsilon_R)[0], REAL(delta_R)[0], REAL(mu_R)[0],
				REAL(T_R)[0], sample_SEXP);
		UNPROTECT(1);
		return sample_SEXP;
	}
}
#endif
