
#include "dtparser.h"
#include <string>
// [[Rcpp::export]]

Rcpp::NumericVector dtparse_cc (Rcpp::CharacterVector time_vec, 
        const int required_comp) 
{
    const unsigned int N = time_vec.size();
    Rcpp::NumericVector num_time(N);
    int lth = 0;
    for (unsigned int i = 0; i < N; ++i) {
        lth = time_vec(i).size();
        num_time(i) = dtparsechar((char*) time_vec(i), lth, required_comp);
    }

    return num_time;
}

// [[Rcpp::export]]

Rcpp::NumericVector dtparseV_cc (Rcpp::CharacterVector time_vec,
        const int required_comp)
{
    return dtparseV(time_vec, required_comp);
}

// [[Rcpp::export]]

Rcpp::NumericVector dtparseV_no_del2_cc (Rcpp::CharacterVector time_vec, 
        const int required_comp)
{
    return dtparseV_no_del2 (time_vec, required_comp);
}

// [[Rcpp::export]]

Rcpp::NumericVector dtparseV_no_del4_cc (Rcpp::CharacterVector time_vec, 
        const int required_comp)
{
    return dtparseV_no_del4 (time_vec, required_comp);
}

// [[Rcpp::export]]

Rcpp::NumericVector tparseV_cc (Rcpp::CharacterVector time_vec)
{
    return tparseV(time_vec);
}

// [[Rcpp::export]]

Rcpp::NumericVector dtparseV_format_cc (Rcpp::CharacterVector day_vec, 
        Rcpp::CharacterVector time_vec, const std::string day_format)
{
    if (day_format.compare("YYYYmmdd") == 0) {
        return dtparseV_no_del4(day_vec, 3) + tparseV(time_vec);
    } else if (day_format.compare("YYmmdd") == 0) {
        return dtparseV_no_del2(day_vec, 3) + tparseV(time_vec);
    } else {
        return dtparseV(day_vec, 3) + tparseV(time_vec);   
    }
}
