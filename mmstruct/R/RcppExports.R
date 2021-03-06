# This file was generated by Rcpp::compileAttributes
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

simulateEKOP_cc <- function(nobs, alpha, epsilon, delta, mu, T) {
    .Call('mmstruct_simulateEKOP_cc', PACKAGE = 'mmstruct', nobs, alpha, epsilon, delta, mu, T)
}

simulateEKOPMis_cc <- function(nobs, alpha, epsilon, delta, mu, mis, T) {
    .Call('mmstruct_simulateEKOPMis_cc', PACKAGE = 'mmstruct', nobs, alpha, epsilon, delta, mu, mis, T)
}

dtparse_cc <- function(time_vec, required_comp) {
    .Call('mmstruct_dtparse_cc', PACKAGE = 'mmstruct', time_vec, required_comp)
}

dtparseV_cc <- function(time_vec, required_comp) {
    .Call('mmstruct_dtparseV_cc', PACKAGE = 'mmstruct', time_vec, required_comp)
}

dtparseV_no_del2_cc <- function(time_vec, required_comp) {
    .Call('mmstruct_dtparseV_no_del2_cc', PACKAGE = 'mmstruct', time_vec, required_comp)
}

dtparseV_no_del4_cc <- function(time_vec, required_comp) {
    .Call('mmstruct_dtparseV_no_del4_cc', PACKAGE = 'mmstruct', time_vec, required_comp)
}

tparseV_cc <- function(time_vec) {
    .Call('mmstruct_tparseV_cc', PACKAGE = 'mmstruct', time_vec)
}

dtparseV_format_cc <- function(day_vec, time_vec, day_format) {
    .Call('mmstruct_dtparseV_format_cc', PACKAGE = 'mmstruct', day_vec, time_vec, day_format)
}

