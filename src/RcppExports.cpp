// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// als_implicit
void als_implicit(const arma::sp_mat& mat, arma::mat& X, arma::mat& XtX, arma::mat& Y, int n_threads);
RcppExport SEXP pkgrecommendr_als_implicit(SEXP matSEXP, SEXP XSEXP, SEXP XtXSEXP, SEXP YSEXP, SEXP n_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::sp_mat& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type XtX(XtXSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< int >::type n_threads(n_threadsSEXP);
    als_implicit(mat, X, XtX, Y, n_threads);
    return R_NilValue;
END_RCPP
}
// als_loss
double als_loss(const arma::sp_mat& mat, arma::mat& X, arma::mat& Y, double lambda, int n_threads);
RcppExport SEXP pkgrecommendr_als_loss(SEXP matSEXP, SEXP XSEXP, SEXP YSEXP, SEXP lambdaSEXP, SEXP n_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::sp_mat& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type n_threads(n_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(als_loss(mat, X, Y, lambda, n_threads));
    return rcpp_result_gen;
END_RCPP
}
// get_nn
IntegerMatrix get_nn(NumericMatrix x, int k, int n_threads);
RcppExport SEXP pkgrecommendr_get_nn(SEXP xSEXP, SEXP kSEXP, SEXP n_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type n_threads(n_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(get_nn(x, k, n_threads));
    return rcpp_result_gen;
END_RCPP
}
// top_k_indices_byrow
IntegerMatrix top_k_indices_byrow(NumericMatrix x, arma::sp_mat mat, int k, int n_threads);
RcppExport SEXP pkgrecommendr_top_k_indices_byrow(SEXP xSEXP, SEXP matSEXP, SEXP kSEXP, SEXP n_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type mat(matSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type n_threads(n_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(top_k_indices_byrow(x, mat, k, n_threads));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"pkgrecommendr_als_implicit", (DL_FUNC) &pkgrecommendr_als_implicit, 5},
    {"pkgrecommendr_als_loss", (DL_FUNC) &pkgrecommendr_als_loss, 5},
    {"pkgrecommendr_get_nn", (DL_FUNC) &pkgrecommendr_get_nn, 3},
    {"pkgrecommendr_top_k_indices_byrow", (DL_FUNC) &pkgrecommendr_top_k_indices_byrow, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_pkgrecommendr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}