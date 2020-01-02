// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// metropolis
List metropolis(double sigma, double x0, int N);
RcppExport SEXP _SC19088_metropolis(SEXP sigmaSEXP, SEXP x0SEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< double >::type x0(x0SEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(metropolis(sigma, x0, N));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SC19088_metropolis", (DL_FUNC) &_SC19088_metropolis, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_SC19088(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
