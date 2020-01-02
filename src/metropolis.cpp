#include <Rcpp.h>
using namespace Rcpp;

//' @title Use three inputs to predict response using R.
//' @description Homework to deal with Rcpp
//' @param sigma the varients in normal distribution (numeric)
//' @param x0 the first number (numeric)
//' @param N the number you want to get (numeric)
//' @import microbenchmark
//' @importFrom Rcpp evalCpp
//' @importFrom stats rnorm rgamma
//' @useDynLib SC19088
//' @examples
//' \dontrun{
//' Metropolis(0.05,25,1000)
//' }
//' @export
// [[Rcpp::export]]
List metropolis(double sigma, double x0, int N){
  NumericVector  x(N);
  NumericVector  u=runif(N);
  x[0]=x0;
  int k=0;
  for(int i = 1; i < N; ++i) {
    double y=rnorm(1,x[i-1],sigma)[0];
    double p=exp(-abs(y))/exp(-abs(x[i-1]));
    if(u[i-1]<= p) {x[i]=y;}
    else{x[i]=x[i-1]; k=k+1; } 
  }
  List L=List::create(Named("x")=x,
                      Named("k")=k);
  return L;
}

