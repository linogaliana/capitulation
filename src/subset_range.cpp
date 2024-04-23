#include <Rcpp.h>
using namespace Rcpp;

//' Wrapper for vector subsetting in Rcpp
//' 
//' @param x Numeric vector
//' @param start Index start. Indexation follows C++ rule hence begins at 0
//' @param end Index of ending. Indexation follows C++ rule hence ends at n-1
//' 
//' @return x values between \code{start} and \code{end}
// [[Rcpp::export]]
Rcpp::NumericVector subset_range(Rcpp::NumericVector x,
                                 int start = 0, int end = 1) {
  
  // Use the Range function to create a positional index sequence
  return x[Rcpp::Range(start, end)];
}
