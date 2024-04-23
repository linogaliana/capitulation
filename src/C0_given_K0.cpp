#include <Rcpp.h>
using namespace Rcpp;

//' Compute initial consumption given initial
//'  wealth in a life-cycle framework (\code{C++} functions)
//' 
//' @param income Income vector
//' @param K0 Initial wealth (fixed, estimated or calibrated)
//' @param r Exogeneous interest rate
//' @param timeIndex Time index (t=0,...,T-1) variable
//' @return Numerical value representing first period consumption
//'  in a life-cycle model without risk aversion (\code{C0_given_K0_noRA})
//'  or with risk aversion (\code{C0_given_K0_RA})
//'  @param timeIndex_var Time vector of t=0,..,T-1 moments
//'  
//' @export
// [[Rcpp::export]]
double C0_given_K0_noRA(NumericVector income,
                        NumericVector timeIndex,
                        double K0,
                        double r){
  
  
  double sum_betaY=0;
  double sum_beta=0;
  
  for (int i=0; i < income.length(); ++i){
    sum_beta += 1/pow(1+r,timeIndex[i]);
    sum_betaY += income[i]/pow(1+r,timeIndex[i]);
  }
  
  // C0 by consumption unit
  double C0 = (K0 + sum_betaY)/sum_beta;
  
  return C0; 
}

//' @rdname C0_given_K0_noRA
//' @param gamma Risk aversion coefficient
//' @export
// [[Rcpp::export]]
double C0_given_K0_RA(NumericVector income,
                      NumericVector timeIndex,
                      double K0,
                      double r,
                      double gamma){
  
  
  double sum_betaY=0;
  double sum_beta=0;
  
  for (int i=0; i < income.length(); ++i){
    if (1-gamma>=0){
      sum_beta += pow(1+r,(1-gamma)*timeIndex[i]/gamma);
    } else{
      sum_beta += 1/pow(1+r,(1-gamma)*timeIndex[i]/gamma);
    }
    sum_betaY += income[i]/pow(1+r,timeIndex[i]);
  }
  
  // C0 by consumption unit
  double C0 = (K0 + sum_betaY)/sum_beta;
  
  return C0; 
}


//' @rdname C0_given_K0_noRA
//' @param beta Utility discount factor
//' @export
// [[Rcpp::export]]
double C0_given_K0_beta(NumericVector income,
                      NumericVector timeIndex,
                      double K0,
                      double r,
                      double gamma,
                      double beta){
  
  double sigma = 1/gamma;
  double alpha = pow(beta*(1+r),sigma)/(1+r);
  double Ti = income.length()+1;
  
  double sum_alpha=(1-pow(alpha,Ti))/(1-alpha);
  
  double sum_rY=0;

  for (int i=0; i < income.length(); ++i){
    sum_rY += income[i]/pow(1+r,timeIndex[i]);
  }
  
  // C0 by consumption unit
  double C0 = (K0 + sum_rY)/sum_alpha;
  
  return C0; 
}
