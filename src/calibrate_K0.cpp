#include <Rcpp.h>
using namespace Rcpp;

//' Given measured wealth in survey, estimate/calibrate K0 (starting wealth).
//' 
//'  Calibrate K0 and C0 such that (i) individual leaves no wealth when he/she dies
//'  (ii) wealth level is consistent with observed level in 2009 (iii) consumption
//'  is fixed at C0 level. This function is programmed with \code{C++}.
//'  \code{calibrate_K0} is designed for risk neutral agents, \code{calibrate_K0_ra}
//'  for risk adverse agents
//'  
//' @section Warning:
//' To account for the possibility that familial structure evolves, it is possible
//' to give the number of consumption unit of the households. In that case,
//' C0 and K0 are values by consumption unit. To get household level values at
//' starting time $t=0$,
//' \eqn{C0*UC(0)$ and $K0*UC(0)} should be considered. To get household level consumption,
//' at time \eqn{t}, \eqn{C0 \times UC(t)} should be computed
//' 
//' @details
//' In pure permanent income approach with no inheritance, consumption path
//'  is fixed at a level such that agent does not leave any inheritance. The
//'  relationship between \eqn{K(t)} and \eqn{K(t-1)} is given by \eqn{K_t = (1+r)(K_{t-1}+Y_t-C_0)}
//'  with \eqn{C_0 = \frac{K_0+\sum_{t=0}^{T-1} \beta^t Y_t}{\sum_{t=0}^{T-1} \beta^t}}. By
//'  inverse recursion, K(0) can be determined from K(t)
//' @param age Age vector
//' @param income Income vector
//' @param findetVector End of studying year
//' @param K2009vector Measured wealth in survey data expressed as a
//'  vector
//' @param timeIndex Time index equals to zero except for
//'  observation year where it should be equal to 1.
//' @param UC Number of consumption unit in household at time t=0
//' @param r Exogeneous interest rate
//' @return List where first element (\code{K0}) is initial wealth
//'  and second element (\code{C0}) is life-cycle consumption
//' @export
//' @seealso \link{simulate_wealth_structural}
// [[Rcpp::export]]
List calibrate_K0(NumericVector age,
                  NumericVector income,
                  NumericVector findetVector,
                  NumericVector K2009vector,
                  NumericVector timeIndex,
                  NumericVector UC,
                  double r){
  
  // ====================================
  // TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  // WEALTH MEASURED IN 2009
  double K2009 = mean(K2009vector);
  
  // START OF LIFE CYCLE
  // double findet = mean(findetVector);
  
  // HANDLE INDEXES
  // LogicalVector res = (age == findet) ;  
  // NumericVector cols_num = as<NumericVector>(res)  ;
  
  // Indexes used
  //int idxstart = which_max(cols_num);
  int idx2009 = which_max(timeIndex);
  
  // Vector of time=0 from beginning of active life
  IntegerVector tVector = seq_len(timeIndex.length())-1;
  
  // Time of death
  int T=max(tVector)+1; 
  
  // K(2009) by consumption unit
  K2009/=UC(idx2009);
  
  // ==========================
  // II - LIFE CYCLE INCOME
  // ==========================
  
  // Income sequence from t=0 to 2009 (included)
  NumericVector incomebefore2009;
  
  if (idx2009>0){
    incomebefore2009 = income[Rcpp::Range(0, idx2009-1)];
  }
  //NumericVector incomeafter2009 = income[Rcpp::Range(idx2009+1, income.length())];
  
  
  double capitalizedBefore2009=0;
  double capitalizedAfter=0;
  
  
  // Terme sum_{i=1}^t (1+r)^(-t) Y_t
  if (idx2009>0){
    for (int i=0; i < incomebefore2009.length(); ++i){
      capitalizedBefore2009 += (incomebefore2009[i]/UC[i])/pow(1+r,tVector[i]);
    }
  }
  
  
  // Terme sum_{i=1}^{T-1} (1+r)^{-t} Y_t
  for (int i=0; i < income.length(); ++i){
    capitalizedAfter += (income[i]/UC[i])/pow(1+r,tVector[i]);
  }
  
  // ==========================
  // III - COMPUTE K0
  // ==========================
  
  
  double K0 = K2009;
  K0 -= (
    capitalizedBefore2009*pow(1+r,tVector[idx2009]) + capitalizedAfter*(1-pow(1+r,tVector[idx2009]))/(1-1/pow(1+r,T))
  );
  K0 *= (1-1/pow(1+r,T))/(1-1/pow(1+r,T-tVector[idx2009])) ;
  
  
  
  // ==========================
  // IV - COMPUTE K0
  // ==========================
  
  double sum_betaY=0;
  double sum_beta=0;
  
  for (int i=0; i < income.length(); ++i){
    sum_beta += 1/pow(1+r,i);
    sum_betaY += (income[i]/UC[i])/pow(1+r,i);
  }
  
  // C0 by consumption unit
  double C0 = (K0 + sum_betaY)/sum_beta;
  
  // K0 is at individual level
  // K0*=UC(0);

  
  // // RETURN R DATAFRAME
  return Rcpp::List::create(
    Named("K0") = K0,
    Named("C0")= C0);

  
}

