#include <Rcpp.h>
using namespace Rcpp;

//' @rdname calibrate_K0
//' @param gamma Risk-aversion coefficient that gives instantaneous utility function curvature
//' @export
//' @seealso \link{simulate_wealth_structural}
// [[Rcpp::export]]
List calibrate_K0_ra(NumericVector age,
                     NumericVector income,
                     NumericVector findetVector,
                     NumericVector K2009vector,
                     NumericVector timeIndex,
                     NumericVector UC,
                     double r=0.02, double gamma = 0.5){
  
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
  
  
  double t = idx2009;
  
  
  // double sumR;
  // 
  // for (int tau=1;tau<idx2009+1;tau++){
  //   if (1>gamma){
  //     sumR += pow(1+r,(tau - tau/gamma));
  //   } else{
  //     sumR += 1/pow(1+r,-(tau - tau/gamma));
  //   }
  // }
  
  double sum_betaY=0;
  double sum_beta=0;
  
  for (int i=0; i < income.length(); ++i){
    if (1-gamma>=0){
      sum_beta += pow(1+r,(1-gamma)*i/gamma);
    } else{
      sum_beta += 1/pow(1+r,(1-gamma)*i/gamma);
    }
    sum_betaY += (income[i]/UC[i])/pow(1+r,i);
  }
  
  
  
  // ==========================
  // II - LIFE CYCLE INCOME
  // ==========================
  
  
  // Terme sum_{i=1}^t (1+r)^t Y_t
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

  // Terms in denominator
  double alpha = pow(1+r, (1-gamma)/gamma);
  double A = pow(1+r,t)*(1-pow(alpha,t))/(1-pow(alpha,T));
  double denom = pow(1+r,t)-A;
    
  
  
  
    
  // Terms in denominator
  // double denom1 = pow(1+r,tVector[idx2009]/gamma)*sumR;
  // double denom = pow(1+r,tVector[idx2009]) - denom1/sum_beta;
  // 
  // 
  // double K0 = K2009;
  // K0 += (
  //   -capitalizedBefore2009*pow(1+r,tVector[idx2009])  + capitalizedAfter*denom1/sum_beta
  // );
  // K0 /= denom;
  
  double K0 = K2009;
  K0 += (
    -capitalizedBefore2009*pow(1+r,tVector[idx2009])  + A*capitalizedAfter
  );
  K0 /= denom;
  
  
  
  // ==========================
  // IV - COMPUTE C0
  // ==========================
  
  
  
  // C0 by consumption unit
  double C0 = (K0 + sum_betaY)/sum_beta;
  
  // K0 is at individual level
  // K0*=UC(0);
  
  
  // // RETURN R DATAFRAME
  return Rcpp::List::create(
    Named("K0") = K0,
    Named("C0") = C0);
  
  
}




//' @rdname calibrate_K0
//' @param beta Utility discount rate
//' @export
// [[Rcpp::export]]
List calibrate_K0_beta(NumericVector age,
                     NumericVector income,
                     NumericVector findetVector,
                     NumericVector K2009vector,
                     NumericVector timeIndex,
                     NumericVector UC,
                     double r=0.02,
                     double gamma = 0.5,
                     double beta = 1){
  
  // ====================================
  // TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  // WEALTH MEASURED IN 2009
  double K2009 = mean(K2009vector);
  
  // Indexes used
  int idx2009 = which_max(timeIndex);
  
  // Vector of time=0 from beginning of active life
  IntegerVector tVector = seq_len(timeIndex.length())-1;
  
  // Time of death
  int T=max(tVector)+1; 
  
  // K(2009) by consumption unit
  K2009 /= UC(idx2009);
  
  // Intertemporal income substitution parameter
  double sigma = 1/gamma;
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  
  double alpha=pow(beta,sigma)*pow(1+r,sigma)/(1+r);

  
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
  
  
  double t = idx2009;
  
  
  double sum_alpha = (1 - pow(alpha,T))/(1 - alpha);
  //double Rt = pow(1+r,t)*(1 - pow(alpha,t))/(1 - pow(alpha,T));
  double Rt = pow(1+r,t)*pow(alpha,t);
    
  double alpha_num = 0;
  double alpha_denom = 0;
  for (int tau = 0; tau < income.length(); tau++){
    if ((tau>0) & (tau<=t)){
      alpha_num += 1/pow(alpha,tau);
    }
    alpha_denom += pow(alpha,tau);
  }
  
  Rt *= alpha_num/alpha_denom;
  

  // double sum_betaY=0;
  // double sum_beta=0;
  // 
  // for (int i=0; i < income.length(); ++i){
  //   if (1-gamma>=0){
  //     sum_beta += pow(1+r,(1-gamma)*i/gamma);
  //   } else{
  //     sum_beta += 1/pow(1+r,(1-gamma)*i/gamma);
  //   }
  //   sum_betaY += (income[i]/UC[i])/pow(1+r,i);
  // }
  
  double sum_rY = 0;
  for (int i=0; i < income.length(); ++i){
    sum_rY += (income[i]/UC[i])/pow(1+r,i);
  }
  
  
  // ==========================
  // II - LIFE CYCLE INCOME
  // ==========================
  
  
  // Terme sum_{i=1}^t (1+r)^t Y_t
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
  
  // Terms in denominator
  double denom = pow(1+r,t)-Rt;
  
  
  double K0 = K2009;
  K0 -= (
    capitalizedBefore2009*pow(1+r,tVector[idx2009]) - Rt*capitalizedAfter
  );
  K0 /= denom;
  
  
  
  // ==========================
  // IV - COMPUTE C0
  // ==========================
  
  
  
  // C0 by consumption unit
  double C0 = (K0 + sum_rY)/sum_alpha;
  
  // K0 is at individual level
  // K0*=UC(0);
  
  
  // // RETURN R DATAFRAME
  return Rcpp::List::create(
    Named("K0") = K0,
    Named("C0") = C0);
  
  
}


