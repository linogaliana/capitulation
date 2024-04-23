#include <Rcpp.h>
using namespace Rcpp;


//' Simulate Kt all along time period for a given K0 (starting wealth)
//' 
//' Capitalize wealth given capital dynamics $K_t = (1+r)(K_{t-1}+Y_t-C_0)$,
//'  initial capital (K0), consumption (C0) and income dynamics. Different
//'  functions that are associated with different models
//'  \itemize{
//'     \item \code{simulate_wealth_structural}: simplest life cycle model where
//'       \eqn{\beta =1} and no risk aversion
//'     \item \code{simulate_wealth_structural}: life cycle model with
//'       \eqn{\beta =1 } and risk aversion
//'     \item \code{simulate_wealth_structural}: complete life cycle model with
//'       \eqn{\beta \leq 1} and risk aversion
//'  }
//' 
//' @param income Income vector
//' @param r Exogeneous interest rate
//' @param K0 Capital for the first period
//' @param consumption0 Consumption path
//' @param returnLast Return last element
//' @export
//' @seealso \link{calibrate_K0}
// [[Rcpp::export]]
NumericVector simulate_wealth_structural(NumericVector K0,
                                         NumericVector consumption0,
                                         NumericVector income,
                                         NumericVector UC,
                                         double r=0.02,
                                         bool returnLast=false){
  
  // SHOULD WE SIMULATE UP TO DEATH OR UP TO T-1
  int lastIndex = K0.length();
  if (returnLast) lastIndex+=1;
  
  // Initialize wealth vector
  NumericVector patrimoine_simulation(lastIndex);
  
  // Consumption at life cycle level
  double C0 = consumption0[0];
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0[0];
  
  
  // Recurrence: patrimoine en fin de periode
  // -----------------------------------------------
  
  // K(t+1) = (1+r)*(K(t) + Y(t) - C(t))
  for (int i = 1; i < lastIndex;i++){
    patrimoine_simulation[i] = (1+r)*(patrimoine_simulation[i-1]+income[i-1]-UC[i-1]*C0);
  }
  // NB: C0 is expressed as per consumption unit capital thus should be corrected
  
  
  
  return patrimoine_simulation;
}

//' @rdname simulate_wealth_structural
//' @inheritParams calibrate_K0
//' @export
// [[Rcpp::export]]
NumericVector simulate_wealth_structural_ra(NumericVector K0,
                                         NumericVector consumption0,
                                         NumericVector income,
                                         NumericVector UC,
                                         double r=0.02,
                                         double gamma=0.5,
                                         bool returnLast=false){
  
  // SHOULD WE SIMULATE UP TO DEATH OR UP TO T-1
  int lastIndex = K0.length();
  if (returnLast) lastIndex+=1;
  
  // Initialize wealth vector
  NumericVector patrimoine_simulation(lastIndex);
  
  // Consumption at life cycle level
  NumericVector C(lastIndex, consumption0[0]);
  for (int i=1;i<lastIndex;i++){
    C[i] = C[i-1]*pow(1+r,1/gamma);
  }

  // Initialize recurrence
  patrimoine_simulation[0] = K0[0];
  
  
  // Recurrence: patrimoine en fin de periode
  // -----------------------------------------------
  
  // K(t+1) = (1+r)*(K(t) + Y(t) - C(t))
  for (int i = 1; i < lastIndex;i++){
    patrimoine_simulation[i] = (1+r)*(patrimoine_simulation[i-1]+income[i-1]-UC[i-1]*C[i-1]);
  }
  // NB: C0 is expressed as per consumption unit capital thus should be corrected
  
  
  
  return patrimoine_simulation;
}


//' @rdname simulate_wealth_structural
//' @param beta Utility discount factor
//' @export
// [[Rcpp::export]]
NumericVector simulate_wealth_structural_beta(NumericVector K0,
                                            NumericVector consumption0,
                                            NumericVector income,
                                            NumericVector UC,
                                            double r=0.02,
                                            double gamma=0.5,
                                            double beta=1,
                                            bool returnLast=false){
  
  double sigma=1/gamma;
  
  // SHOULD WE SIMULATE UP TO DEATH OR UP TO T-1
  int lastIndex = K0.length();
  if (returnLast) lastIndex+=1;
  
  // Initialize wealth vector
  NumericVector patrimoine_simulation(lastIndex);
  
  // Consumption at life cycle level
  NumericVector C(lastIndex, consumption0[0]);
  for (int i=1;i<lastIndex;i++){
    C[i] = C[i-1]*pow(beta*(1+r),sigma);
  }

  // Initialize recurrence
  patrimoine_simulation[0] = K0[0];
  
  
  // Recurrence: patrimoine en fin de periode
  // -----------------------------------------------
  
  // K(t+1) = (1+r)*(K(t) + Y(t) - C(t))
  for (int i = 1; i < lastIndex;i++){
    patrimoine_simulation[i] = (1+r)*(patrimoine_simulation[i-1]+income[i-1]-UC[i-1]*C[i-1]);
  }
  // NB: C0 is expressed as per consumption unit capital thus should be corrected
  
  
  
  return patrimoine_simulation;
}


//' @rdname simulate_wealth_structural
//' @inheritParams simulate_wealth_structural_beta
//' @param tau Time vector since active life began 
//' @export
// [[Rcpp::export]]
NumericVector simulate_wealth_structural_beta2(NumericVector K0,
                                              NumericVector consumption0,
                                              NumericVector income,
                                              NumericVector UC,
                                              NumericVector tau,
                                              double r=0.02,
                                              double gamma=0.5,
                                              double beta=1,
                                              bool returnLast=false){
  
  double sigma=1/gamma;
  
  // SHOULD WE SIMULATE UP TO DEATH OR UP TO T-1
  int lastIndex = K0.length();
  if (returnLast) lastIndex+=1;
  
  // Initialize wealth vector
  NumericVector patrimoine_simulation(lastIndex);
  
  // Consumption at life cycle level
  NumericVector C(lastIndex, consumption0[0]);
  for (int i=0;i<lastIndex;i++){
    // C[i] = C[i-1]*pow(beta*(1+r),sigma);
    C[i] *= pow(beta*(1+r),sigma*tau[i]);
  }

  // Initialize recurrence
  patrimoine_simulation[0] = K0[0];
  
  
  // Recurrence: patrimoine en fin de periode
  // -----------------------------------------------
  
  // K(t+1) = (1+r)*(K(t) + Y(t) - C(t))
  for (int i = 1; i < lastIndex;i++){
    patrimoine_simulation[i] = (1+r)*(patrimoine_simulation[i-1]+income[i-1]/UC[i-1] - C[i-1]);
  }
  // NB: C0 is expressed as per consumption unit capital thus should be corrected
  
  
  
  return patrimoine_simulation;
}
