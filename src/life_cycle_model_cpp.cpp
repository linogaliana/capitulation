#include <Rcpp.h>
using namespace Rcpp;

//' Determine K0 in a life-cycle model.
//' 
//' @inheritParams life_cycle_model_cpp
//' @param K2009vector Observed wealth in survey data
//' @param UC Number of consumption units
// [[Rcpp::export]]
List fit_K0_old(NumericVector income,
            NumericVector K2009vector,
            NumericVector timeIndex,
            NumericVector UC,
            double r=0.02,
            double gamma = 0.5,
            double beta = 1){
  
  // IDENTIFY 2009 OBSERVATION (MAX OF 0/1 VECTOR)
  int idx2009 = which_max(timeIndex);
  
  // WEALTH MEASURED IN 2009 (HOUSEHOLD LEVEL)
  double K2009 = K2009vector[0];
  
  // Time of death
  int T=income.length();
  
  // Vector of time=0 from beginning of active life
  IntegerVector tVector = seq_len(T)-1;
  
  // K(2009) by consumption unit
  K2009 /= UC[idx2009];
  
  // Intertemporal income substitution parameter
  double sigma = 1/gamma;
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  
  double alpha=pow(beta*(1+r),sigma)/(1+r);
  
  
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
  double Rt = pow((1+r)*alpha,t);
  
  double alpha_num = 0;
  double alpha_denom = 0;
  for (int tau = 0; tau < T; tau++){
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
  for (int i=0; i < T; ++i){
    sum_rY += (income[i]/UC[i])/pow(1+r,i);
  }
  
  
  // ==========================
  // II - LIFE CYCLE INCOME
  // ==========================
  
  
  // Terme sum_{i=1}^t (1+r)^t Y_t
  if (idx2009>0){
    for (int i=0; i < t; ++i){
      capitalizedBefore2009 += (incomebefore2009[i]/UC[i])/pow(1+r,tVector[i]);
    }
  }
  
  
  // Terme sum_{i=1}^{T-1} (1+r)^{-t} Y_t
  for (int i=0; i < T; ++i){
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
  
  return Rcpp::List::create(
    Named("K0") = K0,
    Named("sum_rY") = sum_rY,
    Named("sum_alpha") = sum_alpha,
    Named("capitalizedBefore2009") = capitalizedBefore2009,
    Named("capitalizedAfter") = capitalizedAfter,
    Named("Rt") = Rt);
  
}


//' Determine K0 in a life-cycle model.
//' 
//' @inheritParams life_cycle_model_cpp
//' @param K2009vector Observed wealth in survey data
//' @param UC Number of consumption units
//' @export
// [[Rcpp::export]]
List fit_K0(NumericVector income,
            NumericVector K2009vector,
            NumericVector timeIndex,
            NumericVector UC,
            NumericVector inheritanceReceived,
            NumericVector inheritanceGiven,
            double r=0.02,
            double gamma = 0.5,
            double beta = 1){
  
  // IDENTIFY 2009 OBSERVATION (MAX OF 0/1 VECTOR)
  int idx2009 = which_max(timeIndex);
  
  // WEALTH MEASURED IN 2009 (HOUSEHOLD LEVEL)
  double K2009 = K2009vector[0];
  
  // Time of death
  int T=income.length();
  
  // y is now income + inheritance received
  income += inheritanceReceived;
  
  // Vector of time=0 from beginning of active life
  IntegerVector tVector = seq_len(T)-1;
  
  // K(2009) by consumption unit
  K2009 /= UC[idx2009];
  
  // Intertemporal income substitution parameter
  double sigma = 1/gamma;
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  
  double alpha=pow(beta*(1+r),sigma)/(1+r);
  
  
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
  
  double sum_alpha = 0;
  
  for (int i=0; i < T; ++i){
    sum_alpha += pow(alpha,i);
  }
  //double Rt = pow(1+r,t)*(1 - pow(alpha,t))/(1 - pow(alpha,T));
  double Rt = pow((1+r)*alpha,t);
  
  double alpha_num = 0;
  for (int tau = 1; tau <= t; tau++){
    alpha_num += 1/pow(alpha,tau);
  }
  
  Rt *= alpha_num/sum_alpha;
  
  
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
  for (int i=0; i < T; ++i){
    sum_rY += (income[i]/UC[i])/pow(1+r,i);
  }
  
  
  // ==========================
  // II - LIFE CYCLE INCOME
  // ==========================
  
  
  // Terme sum_{i=1}^t (1+r)^t Y_t
  if (idx2009>0){
    for (int i=0; i < t; ++i){
      capitalizedBefore2009 += (incomebefore2009[i]/UC[i])/pow(1+r,tVector[i]);
    }
  }
  
  
  // Terme sum_{i=1}^{T-1} (1+r)^{-t} Y_t
  for (int i=0; i < T; ++i){
    capitalizedAfter += (income[i]/UC[i])/pow(1+r,tVector[i]);
  }
  
  // ==========================
  // III - COMPUTE K0
  // ==========================
  
  // Terms in denominator
  double denom = pow(1+r,t)-Rt;
  
  double Hg = inheritanceGiven[0]; //Bequest column: 0s and Hg
  Hg *= Rt/pow(1+r,T);
  
  double K0 = K2009;
  K0 -= Hg ;
  K0 -= (
    capitalizedBefore2009*pow(1+r,tVector[idx2009]) - Rt*capitalizedAfter
  );
  K0 /= denom;
  
  return Rcpp::List::create(
    Named("K0") = K0,
    Named("Hg") = Hg,
    Named("sum_rY") = sum_rY,
    Named("sum_alpha") = sum_alpha,
    Named("capitalizedBefore2009") = capitalizedBefore2009,
    Named("capitalizedAfter") = capitalizedAfter,
    Named("Rt") = Rt);
  
}

//' Intermediate function to compute some parameters
//' @inheritParams life_cycle_model_cpp
//' @param UC Number of consumption units
//' @export
// [[Rcpp::export]]
List param_K0(double r,
              double gamma,
              double beta,
              NumericVector income,
              NumericVector UC){
  
  double sigma = 1/gamma;
  
  // START LIFE WITH ZERO WEALTH
  double K0 = 0 ;
  
  // Indexes used
  double alpha=pow(beta,sigma)*pow(1+r,sigma)/(1+r);
  
  // Time of death
  int T=income.length();
  
  double sum_alpha = 0;
  
  for (int i=0; i < T; ++i){
    sum_alpha += pow(alpha,i);
  }
  
  double sum_rY = 0;
  
  for (int i=0; i < T; ++i){
    sum_rY += (income[i]/UC[i])/pow(1+r,i);
  }
  
  return Rcpp::List::create(
    Named("K0") = K0,
    Named("sum_rY") = sum_rY,
    Named("sum_alpha") = sum_alpha);
  
}



//' [Discarded] Estimate wealth accumulation with a life-cycle model
//' 
//' See \link{life_cycle_model} for the current implementation.
//' Given individualized income trajectory and one
//' observed wealth point in time,
//' determine wealth trajectory in a life-cycle model
//' (see \code{details}). People whom wealth is
//' not observed at any time are assumed to start
//' without wealth (\eqn{K_0 = 0}). Functions differ
//' in the structure of input required or output returned
//' 
//' @details These functions are designed to determine
//'  the full individual wealth dynamics given income
//'  trajectory. It is assumed that an observed wealth
//'  vector \code{K2009vector} is provided where only
//'  one value is meaningful, the one where \code{timeIndex}
//'  is equal to one (observation dummy).
//'  These functions implementat of
//'  Modigliani-Friedman model in a context of
//'  microsimulation where every year income is observed
//' \describe{
//'   \item{life_cycle_model_cpp}{Interest rate
//'   as a parameter. Return as a list:
//'   \itemize{
//'     \item \eqn{K_0} consistent with \eqn{K_t} and
//'      model hypotheses
//'     \item \eqn{C_0} consistent with \eqn{K_0}
//'     \item \eqn{K_t} sequence
//'     }
//'   }
//'   \item{life_cycle_model_cpp2}{Same but only returns
//'    \eqn{K_t} sequence}
//'   \item{life_cycle_model_heterogeneity_cpp}{Interest rate
//'   as a vector. Return as a list:
//'   \itemize{
//'     \item \eqn{K_0} consistent with \eqn{K_t} and
//'      model hypotheses
//'     \item \eqn{C_0} consistent with \eqn{K_0}
//'     \item \eqn{K_t} sequence
//'     }
//'   }
//' }
//'  
//' @inheritParams estimate_K0
//' @inheritParams simulate_wealth_structural
//' @param K2009vector Observed wealth in survey data or
//'  missing value (in that case K0 will be 0) stored
//'  into long vector
//' @param timeIndex A 0/1 vector where 1 indicates
//'  year where individual is observed on wealth data
//' @param inheritanceGiven Vector storing inheritance that
//'  an individual wants to transmit when dying. Stored into
//'  long vector (i.e. a vector where each
//'  row presents the same value). Missing values are not
//'  allowed
//' @param inheritanceReceived Vector storing inheritance that
//'  an individual receives every year. Zero values when no
//'  inheritance is received and positive amount when
//'  the individual receives.
//' @export
// [[Rcpp::export]]
List life_cycle_model_cpp_old(NumericVector income,
                          NumericVector K2009vector,
                          NumericVector timeIndex,
                          NumericVector UC,
                          double r=0.02,
                          double gamma = 0.5,
                          double beta = 1,
                          bool returnLast=false){
  
  // Intertemporal income substitution parameter
  double sigma = 1/gamma;

  // ====================================
  // TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  List fitK0;
  
  NumericVector inheritanceReceived(income.length());
  NumericVector inheritanceGiven(income.length());
  
  if (NumericVector::is_na(K2009vector[0])){
    
    // WE DO NOT NEED TO RUN ALL PREVIOUS STEPS,
    //   ONLY A FEW
    
    fitK0 = param_K0(r = r,
                     gamma = gamma,
                     beta = beta,
                     income = income,
                     UC = UC);
    
  } else{
    
    fitK0 = fit_K0(income = income,
                   K2009vector = K2009vector,
                   timeIndex = timeIndex,
                   UC = UC,
                   inheritanceReceived = inheritanceReceived,
                   inheritanceGiven = inheritanceGiven,
                   r=r,
                   gamma = gamma,
                   beta = beta);
    
  }
  
  double K0 = fitK0["K0"];
  double sum_rY = fitK0["sum_rY"] ;
  double sum_alpha = fitK0["sum_alpha"];
  
  
  
  // ==========================
  // IV - COMPUTE C0
  // ==========================
  
  // C0 by consumption unit
  double C0 = (K0 + sum_rY)/sum_alpha;
  
  // K0 is at individual level
  // K0*=UC(0);  
  
  // ==========================
  // V - SIMULATE KT DYNAMICS
  // ==========================  
  
  
  // SHOULD WE SIMULATE UP TO DEATH OR UP TO T-1
  int lastIndex = income.length();
  if (returnLast) lastIndex+=1;
  
  
  // Initialize wealth vector
  NumericVector patrimoine_simulation(lastIndex);
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0;
  
  
  // Consumption at life cycle level
  NumericVector C(lastIndex, C0);
  for (int i=1;i<lastIndex;i++){
    C[i] = C[i-1]*pow(beta*(1+r),sigma);
  }
  
  
  // Recurrence: patrimoine en fin de periode
  // -----------------------------------------------
  
  // K(t+1) = (1+r)*(K(t) + Y(t) - C(t))
  for (int i = 1; i < lastIndex;i++){
    patrimoine_simulation[i] = (1+r)*(patrimoine_simulation[i-1]+income[i-1]-UC[i-1]*C[i-1]);
  }
  // NB: C0 is expressed as per consumption unit capital thus should be corrected
  
  
  // RETURN R DATAFRAME
  return Rcpp::List::create(
    Named("K0") = K0,
    Named("C0") = C0,
    Named("Kt") = patrimoine_simulation);
  
}


//' @rdname life_cycle_model_cpp_old
//' @export
// [[Rcpp::export]]
NumericVector life_cycle_model_cpp2_old(NumericVector income,
                                    NumericVector K2009vector,
                                    NumericVector timeIndex,
                                    NumericVector UC,
                                    NumericVector inheritanceGiven,
                                    NumericVector inheritanceReceived,
                                    double r=0.02,
                                    double gamma = 0.5,
                                    double beta = 1,
                                    bool returnLast=false){
  
  // Intertemporal income substitution parameter
  double sigma = 1/gamma;
  
  // ====================================
  // TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  // y is now income + inheritance
  income += inheritanceReceived;  
  
  List fitK0;
  
  if (NumericVector::is_na(K2009vector[0])){
    
    // WE DO NOT NEED TO RUN ALL PREVIOUS STEPS,
    //   ONLY A FEW
    
    fitK0 = param_K0(r = r,
                     gamma = gamma,
                     beta = beta,
                     income = income,
                     UC = UC);
    
  } else{
    
    fitK0 = fit_K0(income = income,
                   K2009vector = K2009vector,
                   timeIndex = timeIndex,
                   UC = UC,
                   inheritanceReceived = inheritanceReceived,
                   inheritanceGiven = inheritanceGiven,
                   r=r,
                   gamma = gamma,
                   beta = beta);
    
  }
  
  double K0 = fitK0["K0"];
  double sum_rY = fitK0["sum_rY"] ;
  double sum_alpha = fitK0["sum_alpha"];
  double Hg = fitK0["Hg"];
  
  
  
  // ==========================
  // IV - COMPUTE C0
  // ==========================
  
  // C0 by consumption unit
  double C0 = (K0 + sum_rY - Hg)/sum_alpha;
  
  // K0 is at individual level
  // K0*=UC(0);  
  
  // ==========================
  // V - SIMULATE KT DYNAMICS
  // ==========================  
  
  
  // SHOULD WE SIMULATE UP TO DEATH OR UP TO T-1
  int lastIndex = income.length();
  if (returnLast) lastIndex+=1;
  
  
  // Initialize wealth vector
  NumericVector patrimoine_simulation(lastIndex);
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0;
  
  
  // Consumption at life cycle level
  NumericVector C(lastIndex, C0);
  for (int i=1;i<lastIndex;i++){
    C[i] = C[i-1]*pow(beta*(1+r),sigma);
    patrimoine_simulation[i] = (1+r)*(patrimoine_simulation[i-1]+income[i-1]-UC[i-1]*C[i-1]);
  }
  
  
  // Recurrence: patrimoine en fin de periode
  // -----------------------------------------------
  
  // K(t+1) = (1+r)*(K(t) + Y(t) - C(t))
  // NB: C0 is expressed as per consumption unit capital thus should be corrected
  
  
  // RETURN R DATAFRAME
  return patrimoine_simulation;
  
}





//' Estimate wealth accumulation with a life-cycle model
//' 
//' Given individualized income trajectory, one
//' observed wealth point in time and a model prediction for
//' inheritance,
//' determine wealth trajectory in a life-cycle model.
//' This implementation uses the closed form formula
//' presented in the paper. 
//' 
//' @details These functions are designed to determine
//'  the full individual wealth dynamics given income
//'  trajectory. It is assumed that an observed wealth
//'  vector \code{observed_wealth} is provided where only
//'  one value is meaningful, the one where \code{timeIndex}
//'  is equal to one (observation dummy).
//'  These functions implement Modigliani-Friedman model 
//'  augmented with inheritance in a context of
//'  microsimulation where every year income is observed
//'  
//' @param income Income vector
//' @param pi Surival probability vector
//' @param observed_wealth Observed wealth in survey data or
//'  stored into long vector (i.e. a vector where each
//'  row presents the same value). Missing values are not
//'  allowed
//' @param timeIndex A 0/1 vector where 1 indicates
//'  year where individual is observed on wealth data
//' @param inheritanceGiven Vector storing inheritance that
//'  an individual wants to transmit when dying. Stored into
//'  long vector (i.e. a vector where each
//'  row presents the same value). Missing values are not
//'  allowed
//' @param inheritanceReceived Vector storing inheritance that
//'  an individual receives every year. Zero values when no
//'  inheritance is received and positive amount when
//'  the individual receives.
//' @param r Interest rate vector. Because we assume an individual
//'  faces the same interest rate every year, only the first value
//'  is used
//' @param gamma Risk-aversion coefficient
//' @param beta Discount factor
//' 
//' @return The sequence of an individual wealth: \eqn{(K_t)_t} vector
//' @export
// [[Rcpp::export]]
NumericVector life_cycle_model_cpp(NumericVector income,
                                   NumericVector observed_wealth,
                                   NumericVector pi, //ignored but helps for r level wrapper
                                   NumericVector timeIndex,
                                   NumericVector inheritanceGiven,
                                   NumericVector inheritanceReceived,
                                   NumericVector r,
                                   NumericVector risk_aversion,
                                   NumericVector discount_factor,
                                   String scale_model = "level",
                                   String outcome = "wealth"
){
  
  double eps = std::numeric_limits<double>::epsilon();
  
  // ================================================
  // I - INTERMEDIATE VECTORS AND VALUES WE WILL USE
  // ================================================  
  
  // Intertemporal income substitution parameter
  double gamma = risk_aversion[0];
  double beta = discount_factor[0];
  double sigma = 1/gamma;
  
  // Interest rate
  double interest = r[0];
  
  // Time of death
  int T=income.length();
  
  if (scale_model == "loglog"){
    income = log(pmax(1, income));
    observed_wealth = log(pmax(1, observed_wealth));
    inheritanceGiven = log(pmax(1, inheritanceGiven));
    inheritanceReceived = log(pmax(1, inheritanceReceived));
  }
  
  
  // y is now income + inheritance
  income += inheritanceReceived;
  double H = inheritanceGiven[0]/pow(1+interest,T);
  
  
  // ALL INTERMEDIATE VECTORS
  NumericVector alpha_power(T);
  NumericVector power_interest(T);
  NumericVector Rt(T) ;
  NumericVector income_actualized(T);   // Terms (1+r)^{-t} with t=0...T-1
  NumericVector cumsum_alpha(T) ;
  NumericVector term_parenthesis(T);
  NumericVector term_denominator(T);
  NumericVector sumincome_to_t(T) ;
  NumericVector patrimoine_simulation(T);   // Initialize wealth vector
  NumericVector H_actualized(T) ;
  NumericVector Ct(T);
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  double alpha=pow(beta*(1+interest),sigma)/(1+interest);
  double sum_alpha = 0;
  double life_cycle_income = 0;
  
  sumincome_to_t[0] += income[0];
  
  // ====================================
  // II - TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  for (int i=0; i < T; ++i){
    alpha_power[i] += pow(alpha,i); // Terms alpha^t with t=0...T-1
    power_interest[i] += pow(1+interest,i);     // Terms (1+r)^t with t=0...T-1
    income_actualized[i] += income[i]/power_interest[i];
    sum_alpha += alpha_power[i];
    life_cycle_income += income_actualized[i];
  }
  
  
  // ==================================
  // III - GET INTERMEDIATE VALUES
  // ==================================
  
  for (int t=1;t<T;++t){
    cumsum_alpha[t] += cumsum_alpha[t-1] + 1/alpha_power[t] ;
    sumincome_to_t[t] += sumincome_to_t[t-1] + income_actualized[t] ;
    Rt[t] += power_interest[t]*alpha_power[t];
    Rt[t] *= cumsum_alpha[t]/sum_alpha;
    term_parenthesis[t] += sumincome_to_t[t-1]*power_interest[t]-Rt[t]*life_cycle_income;
    term_denominator[t] += power_interest[t] - Rt[t];
    H_actualized[t] += Rt[t]*H;
  }
  
  
  // List L = List::create(Named("cumsum_alpha") = cumsum_alpha , _["sumincome_to_t"] = sumincome_to_t,
  //                       Named("Rt") = Rt,
  //                       Named("term_parenthesis") = term_parenthesis ,
  //                       Named("term_denominator") = term_denominator ,
  //                       Named("term_parenthesis") = term_parenthesis,
  //                       power_interest);
  // 
  // return L;
  // 
  
  // // ================================
  // // III - FIT K0
  // // ================================
  // 
  // 
  // IDENTIFY 2009 OBSERVATION (MAX OF 0/1 VECTOR)
  int idx2009 = which_max(timeIndex);
  
  // WEALTH MEASURED IN 2009 (HOUSEHOLD LEVEL)
  double K0 =  observed_wealth[0];
  
  if (idx2009 != 0){
    K0 -= H_actualized[idx2009];
    K0 -= term_parenthesis[idx2009];
    K0 /= std::max(eps, term_denominator[idx2009]); //problem with machine precision sometimes
  }
  
  
  // ==============================
  // IV - SIMULATE KT DYNAMICS
  // ==============================
  
  if (scale_model == "log"){
    K0 = log(std::max(eps,K0)) ;
  }
  if (scale_model == "loglog"){
    K0 = std::max(eps,K0) ;
  }
  
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0;
  
  
  for (int t=1;t<T;++t){
    if (scale_model == "log"){
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*exp(K0) ;
    } else{
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*K0 ;
    }
    patrimoine_simulation[t] += term_parenthesis[t];
    patrimoine_simulation[t] += H_actualized[t] ;
    if (scale_model == "log"){
      patrimoine_simulation[t] = log(
        std::max(eps, patrimoine_simulation[t])
      );
    }
    if (scale_model == "loglog"){
      patrimoine_simulation[t] = std::max(eps,patrimoine_simulation[t]) ;
    }    
  }

  if (outcome == "consumption"){
    for (int t=0;t<(T-1);++t){
      Ct[t] = (1+interest)*patrimoine_simulation[t] + income[t] - patrimoine_simulation[t+1]  ;
      Ct[t] /= (1+interest) ;
    }
    Ct[T-1] = (1+interest)*patrimoine_simulation[T-2] + income[T-1] - H ;
    Ct[T-1] /= (1+interest)  ;
  }
  
  
  // RETURN R DATAFRAME
  return patrimoine_simulation;
}


//' Same function than life_cycle_model_cpp with the
//'  additional possibility of having K_{T+1}
//' @inheritParams life_cycle_model_cpp
//' @param return_last Logical value indicating whether
//'  we would like to return last value
// [[Rcpp::export]]
NumericVector life_cycle_model_cpp_bis(NumericVector income,
                                    NumericVector observed_wealth,
                                    NumericVector pi, //ignored but helps for r level wrapper
                                    NumericVector timeIndex,
                                    NumericVector inheritanceGiven,
                                    NumericVector inheritanceReceived,
                                    NumericVector r,
                                    NumericVector risk_aversion,
                                    NumericVector discount_factor,
                                    String scale_model = "level",
                                    bool return_last = false){
  
  double eps = std::numeric_limits<double>::epsilon();
  
  
  // ================================================
  // I - INTERMEDIATE VECTORS AND VALUES WE WILL USE
  // ================================================  
  
  // Intertemporal income substitution parameter
  double gamma = risk_aversion[0];
  double beta = discount_factor[0];
  double sigma = 1/gamma;
  
  // Interest rate
  double interest = r[0];
  
  // Time of death
  int T=income.length();
  
  if (scale_model == "loglog"){
    income = log(pmax(1, income));
    observed_wealth = log(pmax(1, observed_wealth));
    inheritanceGiven = log(pmax(1, inheritanceGiven));
    inheritanceReceived = log(pmax(1, inheritanceReceived));
  }
  
  
  // y is now income + inheritance
  income += inheritanceReceived;
  double H = inheritanceGiven[0]/pow(1+interest,T);
  
  
  // ALL INTERMEDIATE VECTORS
  NumericVector alpha_power(T);
  NumericVector pi_pow_sigma(T);
  NumericVector power_interest(T);
  NumericVector Rt(T) ;
  NumericVector income_actualized(T);   // Terms (1+r)^{-t} with t=0...T-1
  NumericVector cumsum_alpha(T) ;
  NumericVector term_parenthesis(T);
  NumericVector term_denominator(T);
  NumericVector sumincome_to_t(T) ;
  NumericVector patrimoine_simulation(T);   // Initialize wealth vector
  NumericVector H_actualized(T) ;
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  double alpha=pow(beta*(1+interest),sigma)/(1+interest);
  double sum_alpha = 0;
  double life_cycle_income = 0;
  
  sumincome_to_t[0] += income[0];
  

  for (int i=0; i < T; ++i){
    alpha_power[i] += pow(alpha,i); // Terms alpha^t with t=0...T-1
    power_interest[i] += pow(1+interest,i);     // Terms (1+r)^t with t=0...T-1
    pi_pow_sigma[i] += pow(pi[i], sigma) ;
    income_actualized[i] += income[i]/power_interest[i];
    sum_alpha += pi_pow_sigma[i]*alpha_power[i];
    life_cycle_income += income_actualized[i];
  }
  
  
  // ==================================
  // III - GET INTERMEDIATE VALUES
  // ==================================

  for (int t=1;t<T;++t){
    cumsum_alpha[t] += cumsum_alpha[t-1] + pi_pow_sigma[t]/alpha_power[t] ;
    sumincome_to_t[t] += sumincome_to_t[t-1] + income_actualized[t] ;
    Rt[t] += power_interest[t]*alpha_power[t];
    Rt[t] *= cumsum_alpha[t]/sum_alpha;
    term_parenthesis[t] += sumincome_to_t[t-1]*power_interest[t]-Rt[t]*life_cycle_income;
    term_denominator[t] += power_interest[t] - Rt[t];
    H_actualized[t] += Rt[t]*H;
  }
  
  

  // ================================
  // III - FIT K0
  // ================================
  
  
  // IDENTIFY 2009 OBSERVATION (MAX OF 0/1 VECTOR)
  int idx2009 = which_max(timeIndex);
  
  // WEALTH MEASURED IN 2009 (HOUSEHOLD LEVEL)
  double K0 =  observed_wealth[0];
  
  if (idx2009 != 0){
    K0 -= H_actualized[idx2009];
    K0 -= term_parenthesis[idx2009];
    K0 /=  term_denominator[idx2009];
  }
  
  
  // ==============================
  // IV - SIMULATE KT DYNAMICS
  // ==============================  
  
  if (scale_model == "log"){
    K0 = log(std::max(eps,K0)) ;
  }
  if (scale_model == "loglog"){
    K0 = std::max(eps,K0) ;
  }
  
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0;
  
  
  
  for (int t=1;t<T;++t){
    if (scale_model == "log"){
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*exp(K0) ;
    } else{
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*K0 ;
    }
    patrimoine_simulation[t] += term_parenthesis[t];
    patrimoine_simulation[t] += H_actualized[t] ;
    if (scale_model == "log"){
      patrimoine_simulation[t] = log(
        std::max(eps, patrimoine_simulation[t])
      );
    }
    if (scale_model == "loglog"){
      patrimoine_simulation[t] = std::max(eps,patrimoine_simulation[t]) ;
    }    
  }
  
  
  
  // RETURN R DATAFRAME
  if (!return_last) return patrimoine_simulation;
  
  
  NumericVector K(T+1);
  NumericVector C(T+1);
  double C0 = (K0 + sumincome_to_t[T-1] - H);
  C0 /= sum_alpha;
  
  if (scale_model != "level"){
    patrimoine_simulation = exp(patrimoine_simulation);
  }
    
  
  for (int t=0;t<T;++t){
    K[t] = patrimoine_simulation[t];
    K[t] += income[t] ; //(K_{t+1} = (1+r)*(K_t + y_t - c_t)) we directly integrate + y_t
    C[t] = C0*pow(beta*(1+interest),sigma*t);
  }
  
  double CT=C0*pow(beta*(1+interest),sigma*T);
  
  K[T] = (1+interest)*(K[T-1]-CT) ;
  
  if (scale_model == "log"){
    K[T] = log(
      std::max(eps, K[T])
    );
  }
  if (scale_model == "loglog"){
    K[T] = std::max(eps,K[T]) ;
  }
  
  return K;
  
}


// [[Rcpp::export]]
NumericVector life_cycle_model_cpp_uncertainty(NumericVector income,
                                   NumericVector observed_wealth,
                                   NumericVector pi,
                                   NumericVector timeIndex,
                                   NumericVector inheritanceGiven,
                                   NumericVector inheritanceReceived,
                                   NumericVector r,
                                   NumericVector risk_aversion,
                                   NumericVector discount_factor,
                                   String scale_model = "level"
){
  
  double eps = std::numeric_limits<double>::epsilon();
  
  // ================================================
  // I - INTERMEDIATE VECTORS AND VALUES WE WILL USE
  // ================================================  
  
  // Intertemporal income substitution parameter
  double gamma = risk_aversion[0];
  double beta = discount_factor[0];
  double sigma = 1/gamma;
  
  // Interest rate
  double interest = r[0];
  
  // Time of death
  int T=income.length();
  
  if (scale_model == "loglog"){
    income = log(pmax(1, income));
    observed_wealth = log(pmax(1, observed_wealth));
    inheritanceGiven = log(pmax(1, inheritanceGiven));
    inheritanceReceived = log(pmax(1, inheritanceReceived));
  }
  
  
  // y is now income + inheritance
  income += inheritanceReceived;
  double H = inheritanceGiven[0]/pow(1+interest,T);
  
  
  // ALL INTERMEDIATE VECTORS
  NumericVector alpha_power(T);
  NumericVector pi_pow_sigma(T);
  NumericVector power_interest(T);
  NumericVector Rt(T) ;
  NumericVector income_actualized(T);   // Terms (1+r)^{-t} with t=0...T-1
  NumericVector cumsum_alpha(T) ;
  NumericVector term_parenthesis(T);
  NumericVector term_denominator(T);
  NumericVector sumincome_to_t(T) ;
  NumericVector patrimoine_simulation(T);   // Initialize wealth vector
  NumericVector H_actualized(T) ;
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  double alpha=pow(beta*(1+interest),sigma)/(1+interest);
  double sum_alpha = 0;
  double life_cycle_income = 0;
  
  sumincome_to_t[0] += income[0];
  
  // ====================================
  // II - TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  for (int i=0; i < T; ++i){
    alpha_power[i] += pow(alpha,i); // Terms alpha^t with t=0...T-1
    power_interest[i] += pow(1+interest,i);     // Terms (1+r)^t with t=0...T-1
    pi_pow_sigma[i] += pow(pi[i], sigma) ;
    income_actualized[i] += income[i]/power_interest[i];
    sum_alpha += pi_pow_sigma[i]*alpha_power[i];
    life_cycle_income += income_actualized[i];
  }
  
  
  // ==================================
  // III - GET INTERMEDIATE VALUES
  // ==================================
  
  // NB: par rapport formule papier, chgt var u=t-tau pour faciliter implementation
  
  NumericVector cumsum_pi_sigma(T) ;
  
  for (int t=1;t<T;++t){
    for (int tau=0;tau<(t-1);++tau){
      cumsum_pi_sigma[t] +=  pi_pow_sigma[tau]*alpha_power[tau];
    }
  }
  
  for (int t=1;t<T;++t){
    cumsum_alpha[t] += cumsum_pi_sigma[t]  ;
    //cumsum_alpha[t] += cumsum_alpha[t-1] + 1/alpha_power[t]  ;
    sumincome_to_t[t] += sumincome_to_t[t-1] + income_actualized[t] ;
    Rt[t] += power_interest[t];
    Rt[t] *= cumsum_alpha[t]/sum_alpha;
    term_parenthesis[t] += sumincome_to_t[t-1]*power_interest[t]-Rt[t]*life_cycle_income;
    term_denominator[t] += power_interest[t] - Rt[t];
    H_actualized[t] = Rt[t]*H;
  }
  
  
  // List L = List::create(Named("cumsum_alpha") = cumsum_alpha , _["sumincome_to_t"] = sumincome_to_t,
  //                       Named("Rt") = Rt,
  //                       Named("term_parenthesis") = term_parenthesis ,
  //                       Named("term_denominator") = term_denominator ,
  //                       Named("term_parenthesis") = term_parenthesis,
  //                       power_interest);
  // 
  // return L;
  // 
  
  // // ================================
  // // III - FIT K0
  // // ================================
  // 
  // 
  // IDENTIFY 2009 OBSERVATION (MAX OF 0/1 VECTOR)
  int idx2009 = which_max(timeIndex);
  
  // WEALTH MEASURED IN 2009 (HOUSEHOLD LEVEL)
  double K0 =  observed_wealth[0];
  
  if (idx2009 != 0){
    K0 -= H_actualized[idx2009];
    K0 -= term_parenthesis[idx2009];
    K0 /= std::max(eps, term_denominator[idx2009]); //problem with machine precision sometimes
  }
  
  
  // ==============================
  // IV - SIMULATE KT DYNAMICS
  // ==============================
  
  if (scale_model == "log"){
    K0 = log(std::max(eps,K0)) ;
  }
  if (scale_model == "loglog"){
    K0 = std::max(eps,K0) ;
  }
  
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0;
  
  
  for (int t=1;t<T;++t){
    if (scale_model == "log"){
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*exp(K0) ;
    } else{
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*K0 ;
    }
    patrimoine_simulation[t] += term_parenthesis[t];
    patrimoine_simulation[t] += H_actualized[t] ;
    if (scale_model == "log"){
      patrimoine_simulation[t] = log(
        std::max(eps, patrimoine_simulation[t])
      );
    }
    if (scale_model == "loglog"){
      patrimoine_simulation[t] = std::max(eps,patrimoine_simulation[t]) ;
    }    
  }
  
  // RETURN R DATAFRAME
  return patrimoine_simulation;
}


//' Same function than life_cycle_model_cpp with the
//'  additional possibility of having K_{T+1}
//' @inheritParams life_cycle_model_cpp
//' @param return_last Logical value indicating whether
//'  we would like to return last value
// [[Rcpp::export]]
NumericVector life_cycle_model_cpp_bis_uncertainty(NumericVector income,
                                       NumericVector observed_wealth,
                                       NumericVector pi,
                                       NumericVector timeIndex,
                                       NumericVector inheritanceGiven,
                                       NumericVector inheritanceReceived,
                                       NumericVector r,
                                       NumericVector risk_aversion,
                                       NumericVector discount_factor,
                                       String scale_model = "level",
                                       bool return_last = false){
  
  double eps = std::numeric_limits<double>::epsilon();
  
  
  // ================================================
  // I - INTERMEDIATE VECTORS AND VALUES WE WILL USE
  // ================================================  
  
  // Intertemporal income substitution parameter
  double gamma = risk_aversion[0];
  double beta = discount_factor[0];
  double sigma = 1/gamma;
  
  // Interest rate
  double interest = r[0];
  
  // Time of death
  int T=income.length();
  
  if (scale_model == "loglog"){
    income = log(pmax(1, income));
    observed_wealth = log(pmax(1, observed_wealth));
    inheritanceGiven = log(pmax(1, inheritanceGiven));
    inheritanceReceived = log(pmax(1, inheritanceReceived));
  }
  
  
  // y is now income + inheritance
  income += inheritanceReceived;
  double H = inheritanceGiven[0]/pow(1+interest,T);
  
  
  // ALL INTERMEDIATE VECTORS
  NumericVector alpha_power(T);
  NumericVector power_interest(T);
  NumericVector pi_pow_sigma(T);
  NumericVector Rt(T) ;
  NumericVector income_actualized(T);   // Terms (1+r)^{-t} with t=0...T-1
  NumericVector cumsum_alpha(T) ;
  NumericVector term_parenthesis(T);
  NumericVector term_denominator(T);
  NumericVector sumincome_to_t(T) ;
  NumericVector patrimoine_simulation(T);   // Initialize wealth vector
  NumericVector H_actualized(T) ;
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  double alpha=pow(beta*(1+interest),sigma)/(1+interest);
  double sum_alpha = 0;
  double life_cycle_income = 0;
  
  sumincome_to_t[0] += income[0];
  
  // ====================================
  // II - TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  for (int i=0; i < T; ++i){
    alpha_power[i] += pow(alpha,i); // Terms alpha^t with t=0...T-1
    pi_pow_sigma[i] += pow(std::max(eps,pi[i]), sigma) ;
    power_interest[i] += pow(1+interest,i);     // Terms (1+r)^t with t=0...T-1
    income_actualized[i] += income[i]/power_interest[i];
    sum_alpha += pi_pow_sigma[i]*alpha_power[i];
    life_cycle_income += income_actualized[i];
  }
  
  
  // ==================================
  // III - GET INTERMEDIATE VALUES
  // ==================================
  
  // NB: par rapport formule papier, chgt var u=t-tau pour faciliter implementation
  
  NumericVector cumsum_pi_sigma(T) ;
  
  for (int t=1;t<T;++t){
    for (int tau=0;tau<(t-1);++tau){
      cumsum_pi_sigma[t] +=  pi_pow_sigma[tau]*alpha_power[tau];
    }
  }
  
  for (int t=1;t<T;++t){
    cumsum_alpha[t] += cumsum_pi_sigma[t]  ;
    //cumsum_alpha[t] += cumsum_alpha[t-1] + 1/alpha_power[t]  ;
    sumincome_to_t[t] += sumincome_to_t[t-1] + income_actualized[t] ;
    Rt[t] += power_interest[t];
    Rt[t] *= cumsum_alpha[t]/sum_alpha;
    term_parenthesis[t] += sumincome_to_t[t-1]*power_interest[t]-Rt[t]*life_cycle_income;
    term_denominator[t] += power_interest[t] - Rt[t];
    H_actualized[t] = Rt[t]*H;
  }
  
  
  
  // ================================
  // III - FIT K0
  // ================================
  
  
  // IDENTIFY 2009 OBSERVATION (MAX OF 0/1 VECTOR)
  int idx2009 = which_max(timeIndex);
  
  // WEALTH MEASURED IN 2009 (HOUSEHOLD LEVEL)
  double K0 =  observed_wealth[0];
  
  if (idx2009 != 0){
    K0 -= H_actualized[idx2009];
    K0 -= term_parenthesis[idx2009];
    K0 /=  term_denominator[idx2009];
  }
  
  
  // ==============================
  // IV - SIMULATE KT DYNAMICS
  // ==============================  
  
  if (scale_model == "log"){
    K0 = log(std::max(eps,K0)) ;
  }
  if (scale_model == "loglog"){
    K0 = std::max(eps,K0) ;
  }
  
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0;
  
  
  
  for (int t=1;t<T;++t){
    if (scale_model == "log"){
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*exp(K0) ;
    } else{
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*K0 ;
    }
    patrimoine_simulation[t] += term_parenthesis[t];
    patrimoine_simulation[t] += H_actualized[t] ;
    if (scale_model == "log"){
      patrimoine_simulation[t] = log(
        std::max(eps, patrimoine_simulation[t])
      );
    }
    if (scale_model == "loglog"){
      patrimoine_simulation[t] = std::max(eps,patrimoine_simulation[t]) ;
    }    
  }
  
  
  
  // RETURN R DATAFRAME
  if (!return_last) return patrimoine_simulation;
  
  
  NumericVector K(T+1);
  NumericVector C(T+1);
  double C0 = (K0 + sumincome_to_t[T-1] - H);
  C0 /= sum_alpha;
  
  if (scale_model != "level"){
    patrimoine_simulation = exp(patrimoine_simulation);
  }
  
  
  for (int t=0;t<T;++t){
    K[t] = patrimoine_simulation[t];
    K[t] += income[t] ; //(K_{t+1} = (1+r)*(K_t + y_t - c_t)) we directly integrate + y_t
    C[t] = C0*pow(beta*(1+interest),sigma*t);
  }
  
  double CT=C0*pow(beta*(1+interest),sigma*T);
  
  K[T] = (1+interest)*(K[T-1]-CT) ;
  
  if (scale_model == "log"){
    K[T] = log(
      std::max(eps, K[T])
    );
  }
  if (scale_model == "loglog"){
    K[T] = std::max(eps,K[T]) ;
  }
  
  return K;
  
}







//' A wrapper to transform list into inputs to life cycle model functions
//' @inheritParams life_cycle_model_cpp2_old
//' @param elem List of data
//' @param f Function to apply many times
//' @param id_var Identifier variable name
//' @param annee_var Time vector name
//' @param income_var Income variable name
//' @param observed_wealth_var Observed wealth variable name
//' @param time_var 0-1 variable name with 1 for 2009 value
//' @param weight_var Weight variable. See \link{prepare_data}
//' @param inheritanceGiven_var Inheritance given variable
//' @param inheritanceReceived_var Inheritance received variable
//' @export
//' 
// [[Rcpp::export]]
Rcpp::DataFrame wrapper_function(List elem,
                      Function f,
                      String id_var = "Id",
                      String annee_var = "annee",
                      String income_var = "salaire_tot",
                      String observed_wealth_var = "wealth2009",
                      String time_var = "tt",
                      String weight_var = "UC",
                      String inheritanceGiven_var = "H_given",
                      String inheritanceReceived_var = "H_received",
                      double r=0.02,
                      double gamma = 0.5,
                      double beta = 1,
                      bool returnLast=false){
  
  NumericVector income = elem[income_var];
  NumericVector K2009vector = elem[observed_wealth_var];
  NumericVector timeIndex = elem[time_var];
  NumericVector UC = elem[weight_var];
  NumericVector Id = elem[id_var];
  NumericVector annee = elem[annee_var];
  NumericVector inheritanceGiven = elem[inheritanceGiven_var];
  NumericVector inheritanceReceived = elem[inheritanceReceived_var];
  
  NumericVector x = f(income = income,
             K2009vector = K2009vector,
             timeIndex = timeIndex,
             UC = UC,
             r=r,
             gamma = gamma,
             beta = beta,
             returnLast=returnLast);
  
  NumericVector y = x ;
  int idxmax = x.size() ;
  
  if (returnLast) y = x[Rcpp::Range(1, idxmax-1)];

  return Rcpp::DataFrame::create(
    Named("Id") = Id,
    Named("annee") = annee,
    Named("x") = y
    );  
  
}

//' Life cycle model
//' @inheritParams wrapper_function
//' @inheritParams life_cycle_model_cpp2_old
//' @param input List of data
//' @param Hgiven_var Inheritance given variable
//' @param Hreceived_var Inheritance received variable
//' @export
//' @return A list of dataframe
//' 
// [[Rcpp::export]]
List life_cycle_apply(List input,
                      Function f,
                      String id_var = "Id",
                      String annee_var = "annee",
                      String income_var = "salaire_tot",
                      String observed_wealth_var = "wealth2009",
                      String time_var = "tt",
                      String weight_var = "UC",
                      String Hgiven_var = "tempHg",
                      String Hreceived_var = "tempHr",
                      double r=0.02,
                      double gamma = 0.5,
                      double beta = 1,
                      bool returnLast=false){
  
  // Number of elements in the List input
  int N = input.length();
  
  // Creating a List for output
  List out(N);
  
  for(int i = 0; i < N; ++i) {
    out[i] = wrapper_function(input[i],
                              f,
                              id_var = "Id",
                              annee_var = "annee",
                              income_var = income_var,
                              observed_wealth_var = observed_wealth_var,
                              time_var = time_var,
                              weight_var = weight_var,
                              r=r,
                              gamma = gamma,
                              beta = beta,
                              returnLast=returnLast);
  }
  return out;
  
}


// [[Rcpp::export]]
NumericVector uncertainty_conso(NumericVector income,
                                NumericVector observed_wealth,
                                NumericVector pi,
                                NumericVector timeIndex,
                                NumericVector inheritanceGiven,
                                NumericVector inheritanceReceived,
                                NumericVector r,
                                NumericVector risk_aversion,
                                NumericVector discount_factor,
                                String scale_model = "level"
){
  
  double eps = std::numeric_limits<double>::epsilon();
  
  // ================================================
  // I - INTERMEDIATE VECTORS AND VALUES WE WILL USE
  // ================================================  
  
  // Intertemporal income substitution parameter
  double gamma = risk_aversion[0];
  double beta = discount_factor[0];
  double sigma = 1/gamma;
  
  // Interest rate
  double interest = r[0];
  
  // Time of death
  int T=income.length();
  
  if (scale_model == "loglog"){
    income = log(pmax(1, income));
    observed_wealth = log(pmax(1, observed_wealth));
    inheritanceGiven = log(pmax(1, inheritanceGiven));
    inheritanceReceived = log(pmax(1, inheritanceReceived));
  }
  
  
  // y is now income + inheritance
  income += inheritanceReceived;
  double H = inheritanceGiven[0]/pow(1+interest,T);
  
  
  // ALL INTERMEDIATE VECTORS
  NumericVector alpha_power(T);
  NumericVector pi_pow_sigma(T);
  NumericVector power_interest(T);
  NumericVector Ct(T) ;
  NumericVector Rt(T) ;
  NumericVector income_actualized(T);   // Terms (1+r)^{-t} with t=0...T-1
  NumericVector cumsum_alpha(T) ;
  NumericVector term_parenthesis(T);
  NumericVector term_denominator(T);
  NumericVector sumincome_to_t(T) ;
  NumericVector patrimoine_simulation(T);   // Initialize wealth vector
  NumericVector H_actualized(T) ;
  
  
  // alpha parameter that mixes beta,r and gamma
  // ----------------------------------------------
  double alpha=pow(beta*(1+interest),sigma)/(1+interest);
  double sum_alpha = 0;
  double life_cycle_income = 0;
  
  sumincome_to_t[0] += income[0];
  
  // ====================================
  // II - TRANSFORM VECTORS INTO VALUES
  // ====================================
  
  for (int i=0; i < T; ++i){
    alpha_power[i] += pow(alpha,i); // Terms alpha^t with t=0...T-1
    power_interest[i] += pow(1+interest,i);     // Terms (1+r)^t with t=0...T-1
    pi_pow_sigma[i] += pow(pi[i], sigma) ;
    income_actualized[i] += income[i]/power_interest[i];
    sum_alpha += pi_pow_sigma[i]*alpha_power[i];
    life_cycle_income += income_actualized[i];
  }
  
  
  // ==================================
  // III - GET INTERMEDIATE VALUES
  // ==================================
  
  // NB: par rapport formule papier, chgt var u=t-tau pour faciliter implementation
  
  NumericVector cumsum_pi_sigma(T) ;
  
  for (int t=1;t<T;++t){
    for (int tau=0;tau<(t-1);++tau){
      cumsum_pi_sigma[t] +=  pi_pow_sigma[tau]*alpha_power[tau];
    }
  }
  
  for (int t=1;t<T;++t){
    cumsum_alpha[t] += cumsum_pi_sigma[t]  ;
    //cumsum_alpha[t] += cumsum_alpha[t-1] + 1/alpha_power[t]  ;
    sumincome_to_t[t] += sumincome_to_t[t-1] + income_actualized[t] ;
    Rt[t] += power_interest[t];
    Rt[t] *= cumsum_alpha[t]/sum_alpha;
    term_parenthesis[t] += sumincome_to_t[t-1]*power_interest[t]-Rt[t]*life_cycle_income;
    term_denominator[t] += power_interest[t] - Rt[t];
    H_actualized[t] = Rt[t]*H;
  }
  
  
  // List L = List::create(Named("cumsum_alpha") = cumsum_alpha , _["sumincome_to_t"] = sumincome_to_t,
  //                       Named("Rt") = Rt,
  //                       Named("term_parenthesis") = term_parenthesis ,
  //                       Named("term_denominator") = term_denominator ,
  //                       Named("term_parenthesis") = term_parenthesis,
  //                       power_interest);
  // 
  // return L;
  // 
  
  // // ================================
  // // III - FIT K0
  // // ================================
  // 
  // 
  // IDENTIFY 2009 OBSERVATION (MAX OF 0/1 VECTOR)
  int idx2009 = which_max(timeIndex);
  
  // WEALTH MEASURED IN 2009 (HOUSEHOLD LEVEL)
  double K0 =  observed_wealth[0];
  
  if (idx2009 != 0){
    K0 -= H_actualized[idx2009];
    K0 -= term_parenthesis[idx2009];
    K0 /= std::max(eps, term_denominator[idx2009]); //problem with machine precision sometimes
  }
  
  
  // ==============================
  // IV - SIMULATE KT DYNAMICS
  // ==============================
  
  if (scale_model == "log"){
    K0 = log(std::max(eps,K0)) ;
  }
  if (scale_model == "loglog"){
    K0 = std::max(eps,K0) ;
  }
  
  
  // Initialize recurrence
  patrimoine_simulation[0] = K0;
  
  
  for (int t=1;t<T;++t){
    if (scale_model == "log"){
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*exp(K0) ;
    } else{
      patrimoine_simulation[t] += std::max(eps, term_denominator[t])*K0 ;
    }
    patrimoine_simulation[t] += term_parenthesis[t];
    patrimoine_simulation[t] += H_actualized[t] ;
    if (scale_model == "log"){
      patrimoine_simulation[t] = log(
        std::max(eps, patrimoine_simulation[t])
      );
    }
    if (scale_model == "loglog"){
      patrimoine_simulation[t] = std::max(eps,patrimoine_simulation[t]) ;
    }    
  }
  
  for (int t=0;t<(T-1);++t){
    Ct[t] = (1+interest)*patrimoine_simulation[t] + income[t] - patrimoine_simulation[t+1]  ;
    Ct[t] /= (1+interest) ;
  }
  Ct[T-1] = (1+interest)*patrimoine_simulation[T-2] + income[T-1] - H ;
  Ct[T-1] /= (1+interest)  ;
  return Ct ;
  
}

