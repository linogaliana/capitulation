#' Life cycle model
#' 
#' These functions are \code{R}-level function
#'  to run life-cycle model
#'  
#' @details \code{life_cycle_model} for the homogeneous interest
#'  rate model, \code{life_cycle_model_heterogeneity} for the
#'  individual varying interest rate 
#'  
#' @inheritParams life_cycle_model_cpp
#' @inheritParams constructor_interest
#' @inheritParams estimate_K0
#' @param non_ricardian Should we introduce some non-Ricardian household
#' @param id_var Variable name for identifier
#' @param year_var Variable name for time vector
#' @param age_var Age variable
#' @param get_capital_income Logical value indicating whether
#'  we should deduce capital income from wealth
#' @param output_var Wealth variable name that should be used
#' @param return_last Logical value indicating whether we want the sequence
#'  of wealth to be \eqn{\{K_0,...,K_{T-1}\}} (\code{FALSE}, default)
#'  or  \eqn{\{K_1,...,K_{T}\}} (\code{TRUE})
#' @param ... Additional arguments that are currently ignored by \code{life_cycle_model}
#' @export

life_cycle_model <- function(data,
                             r = 0.02,
                             beta = 1,
                             gamma = 0.5,
                             alpha = NULL,
                             r.parameters = NULL,
                             gamma.parameters = NULL,
                             beta.parameters = NULL,
                             non_ricardian = FALSE,
                             observation_year = 2009,
                             use_consumption = FALSE,
                             id_var = "Id",
                             year_var = "annee",
                             wealthvar_survey = "K_observed",
                             income_var = "y_indiv",
                             age_var = "age",
                             output_var = "wealth",
                             probability_survival_var = NULL,
                             non_ricardian_var = NULL,
                             weight_var = NULL,
                             get_capital_income = FALSE,
                             return_last = FALSE,
                             additional_vars = NULL,
                             scale_model = c("level","log","loglog"),
                             ...){
  
  
  
  # Avoid NSE notes
  .<-NULL
  
  args <- list(...)
  
  if (is.null(weight_var)){
    data[, "weight" := 1L]
    weight_var <- "weight"
  }
  
  # SITUATIONS WHERE BETA(1+R) = 0
  if (is.null(r) && !is.null(beta)){
    r <- 1/beta - 1
    print(sprintf("Assuming beta(1+r)=1, assuming r = %s percent", round(100*r, 2)))
  }
  if (is.null(beta) && !is.null(r)){
    beta <- 1/(1+r)
    print(sprintf("Assuming beta(1+r)=1, assuming beta = %s", round(beta, 2)))
  }
  
  
  # HANDLE INHERITANCE  -------------------------
  
  if ('Hgiven_var' %in% names(args)){
    Hgiven_var <- args[['Hgiven_var']]
  } else{
    Hgiven_var <- "tempHg"
  }
  
  if ('Hreceived_var' %in% names(args)){
    Hreceived_var <- args[['Hreceived_var']]
  } else{
    Hreceived_var <- "tempHr"
  }
  
  
  # NO INHERITANCE VARIABLE: COLUMNS OF ZEROS
  if (!(Hgiven_var %in% colnames(data))){
    data[,"tempHg" := 0]
    data.table::setnames(data, old = "tempHg",
                         new = Hgiven_var)
  }
  
  if (!(Hreceived_var %in% colnames(data))){
    data[,"tempHr" := 0]
    data.table::setnames(data, old = "tempHr",
                         new = Hreceived_var)
  }
  
  if (is.null(probability_survival_var)){
    probability_survival_var <- "pi"
    data[, c(probability_survival_var) := 1L]
    fun_to_use <- life_cycle_model_cpp
  } else{
    fun_to_use <- life_cycle_model_cpp_uncertainty
  }
  
  # force function when we want conso
  if (use_consumption){
    fun_to_use <- uncertainty_conso
  }
  
  cols_to_keep <- c(id_var,year_var,income_var,
                    wealthvar_survey, age_var,
                    "tt", weight_var,
                    Hgiven_var, Hreceived_var,
                    probability_survival_var)
  
  
  if (!is.null(additional_vars)) cols_to_keep <- c(cols_to_keep,
                                                   additional_vars[!is.na(additional_vars)])
  
  
  traj_complete <- data[,.SD,
                        .SDcols = cols_to_keep]
  
  # CONSTRUCT MODEL PARAMETERS FROM FORMULAS OR SCALARS -----------
  
  #data[,c('r','gamma','beta') := NULL]
  traj_complete <- constructor_interest(r = r, data = traj_complete,
                                        r.parameters = r.parameters,
                                        ...)
  traj_complete <- constructor_gamma(gamma = gamma, data = traj_complete,
                                     gamma.parameters = gamma.parameters,
                                     ...)
  traj_complete <- constructor_beta(beta = beta, data = traj_complete,
                                    beta.parameters = beta.parameters,
                                    ...)
  if (!is.null(alpha)){
    traj_complete[,'alpha' := alpha]
  }
  
  print(sprintf("r: %s, beta: %s, gamma: %s", r, beta, gamma))
  
  # APPLY C++ FUNCTION -------------------------------------
  
  if (isTRUE(non_ricardian)){
    if (is.null(non_ricardian_var)){
      traj_non_ricardian <- traj_complete[get(Hgiven_var) == 0]
      traj_complete <- traj_complete[get(Hgiven_var) != 0]
    } else{
      traj_non_ricardian <- traj_complete[get(non_ricardian_var)]
      traj_complete <- traj_complete[!get(non_ricardian_var)]
    }
    traj_non_ricardian[,c('wealth') := 0] #non ricardian individuals eat every penny at each period
  }
  
  
  if (!return_last){
    traj_complete[, c(output_var) := fun_to_use(income = get(income_var),
                                                observed_wealth = get(wealthvar_survey),
                                                pi = get(probability_survival_var),
                                                timeIndex = get("tt"),
                                                inheritanceGiven = get(Hgiven_var),
                                                inheritanceReceived = get(Hreceived_var),
                                                r = get('r'),
                                                risk_aversion = get('gamma'),
                                                discount_factor = get('beta'),
                                                scale_model = scale_model),
                  by = id_var]
  } else{
    # no scale_model option
    warning("uncertainty not implemented in this version")
    traj_complete[, c(output_var) := life_cycle_model_cpp_bis(income = get(income_var),
                                                              observed_wealth = get(wealthvar_survey),
                                                              pi = get(probability_survival_var),
                                                              timeIndex = get("tt"),
                                                              inheritanceGiven = get(Hgiven_var),
                                                              inheritanceReceived = get(Hreceived_var),
                                                              r = get('r'),
                                                              risk_aversion = get('gamma'),
                                                              discount_factor = get('beta'),
                                                              scale_model = scale_model,
                                                              return_last = TRUE)[-1],
                  by = id_var]
  }
  
  if (isTRUE(non_ricardian)){
    traj_complete <- data.table::rbindlist(
      list(traj_complete, traj_non_ricardian), 
      use.names=TRUE, fill = TRUE)
  }
  
  if (!('Hgiven_var' %in% names(args))){
    traj_complete[,c(Hgiven_var) := NULL]
  }
  if (!('Hreceived_var' %in% names(args))){
    traj_complete[,c(Hreceived_var) := NULL]
  }
  
  
  if (!get_capital_income) return(traj_complete)
  
  wealth_simulation <- capital_income(simulations = traj_complete,
                                      wealth_var = output_var,
                                      income_var = income_var,
                                      scale = scale_model,
                                      r = r)
  
  
  
  return(wealth_simulation)
}



life_cycle_model2 <- function(data,
                              r = 0.02,
                              beta = 1,
                              gamma = 0.5,
                              observation_year = 2009,
                              id_var = "Id",
                              year_var = "annee",
                              wealthvar_survey = "PATFISOM",
                              income_var = "salaire_tot",
                              age_var = "age",
                              output_var = "wealth",
                              weight_var = NULL,
                              get_capital_income = FALSE,
                              return_last = FALSE,
                              ...){
  
  
  
  # Avoid NSE notes
  .  <- NULL
  V1 <- NULL
  
  args <- list(...)
  
  if (is.null(weight_var)){
    data[, "weight" := 1L]
    weight_var <- "weight"
  }
  
  
  # HANDLE INHERITANCE  -------------------------
  
  if ('Hgiven_var' %in% names(args)){
    Hgiven_var <- args[['Hgiven_var']]
  } else{
    Hgiven_var <- "tempHg"
  }
  
  if ('Hreceived_var' %in% names(args)){
    Hreceived_var <- args[['Hreceived_var']]
  } else{
    Hreceived_var <- "tempHr"
  }
  
  
  # NO INHERITANCE VARIABLE: COLUMNS OF ZEROS
  if (!(Hgiven_var %in% colnames(data))){
    data[,"tempHg" := 0]
    data.table::setnames(data, old = "tempHg",
                         new = Hgiven_var)
  }
  
  if (!(Hreceived_var %in% colnames(data))){
    data[,"tempHr" := 0]
    data.table::setnames(data, old = "tempHr",
                         new = Hreceived_var)
  }
  
  
  traj_complete <- data[,.SD,
                        .SDcols = c(id_var,year_var,income_var,
                                    wealthvar_survey, age_var,
                                    "tt", weight_var,
                                    Hgiven_var, Hreceived_var)]
  
  
  # CONSTRUCT MODEL PARAMETERS FROM FORMULAS OR SCALARS -----------
  
  traj_complete <- constructor_interest(r = r, data = data, ...)
  traj_complete <- constructor_gamma(gamma = gamma, data = traj_complete,
                                     ...)
  traj_complete <- constructor_beta(beta = beta, data = traj_complete,
                                    ...)
  
  
  traj_complete[,'id2' := get(id_var)]
  data2 <- traj_complete[,list(list(.SD)), by = "id2"]
  
  
  # APPLY C++ FUNCTION -------------------------------------
  
  data2[, 'K' := lapply(V1, function(d) life_cycle_model_cpp(income = d[[income_var]],
                                                             observed_wealth = d[[wealthvar_survey]],
                                                             timeIndex = d[["tt"]],
                                                             inheritanceGiven = d[[Hgiven_var]],
                                                             inheritanceReceived = d[[Hreceived_var]],
                                                             r = d[['r']],
                                                             risk_aversion = get('gamma'),
                                                             discount_factor = get('beta'))
  )]
  
  
  
  
  data2_a <- unnest(data2,
                    col = "K", 
                    id = "id2")
  
  data2 <- unnest(data2,
                  col = "V1", 
                  id = "id2")
  
  data2[, c(output_var) := data2_a[['V1']]]
  
  
  if (!('Hgiven_var' %in% names(args))){
    data2[,c(Hgiven_var) := NULL]
  }
  if (!('Hreceived_var' %in% names(args))){
    data2[,c(Hreceived_var) := NULL]
  }
  
  
  if (!get_capital_income) return(data2)
  
  wealth_simulation <- capital_income(simulations = data2,
                                      wealth_var = output_var,
                                      income_var = income_var,
                                      r = r)
  
  
  
  return(wealth_simulation)
}




life_cycle_model2_parallel <- function(data,
                                       r = 0.02,
                                       beta = 1,
                                       gamma = 0.5,
                                       observation_year = 2009,
                                       id_var = "Id",
                                       year_var = "annee",
                                       wealthvar_survey = "PATFISOM",
                                       income_var = "salaire_tot",
                                       age_var = "age",
                                       output_var = "wealth",
                                       weight_var = NULL,
                                       get_capital_income = FALSE,
                                       return_last = FALSE,
                                       ...){
  
  
  
  # Avoid NSE notes
  .  <- NULL
  V1 <- NULL
  
  args <- list(...)
  
  if (is.null(weight_var)){
    data[, "weight" := 1L]
    weight_var <- "weight"
  }
  
  
  # HANDLE INHERITANCE  -------------------------
  
  if ('Hgiven_var' %in% names(args)){
    Hgiven_var <- args[['Hgiven_var']]
  } else{
    Hgiven_var <- "tempHg"
  }
  
  if ('Hreceived_var' %in% names(args)){
    Hreceived_var <- args[['Hreceived_var']]
  } else{
    Hreceived_var <- "tempHr"
  }
  
  
  # NO INHERITANCE VARIABLE: COLUMNS OF ZEROS
  if (!(Hgiven_var %in% colnames(data))){
    data[,"tempHg" := 0]
    data.table::setnames(data, old = "tempHg",
                         new = Hgiven_var)
  }
  
  if (!(Hreceived_var %in% colnames(data))){
    data[,"tempHr" := 0]
    data.table::setnames(data, old = "tempHr",
                         new = Hreceived_var)
  }
  
  
  traj_complete <- data[,.SD,
                        .SDcols = c(id_var,year_var,income_var,
                                    wealthvar_survey, age_var,
                                    "tt", weight_var,
                                    Hgiven_var, Hreceived_var)]
  
  
  traj_complete[,'r' := r]
  
  
  traj_complete[,'id2' := get(id_var)]
  data2 <- traj_complete[,list(list(.SD)), by = "id2"]
  
  
  # APPLY C++ FUNCTION -------------------------------------
  
  cl <- parallel::makeCluster(10L)
  
  data2[, "K" := parallel::parLapplyLB(cl, V1, function(d) life_cycle_model_cpp(income = d[[income_var]],
                                                                                observed_wealth = d[[wealthvar_survey]],
                                                                                timeIndex = d[["tt"]],
                                                                                inheritanceGiven = d[[Hgiven_var]],
                                                                                inheritanceReceived = d[[Hreceived_var]],
                                                                                r = d[['r']],
                                                                                risk_aversion = get('gamma'),
                                                                                discount_factor = get('beta'))
  )]
  
  parallel::stopCluster(cl)
  
  
  data2_a <- unnest(data2,
                    col = "K", 
                    id = "id2")
  
  data2 <- unnest(data2,
                  col = "V1", 
                  id = "id2")
  
  data2[, c(output_var) := data2_a[['V1']]]
  
  
  if (!('Hgiven_var' %in% names(args))){
    data2[,c(Hgiven_var) := NULL]
  }
  if (!('Hreceived_var' %in% names(args))){
    data2[,c(Hreceived_var) := NULL]
  }
  
  
  if (!get_capital_income) return(data2)
  
  wealth_simulation <- capital_income(simulations = data2,
                                      wealth_var = output_var,
                                      income_var = income_var,
                                      r = r)
  
  
  
  return(wealth_simulation)
}


