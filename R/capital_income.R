#' Compute capital income trajectory from wealth simulations
#' 
#' @inheritParams wealth_accumulation
#' @param simulations Household level wealth simulations
#' @param wealth_var Variable name storing household wealth in \code{simulations}
#' @return \code{simulations} with variables \code{rK} and \code{rKY} (rK/(rK+w))
#' @export


capital_income <- function(simulations,
                           wealth_var = "wealth",
                           income_var = "y_real",
                           scale = c("level","log","loglog"),
                           r = 0.02){
  
  . <- NULL
  scale = match.arg(scale)
  
  if (scale != "level"){
    simulations[,c(wealth_var) := exp(get(wealth_var))]
  }
  
  # data <- simulations[!is.na(get(wealth_var))]
  # 
  # 
  # data <- data[order(get('Id'),get('annee'))]
  
  simulations[,'rK' := get(wealth_var)*r]
  simulations[,'Y'  := get(income_var)+get('rK')]  
  simulations[,'rKY':= get('rK')/get('Y')]  
  
  if (scale != "level"){#retour échelle initiale
    simulations[,c(wealth_var) := log(get(wealth_var))]
  }
  
  return(simulations)
}


#' @rdname capital_income
#' @param interest_rate_var Interest rate vector
#' @export

capital_income_heterogeneity <- function(simulations,
                           wealth_var = "wealth",
                           income_var = "y_real",
                           interest_rate_var = "rvector"){
  
  . <- NULL
  
  # data <- simulations[!is.na(get(wealth_var))]
  # 
  # 
  # data <- data[order(get('Id'),get('annee'))]
  
  simulations[,'rK' := get(wealth_var)*get(interest_rate_var)]
  simulations[,'Y'  := get(income_var)+get('rK')]  
  simulations[,'rKY':= get('rK')/get('Y')]  
  
  return(simulations)
}
