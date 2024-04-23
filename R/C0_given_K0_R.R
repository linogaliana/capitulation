#' Compute initial consumption given initial
#'  wealth in a life-cycle framework (\code{C++} functions)
#'  
#' @inheritParams C0_given_K0_RA
#' @inheritParams C0_given_K0_noRA
#' @param z Household longitudinal data
#' @param timeIndex_var Time index (t=0,...,T-1) variable
#' 
#' @export



C0_given_K0 <- function(z,
                        r = 0.02,
                        gamma=0.5,
                        beta=1,
                        K0 = 0,
                        timeIndex_var = 'tau'){
  

  if ('K0' %in% colnames(z)) z[,'K0' := NULL]
  
  if (is.null(gamma)){
    
    z[, `:=`('C0' = C0_given_K0_noRA(
      income = get('salaire_tot'),
      timeIndex = get(timeIndex_var),
      K0 = K0,
      r = r)
    ), by = 'Id']
    
    return(z)
  }
  
  if (is.null(beta)){
    
    z[, `:=`('C0' = C0_given_K0_RA(
      income = get('salaire_tot'),
      timeIndex = get(timeIndex_var),
      K0 = K0,
      r = r,
      gamma = gamma)
    ), by = 'Id']
    
    return(z)
    
  }
  
  
  z[, `:=`('C0' = C0_given_K0_beta(
    income = get('salaire_tot'),
    timeIndex = get(timeIndex_var),
    K0 = K0,
    r = r,
    gamma = gamma,
    beta = beta)
  ), by = 'Id']  
  
  
  return(z)
}

